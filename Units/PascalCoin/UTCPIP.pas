unit UTCPIP;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

{$IFDEF FPC}
  {$mode objfpc}
{$ENDIF}

{$I config.inc}

{.$DEFINE DelphiSockets}
{$DEFINE Synapse}
{$IFDEF DelphiSockets}{$IFDEF Synapse}DelphiSockets and Synapse are defined! Choose one!{$ENDIF}{$ENDIF}
{$IFNDEF DelphiSockets}{$IFNDEF Synapse}Nor DelphiSockets nor Synapse are defined! Choose one!{$ENDIF}{$ENDIF}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  {$IFDEF Synapse}
  blcksock,
  synsock,  // synsock choose Socket by OS
  {$ENDIF}
  {$IFDEF DelphiSockets}
  Sockets,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  Classes, Sysutils,
  UThread, SyncObjs;

type
  {$IFDEF DelphiSockets}
  TTCPBlockSocket = TCustomIpClient;
  {$ENDIF}

  TNetTcpIpClient = Class(TComponent)
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    function Connect(address : AnsiString; port : Word; timeoutSeconds : Cardinal; stopFlag : PBoolean) : Boolean;
    procedure Disconnect;

    function WaitForData(WaitMilliseconds : Integer) : Boolean;

    function ClientRemoteAddr : AnsiString;
    function BytesReceived : Int64;
    function BytesSent : Int64;

    function Recv(stream : TStream) : Cardinal;
    function Send(Buffer: TMemory; Length: Integer): Integer;

    procedure SetSocks5(address : string; port : Word);
  private
    FTcpBlockSocket : TTCPBlockSocket;
    {$IFDEF Synapse}
    FConnected : Boolean;
    FBytesReceived, FBytesSent : Int64;
    FLock : TCriticalSection;
    {$ENDIF}
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    function GetConnected: Boolean;
    function GetRemoteHost: AnsiString;
    function GetRemotePort: Word;
    procedure SetOnConnect(const Value: TNotifyEvent);
    procedure SetOnDisconnect(const Value: TNotifyEvent);
    {$IFDEF DelphiSockets}
    procedure TCustomIpClient_OnError(Sender: TObject; ASocketError: Integer);
    {$ENDIF}

  public
    property RemoteHost : AnsiString read GetRemoteHost;
    property RemotePort : Word read GetRemotePort;
    property Connected : Boolean read GetConnected;
    property OnConnect : TNotifyEvent read FOnConnect write SetOnConnect;
    property OnDisconnect : TNotifyEvent read FOnDisconnect write SetOnDisconnect;
  end;

  TNetTcpIpClientClass = Class of TNetTcpIpClient;

  {$IFDEF Synapse}
  TNetTcpIpServer = Class;
  TTcpIpServerListenerThread = Class;

  TTcpIpSocketThread = Class(TThread)
  private
    FSock: TTCPBlockSocket;
    FListenerThread : TTcpIpServerListenerThread;
  protected
    procedure Execute; override;
  public
    Constructor Create(AListenerThread : TTcpIpServerListenerThread; ASocket : TSocket);
    Destructor Destroy; override;
  End;

  TTcpIpServerListenerThread = Class(TThread)
  private
    FNetTcpIpServerServer : TNetTcpIpServer;
    FServerSocket : TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    Constructor Create(ANetTcpIpServer : TNetTcpIpServer);
    Destructor Destroy; override;
  End;
  {$ENDIF}

  TNetTcpIpServer = Class(TObject)
  private
    {$IFDEF DelphiSockets}
    FTcpIpServer : TTcpServer;
    {$ENDIF}
    {$IFDEF Synapse}
    FTcpIpServer : TTcpIpServerListenerThread;
    FIp : String;
    FPort : Word;
    FActive : Boolean;
    {$ENDIF}
    FNetClients : TPCThreadList;
    FMaxConnections : Integer;
    FNetTcpIpClientClass : TNetTcpIpClientClass;
    function GetActive: Boolean;
    procedure SetPort(const Value: Word);  // When a connection is established to a new client, a TNetConnection is created (p2p)
    function GetPort: Word;
    procedure OnTcpServerAccept(Sender: TObject; ClientSocket: TTCPBlockSocket);
    procedure SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); virtual; abstract;
    procedure SetActive(const Value: Boolean); virtual;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Property Active : Boolean read GetActive write SetActive;
    Property Ip : String read FIp Write FIp;
    Property Port : Word read GetPort Write SetPort;
    Property MaxConnections : Integer read FMaxConnections Write FMaxConnections;
    Property NetTcpIpClientClass : TNetTcpIpClientClass read FNetTcpIpClientClass write SetNetTcpIpClientClass;
    Function NetTcpIpClientsLock : TList;
    Procedure NetTcpIpClientsUnlock;
    Procedure WaitUntilNetTcpIpClientsFinalized;
  End;


implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  UConst, ULog;

{ TNetTcpIpClient }

function TNetTcpIpClient.BytesReceived: Int64;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.BytesReceived;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FBytesReceived;
  {$ENDIF}
end;

function TNetTcpIpClient.BytesSent: Int64;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.BytesSent;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FBytesSent;
  {$ENDIF}
end;

function TNetTcpIpClient.ClientRemoteAddr: AnsiString;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.RemoteHost+':'+FTcpBlockSocket.RemotePort;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := GetRemoteHost + ':' + IntToStr(GetRemotePort)
  {$ENDIF}
end;

function TNetTcpIpClient.Connect(address : AnsiString; port : Word; timeoutSeconds : Cardinal; stopFlag : PBoolean) : Boolean;
var
  lastError : Cardinal;
  secondsElapsed : Cardinal;
  checkFlag : Boolean;
  previousState : Boolean;
begin
  {$IFDEF DelphiSockets}
  FSocketError := 0;
  Result := FTcpBlockSocket.Connect;
  {$ENDIF}
  {$IFDEF Synapse}
  Try
    Result := false;

    checkFlag := Assigned(stopFlag);

    previousState := FTcpBlockSocket.NonBlockMode;
    FTcpBlockSocket.NonBlockMode := true;
    FTcpBlockSocket.ConnectionTimeout := timeoutSeconds * 1000;
    FTcpBlockSocket.Connect(address, IntToStr(port));
    lastError := FTcpBlockSocket.LastError;
    if lastError = 0 then begin
      FConnected := true;
    end else if (lastError = {$IFDEF UNIX}ESysEWOULDBLOCK{$ELSE}EWOULDBLOCK{$ENDIF}) or
                (lastError = {$IFDEF UNIX}ESysEINPROGRESS{$ELSE}EINPROGRESS{$ENDIF}) then begin
      FConnected := false;
      for secondsElapsed := 0 to timeoutSeconds do begin
        if FTcpBlockSocket.CanWrite(1000) then begin
          FConnected := true;
          break;
        end;
        if checkFlag and stopFlag^then begin
          break;
        end;
      end;
    end;
    FTcpBlockSocket.NonBlockMode := previousState;

    if FConnected then begin
      FTcpBlockSocket.GetSins;
      FConnected := (FTcpBlockSocket.GetRemoteSinIP <> '') and (FTcpBlockSocket.GetRemoteSinPort <> 0);
    end;

    if FConnected and Assigned(FOnConnect) then begin
       FOnConnect(Self);
    end else TLog.NewLog(ltdebug,Classname,'Cannot connect to a server at: '+ address +':' + IntToStr(port) + ' Reason: '+FTcpBlockSocket.GetErrorDescEx);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error Connecting to '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
      Disconnect;
    end;
  End;
  Result := FConnected;
  {$ENDIF}
end;

constructor TNetTcpIpClient.Create(AOwner : TComponent);
begin
  inherited;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket := TTcpClient.Create(Nil);
  FTcpBlockSocket.OnConnect := OnConnect;
  FTcpBlockSocket.OnDisconnect := OnDisconnect;
  FTcpBlockSocket.OnError := TCustomIpClient_OnError;
  {$ENDIF}
  {$IFDEF Synapse}
  FLock := TCriticalSection.Create;
  FTcpBlockSocket := TTCPBlockSocket.Create;
  FTcpBlockSocket.OnAfterConnect := OnConnect;
  FTcpBlockSocket.SocksTimeout := 10000;
  FBytesReceived := 0;
  FBytesSent := 0;
  FConnected := False;
  {$ENDIF}
end;

destructor TNetTcpIpClient.Destroy;
begin
  Disconnect;
  FreeAndNil(FLock);
  FreeAndNil(FTcpBlockSocket);
  inherited;
end;

procedure TNetTcpIpClient.Disconnect;
begin
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.Disconnect;
  {$ENDIF}
  {$IFDEF Synapse}
  FLock.Acquire;
  Try
    if Not FConnected then exit;
    FConnected := false;
    FTcpBlockSocket.CloseSocket;
    if Assigned(FOnDisconnect) then FOnDisconnect(Self);
  Finally
    FLock.Release;
  End;
  {$ENDIF}
end;

function TNetTcpIpClient.GetConnected: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.Connected;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FConnected;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemoteHost: AnsiString;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.RemoteHost;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FTcpBlockSocket.GetRemoteSinIP;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemotePort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := StrToIntDef(FTcpBlockSocket.RemotePort,0);
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FTcpBlockSocket.GetRemoteSinPort;
  {$ENDIF}
end;

function TNetTcpIpClient.Recv(stream : TStream) : Cardinal;
var
  chunk : TMemoryStream;
  received : Integer;
begin
  Result := 0;
  chunk := TMemoryStream.Create;
  try
    chunk.SetSize(4096);

    while FTcpBlockSocket.CanRead(0) do
    begin
      received := FTcpBlockSocket.RecvBuffer(chunk.Memory, chunk.Size);
      if received <= 0 then
      begin
        Disconnect;
        exit;
      end;

      FBytesReceived := FBytesReceived + received;

      stream.Write(chunk.Memory^, received);
      Result := Result + received;
    end;
  finally
    chunk.Free;
  end;
end;

function TNetTcpIpClient.Send(Buffer: TMemory; Length: Integer): Integer;
begin
  Result := FTcpBlockSocket.SendBuffer(Buffer, Length);
  if FTcpBlockSocket.LastError <> 0 then
  begin
    Disconnect;
  end;
  if Result >= 0 then
  begin
    FBytesSent := FBytesSent + Result;
  end;
end;

procedure TNetTcpIpClient.SetSocks5(address : string; port : Word);
begin
  FTcpBlockSocket.SocksType := ST_Socks5;
  FTcpBlockSocket.SocksIP := address;
  FTcpBlockSocket.SocksPort := IntToStr(port);
end;

procedure TNetTcpIpClient.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.OnConnect := FOnConnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetOnDisconnect(const Value: TNotifyEvent);
begin
  FOnDisconnect := Value;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.OnDisconnect := FOnDisconnect;
  {$ENDIF}
end;

{$IFDEF DelphiSockets}
procedure TNetTcpIpClient.TCustomIpClient_OnError(Sender: TObject; ASocketError: Integer);
begin
  SocketError := ASocketError;
  Disconnect;
end;
{$ENDIF}

function TNetTcpIpClient.WaitForData(WaitMilliseconds: Integer): Boolean;
begin
  Result := FTcpBlockSocket.CanRead(WaitMilliseconds);
end;

constructor TNetTcpIpServer.Create;
begin
  inherited;
  FNetTcpIpClientClass := TNetTcpIpClient;
  FTcpIpServer := Nil;
  FIp := '0.0.0.0';
  FMaxConnections := 10;
  {$IFDEF DelphiSockets}
  FTcpIpServer := TTcpServer.Create(Nil);
  FTcpIpServer.OnAccept := OnTcpServerAccept;
  FTcpIpServer.ServerSocketThread.ThreadCacheSize := CT_MaxClientsConnected;
  {$ELSE}
  FActive := false;
  {$ENDIF}
  FNetClients := TPCThreadList.Create;
end;

destructor TNetTcpIpServer.Destroy;
begin
  Active := false;
  {$IFDEF DelphiSockets}
  FreeAndNil(FTcpIpServer);
  {$ENDIF}
  FreeAndNil(FNetClients);
  inherited;
end;

function TNetTcpIpServer.GetActive: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpServer.Active;
  {$ELSE}
  Result := Assigned(FTcpIpServer) And (FActive);
  {$ENDIF}
end;

function TNetTcpIpServer.GetPort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := StrToIntDef(FTcpIpServer.LocalPort,0);
  {$ELSE}
  Result := FPort;
  {$ENDIF}
end;

function TNetTcpIpServer.NetTcpIpClientsLock: TList;
begin
  Result := FNetClients.LockList;
end;

procedure TNetTcpIpServer.NetTcpIpClientsUnlock;
begin
  FNetClients.UnlockList;
end;

procedure TNetTcpIpServer.OnTcpServerAccept(Sender: TObject; ClientSocket: TTCPBlockSocket);
Var n : TNetTcpIpClient;
  oldSocket : TTCPBlockSocket;
begin
  {$IFDEF DelphiSockets}
  If FTcpIpServer.ServerSocketThread.ThreadCacheSize <> MaxConnections then
      FTcpIpServer.ServerSocketThread.ThreadCacheSize := MaxConnections;
  {$ENDIF}

  n := FNetTcpIpClientClass.Create(Nil);
  Try
    oldSocket := n.FTcpBlockSocket;
    n.FTcpBlockSocket := ClientSocket;
    {$IFDEF Synapse}
    n.FConnected := True;
    {$ENDIF}
    FNetClients.Add(n);
    try
      OnNewIncommingConnection(Sender,n);
    finally
      FNetClients.Remove(n);
    end;
  Finally
    n.FTcpBlockSocket := oldSocket;
    FreeAndNil(n);
  End;
end;

procedure TNetTcpIpServer.SetActive(const Value: Boolean);
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer.Active := Value;
  {$ELSE}
  if Value then begin
    if (Assigned(FTcpIpServer)) then exit;
    FTcpIpServer := TTcpIpServerListenerThread.Create(Self);
    FActive := true;
  end else begin
    if (Not Assigned(FTcpIpServer)) then exit;
    FActive := false;
    FTcpIpServer.Terminate;
    FTcpIpServer.WaitFor;
    FreeAndNil(FTcpIpServer);
  end;
  {$ENDIF}
end;

procedure TNetTcpIpServer.SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
begin
  if FNetTcpIpClientClass=Value then exit;
  FNetTcpIpClientClass := Value;
  Active := false;
end;

procedure TNetTcpIpServer.SetPort(const Value: Word);
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer.LocalPort := IntToStr(Value);
  {$ELSE}
  FPort := Value;
  {$ENDIF}
end;

procedure TNetTcpIpServer.WaitUntilNetTcpIpClientsFinalized;
Var l : TList;
begin
  if Active then Active := false;
  Repeat
    l := FNetClients.LockList;
    try
      if (l.Count=0) then exit;
    finally
      FNetClients.UnlockList;
    end;
    sleep(10);
  Until false;
end;

{$IFDEF Synapse}

procedure TTcpIpServerListenerThread.Execute;
var
  clientSocket: TSocket;
  clientThread: TTcpIpSocketThread;
  sockets : TList;
  i : Integer;
begin
  FServerSocket.CreateSocket;
  if FServerSocket.LastError<>0 then begin
    TLog.NewLog(lterror,Classname,'Error initializing the socket: '+FServerSocket.GetErrorDescEx);
    exit;
  end;
  try
    FServerSocket.Family := SF_IP4;
    FServerSocket.SetLinger(true,10000);
    FServerSocket.Bind(FNetTcpIpServerServer.Ip, IntToStr(FNetTcpIpServerServer.Port));
    if FServerSocket.LastError<>0 then begin
      TLog.NewLog(lterror,Classname,'Cannot bind port '+IntToStr(FNetTcpIpServerServer.Port)+': '+FServerSocket.GetErrorDescEx);
      exit;
    end;
    FServerSocket.Listen;
    sockets := TList.Create;
    try
      while (Not Terminated) And (FNetTcpIpServerServer.Active) do
      begin
        If (FServerSocket.CanRead(100)) And (sockets.Count < FNetTcpIpServerServer.MaxConnections) then
        begin
          clientSocket := FServerSocket.Accept;
          if FServerSocket.LastError = 0 then begin
            clientThread := TTcpIpSocketThread.Create(Self, clientSocket);
            sockets.Add(ClientThread);
            clientThread.Suspended := false;
          end;
        end;
        // Clean finished threads
        for i := sockets.Count - 1 downto 0 do
        begin
          clientThread := TTcpIpSocketThread(sockets[i]);
          if clientThread.Finished then
          begin
            sockets.Delete(i);
            clientThread.Free;
          end;
        end;
      end;
    finally
      // Finalize all threads
      for i := 0 to sockets.Count - 1 do
      begin
        // Here we no wait until terminated...
        TTcpIpSocketThread(sockets[i]).FListenerThread := Nil;
        TTcpIpSocketThread(sockets[i]).Terminate;
        TTcpIpSocketThread(sockets[i]).WaitFor;
        TTcpIpSocketThread(sockets[i]).Free;
      end;
      sockets.free;
    end;
  finally
    FServerSocket.CloseSocket;
  end;
end;

constructor TTcpIpServerListenerThread.Create(ANetTcpIpServer: TNetTcpIpServer);
begin
  FServerSocket := TTCPBlockSocket.Create;
  FNetTcpIpServerServer := ANetTcpIpServer;
  FNetTcpIpServerServer.FTcpIpServer := Self;
  inherited Create(false);
end;

destructor TTcpIpServerListenerThread.Destroy;
begin
  FNetTcpIpServerServer.FTcpIpServer := Nil;
  FServerSocket.Free;
  inherited;
end;

constructor TTcpIpSocketThread.Create(AListenerThread: TTcpIpServerListenerThread; ASocket: TSocket);
begin
  FSock := TTCPBlockSocket.Create;
  FSock.Socket := ASocket;
  FListenerThread := AListenerThread;
  inherited Create(true);
end;

destructor TTcpIpSocketThread.Destroy;
begin
  Try
    if FSock.Socket<>INVALID_SOCKET then begin
      FSock.CloseSocket;
    end;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error closign socket: '+E.Message);
    end;
  End;
  Try
    FreeAndNil(FSock);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error destroying socket: '+E.Message);
    end;
  End;
  inherited;
end;

procedure TTcpIpSocketThread.Execute;
begin
  if (Not Terminated) And (Assigned(FSock)) And (Assigned(FListenerThread)) then
    FListenerThread.FNetTcpIpServerServer.OnTcpServerAccept(Self,FSock);
end;

{$ENDIF}

end.

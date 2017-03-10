unit upcdaemon;

{$mode objfpc}{$H+}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, SysUtils, daemonapp,
  SyncObjs, UOpenSSL, UCrypto, UNode, UFileStorage, UFolderHelper, UWalletKeys, UConst, ULog, UNetProtocol,
  IniFiles,
  UThread, URPC, UPoolMining, UAccounts, UAppParams;

Type
  { TPCDaemonThread }

  TPCDaemonThread = Class(TPCThread)
  private
    FAppParams : TAppParams;
  protected
    Procedure BCExecute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TPCDaemon }

  TPCDaemon = Class(TCustomDaemon)
  Private
    FThread : TPCDaemonThread;
    Procedure ThreadStopped (Sender : TObject);
  public
    Function Start : Boolean; override;
    Function Stop : Boolean; override;
    Function Pause : Boolean; override;
    Function Continue : Boolean; override;
    Function Execute : Boolean; override;
    Function ShutDown : Boolean; override;
    Function Install : Boolean; override;
    Function UnInstall: boolean; override;
  end;

  { TPCDaemonMapper }

  TPCDaemonMapper = Class(TCustomDaemonMapper)
  private
    FLog : TLog;
    procedure OnPascalCoinInThreadLog(logtype : TLogType; Time : TDateTime; AThreadID : Cardinal; Const sender, logtext : AnsiString);
  protected
    Procedure DoOnCreate; override;
    Procedure DoOnDestroy; override;
  public
  end;


implementation

Var _FLog : TLog;

{ TPCDaemonThread }

procedure TPCDaemonThread.BCExecute;
var
  FNode : TNode;
  FWalletKeys : TWalletKeysExt;
  FRPC : TRPCServer;
  FMinerServer : TPoolMiningServer;

  procedure InitRPCServer;
  var
    ip : string;
    port : Integer;
  begin
    port := FAppParams.GetValue(CT_PARAM_RPC_PORT, -1);
    if (port<=0) then begin
      FAppParams.SetValue(CT_PARAM_RPC_PORT, CT_RPC_DEFAULT_PORT);
      port := CT_RPC_DEFAULT_PORT;
      TLog.NewLog(ltInfo,ClassName,'Saving RPC server port to IniFile: '+IntToStr(port));
    end;

    if not FAppParams.IsNil(CT_PARAM_RPC_BIND_IP) then begin
      ip := FAppParams.GetValue(CT_PARAM_RPC_BIND_IP, CT_RPC_DEFAULT_BIND_IP);
    end else begin
      ip := CT_RPC_DEFAULT_BIND_IP;
      FAppParams.SetValue(CT_PARAM_RPC_BIND_IP, ip);
    end;

    FRPC := TRPCServer.Create(ip, port);
    FRPC.WalletKeys := FWalletKeys;
    FRPC.Active := true;
    TLog.NewLog(ltInfo, ClassName, 'RPC server is active on port ' + IntToStr(port));
    If FAppParams.GetValue(CT_PARAM_RPC_SAVELOGS, true) then begin
      FAppParams.SetValue(CT_PARAM_RPC_SAVELOGS, true);
      FRPC.LogFileName := TFolderHelper.GetPascalCoinDataFolder + PathDelim + 'pascalcoin_rpc.log';
      TLog.NewLog(ltInfo, ClassName, 'Activating RPC logs on file ' + FRPC.LogFileName);
    end else begin
      FAppParams.SetValue(CT_PARAM_RPC_SAVELOGS, false);
      TLog.NewLog(ltInfo,ClassName,'RPC logs not enabled on IniFile value ' + CT_PARAM_RPC_SAVELOGS + '=0');
    end;
  end;

  Procedure InitRPCMinerServer;
  var
    i : Integer;
    port, maxconnections : Integer;
    s : String;
    ip : String;
    pubkey : TAccountKey;
    errors : AnsiString;
    ECPK : TECPrivateKey;
  Begin
    if not FAppParams.GetValue(CT_PARAM_MINING_SERVER_ACTIVE, true) then begin
      TLog.NewLog(ltinfo, ClassName, Format('Mining server is DISABLED (to enable set %s=1)', [CT_PARAM_MINING_SERVER_ACTIVE]));
      exit;
    end;

    if FAppParams.IsNil(CT_PARAM_MINING_SERVER_PORT) then begin
      port := CT_MINING_SERVER_DEFAULT_PORT;
      FAppParams.SetValue(CT_PARAM_MINING_SERVER_PORT, port);
    end else begin
      port := FAppParams.GetValue(CT_PARAM_MINING_SERVER_PORT, CT_MINING_SERVER_DEFAULT_PORT);
    end;

    ip := FAppParams.GetValue(CT_PARAM_MINING_SERVER_BIND_IP, CT_MINING_SERVER_DEFAULT_BIND_IP);

    pubkey := CT_TECDSA_Public_Nul;
    s := Trim(FAppParams.GetValue(CT_PARAM_MINER_B58_PUBLICKEY,''));
    If (s='') Or (Not TAccountComp.AccountKeyFromImport(s,pubkey,errors)) then begin
      If s<>'' then TLog.NewLog(lterror,Classname,'Invalid INI file public key: '+errors);
      i := 0;
      While (i<FWalletKeys.Count) And (pubkey.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) do begin
        if (FWalletKeys.Key[i].CryptedKey<>'') then pubkey := FWalletKeys[i].AccountKey
        else inc(i);
      end;
      if (pubkey.EC_OpenSSL_NID=CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        // New key
        ECPK := TECPrivateKey.Create;
        try
          ECPK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
          FWalletKeys.AddPrivateKey('RANDOM NEW BY DAEMON '+FormatDateTime('yyyy-mm-dd hh:nn:dd',now),ECPK);
          pubkey := ECPK.PublicKey;
          FAppParams.SetValue(CT_PARAM_MINER_B58_PUBLICKEY, TAccountComp.AccountKeyToExport(pubkey));
          TLog.NewLog(ltInfo,ClassName, 'Generated new pubkey for miner: '+TAccountComp.AccountKeyToExport(pubkey));
        finally
          ECPK.Free;
        end;
      end;
    end else begin
      // pubkey is mine?
      if (FWalletKeys.IndexOfAccountKey(pubkey)<0) then begin
        TLog.NewLog(lterror, classname, 'WARNING: Using a public key without private key in wallet!' + TAccountComp.AccountKeyToExport(pubkey));
      end;
    end;

    i := FWalletKeys.IndexOfAccountKey(pubkey);
    s := Trim(FAppParams.GetValue(CT_PARAM_MINER_NAME, ''));
    if (SameText(s,'TIME')) then begin
      s := FormatDateTime('yyyy-mm-dd hh:nn',Now);
      TLog.NewLog(ltInfo,ClassName,'Generated new miner name: '+s);
    end;
    maxconnections := FAppParams.GetValue(CT_PARAM_MINING_SERVER_MAX_CONNECTIONS, 1000);
    TLog.NewLog(ltinfo,ClassName,Format('Activating RPC Miner Server on %s:%d, name "%s", max conections %d and public key %s',
      [ip, port, s, maxconnections, TAccountComp.AccountKeyToExport(pubkey)]));

    FMinerServer := TPoolMiningServer.Create;
    FMinerServer.UpdateAccountAndPayload(pubkey, s);
    FMinerServer.Ip := ip;
    FMinerServer.Port := port;
    FMinerServer.Active := True;
    FMinerServer.MaxConnections := maxconnections;

    TLog.NewLog(ltinfo,
                ClassName,
                Format('Mining server is ENABLED and is listening on %s:%d. ' +
                       'To change listening ip or port use parameters %s and %s',
                       [ip, port, CT_PARAM_MINING_SERVER_BIND_IP, CT_PARAM_MINING_SERVER_PORT]));
  end;

begin
  FMInerServer := Nil;
  TLog.NewLog(ltinfo,Classname,'START PascalCoin Server');
  try
    try
      FWalletKeys := TWalletKeysExt.Create(Nil);
      // Load Node
      // Check OpenSSL dll
      if Not LoadSSLCrypt then begin
        WriteLn('Cannot load '+SSL_C_LIB);
        WriteLn('To use this software make sure this file is available on you system or reinstall the application');
        raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
      end;
      TCrypto.InitCrypto;
      FWalletKeys.WalletFileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'WalletKeys.dat';
      // Creating Node:
      FNode := TNode.Node;
      // RPC Server
      InitRPCServer;
      Try
        // Check Database
        FNode.Bank.StorageClass := TFileStorage;
        TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'Data';
        // Reading database
        FNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
        FWalletKeys.SafeBox := FNode.Node.Bank.SafeBox;
        FNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
        FNode.Node.NetServer.Active := true;

        // RPC Miner Server
        InitRPCMinerServer;
        Try
          Repeat
            Sleep(100);
          Until Terminated;
        finally
          FreeAndNil(FMinerServer);
        end;
      Finally
        FreeAndNil(FRPC);
      end;
      FNode.NetServer.Active := false;
      TNetData.NetData.Free;
      FreeAndNil(FNode);
    except
      on e:Exception do begin
        TLog.NewLog(lterror,Classname,'Exception '+E.Classname+': '+E.Message);
      end;
    end;
  finally
    TLog.NewLog(ltinfo,Classname,'EXIT PascalCoin Server');
  end;
end;

constructor TPCDaemonThread.Create;
begin
  inherited Create(True);
  FAppParams := TAppParams.Create(Nil);
  If FAppParams.GetValue(CT_PARAM_SaveLogFiles, true) then begin
    _FLog.SaveTypes := CT_TLogTypes_ALL;
    _FLog.FileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'pascalcoin_'+FormatDateTime('yyyymmddhhnn',Now)+'.log';
    FAppParams.SetValue(CT_PARAM_SaveLogFiles, true);
  end else begin
    FAppParams.SetValue(CT_PARAM_SaveLogFiles, false);
  end;
end;

destructor TPCDaemonThread.Destroy;
begin
  FreeAndNil(FAppParams);
  inherited Destroy;
end;


{ TPCDaemon }

procedure TPCDaemon.ThreadStopped(Sender: TObject);
begin
  FreeAndNil(FThread);
end;

function TPCDaemon.Start: Boolean;
begin
  Result:=inherited Start;
  TLog.NewLog(ltinfo,ClassName,'Daemon Start '+BoolToStr(Result));
  FThread:=TPCDaemonThread.Create;
  FThread.OnTerminate:=@ThreadStopped;
  FThread.FreeOnTerminate:=False;
  FThread.Resume;
end;

function TPCDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  TLog.NewLog(ltinfo,ClassName,'Daemon Stop: '+BoolToStr(Result));
  FThread.Terminate;
end;

function TPCDaemon.Pause: Boolean;
begin
  Result:=inherited Pause;
  TLog.NewLog(ltinfo,ClassName,'Daemon pause: '+BoolToStr(Result));
  FThread.Suspend;
end;

function TPCDaemon.Continue: Boolean;
begin
  Result:=inherited Continue;
  TLog.NewLog(ltinfo,ClassName,'Daemon continue: '+BoolToStr(Result));
  FThread.Resume;
end;

function TPCDaemon.Execute: Boolean;
begin
  Result:=inherited Execute;
  TLog.NewLog(ltinfo,ClassName,'Daemon execute: '+BoolToStr(Result));
end;

function TPCDaemon.ShutDown: Boolean;
begin
  Result:=inherited ShutDown;
  TLog.NewLog(ltinfo,ClassName,'Daemon Shutdown: '+BoolToStr(Result));
  FThread.Terminate;
end;

function TPCDaemon.Install: Boolean;
begin
  Result:=inherited Install;
  TLog.NewLog(ltinfo,ClassName,'Daemon Install: '+BoolToStr(Result));
end;

function TPCDaemon.UnInstall: boolean;
begin
  Result:=inherited UnInstall;
  TLog.NewLog(ltinfo,ClassName,'Daemon UnInstall: '+BoolToStr(Result));
end;

{ TPCDaemonMapper }

procedure TPCDaemonMapper.OnPascalCoinInThreadLog(logtype: TLogType;
  Time: TDateTime; AThreadID: Cardinal; const sender, logtext: AnsiString);
Var s : AnsiString;
begin
//  If Not SameText(sender,TPCDaemonThread.ClassName) then exit;
  If logtype in [lterror,ltinfo] then begin
    if AThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
    WriteLn(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(AThreadID,8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  end;
end;

procedure TPCDaemonMapper.DoOnCreate;
Var D : TDaemonDef;
begin
  inherited DoOnCreate;
  WriteLn('');
  WriteLn(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',now)+' Starting PascalCoin server');
  FLog := TLog.Create(Nil);
  FLog.OnInThreadNewLog:=@OnPascalCoinInThreadLog;
  _FLog := FLog;
  D:=DaemonDefs.Add as TDaemonDef;
  D.DisplayName:='Pascal Coin Daemon';
  D.Name:='PascalCoinDaemon';
  D.DaemonClassName:='TPCDaemon';
  D.WinBindings.ServiceType:=stWin32;
end;

procedure TPCDaemonMapper.DoOnDestroy;
begin
  If Assigned(FLog) then begin
    FLog.OnInThreadNewLog:=Nil;
    FreeAndNil(FLog);
  end;
  inherited DoOnDestroy;
end;

end.


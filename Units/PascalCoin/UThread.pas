unit UThread;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

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
{$IFnDEF FPC}
  Windows,
{$ELSE}
  {$IFDEF LINUX}cthreads,{$ENDIF}
{$ENDIF}
  Classes, SyncObjs;

Type
  TPCThread = Class;
  TPCThreadClass = Class of TPCThread;
  TPCThread = Class(TThread)
  private
    FDebugStep: String;
    FStartTickCount : Cardinal;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure BCExecute; virtual; abstract;
  public
    Class Procedure ProtectEnterCriticalSection(Const Sender : TObject; var Lock : TCriticalSection);
    Class Function TryProtectEnterCriticalSection(Const Sender : TObject; MaxWaitMilliseconds : Cardinal; var Lock : TCriticalSection) : Boolean;
    Property DebugStep : String read FDebugStep write FDebugStep;
    property Terminated;
  End;

  TPCThreadList = class
  private
    FList: TList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: Pointer) : Integer;
    procedure Clear;
    procedure Remove(Item: Pointer); inline;
    function LockList: TList;
    function TryLockList(MaxWaitMilliseconds : Cardinal; var lockedList : TList) : Boolean;
    procedure UnlockList; inline;
  end;

implementation

uses
  SysUtils, ULog;

{ TPCThread }

procedure TPCThread.DoTerminate;
begin
  inherited;
end;

procedure TPCThread.Execute;
begin
  FStartTickCount := GetTickCount;
  FDebugStep := '';
  try
    TLog.NewLog(ltdebug,Classname,'Starting Thread');
    Try
      Try
        BCExecute;
        FDebugStep := 'Finalized BCExecute';
      Finally
        Terminate;
      End;
    Except
      On E:Exception do begin
        TLog.NewLog(lterror,Classname,'Exception inside a Thread at step: '+FDebugStep+' ('+E.ClassName+'): '+E.Message);
        Raise;
      end;
    End;
  finally
    TLog.NewLog(ltdebug,Classname,'Finalizing Thread. Working time: '+FormatFloat('0.000',(GetTickCount-FStartTickCount) / 1000)+' sec');
  end;
end;

class procedure TPCThread.ProtectEnterCriticalSection(Const Sender : TObject; var Lock: TCriticalSection);
begin
  if Not Lock.TryEnter then begin
//    TLog.NewLog(ltdebug,Sender.Classname,Format('Locked critical section (WAIT): LockCount:%d RecursionCount:%d Semaphore:%d LockOwnerThread:%s',[
//      Lock.LockCount,Lock.RecursionCount,Lock.LockSemaphore,IntToHex(Lock.OwningThread,8) ]));
    Lock.Acquire;
//    TLog.NewLog(ltdebug,Sender.Classname,Format('UnLocked critical section (ENTER): LockCount:%d RecursionCount:%d Semaphore:%d LockOwnerThread:%s',[
//      Lock.LockCount,Lock.RecursionCount,Lock.LockSemaphore,IntToHex(Lock.OwningThread,8) ]));
  end;
end;

class function TPCThread.TryProtectEnterCriticalSection(const Sender: TObject;
  MaxWaitMilliseconds: Cardinal; var Lock: TCriticalSection): Boolean;
Var tc : Cardinal;
  s : String;
begin
  if MaxWaitMilliseconds>60000 then MaxWaitMilliseconds := 60000;
  tc := GetTickCount;
  Repeat
    Result := Lock.TryEnter;
    if Not Result then sleep(1);
  Until (Result) Or (GetTickCount > (tc + MaxWaitMilliseconds));
  if Not Result then begin
    s := Format('Cannot Protect a critical section by %s after %d milis',
      [Sender.ClassName,MaxWaitMilliseconds]);
    TLog.NewLog(ltdebug,Classname,s);
  end;
end;

{ TPCThreadList }

function TPCThreadList.Add(Item: Pointer) : Integer;
begin
  LockList;
  Try
    Result := FList.Add(Item);
  Finally
    UnlockList;
  End;
end;

procedure TPCThreadList.Clear;
begin
  LockList;
  Try
    FList.Clear;
  Finally
    UnlockList;
  End;
end;

constructor TPCThreadList.Create;
begin
  FLock := TCriticalSection.Create;
  FList := TList.Create;
end;

destructor TPCThreadList.Destroy;
begin
  LockList;
  try
    FreeAndNil(FList);
    inherited Destroy;
  finally
    UnlockList;
    FreeAndNil(FLock);
  end;
end;

function TPCThreadList.LockList: TList;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FLock);
  Result := FList;
end;

procedure TPCThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

function TPCThreadList.TryLockList(MaxWaitMilliseconds: Cardinal;
  var lockedList: TList): Boolean;
begin
  lockedList := FList;
  Result := TPCThread.TryProtectEnterCriticalSection(Self,MaxWaitMilliseconds,FLock);
end;

procedure TPCThreadList.UnlockList;
begin
  FLock.Release;
end;

end.

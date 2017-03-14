program pascallited;

{$mode objfpc}{$H+}
{$define usecthreads}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Classes, daemonapp,
  UCrypto, upcdaemon, UAccounts, UPoolMining, UAppParams;


begin
  Application.Title:='PascalLite Daemon application';
  RegisterDaemonClass(TPCDaemon);
  RegisterDaemonMapper(TPCDaemonMapper);
  TCrypto.InitCrypto;
  Application.Run;
end.


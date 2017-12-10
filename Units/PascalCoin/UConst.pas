unit UConst;

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

Uses UOpenSSLdef;
{$I config.inc}


{$IFNDEF FPC}
  // See http://wiki.freepascal.org/Code_Conversion_Guide
type
  PtrInt = integer;
  PtrUInt = cardinal;
{$ENDIF}

Const
  CT_Genesis_Magic_String_For_Old_Block_Hash :
    AnsiString =
    'February 1 2017 - CNN - Trump puts on a flawless show in picking Gorsuch for Supreme Court ';

  CT_Zero_Block_Proof_of_work_in_Hexa =
    {$IFDEF PRODUCTION}'000000000EAE7A91B748C735A5338A11715D815101E0C075F7C60FA52B769EC7'{$ELSE}{$IFDEF TESTNET}''{$ELSE}{$ENDIF}{$ENDIF};

  CT_AccountsPerBlock = 5;

  CT_NetServer_Port = {$IFDEF PRODUCTION}4004{$ELSE}{$IFDEF TESTNET}4104{$ELSE}{$ENDIF}{$ENDIF};
  CT_RPC_DEFAULT_BIND_IP = '127.0.0.1';
  CT_RPC_DEFAULT_PORT = {$IFDEF PRODUCTION}4003{$ELSE}{$IFDEF TESTNET}4103{$ELSE}{$ENDIF}{$ENDIF};
  CT_MINING_SERVER_DEFAULT_BIND_IP = '0.0.0.0';
  CT_MINING_SERVER_DEFAULT_PORT = {$IFDEF PRODUCTION}4009{$ELSE}{$IFDEF TESTNET}4109{$ELSE}{$ENDIF}{$ENDIF};

  CT_NewLineSecondsAvg: Cardinal = {$IFDEF PRODUCTION}300{$ELSE}{$IFDEF TESTNET}30{$ELSE}{$ENDIF}{$ENDIF};
    // 60*5=300 seconds -> 5 minutes avg
    //   -> 1 day = 86400 seconds -> 1 year = 31536000 seconds (aprox)
    //   Each year = 105120 new blocks (aprox)
    //   -> *5 accounts per block = 525600 new accounts each year (aprox)

  CT_FirstReward: UInt64 = 500000; // 4 decimals... First reward = 50,0000
  CT_MinReward: UInt64 = 10000; // 4 decimals... Min reward = 1,0000
  CT_NewLineRewardDecrease: Cardinal = 420480; // Avg 4 year

  CT_WaitNewBlocksBeforeTransaction = 100;

  CT_RecoverFoundsWaitInactiveCount = 420480;  // After 4 years... if an account has no operations, money will be a reward for a miner!

  CT_MaxTransactionAmount = 1000000000000;
  CT_MaxTransactionFee = 100000000;
  CT_MaxWalletAmount = 10000000000000;
  //
  CT_MinCompactTarget: Cardinal = {$IFDEF PRODUCTION}$24000000{$ELSE}{$IFDEF TESTNET}$17000000{$ELSE}{$ENDIF}{$ENDIF}; // First compact target of block 0

  CT_CalcNewTargetBlocksAverage: Cardinal = 100;
  CT_MaxAccount : Cardinal = $FFFFFFFF;
  CT_MaxBlock : Cardinal = $FFFFFFFF;

  CT_MaxPayloadSize = 255; // Max payload size in bytes
  CT_MaxSecondsDifferenceOfNetworkNodes = 180; // 3 minutes. If a Node has a +- value difference, will be blacklisted

  // For blocks with future timestamp only CT_MaxSecondsFutureBlockTime difference between network time and block time is allowed
  CT_MaxSecondsFutureBlockTime = 15;

  CT_MinServersConnected = 3;
  CT_MaxServersConnected = 8;

  CT_MaxClientsConnected = 125;

  CT_BankToDiskEveryNBlocks = 500; // Build 1.3.0 Changed from 1000 to 500

  CT_Default_EC_OpenSSL_NID = NID_secp256k1;

  CT_BlockChain_Protocol_Version: Word = $0001; // Version 1
  CT_BlockChain_Protocol_Available: Word = $0001; // Build 1.4 Protocol available changed 0->1

  CT_MagicNetIdentification = $5891E4FF; // Unix timestamp Feb 2017

  // Build 1.0.4 - introducing NetProtocol versioning:
  CT_NetProtocol_Version: Word = $0003;
  // IMPORTANT NOTE!!!
  // NetProtocol_Available MUST BE always >= NetProtocol_version
  CT_NetProtocol_Available: Word = $0004;  // Remember, >= NetProtocol_version !!!

  CT_SafeBoxBankVersion : Word = 2;

  CT_MagicIdentificator: AnsiString = 'Pascallite'; //

  // Value of Operations type in Protocol 1
  CT_Op_Transaction = $01;
  CT_Op_Changekey = $02;
  CT_Op_Recover = $03;

  CT_ClientAppVersion : AnsiString = {$IFDEF PRODUCTION}'1.4.8'{$ELSE}{$IFDEF TESTNET}'TESTNET'{$ELSE}{$ENDIF}{$ENDIF};

  CT_Discover_IPs =  'pascallite.ddns.net;pascallite2.ddns.net;pascallite3.ddns.net;pascallite4.dynamic-dns.net;pascallite5.dynamic-dns.net;pascallite.dynamic-dns.net;pascallite2.dynamic-dns.net;pascallite3.dynamic-dns.net';

  CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');

  // App Params
  CT_SETTINGS_FILENAME = 'pascallite.ini';

  CT_LAST_DESTINATIONS_COUNT = 10;

  CT_PARAM_LAST_DESTINATIONS = 'LAST_DESTINATIONS';
  CT_PARAM_GridAccountsStream = 'GridAccountsStream';
  CT_PARAM_GridAccountsPos = 'GridAccountsPos';
  CT_PARAM_DefaultFee = 'DefaultFee';
  CT_PARAM_InternetServerPort = 'InternetServerPort';
  {$IFDEF TESTNET}CT_PARAM_AutomaticMineWhenConnectedToNodes = 'AutomaticMineWhenConnectedToNodes';{$ENDIF}
  CT_PARAM_MinerPrivateKeyType = 'MinerPrivateKeyType';
  CT_PARAM_MinerPrivateKeySelectedPublicKey = 'MinerPrivateKeySelectedPublicKey';
  CT_PARAM_SaveLogFiles = 'SaveLogFiles';
  CT_PARAM_SaveDebugLogs = 'SaveDebugLogs';
  CT_PARAM_ShowLogs = 'ShowLogs';
  CT_PARAM_FirstTime = 'FirstTime';
  CT_PARAM_ShowModalMessages = 'ShowModalMessages';
  {$IFDEF TESTNET}CT_PARAM_MaxCPUs = 'MaxCPUs'; {$ENDIF} //deprecated
  CT_PARAM_PeerCache = 'PeerCache';
  CT_PARAM_TryToConnectOnlyWithThisFixedServers = 'TryToConnectOnlyWithFixedServers';

  CT_PARAM_RPC_BIND_IP = 'RPC_BIND_IP';
  CT_PARAM_RPC_PORT = 'RPC_PORT';
  CT_PARAM_RPC_SAVELOGS = 'RPC_SAVELOGS';
  CT_PARAM_MINING_SERVER_ACTIVE = 'MINING_SERVER_ACTIVE';
  CT_PARAM_MINING_SERVER_BIND_IP = 'MINING_SERVER_BIND_IP';
  CT_PARAM_MINING_SERVER_PORT = 'MINING_SERVER_PORT';
  CT_PARAM_MINING_SERVER_MAX_CONNECTIONS = 'MINING_SERVER_MAX_CONNECTIONS';
  CT_PARAM_MINER_B58_PUBLICKEY = 'MINER_B58_PUBKEY';
  CT_PARAM_MINER_NAME = 'MINER_NAME';
  CT_PARAM_SOCKS5_PROXY = 'SOCKS5_PROXY';

implementation

end.

# PascalLite: User-friendly cryptocurrency

PascalLite is a P2P Cryptocurrency with human-friendly account numbers (addresses) and without need of keeping historical opeartions, blockchain can be safely removed without any harm to users accounts and balances.  
To start using it download precompiled Wallet application from `Releases` section or compile it from the source code.  
Exchanges, processing systems, online shops can use console linux Daemon to control accounts, send and receive payments.  

# Compiling PascalLite from Source

## Daemon (Linux)

Checkout project's repository and build it
```bash
apt-get update && apt-get install git fpc libssl1.1
git clone https://github.com/PascalLite/wallet
cd wallet
fpc -FuUnits/PascalCoin/ -FuSynapse/lib/ -FuUnits/Utils/ pascallited.pp
```

Run PascalLite Daemon in background
```
nohup ./pascallited -r &
```

Configure daemon settings in `~/PascalLite/pascallite.ini`

> If any of the above steps fail for you, follow [the complete build manual](https://github.com/PascalLite/wallet/blob/master/doc/build_complete.md)

## Wallet (Windows/Linux)

1. Install Lazarus IDE 
2. Open `PascalLiteWallet.lpi`
3. Run -> Compile (CTRL + F9)

# RPC API

RPC control port is binded to 127.0.0.1:4003 by default. It serves [HTTP JSON-RPC](http://json-rpc.org/wiki/specification) requests.  
For detailed API description with examples read our [API JSON-RPC documentation](https://github.com/PascalLite/wallet/blob/master/doc/api_json-rpc.md).

# License
 
Distributed under the MIT software license, see the accompanying file LICENSE or visit http://www.opensource.org/licenses/mit-license.php.  

This product includes software developed by the OpenSSL Project and Denis Grinyuk [https://github.com/Arvur/OpenSSL-Delphi](https://github.com/Arvur/OpenSSL-Delphi), and some cryptographic functions inspirated in code written by Ladar Levison and Marco Ferrante.  

Starting point for the project was PascalCoin source code originally written by Albert Molina and available at [https://github.com/PascalCoin/PascalCoin](https://github.com/PascalCoin/PascalCoin)

# Supporting the Project

Donations can be sent directly to PascalLite development account: `0-10`

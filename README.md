# PascalLite: P2P Cryptocurrency without need of historical operations

PascalLite is a P2P Cryptocurrency with human-friendly account numbers (addresses) and without need of keeping historical opeartions, blockchain can be safely removed without any harm to users accounts and balances.  
To start using it download precompiled Wallet application from `Releases` section or compile it from source code.  
Exchanges, processing systems, online shops can use console linux Daemon to control accounts, send and receive payments.  

# Compiling PascalLite from Source

## Wallet (Windows/Ubuntu)

1. Install Lazarus IDE 
2. Open `PascalLiteWallet.lpi`
3. Run -> Compile (CTRL + F9)

## Daemon (Linux)

## Compilation steps

1. Retrieve new lists of packages and install `gcc` and `make`

  ```bash
sudo apt-get update
sudo apt-get install make gcc
  ```

2. Build `Free Pascal Compiler`

  Download FPC source code version appropriate to your platform (ex.: `fpc-3.0.2.x86_64-linux.tar`) from [https://sourceforge.net/projects/freepascal/files/Linux/3.0.2/](https://sourceforge.net/projects/freepascal/files/Linux/3.0.2/)
  ```bash
tar -xvf fpc-3.0.2.x86_64-linux.tar  
cd fpc-3.0.2.x86_64-linux  
./install.sh
  ```
3. Build `OpenSSL`

  Download latest available OpenSSL 1.1.x source code [https://www.openssl.org/source/](https://www.openssl.org/source/)  (ex.: `openssl-1.1.0e.tar.gz`)

  ```bash
tar -xvf openssl-1.1.0e.tar.gz   
cd openssl-1.1.0e.tar.gz   
./config shared --prefix=$(pwd)/output   
make install
  ```

4. Install `lazarus` package

  ```bash
sudo apt-get install lazarus
  ```

5. Get latest release at [https://github.com/xiphon/PascalLite/releases](https://github.com/xiphon/PascalLite/releases) or checkout bleeding edge development version of PascalLite's `master`
6. Change to the root of the source code directory, copy `libcrypto.so.1.1` from step 3, build PascalLite daemon

  ```bash
cd PascalLite  
cp <OPENSSL_FOLDER>/output/lib/libcrypto.so.1.1 .  
fpc -FuUnits/PascalCoin/ -FuSynapse/lib/ -FuUnits/Utils/ pascallited.pp
  ```
7. Run PascalLite daemon in background

  ```bash
nohup ./pascallited -r &
  ```
8. Configure daemon settings in `~/PascalLite/pascallite.ini`

# RPC 

RPC control port is binded to 127.0.0.1:4003 by default. It serves HTTP requsts with JSON payload.  
Both request and response are JSON objects.

> **Request**
> ```
> {  
>   "id": number - request identifier,
>   "method": string - one of the methods supported by PascalLite RPC,
>   "params": object - additional parameters for the specified method,
> }
> ```

> **Response on success**
> ```
> {  
>   "id": number - respective request identifier,
>   "result": object - fields and data are based on the requested RPC method,
>   "jsonrpc": "2.0"
> }
> ```

> **Response on error**
> ```
> {  
>   "id": number - respective request identifier,
>   "error": {
>     "message": string - error message,
>     "code": number - error code
>   },
>   "jsonrpc": "2.0"
>}
>```

## Methods, parameters and examples

**getaccount**
* **account** number  
Number of any existing account in the PascalLite network without checksum field appended (i.e. 0, 5, 43411 etc.)

> `{"id": 1, "method": "getaccount", "params": {"account": 5}}`  
>
> `{"result": {"account": 5,"enc_pubkey": "CA02200068C28485A677EA6520383B2001BF8230EA41FC8A9027EB72B99465B1587FCC97200040120C3178218C53B705698F7C1E7F42DC49EF50B7CA9E5ADC6332F7557C6015","balance": 237060.7542, "n_operation": 30, "updated_b": 16982}, "id": 1,"jsonrpc": "2.0"}`

**getaccountoperations**
* **account** number  
Account number (without checksum field appended)

> `{"id": 1, "method": "getaccountoperations", "params": {"account": 56219}}`  
>
> `{"result":[{"block":11345,"time":1488744081,"opblock":1,"optype":1,"account":56219,"optxt":"Transaction Received from 56215-59","amount":15.7603,"fee":0,"balance":15.7603,"payload":"","sender_account":56215,"dest_account":56219,"ophash":"512C000097DB0000010000004642443636433637354444344639443437303034"},{"block":11345,"time":1488744081,"opblock":0,"optype":2,"account":56219,"optxt":"Change Key to secp256k1","amount":0,"fee":0,"balance":0,"payload":"","enc_pubkey":"CA022000AC3ACF6D35C80845417CAAFF28C00CF27A733CC9282B236BD3343D1BA3F0E069200061697AA82AB6947DB7A95689AEDF6595212EBF97FF7D5EEAF9E6C1BF1458FDB6","ophash":"512C00009BDB0000010000003239304434353143344434423844343133463133"}],"id":1,"jsonrpc":"2.0"}`

**getblock**
* **block** number  
Block index

> `{"id": 1, "method": "getblock", "params": {"block": 7409}}`  
> 
> `{"result":{"block":7409,"enc_pubkey":"CA0220005D034AF7777D9E4F7520C0D8827B74F5C97C44FC0E32B7546D87E5CF950C1310200014CAB82FC4E1423EC16EA16E000EDD9EE34D68ACA906656D943FCACE754C3690","reward":50,"fee":0,"ver":1,"ver_a":1,"timestamp":1487727945,"target":694482558,"nonce":2449493167,"payload":"onwork                    J\\u00C3\\u0084.-----","sbh":"884C88CDECD981C228FF3DD209F7A4259C5F6705D845D93AD78F5603936C73E6","oph":"E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855","pow":"00000000004D9E000C6EEA41C6982216F435F748E11ADA66A7588D213F81A486","operations":0,"hashratekhs":13239559,"maturation":9582},"id":1,"jsonrpc":"2.0"}`

**getblocks**
* **last** number  
Return last N blocks  

Or:  
* **start** number  
Starting block index  
* **end** number  
Last block index  

Or:  
* **start** number  
Starting block index  
* **max** number  
Number of blocks to return

> `{"id": 1, "method": "getblocks", "params": {"start": 7409, "max": 2}}`  
>
> `{"result":[{"block":7411,"enc_pubkey":"CA022000B793C243DBDB337EF87BD0A205770A0AD4338DF1D3A86CE67AE32361A0F38D492000BA7165AB61FE77D5C93D788CFF1B99FB3A07901CC463A7F58F86270056327F9C","reward":50,"fee":0,"ver":1,"ver_a":1,"timestamp":1487729006,"target":695018620,"nonce":1498074272,"payload":"6969696901","sbh":"71C2B7C26A2D035F4E670A9607327D61EB14F8A94790EA087AE202361B72300C","oph":"E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855","pow":"00000000003D39061F78B3DB9B8C19F3D680535523D04DEE835E69394C4D25C2","operations":0,"hashratekhs":13134639,"maturation":9581},{"block":7410,"enc_pubkey":"CA022000FC794EF7F7AE8BB0AF12A3E90B999C499F9B984362C1FEE13F79F8CD2849FE2820003AEC82CCDC5C183CA80D9DCAB7C8E4EFC3A65C36A3507ED050D5AD0F6D1D4F84","reward":50,"fee":0,"ver":1,"ver_a":1,"timestamp":1487728263,"target":694751936,"nonce":3852888025,"payload":"mrjaekinRX480Rig\\/0----------------","sbh":"68D7075B83B70EDC3AD051E4A05E74365B344A1E0779A0BADBCF478AFEE0DF06","oph":"E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855","pow":"0000000000256F2AAECE262DEF226227FA19466634FDA90ECC638571B4D4AE75","operations":0,"hashratekhs":12992418,"maturation":9582},{"block":7409,"enc_pubkey":"CA0220005D034AF7777D9E4F7520C0D8827B74F5C97C44FC0E32B7546D87E5CF950C1310200014CAB82FC4E1423EC16EA16E000EDD9EE34D68ACA906656D943FCACE754C3690","reward":50,"fee":0,"ver":1,"ver_a":1,"timestamp":1487727945,"target":694482558,"nonce":2449493167,"payload":"onwork                    J\\u00C3\\u0084.-----","sbh":"884C88CDECD981C228FF3DD209F7A4259C5F6705D845D93AD78F5603936C73E6","oph":"E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855","pow":"00000000004D9E000C6EEA41C6982216F435F748E11ADA66A7588D213F81A486","operations":0,"hashratekhs":13239559,"maturation":9583}],"id":1,"jsonrpc":"2.0"}`

**findoperation**
* **ophash** string  
Unique operations hash for transaction
> `{"id": 1, "method": "findoperation", "params": {"ophash":  "62420000ED480100050000004538314141333541303839453037423739364334"}}`  
>
>`{"result":{"block":16994,"time":1490131975,"opblock":7,"optype":1,"account":84205,"optxt":"Transaction Sent to 84469-17","amount":-12.25,"fee":0,"payload":"","sender_account":84205,"dest_account":84469,"ophash":"62420000ED480100050000004538314141333541303839453037423739364334"},"id":1,"jsonrpc":"2.0"}`

**sendto**
* **sender** number  
Source account
* **target**: number  
Destination account
* **amount**: float  
Amount of PASL to send
* **fee**: float *Optional*  
Amount of fees to be included into transaction. Default: 0  
* **payload**: string *Optional*  
Hex-encoded data to be included as payload into transaction  
* **payload_method**: string *Optional, ignored if "payload" field is not set*  
Can be one of "none", "dest", "sender".  
"none" - do not encrypt payload (everyone will see payload contents)  
"dest" - encrypt paylaod with destination account's public key (only receiver of the transaction will be able to decode the payload)  
"sender" - encrypt payload with source account public key (only sender will be able to see payload contents)
> `{"id": 1, "method": "sendto", "params": {"sender": 84140, "target": 84135, "amount": 0.9, "fee": 0.1}}`  
>
> `{"result":{"block":0,"time":0,"opblock":-1,"optype":1,"account":84140,"optxt":"Transaction Sent to 84135-14","amount":-0.9,"fee":-0.1,"balance":0,"payload":"","sender_account":84140,"dest_account":84135,"ophash":"00000000AC480100060000003132413239373946443332333133393542454242"},"id":1,"jsonrpc":"2.0"}`

**payloadencrypt**
* **payload** string  
Hex-string with payload data to be encrypted
* **payload_method** string *Optional*  
Can be one of "none", "pubkey", "aes".  
"none" - do nothing, return payload as is.  
"pubkey" - encrypt payload with public key provided.  
If **payload_method** is **"pubkey"** you must provide one of the following parameters:  
* **enc_pubkey** string *Required*  
Public key in encrypted format
* **b58_pubkey** string *Required*  
Public key in Base58 format  
If **payload_method** is **"aes"**:
* **pwd** string *Required*  
Password that will be used to encrypt payload with AES

**payloaddecrypt**
* **payload** string  
Hex-string with payload data to be decrypted
* **pwds** array *Required*  
Passwords to be used if payload was encrypted with AES.  
Provide an empty array if the payload wasn't encrypted with AES.  

**getblockcount**  
No parameters  

**getwalletaccounts**
* **start** number  
Internal index of account to start from. First account in the wallet will have index 0
* **max** number  
Number of accounts to return  
* **enc_pubkey** string *Optional*  
Return accounts that are belong only to specified encryted public key
* **b58_pubkey** string *Optional*  
Return accounts that are belong only to specified Base58 public key

**getwalletpubkey**  
You must specify one of the following parameters:
* **enc_pubkey** string  
Return information about encryted public key
* **b58_pubkey** string  
Return information about Base58 public key

**getwalletpubkeys**
* **start** number  
Internal index of public key to start from. First public key in the wallet will have index 0
* **max** number  
Number of public keys to return  

**getwalletaccountscount**
* **enc_pubkey** string *Optional*  
* **b58_pubkey** string *Optional*  

**changekey**
* **account** number
* **new_enc_pubkey** string *Optional*  
* **new_b58c_pubkey** string *Optional*  
* **fee** float *Optional*  
* **payload** string *Optional*  
* **payload_method** string *Optional*  

**changekeys**
* **accountы** string
* **new_enc_pubkey** string *Optional*  
* **new_b58c_pubkey** string *Optional*  
* **fee** float *Optional*  
* **payload** string *Optional*  
* **payload_method** string *Optional*  

**getwalletcoins**
* **enc_pubkey** string *Optional*  
* **b58_pubkey** string *Optional*  

**getblockoperation**
* **block** number
* **opblock** number

**getblockoperations**
* **block** number
* **start** number *Optional*
* **max** number *Optional*

**getpendings**  
No parameters  

**addnode**
* **nodes** string

**signsendto**

**signchangekey**

**operationsinfo**
* **rawoperations** string

**executeoperations**

**encodepubkey**

**decodepubkey**

**lock**  
No parameters  

**unlock**
* **pwd** string

**setwalletpassword**
* **pwd** string

**nodestatus**  
No parameters  

**getconnections**  
No parameters  

**stopnode**  
No parameters  

**startnode**  
No parameters  

# License
 
Distributed under the MIT software license, see the accompanying file LICENSE or visit http://www.opensource.org/licenses/mit-license.php.  

This product includes software developed by the OpenSSL Project and Denis Grinyuk [https://github.com/Arvur/OpenSSL-Delphi](https://github.com/Arvur/OpenSSL-Delphi), and some cryptographic functions inspirated in code written by Ladar Levison and Marco Ferrante.  

Starting point for the project was PascalCoin source code originally written by Albert Molina and available at [https://github.com/PascalCoin/PascalCoin](https://github.com/PascalCoin/PascalCoin)

# Supporting the Project

Donations can be sent directly to PascalLite development account: `0-10`

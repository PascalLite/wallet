# API reference (JSON RPC)

RPC control port is binded to 127.0.0.1:4003 by default. It serves [HTTP JSON-RPC](http://json-rpc.org/wiki/specification) requests.  
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

# Accounts numeration and string representation

String representation of a particular account consists of `account_number` and `checksum` (2 digits) separated by `-` sign.  
For example, valid accounts are:
```json
0-10
5-70
54321-26
```

Checksum field is intended to reduce mistypes probability.  
To check or calculate a checksum use the given formula:
```json
checksum = (account_number * 101) % 89 + 10
```

Internally and during API calls accounts are represented by `account_number` skipping `checksum` field.  

# Sample Applications and libraries

* [Go](#go)
* [Python](#python)
* [Node.js](#nodejs)

## Go

Install `jsonrpc` package
```bash
go get -u github.com/ybbus/jsonrpc
```

Usage example
```go
package main

import (
    "github.com/ybbus/jsonrpc"
    "fmt"
)

type Account struct {
    Number int `json:"account"`
    Balance float64 `json:"balance"`
    EncryptedPubKey string `json:"enc_pubkey"`
    OperationsCoun int `json:"n_operation"`
    UpdatedBlock int `json:"updated_b"`
}

func main() {
    rpcClient := jsonrpc.NewRPCClient("http://localhost:4003")

    response, _ := rpcClient.CallNamed("getaccount", map[string]interface{}{"account": 5})

    account := Account{}
    response.GetObject(&account)

    fmt.Printf( "Account %d-%02d has %.4f PASL on its balance", 
                account.Number,
                (account.Number * 101) % 89 + 10,
                account.Balance)
}
```
Output
```json
Account 5-70 has 3938.0311 PASL on its balance
```

## Python

Install `jsonrpcclient[requests]` package
```bash
pip install 'jsonrpcclient[requests]'
```

Uasge example
```python
import jsonrpcclient

block = jsonrpcclient.request('http://localhost:4003', 'getblock', block=51513)
print("Block #{} reward is {} PASL".format(block["block"], block["reward"]))
```
Output
```json
Block #51513 reward is 50 PASL
```

## Node.js

Install `json-rpc2` package
```bash
npm install json-rpc2 --save
node
```

Usage example
```javascript
var jsonrpc = require('json-rpc2');
var client = jsonrpc.Client.$create(4003, 'localhost');

client.call('getwalletcoins', {}, function(err, result) {
    console.log("You have " + result.toFixed(4) + " PASL total in your wallet");
});
```
Output
```json
You have 311.5138 PASL total in your wallet
```

# Methods, parameters and examples

[getwalletcoins](#getwalletcoins)  
[getwalletaccounts](#getwalletaccounts)  
[sendto](#sendto)  
[findoperation](#findoperation)  
[payloaddecrypt](#payloaddecrypt)  
[payloadencrypt](#payloadencrypt)  
[getwalletaccountscount](#getwalletaccountscount)  
[getwalletpubkey](#getwalletpubkey)  
[getwalletpubkeys](#getwalletpubkeys)  
[changekey](#changekey)  
[changekeys](#changekeys)  
[lock](#lock)  
[unlock](#unlock)  
[setwalletpassword](#setwalletpassword)  

[getaccount](#getaccount)  
[getaccountoperations](#getaccountoperations)  

[getblockcount](#getblockcount)  
[getblock](#getblock)  
[getblocks](#getblocks)  
[getblockoperation](#getblockoperation)  
[getblockoperations](#getblockoperations)  

[nodestatus](#nodestatus)  
[getpendings](#getpendings)  
[getconnections](#getconnections)  
[addnode](#addnode)  
[stopnode](#stopnode)  
[startnode](#startnode)  

[signsendto](#signsendto)  
[signchangekey](#signchangekey)  
[executeoperations](#executeoperations)  
[operationsinfo](#operationsinfo)  
[encodepubkey](#encodepubkey)  
[decodepubkey](#decodepubkey)  
___
### getwalletcoins

Parameters
* **enc_pubkey** `string` *Optional*  
* **b58_pubkey** `string` *Optional*  

Request
```
wget --post-data '{"method":"getwalletcoins","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": 307.9967
}
```
___
### getwalletaccounts

Parameters
* **start** `number`  
Internal index of account to start from. First account in the wallet will have index 0
* **max** `number`  
Number of accounts to return  
* **enc_pubkey** `string` *Optional*  
Return accounts that are belong only to specified encryted public key
* **b58_pubkey** `string` *Optional*  
Return accounts that are belong only to specified Base58 public key

Request
```
wget --post-data '{"method":"getwalletaccounts","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "account": 12495,
            "balance": 0,
            "enc_pubkey": "CA0220003E2F08F1E098AE2E2AECDB9169D6355376FD9864292E3DCFB750F67698C5663A20005176415F852B97645F1DAAB20DFA007DB923C06C9D9DA59CAC548519124E4AD0",
            "n_operation": 5,
            "updated_b": 51208
        },
        {
            "account": 3497,
            "balance": 10.1231,
            "enc_pubkey": "CA0220003E2F08F1E098AE2E2AECDB9169D6355376FD9864292E3DCFB750F67698C5663A20005176415F852B97645F1DAAB20DFA007DB923C06C9D9DA59CAC548519124E4AD0",
            "n_operation": 0,
            "updated_b": 11663
        }
    ]
}
```
___
### sendto

Parameters
* **sender** `number`  
Source account
* **target** `number`  
Destination account
* **amount** `float`  
Amount of PASL to send
* **fee** `float` *Optional*  
Amount of fees to be included into transaction. Default: 0  
* **payload** `string` *Optional*  
User-defined data in hex-encoded string format.  
Payload contents will be assigned to a transaction and stored in the blockchain and will be accessible anytime later.  
For example, a payload can be a comment for a specific payment  
* **payload_method** `string` *Optional, ignored if "payload" field is not set*  
Payload contents can be encrypted or stored as-is.  
Can be one of:  
"none" - do not encrypt payload (everyone will see payload contents)  
"dest" - encrypt payload with destination account's public key (only receiver of the transaction will be able to decode the payload) (Default)  
"sender" - encrypt payload with source account public key (only sender will be able to see payload contents)

Request
```
wget --post-data '{"method":"sendto","params":{"sender":84140,"target":84135,"amount":0.9,"fee":0.1,"payload":"48656c6c6f2c20576f726c6421"},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "account": 84140,
        "amount": -0.9,
        "balance": 0,
        "block": 0,
        "dest_account": 84135,
        "fee": -0.1,
        "opblock": -1,
        "ophash": "00000000AC480100060000003132413239373946443332333133393542454242",
        "optxt": "Transaction Sent to 84135-14",
        "optype": 1,
        "payload": "21100D0010000264D8FAFAE5548ABA5D346E5BCC6EE23225CE30AF00C8F42F29618E7CE6F339DD4BA60416A18BCA5046197685C2FD1CDBD378C3120F0FCF187C5408E9F92FBCBD",
        "sender_account": 84140,
        "time": 0
    }
}
```
___
### findoperation

Parameters
* **ophash** `string`  
Unique transaction id (operation hash) to look for

Request
```
wget --post-data '{"method":"findoperation","params":{"ophash":"62420000AC480100060000003132413239373946443332333133393542454242"},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "account": 84140,
        "amount": -0.9,
        "block": 16994,
        "dest_account": 84135,
        "fee": -0.1,
        "opblock": 7,
        "ophash": "62420000AC480100060000003132413239373946443332333133393542454242",
        "optxt": "Transaction Sent to 84135-14",
        "optype": 1,
        "payload": "21100D0010000264D8FAFAE5548ABA5D346E5BCC6EE23225CE30AF00C8F42F29618E7CE6F339DD4BA60416A18BCA5046197685C2FD1CDBD378C3120F0FCF187C5408E9F92FBCBD",
        "sender_account": 84140,
        "time": 1490131975
    }
}
```
___
### payloaddecrypt

Use this method to decrypt an encrypted payload obtained, for example, from `findoperation` call.

Parameters
* **payload** `string`  
Hex-string with payload data to be decrypted
* **pwds** `array` *Optional*  
Leave this parameter empty to decrypt a payload with one of the wallet's private keys.  
Passwords will be used if payload was encrypted with AES.  

Request
```
wget --post-data '{"method":"payloaddecrypt","params":{"payload":"21100D0010000264D8FAFAE5548ABA5D346E5BCC6EE23225CE30AF00C8F42F29618E7CE6F339DD4BA60416A18BCA5046197685C2FD1CDBD378C3120F0FCF187C5408E9F92FBCBD"},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "enc_payload": "21100D0010000264D8FAFAE5548ABA5D346E5BCC6EE23225CE30AF00C8F42F29618E7CE6F339DD4BA60416A18BCA5046197685C2FD1CDBD378C3120F0FCF187C5408E9F92FBCBD",
        "enc_pubkey": "CA022000011820968B3DDF60C21D686D49838EC4863A8AE8F9229CA5C4DC9EA8119EAC99200072576E709DE942879C5522B177650510C26681772F5DDE10D9329FAFE05DCC36",
        "payload_method": "key",
        "result": true,
        "unenc_hexpayload": "48656C6C6F2C20576F726C6421",
        "unenc_payload": "Hello, World!"
    }
}
```
___
### payloadencrypt

Parameters
* **payload** `string`  
Hex-string with payload data to be encrypted
* **payload_method** `string` *Optional*  
Can be one of "none", "pubkey", "aes".  
"none" - do nothing, return payload as is.  
"pubkey" - encrypt payload with public key provided.

If **payload_method** is **"pubkey"** you must provide one of the following parameters:  
* **enc_pubkey** `string` *Required*  
Public key in encrypted format
* **b58_pubkey** `string` *Required*  
Public key in Base58 format  

If **payload_method** is **"aes"**:
* **pwd** `string` *Required*  
Password that will be used to encrypt payload with AES

Request
```
wget --post-data '{"method":"payloadencrypt","params":{"payload":"48656C6C6F2C20576F726C6421","payload_method":"pubkey","b58_pubkey":"3GhhbojgrHApx2oXpK8178mY8tb8PDY84jckb5ALkZji8VuwuD7uFtY4nxqwYBYsgCPdehSPvTY9bNFk7RJfB28Xw7e9HMBUYz6TTm"},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "21100D0010000264D8FAFAE5548ABA5D346E5BCC6EE23225CE30AF00C8F42F29618E7CE6F339DD4BA60416A18BCA5046197685C2FD1CDBD378C3120F0FCF187C5408E9F92FBCBD"
}
```
___
### getwalletaccountscount

Parameters
* **enc_pubkey** `string` *Optional*  
* **b58_pubkey** `string` *Optional*  

Request
```
wget --post-data '{"method":"getwalletaccountscount","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": 3
}
```
___
### getwalletpubkey

Parameters (at least one of the following)
* **enc_pubkey** `string`  
Return information about encryted public key
* **b58_pubkey** `string`  
Return information about Base58 public key

Request
```
wget --post-data '{"method":"getwalletpubkey","params":{"b58_pubkey": "3GhhbopQp47PHYm6R3yF8jta6pTYptNRFCjGioyUZbypEpPEStbX3QEHZDQJoyG7NmDXzasddzdm9jMvfUt7dvX9Kt1dHDSw3UQiQp"},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "b58_pubkey": "3GhhbopQp47PHYm6R3yF8jta6pTYptNRFCjGioyUZbypEpPEStbX3QEHZDQJoyG7NmDXzasddzdm9jMvfUt7dvX9Kt1dHDSw3UQiQp",
        "ec_nid": 714,
        "enc_pubkey": "CA0220003E2F08F1E098AE2E2AECDB9169D6355376FD9864292E3DCFB750F67698C5663A20005176415F852B97645F1DAAB20DFA007DB923C06C9D9DA59CAC548519124E4AD0",
        "x": "3E2F08F1E098AE2E2AECDB9169D6355376FD9864292E3DCFB750F67698C5663A",
        "y": "5176415F852B97645F1DAAB20DFA007DB923C06C9D9DA59CAC548519124E4AD0"
    }
}
```
___
### getwalletpubkeys

Parameters
* **start** `number`  *Optional*
Internal index of public key to start from. First public key in the wallet will have index 0
* **max** `number`  *Optional*
Number of public keys to return  

Request
```
wget --post-data '{"method":"getwalletpubkeys","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "b58_pubkey": "3GhhbopQp47PHYm6R3yF8jta6pTYptNRFCjGioyUZbypEpPEStbX3QEHZDQJoyG7NmDXzasddzdm9jMvfUt7dvX9Kt1dHDSw3UQiQp",
            "can_use": true,
            "ec_nid": 714,
            "enc_pubkey": "CA0220003E2F08F1E098AE2E2AECDB9169D6355376FD9864292E3DCFB750F67698C5663A20005176415F852B97645F1DAAB20DFA007DB923C06C9D9DA59CAC548519124E4AD0",
            "name": "mywallet",
            "x": "3E2F08F1E098AE2E2AECDB9169D6355376FD9864292E3DCFB750F67698C5663A",
            "y": "5176415F852B97645F1DAAB20DFA007DB923C06C9D9DA59CAC548519124E4AD0"
        }
    ]
}
```
___
### changekey

Parameters
* **account** `number`
* **new_enc_pubkey** `string` *Optional*  
* **new_b58c_pubkey** `string` *Optional*  
* **fee** `float` *Optional*  
* **payload** `string` *Optional*  
* **payload_method** `string` *Optional*  
___
### changekeys

Parameters
* **accountы** `string`
* **new_enc_pubkey** `string` *Optional*  
* **new_b58c_pubkey** `string` *Optional*  
* **fee** `float` *Optional*  
* **payload** `string` *Optional*  
* **payload_method** `string` *Optional*  
___
### lock

No parameters  
___
### unlock

Parameters
* **pwd** `string`
___
### setwalletpassword

Parameters
* **pwd** `string`
___
### getaccount

Parameters
* **account** `number`  
An account number of any existing account in the PascalLite network without checksum field appended (i.e. 0, 5, 43411 etc.)

Request
```
wget --post-data '{"method":"getaccount","params":{"account": 5},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "account": 5,
        "balance": 1607843.1498,
        "enc_pubkey": "CA02200068C28485A677EA6520383B2001BF8230EA41FC8A9027EB72B99465B1587FCC97200040120C3178218C53B705698F7C1E7F42DC49EF50B7CA9E5ADC6332F7557C6015",
        "n_operation": 209,
        "updated_b": 58514
    }
}
```
___
### getaccountoperations

Parameters
* **account** `number`  
An account number (without checksum field appended)

Request
```
wget --post-data '{"method":"getaccountoperations","params":{"account":56240},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "account": 56240,
            "amount": 0,
            "balance": 0,
            "block": 16351,
            "enc_pubkey": "CA0220007D54FA072AA0F462FA0A6DBACFD1FFCE64304203AABAFE4FF287CF4ACAD0B6492000BBB452EF460E4CF1367BBDF0B1AE2D53F0402C33AE24409A02C69308B587F679",
            "fee": 0,
            "opblock": 85,
            "ophash": "DF3F0000B0DB0000020000004531333041334534323645354345433841323731",
            "optxt": "Change Key to secp256k1",
            "optype": 2,
            "payload": "2110070010000323D97F79162529F612BC3EB25A6EBAC37D5788B22DEC14207FA928AA626FAAC0B0C45D3BC738C6A63C0A544E871AB35ED48D5E5EAD2A231345057ACAE0A2FF1E",
            "time": 1489942737
        },
        {
            "account": 56240,
            "amount": -50,
            "balance": 0,
            "block": 13772,
            "dest_account": 41000,
            "fee": 0,
            "opblock": 88,
            "ophash": "CC350000B0DB0000010000004339413434424236413144314235393345314337",
            "optxt": "Transaction Sent to 41000-18",
            "optype": 1,
            "payload": "",
            "sender_account": 56240,
            "time": 1489322803
        },
        {
            "account": 56240,
            "amount": 50,
            "balance": 50,
            "block": 11248,
            "fee": 0,
            "opblock": -1,
            "optxt": "Blockchain reward",
            "optype": 0,
            "payload": "",
            "time": 1488702921
        }
    ]
}
```
___
### getblockcount

No parameters

Request
```
wget --post-data '{"method":"getblockcount","params":{"block":1},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": 58546
}
```
___
### getblock

Parameters
* **block** `number`  
Block index

Request
```
wget --post-data '{"method":"getblock","params":{"block":7409},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "block": 7409,
        "enc_pubkey": "CA0220005D034AF7777D9E4F7520C0D8827B74F5C97C44FC0E32B7546D87E5CF950C1310200014CAB82FC4E1423EC16EA16E000EDD9EE34D68ACA906656D943FCACE754C3690",
        "fee": 0,
        "hashratekhs": 13239559,
        "maturation": 51112,
        "nonce": 2449493167,
        "operations": 0,
        "oph": "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855",
        "payload": "onwork                    J\u00c3\u0084.-----",
        "pow": "00000000004D9E000C6EEA41C6982216F435F748E11ADA66A7588D213F81A486",
        "reward": 50,
        "sbh": "884C88CDECD981C228FF3DD209F7A4259C5F6705D845D93AD78F5603936C73E6",
        "target": 694482558,
        "timestamp": 1487727945,
        "ver": 1,
        "ver_a": 1
    }
}
```
---
### getblocks

Parameters
* **last** `number`  
Return last N blocks  

Or:  
* **start** `number`  
Starting block index  
* **end** `number`  
Last block index  

Or:  
* **start** `number`  
Starting block index  
* **max** `number`  
Number of blocks to return

Request
```
wget --post-data '{"method":"getblocks","params":{"start":7409,"max":2},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "block": 7411,
            "enc_pubkey": "CA022000B793C243DBDB337EF87BD0A205770A0AD4338DF1D3A86CE67AE32361A0F38D492000BA7165AB61FE77D5C93D788CFF1B99FB3A07901CC463A7F58F86270056327F9C",
            "fee": 0,
            "hashratekhs": 13134639,
            "maturation": 51111,
            "nonce": 1498074272,
            "operations": 0,
            "oph": "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855",
            "payload": "6969696901",
            "pow": "00000000003D39061F78B3DB9B8C19F3D680535523D04DEE835E69394C4D25C2",
            "reward": 50,
            "sbh": "71C2B7C26A2D035F4E670A9607327D61EB14F8A94790EA087AE202361B72300C",
            "target": 695018620,
            "timestamp": 1487729006,
            "ver": 1,
            "ver_a": 1
        },
        {
            "block": 7410,
            "enc_pubkey": "CA022000FC794EF7F7AE8BB0AF12A3E90B999C499F9B984362C1FEE13F79F8CD2849FE2820003AEC82CCDC5C183CA80D9DCAB7C8E4EFC3A65C36A3507ED050D5AD0F6D1D4F84",
            "fee": 0,
            "hashratekhs": 12992418,
            "maturation": 51112,
            "nonce": 3852888025,
            "operations": 0,
            "oph": "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855",
            "payload": "mrjaekinRX480Rig/0----------------",
            "pow": "0000000000256F2AAECE262DEF226227FA19466634FDA90ECC638571B4D4AE75",
            "reward": 50,
            "sbh": "68D7075B83B70EDC3AD051E4A05E74365B344A1E0779A0BADBCF478AFEE0DF06",
            "target": 694751936,
            "timestamp": 1487728263,
            "ver": 1,
            "ver_a": 1
        },
        {
            "block": 7409,
            "enc_pubkey": "CA0220005D034AF7777D9E4F7520C0D8827B74F5C97C44FC0E32B7546D87E5CF950C1310200014CAB82FC4E1423EC16EA16E000EDD9EE34D68ACA906656D943FCACE754C3690",
            "fee": 0,
            "hashratekhs": 13239559,
            "maturation": 51113,
            "nonce": 2449493167,
            "operations": 0,
            "oph": "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855",
            "payload": "onwork                    J\u00c3\u0084.-----",
            "pow": "00000000004D9E000C6EEA41C6982216F435F748E11ADA66A7588D213F81A486",
            "reward": 50,
            "sbh": "884C88CDECD981C228FF3DD209F7A4259C5F6705D845D93AD78F5603936C73E6",
            "target": 694482558,
            "timestamp": 1487727945,
            "ver": 1,
            "ver_a": 1
        }
    ]
}
```
___
### getblockoperation

Parameters
* **block** `number`
* **opblock** `number`

Request
```
wget --post-data '{"method":"getblockoperation","params":{"block":50100,"opblock":1},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "account": 248100,
        "amount": -6.2705,
        "block": 50100,
        "dest_account": 249999,
        "fee": 0,
        "opblock": 1,
        "ophash": "B4C3000024C90300060000003932433241394546424441334537353733413139",
        "optxt": "Transaction Sent to 249999-75",
        "optype": 1,
        "payload": "",
        "sender_account": 248100,
        "time": 1500098356
    }
}
```
___
### getblockoperations

Parameters
* **block** `number`
* **start** `number` *Optional*
* **max** `number` *Optional*

Request
```
wget --post-data '{"method":"getblockoperations","params":{"block":50100},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "account": 249999,
            "amount": 0,
            "block": 50100,
            "enc_pubkey": "CA02200089927599939EDD01C65628E7E25F7A1FF511C9806D75DBF2917131C4217814DD20006CF4BC42292ED111C111D17D1A7B37D36F077340B60E918DA2AA424CE2777D8B",
            "fee": 0,
            "opblock": 0,
            "ophash": "B4C300008FD00300010000003532454142363930464233383130444234433437",
            "optxt": "Change Key to secp256k1",
            "optype": 2,
            "payload": "",
            "time": 1500098356
        },
        {
            "account": 248100,
            "amount": -6.2705,
            "block": 50100,
            "dest_account": 249999,
            "fee": 0,
            "opblock": 1,
            "ophash": "B4C3000024C90300060000003932433241394546424441334537353733413139",
            "optxt": "Transaction Sent to 249999-75",
            "optype": 1,
            "payload": "",
            "sender_account": 248100,
            "time": 1500098356
        },
        {
            "account": 248095,
            "amount": -5.9795,
            "block": 50100,
            "dest_account": 249999,
            "fee": 0,
            "opblock": 2,
            "ophash": "B4C300001FC90300060000003034374134423144354438323441464439333331",
            "optxt": "Transaction Sent to 249999-75",
            "optype": 1,
            "payload": "",
            "sender_account": 248095,
            "time": 1500098356
        }
    ]
}
```
___
### nodestatus

No parameters

Request
```
wget --post-data '{"method":"nodestatus","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "blocks": 58545,
        "locked": false,
        "netprotocol": {
            "ver": 3,
            "ver_a": 4
        },
        "netstats": {
            "active": 25,
            "breceived": 227342451,
            "bsend": 949337836,
            "clients": 22,
            "servers": 3,
            "servers_t": 3,
            "tclients": 1141,
            "total": 1177,
            "tservers": 36
        },
        "nodeservers": [
            {
                "attempts": 0,
                "ip": "141.184.137.39",
                "lastcon": 1502968735,
                "port": 4004
            },
            {
                "attempts": 0,
                "ip": "119.183.126.192",
                "lastcon": 1502968685,
                "port": 4004
            },
            {
                "attempts": 0,
                "ip": "93.181.54.70",
                "lastcon": 1502966346,
                "port": 4004
            }
        ],
        "port": 4004,
        "pow": "000000000000D232873B82B148BC9C3B03AB0EBFCE4CA09902BEF54B9DA41072",
        "ready": true,
        "ready_s": "",
        "sbh": "36068B8C6D2388976BC095B69AE6383DF1673DA7ABC20C831FFABBB860F4511C",
        "status_s": "Running",
        "timestamp": 1502968763,
        "version": "1.4.7"
    }
}
```  
___
### getpendings

No parameters  

Request
```
wget --post-data '{"method":"getpendings","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "account": 290305,
            "amount": -12.25,
            "balance": 29.0791,
            "block": 0,
            "dest_account": 292224,
            "fee": 0,
            "opblock": 9,
            "ophash": "00000000016E0400020000003838334639464337423845313044434442414331",
            "optxt": "Transaction Sent to 292224-98",
            "optype": 1,
            "payload": "",
            "sender_account": 290305,
            "time": 0
        },
        {
            "account": 292224,
            "amount": 0,
            "balance": 12.25,
            "block": 0,
            "enc_pubkey": "CA0220009D07F9C4B74BF29B8E17971095F90856DC0E4F1726AC7995E2FD9A1FC74C0A4D2000A01BE9217252006E6054CCC07B574173B9EC73004BB15BE004DA7BD8EFD38ABE",
            "fee": 0,
            "opblock": 8,
            "ophash": "0000000080750400010000004443363231303336433135444530453033463433",
            "optxt": "Change Key to secp256k1",
            "optype": 2,
            "payload": "",
            "time": 0
        }
    ]
}
```
___
### getconnections

No parameters

Request
```
wget --post-data '{"method":"getconnections","params":{},"id":1,"jsonrpc":"2.0"}' -qO- http://localhost:4003
```
Response
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "appver": "1.4.6lF",
            "ip": "47.71.2.114",
            "netver": 3,
            "netver_a": 4,
            "port": 4004,
            "recv": 13069794,
            "secs": 1049274,
            "sent": 19812284,
            "server": true
        },
        {
            "appver": "1.4.7lF",
            "ip": "194.167.12.205",
            "netver": 3,
            "netver_a": 4,
            "port": 62308,
            "recv": 10492256,
            "secs": 1047327,
            "sent": 18458351,
            "server": false
        },
        {
            "appver": "1.4.6lF",
            "ip": "51.111.190.48",
            "netver": 3,
            "netver_a": 4,
            "port": 55328,
            "recv": 12809639,
            "secs": 855351,
            "sent": 13108329,
            "server": false
        }
   ]
}
```
___
### addnode

Parameters
* **nodes** `string`
___
### stopnode

No parameters  
___
### startnode

No parameters  
___
### signsendto
___
### signchangekey
___
### executeoperations
___
### operationsinfo

Parameters
* **rawoperations** `string`
___
### encodepubkey
___
### decodepubkey
___

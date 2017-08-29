# Build steps (PascalLite Daemon)

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
  Under Ubuntu you'll need to install `fp-units-fcl` package
  ```bash
sudo apt-get install fp-units-fcl
  ```

5. Get latest release at [https://github.com/PascalLite/wallet/releases](https://github.com/PascalLite/wallet/releases) or checkout bleeding edge development version of PascalLite's `master`
  ```bash
  git clone https://github.com/PascalLite/wallet
  ```
6. Change to the root of the source code directory, copy `libcrypto.so.1.1` from step 3, build PascalLite daemon

  ```bash
cd wallet  
cp <OPENSSL_FOLDER>/output/lib/libcrypto.so.1.1 .  
fpc -FuUnits/PascalCoin/ -FuSynapse/lib/ -FuUnits/Utils/ pascallited.pp
  ```
7. Run PascalLite daemon in background

  ```bash
nohup ./pascallited -r &
  ```
8. Configure daemon settings in `~/PascalLite/pascallite.ini`
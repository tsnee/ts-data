# Setup Instructions

Follow these steps to install the Haskell toolchain for this project.

1. **Install ghcup** using apt-get:
   ```bash
   sudo apt-get update
   sudo apt-get install -y ghcup-hs
   ```
2. **Install cabal, ghc, and fourmolu** with ghcup:
   ```bash
   ghcup install ghc 9.12.2
   ghcup set ghc 9.12.2
   ghcup install cabal 3.14.2.0
   ```
   Ensure `ghcup` has added the binaries to your PATH.

3. **Install fourmolu and hlint** with cabal:
   ```bash
   cabal update
   cabal install fourmolu hlint
   ```

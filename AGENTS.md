# Setup Instructions

Follow these steps to install the Haskell toolchain for this project.

1. **Install prerequisites** using apt-get:
   ```bash
   sudo apt-get update
   sudo apt-get install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config
   ```
2. **Install ghcup** using curl:
   ```bash
   export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
   export BOOTSTRAP_HASKELL_GHC_VERSION=9.12.2
   export BOOTSTRAP_HASKELL_CABAL_VERSION=3.14.2.0
   export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
   export BOOTSTRAP_HASKELL_INSTALL_HLS=0
   export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1
   curl -sSf https://get-ghcup.haskell.org | bash
   ```
3. **Update running shell**:
   ```bash
   . ~/.ghcup/env
   ```
4. **Install fourmolu and hlint** with cabal:
   ```bash
   cabal update
   cabal install fourmolu hlint
   ```

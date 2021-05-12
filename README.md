# nasa-hs

This is a demonstration piece to illustrate how the nasa module management with yaml files could be done. This small CLI tool searches, parses and lists modules. As parsing failes for broken yaml files we get validation for free.

### Install

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.

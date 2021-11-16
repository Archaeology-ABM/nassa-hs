# nassa-hs

`nassa` is a small CLI tool that searches, parses and lists modules. As parsing failes for broken yaml files we get validation for free.

See a collection of example modules in `playground/`, which can be validated and listed with

```
nassa list -d .
```

### Install

For stable release versions we automatically prepare binaries that can be downloaded and run without further installation.

You can download them here: [ [Linux 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-Linux) | [macOS 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-macOS) | [Windows 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-Windows.exe) ]. Older release versions are available [here](https://github.com/Archaeology-ABM/nassa-hs/releases).

So in Linux you can run the following commands to get started:

```bash
# download the current stable release binary
wget https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-Linux
# make it executable
chmod +x nassa-Linux
# test it
./nassa-Linux list -d /path/to/your/nassa/modules
```

### For developers

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). Stack will install the compiler and all dependencies into folders that won't interfere with any other Haskell installation you might already have.

#### Preparing a new stable release

The Github Actions script in `.github/workflows/release.yml` registers a new draft release and automatically builds and uploads binaries when a new Git tag with the prefix `v*` is pushed. 

```bash
# locally register a new tag (e.g. 0.3.1)
git tag -a v0.3.1 -m "see CHANGELOG.md"
# push tag
git push origin v0.3.1
```

In case of a failing build delete the tag and the release draft on Github and then delete the tag locally with

```bash
git tag -d v0.3.1
```

before rerunning the procedure above.

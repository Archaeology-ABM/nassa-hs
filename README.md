# nassa-hs

`nassa` is a small CLI tool that searches, validates and lists NASSA modules.

See a collection of example modules in `playground/`, which can be validated and listed with

```
nassa list -d .
```

### Install

For stable release versions we automatically prepare binaries that can be downloaded and run without further installation.

You can download them here: [ [Linux 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-Linux) | [macOS (ARM64) 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-macOS-ARM64) | [macOS (X64) 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-macOS-X64) | [Windows 📥](https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-Windows.exe) ]. Older release versions are available [here](https://github.com/Archaeology-ABM/nassa-hs/releases).

#### Linux

So in Linux you can run the following commands to get started:

```bash
# download the current stable release binary
wget https://github.com/Archaeology-ABM/nassa-hs/releases/latest/download/nassa-Linux
# make it executable
chmod +x nassa-Linux
# test it
./nassa-Linux list -d /path/to/your/nassa/modules
```

#### Windows

In Windows 10, you may encounter a problem when downloading the executable file ("nassa-Windows.exe"). Please follow the [instructions](https://github.com/Archaeology-ABM/nassa-hs/blob/main/instructions-windows/images/instructions-windows.md) that will allow you to access and use the file.

Once you have access to the file and moved it to the preferred location, open Command Prompt (you can search for it by typing "Command Prompt" in the task bar). Move to the file location by typing "cd", SPACE, and the full directory path, by either typing it or dragging-and-dropping the folder containing the file from the File Explorer to the Command Prompt window, and then hit Intro. Note that you may need to change hard drives before changing directories; this can be done by typing the drive letter followed by colon and hitting Intro. For example:

```bat
C:\Users\my-name>E:

E:\>cd E:\my-local-folder\

E:\my-local-folder\>
```

To run the file on a specific directory containing potential NASSA modules with NASSA.yml files:

```bat
E:\my-local-folder\>nassa-Windows.exe list -d E:\another-local-folder\
```

The output will be printed in the Command Prompt screen. Successfully validated modules will be displayed in a table while any issues in validation will produce exception warnings showing which yml file could not be validated and the line causing the error.

![example of output](https://github.com/Archaeology-ABM/nassa-hs/blob/main/instructions-windows/images/command-prompt-output.png)

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

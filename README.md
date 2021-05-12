# nasa-hs

This is a demonstration piece to illustrate how the nasa module management with yaml files could be done. This small CLI tool searches, parses and lists modules. As parsing failes for broken yaml files we get validation for free.

### Example (in `playground/`)

We have a repository with three repos.

module1: 
```
id: 1
title: Green random walk
category: Random walk
authorship: Clemens Schmid
language: R
```

module2:
```
id: 2
title: Red random walk
category: Random walk
authorship: Clemens Schmid
language: Python
```

module3:
```
id: 3
title: Blue random walk
category: Random walk
tags:
  - foo
  - bar
authorship: Clemens Schmid
language: Netlogo
license: MIT
interactions:
  dependencies:
    - 1
  suggests:
    - "2"
```

Now we can run `nasa` to validate and list them: 

```
> nasa list -d .
Searching NASA.yml files... 
3 found
Loading NASA modules... 
Some modules were skipped:
Could not parse YAML file ./testData/module3/NASA.yml: AesonException 
"Error in $.interactions.suggests[0]: parsing Int failed, expected Number, but encountered String"
2 loaded
.----.-------------------.-------------.----------.
| id |       title       |  category   | language |
:====:===================:=============:==========:
| 1  | Green random walk | Random walk | R        |
| 2  | Red random walk   | Random walk | Python   |
'----'-------------------'-------------'----------'
```

### Install

To install the latest development version you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.

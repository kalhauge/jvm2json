# jvm2json

This is a tool for converting Java bytecode into JSON.

## Usage

This 



## Installation

To install the tool there are several options.

### Stack / Ghcup

The first solution is to install from scratch using Haskell. The easiest way to do that is to download [ghcup](https://www.haskell.org/ghcup/), and 
run the installation process, you should install 'stack'. You do not need HLS unless you want to develop on the tool.

Make sure that a new version of stack is installed. I have tested with:
```bash
>$ stack --version
Version 2.11.1, Git revision c1167a6abc3f4978ccded5ba0246a57387da0e2f x86_64 hpack-0.35.2
```

Now you should be able to build and install the program, which might take some time the first 
time around.

```bash
>$ stack build
```

### Nix

To build the executable simply write:

```
nix build
```



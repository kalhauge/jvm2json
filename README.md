# jvm2json

This is a tool for converting Java bytecode into JSON.

## Usage

```bash
$ jvm2json --help
Usage: jvm2json [-s|--classfile CLASSFILE] [-t|--json JSONFILE] [-R|--reverse]

  a tool for converting between java classfile and json

Available options:
  -s,--classfile CLASSFILE the path to the classfile, - for stdin/out
  -t,--json JSONFILE       the path to the jsonfile, - for stdin/out
  -R,--reverse             input json and output a jvm class
  -h,--help                Show this help text
```

Both of commands convert `Test.class` into `test.json`

```bash
$ jvm2json -s Test.class -t test.json
$ jvm2json >Test.class <test.json
```

And both of these commands convert `test.json` into `Test.class` 

```bash
$ jvm2json -R -s Test.class -t test.json
$ jvm2json -R <Test.class >test.json
```

A specification of the codec is available in `CODEC.txt`. A nice html version is incoming.

## Docker 

A limited usage is available using docker. The following command should just work and produce Json:

```bash
docker run --rm -i kalhauge/jvm2json:latest jvm2json <Test.class >test.json
```

You should also be able to run it in reverse.

```bash
docker run --rm -i kalhauge/jvm2json:latest jvm2json -R <test.json >Test.class
```

However, the file part of the CLI requires that you mount your filesystem.

## Installation

To install the tool there are several options.

### Stack / Ghcup

The first solution is to install from scratch using Haskell. The easiest way to do that is to download [ghcup](https://www.haskell.org/ghcup/), and 
run the installation process, you should install 'stack'. You do not need HLS unless you want to develop on the tool.

Make sure that a new version of stack is installed. I have tested with:
```bash
$ stack --version
Version 2.11.1, Git revision c1167a6abc3f4978ccded5ba0246a57387da0e2f x86_64 hpack-0.35.2
```

Now you should be able to build the program, which might take some time the first 
time around.

```bash
$ stack build
```

After which you should be able to install the program using the following command.

```bash
$ stack install
```

And if all goes well, you should be able to see that you can run `jvm2json -h` and get the above help message.

### Nix

To build the executable simply write:

```
nix build
```

### M1 Issues

You might get a 'ffi.h' not installed error, in this case you should consider running:
```
xcode-select --install
```

[See here for more](https://stackoverflow.com/questions/75844764/fatal-error-ffi-h-file-not-found-when-trying-to-install-hashable-via-cabal)





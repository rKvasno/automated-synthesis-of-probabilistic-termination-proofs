# Automated Synthesis of Probabilistic Proofs

A rust library and command line tool for static termination analysis of affine probabilistic programs with non-determinism. The library is build from different modules providing functions such as parsing of the input programs or proof synthesis. For further information about the library turn to its [documentation](#documentation), the rest of this text will focus on the command line tool provided.


## Installation

### Docker
If you don't have the rust compiler installed, the easiest way to get started is using the provided Dockerfile. The image build from it effectively functions as an executable for the CLI. Using [Docker](https://docker.com) (or [Podman](https://podman.io)) execute: 
```
$ docker build -t asptp-cli PROJECT_DIR 
```

### Build
The only requirement to build this project is the [rust compiler](https://github.com/rust-lang/rust#the-rust-programming-language). Using rusts build system [cargo](https://doc.rust-lang.org/cargo/guide/index.html):
```
$ cargo install -F cli --path PROJECT_DIR
```
Binaries installed this way are located in Cargos root directory ($HOME/.cargo/bin by default). It is recommended to put this directory into your $PATH.

#### Documentation
The library documentation can be generated using the `cargo doc` command with the optional `--open` flag to open generated documentation in a browser.


## Usage
If using a container on Linux run:
```
$ cat INPUT_PROGRAM | docker run -i asptp-cli -a ALGORITHM PARSER
```

If using a container on Windows run:
```
$ type INPUT_PROGRAM | docker run -i asptp-cli -a ALGORITHM PARSER
```


If using a local installation run:
```
$ asptp -i INPUT_PROGRAM -a ALGORITHM PARSER
```

Replacing INPUT_PROGRAM, ALGORITHM and PARSER respectively. The input program must be written in the [language](/languages) parsed by the selected parser. For more information on CLI arguments use `-h`.
Program produces one (or multiple) [DOT](https://graphviz.org/doc/info/lang.html) graphs, representing the input program and its proof of termination, depending on algorithm chosen. 


## License
This project is dual-licensed under the MIT and Apache 2.0 licenses.


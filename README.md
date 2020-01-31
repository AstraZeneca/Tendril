# Tendril plot package v2.0.4

R library for tendril plotting between two treatments.

# Download

The package can be downloaded as a .tar.gz from the [appropriate release page](https://github.com/Karpefors/Tendril/releases)

# Installation

## In RStudio (for end-users)

The package can be installed with RStudio in Tools -> Install Packages -> Install from package archive file

## In a packrat environment (for developers)

You can ensure that tendril is installed in an isolated packrat environment.
This is only required if you want to keep its installation isolated and you don't have access
to the dependencies in your normal working environment

### OSX Mojave

To install the dependencies in your packrat environment on OSX you will need clang with OpenMP support.
Unfortunately Mojave does not ship with it. 

1. open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg
2. Install homebrew from https://brew.sh/
3. Install the required packages to compile Tendril dependencies 

```
    brew install gfortran
    brew install llvm
    brew install boost
    brew install libomp
```

4. add this to ~/.R/Makevars

```
    CC=/usr/local/opt/llvm/bin/clang
    CXX=/usr/local/opt/llvm/bin/clang++
    CXX11=/usr/local/opt/llvm/bin/clang++
    CXX14=/usr/local/opt/llvm/bin/clang++
    CXX17=/usr/local/opt/llvm/bin/clang++
    CXX1X=/usr/local/opt/llvm/bin/clang++
    LDFLAGS=-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib
    CFLAGS=-I/usr/local/opt/llvm/include
    CPPFLAGS=-I/usr/local/opt/llvm/include
```

5. Type 

    make env

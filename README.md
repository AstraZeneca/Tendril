tendril plot package
======================

Repository for tendril plot package.

# Installation

## Creation of the packrat environment

You can ensure that tendril is installed in an isolated packrat environment.
This is only required if you want to keep its installation isolated and you don't have access
to the dependencies in your normal working environment

### OSX Mojave

To install the dependencies in your packrat environment on OSX you will need clang with OpenMP support.
Unfortunately Mojave does not ship with it. 

1. open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg
2. Install homebrew from https://brew.sh/
3. Install the required packages to compile Tendril dependencies 

    brew install gfortran
    brew install llvm
    brew install boost
    brew install libomp

4. add this to ~/.R/Makevars

    CC=/usr/local/opt/llvm/bin/clang
    CXX=/usr/local/opt/llvm/bin/clang++
    CXX11=/usr/local/opt/llvm/bin/clang++
    CXX14=/usr/local/opt/llvm/bin/clang++
    CXX17=/usr/local/opt/llvm/bin/clang++
    CXX1X=/usr/local/opt/llvm/bin/clang++
    LDFLAGS=-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib
    CFLAGS=-I/usr/local/opt/llvm/include
    CPPFLAGS=-I/usr/local/opt/llvm/include

5. Type 

    make env



# Contact

Contact <Martin.Karpefors@astrazeneca.com> for further details.

# Tendril plot package v0.8.0

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

# Changelog

See the [CHANGELOG](CHANGELOG.md)

- Version 0.8.0 (14 Oct 2019)
    - #59 Add Frequency to hover text
    - #57 Time series of net event
    - #56 Corrected colouring on percentile and permutation plots for Terms, rdiff, RR and OR
    - #55 Change colouring in perm perc.
    - #54 Allow single, multiple or all tendrils to be plotted
    - #52 Changed ggplot2 options to be consistent with plotbasic
    - #51 Various plot changes for consistency and minor cosmetics
    - #46 Introduces the timeseries function to plot a timeseries plot of net event
    - #43 Create separate percentile object and plot function
    - #42 Added scaling of markers according to appropriate range of termscount
    - #41 Added interactive option to plot using plotly or ggplot2
    - #40 Show left and right labels in the plots
    - #36 Pinned to R version 3.5 or above
    - Various cleanups and refactorings (#29, #1, #32, #34, #35, #37, #38, #44, #58)

- Version 0.7 (1 Oct 2019)
    - First public release

# Contact information

For further info please contact:
<Martin.Karpefors@astrazeneca.com>

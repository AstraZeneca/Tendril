# Changelog

- Version 2.0.2 (22 January 2020)

    - #98 Fixed missing FDR.tot in other coloring scale selections.

- Version 2.0.1 (21 January 2020)

    - #94 Fixed label p-value to log(p-val)
    - #95 Fixed incorrect scale for limited tendril plotting.
    - #96 Add FDR.tot for coloring

- Version 2.0.0 (13 January 2020)

    - #63 Update TendrilPerm and TendrilPi functions to output only relevent data
    - #61 Typo in Tendril object element
    - #62 Update Tendril function to retain other input data
    - #67 Updated vignette for Tendril package
    - #69 Fix tests that are currently failing
    - #75 Added error message for plotting with an unexisting term
    - #19 Tendril.cx should return information without modifying data in place (Fixed as #76 Cleanup of code and documentation of behavior)
    - #21 Remove usage of dots in non S3 methods
    - #72 plot\_timeseries problems (incorrect indexing leads to wrong data being plotted)
    - #73 Interactive plot color palette (should matching non-interactive one)
    - #87 Removed internal opacity term

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


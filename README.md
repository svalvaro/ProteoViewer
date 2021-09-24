
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ProteoViewer

<!-- badges: start -->
<!-- badges: end -->

The goal of ProteoViewer is to …

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("svalvaro/ProteoViewer")
```

## Use the shiny app.

Launch the app locally.

``` r
library(ProteoViewer)

runProteoViewerApp()
```

You can also provide the evidence.txt to the main function and obtain
the URL that you can copy and paste into any browser to obtain the
visualization of the protein of interest.

``` r
df <- read.delim('evidence.tx')

connectProtterAPI(evidence = df,
                SelectedExperiment = 'wt_1',
                SelectedProtein = 'Q001',combineExperiments = FALSE,
                plot_palette = FALSE,
                peptideCutter = TRUE)
```

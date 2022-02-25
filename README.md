
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ProteoViewer

<!-- badges: start -->
<!-- badges: end -->

The goal of ProteoViewer is to analyse the peptide intensities in a
proteomics LC-MS/MS experiment. Besides, it provides visualization for
the protein topology, Post-translational modifications and protein
Coverage.

## Installation

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

Or access to it trough the web application hosted at:
<https://proteomics.fgu.cas.cz/ProteoViewer/>

## Data

ProteoViewer accepts the `evidence.txt` from
[MaxQuant](https://www.maxquant.org/) or the output from
[Spectronaut](https://biognosys.com/software/spectronaut/)

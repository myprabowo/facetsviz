
# facetsviz

## Overview

facetsviz is an R package for parsing FACETS `.out` files and producing
reusable diagnostic visualizations from multi-facet Rasch measurement
output. It provides a suite of functions to easily create beautiful,
publication-ready plots using [ggplot2](https://ggplot2.tidyverse.org).

- `plot_wright()`
- `plot_probability()`
- `plot_distribution()`
- `plot_usage()`
- `plot_fairness()`

``` r
library(facetsviz)

facets <- read_facets("peerAssessment.out")
plot_wright(facets)
```

## Installation

``` r
# Install from local source
install.packages("facetsviz_0.1.0.tar.gz", repos = NULL, type = "source")

# Or using devtools if published on GitHub
# devtools::install_github("username/facetsviz")
```

## Usage

facetsviz provides R functions for reading data, plotting, batch
rendering, and an interactive Shiny app.

``` r
library(facetsviz)

# 1. Read FACETS output
facets <- read_facets("peerAssessment.out")

# 2. Inspect available measurement reports
names(facets$measurement_reports)

# 3. Create individual plots
plot_wright(facets)
plot_probability(facets)

# 4. Render all diagnostic plots to a folder
render_plots(facets, output_dir = "figures", formats = "png")

# 5. Launch the interactive Shiny app
facetsviz::run_app()
```

## Examples

Use the plotting functions to inspect your Rasch measurement data:

- `plot_wright(facets)`: Visualizes person measures and item
  difficulties on a common logit scale.
- `plot_probability(facets)`: Shows category probability curves.
- `plot_distribution(facets)`: Displays the distribution of measures for
  a specific facet.
- `plot_usage(facets)`: Shows how rating scale categories are used.

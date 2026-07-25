# ggheat

`ggheat` creates publication-ready heatmaps with `ggplot2`. It accepts numeric
matrices, wide data frames, and tidy long-form data.

## Installation

```r
pak::pak("YaoxiangLi/ggheat")
```

## Example

```r
library(ggheat)

data <- read.csv(
  system.file("extdata", "example.csv", package = "ggheat"),
  row.names = 1,
  check.names = FALSE
)

ggheat(
  data,
  scale = "row",
  cluster_rows = TRUE,
  cluster_cols = TRUE
)
```

Launch the interactive application with:

```r
install.packages(c("shiny", "golem", "config"))
ggheat::run_app()
```

# Create a publication-ready heatmap

\`ggheat()\` accepts a numeric matrix or wide data frame directly. Tidy
long-form data is also supported when \`x\`, \`y\`, and \`fill\`
identify the column, row, and value variables.

## Usage

``` r
ggheat(
  data,
  x = NULL,
  y = NULL,
  fill = NULL,
  scale = c("none", "row", "column"),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  palette = c("#3B4CC0", "#F7F7F7", "#B40426"),
  midpoint = NULL,
  na_color = "grey90",
  show_values = FALSE,
  digits = 2,
  text_color = "black",
  border_color = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  xlab = NULL,
  ylab = NULL,
  rotate_x = 45
)
```

## Arguments

- data:

  A numeric matrix/data frame, or a long-form data frame.

- x, y, fill:

  Column names for long-form data. Supply all three or none.

- scale:

  Standardization mode: \`"none"\`, \`"row"\`, or \`"column"\`.

- cluster_rows, cluster_cols:

  Apply hierarchical clustering to rows or columns?

- palette:

  Three colours for low, midpoint, and high values.

- midpoint:

  Fill-scale midpoint. \`NULL\` chooses zero when the range crosses zero
  and the median otherwise.

- na_color:

  Colour used for missing cells.

- show_values:

  Display formatted values inside cells?

- digits:

  Number of decimal places used for displayed values.

- text_color:

  Colour for displayed values.

- border_color:

  Optional cell-border colour. \`NULL\` removes borders.

- title, subtitle, caption:

  Plot annotations.

- xlab, ylab:

  Axis labels.

- rotate_x:

  Rotation in degrees for horizontal-axis labels.

## Value

A \`ggplot2\` object. The transformed matrix is available in the plot's
\`data\` component.

## Examples

``` r
matrix_data <- matrix(
  c(1, 3, 2, 5, 4, 6),
  nrow = 2,
  dimnames = list(c("Pathway A", "Pathway B"), c("A", "B", "C"))
)

ggheat(matrix_data, scale = "row", show_values = TRUE)
#> Warning: Ignoring empty aesthetic: `colour`.
```

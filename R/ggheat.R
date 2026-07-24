#' Create a publication-ready heatmap
#'
#' `ggheat()` accepts a numeric matrix or wide data frame directly. Tidy
#' long-form data is also supported when `x`, `y`, and `fill` identify the
#' column, row, and value variables.
#'
#' @param data A numeric matrix/data frame, or a long-form data frame.
#' @param x,y,fill Column names for long-form data. Supply all three or none.
#' @param scale Standardization mode: `"none"`, `"row"`, or `"column"`.
#' @param cluster_rows,cluster_cols Apply hierarchical clustering to rows or
#'   columns?
#' @param palette Three colours for low, midpoint, and high values.
#' @param midpoint Fill-scale midpoint. `NULL` chooses zero when the range
#'   crosses zero and the median otherwise.
#' @param na_color Colour used for missing cells.
#' @param show_values Display formatted values inside cells?
#' @param digits Number of decimal places used for displayed values.
#' @param text_color Colour for displayed values.
#' @param border_color Optional cell-border colour. `NULL` removes borders.
#' @param title,subtitle,caption Plot annotations.
#' @param xlab,ylab Axis labels.
#' @param rotate_x Rotation in degrees for horizontal-axis labels.
#'
#' @return A `ggplot2` object. The transformed matrix is available in the
#'   plot's `data` component.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' matrix_data <- matrix(
#'   c(1, 3, 2, 5, 4, 6),
#'   nrow = 2,
#'   dimnames = list(c("Pathway A", "Pathway B"), c("A", "B", "C"))
#' )
#'
#' ggheat(matrix_data, scale = "row", show_values = TRUE)
ggheat <- function(data,
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
                   rotate_x = 45) {
  scale <- match.arg(scale)
  matrix_data <- heatmap_matrix(data, x = x, y = y, fill = fill)

  if (!is.logical(cluster_rows) || length(cluster_rows) != 1 ||
      is.na(cluster_rows) ||
      !is.logical(cluster_cols) || length(cluster_cols) != 1 ||
      is.na(cluster_cols)) {
    stop("cluster_rows and cluster_cols must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.character(palette) || length(palette) != 3 || anyNA(palette)) {
    stop("palette must contain exactly three colours", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1 ||
      !is.finite(digits) || digits < 0 || digits != as.integer(digits)) {
    stop("digits must be a single non-negative integer", call. = FALSE)
  }
  if (!is.numeric(rotate_x) || length(rotate_x) != 1 || !is.finite(rotate_x)) {
    stop("rotate_x must be a single finite number", call. = FALSE)
  }

  matrix_data <- standardize_heatmap(matrix_data, scale)

  if ((cluster_rows || cluster_cols) && anyNA(matrix_data)) {
    stop("clustering requires a matrix without missing values", call. = FALSE)
  }
  if (cluster_rows && nrow(matrix_data) > 1) {
    order <- stats::hclust(stats::dist(matrix_data))$order
    matrix_data <- matrix_data[order, , drop = FALSE]
  }
  if (cluster_cols && ncol(matrix_data) > 1) {
    order <- stats::hclust(stats::dist(t(matrix_data)))$order
    matrix_data <- matrix_data[, order, drop = FALSE]
  }

  plot_data <- as.data.frame(as.table(matrix_data), stringsAsFactors = FALSE)
  names(plot_data) <- c("row", "column", "value")
  plot_data$row <- factor(
    plot_data$row,
    levels = rev(rownames(matrix_data))
  )
  plot_data$column <- factor(
    plot_data$column,
    levels = colnames(matrix_data)
  )

  finite_values <- plot_data$value[is.finite(plot_data$value)]
  if (is.null(midpoint)) {
    midpoint <- if (min(finite_values) <= 0 && max(finite_values) >= 0) {
      0
    } else {
      stats::median(finite_values)
    }
  }
  if (!is.numeric(midpoint) || length(midpoint) != 1 || !is.finite(midpoint)) {
    stop("midpoint must be a single finite number", call. = FALSE)
  }

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$column, y = .data$row, fill = .data$value)
  ) +
    ggplot2::geom_tile(colour = border_color) +
    ggplot2::scale_fill_gradient2(
      low = palette[[1]],
      mid = palette[[2]],
      high = palette[[3]],
      midpoint = midpoint,
      na.value = na_color
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = rotate_x,
        hjust = if (rotate_x == 0) 0.5 else 1
      ),
      axis.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      fill = if (is.null(fill)) "Value" else fill,
      title = title,
      subtitle = subtitle,
      caption = caption
    )

  if (isTRUE(show_values)) {
    plot <- plot + ggplot2::geom_text(
      ggplot2::aes(label = format_heatmap_value(.data$value, digits)),
      colour = text_color,
      na.rm = TRUE,
      size = 3
    )
  }

  plot
}

heatmap_matrix <- function(data, x, y, fill) {
  long_arguments <- c(x = !is.null(x), y = !is.null(y), fill = !is.null(fill))
  if (any(long_arguments) && !all(long_arguments)) {
    stop("x, y, and fill must be supplied together", call. = FALSE)
  }

  if (all(long_arguments)) {
    data <- as.data.frame(data)
    columns <- c(x, y, fill)
    if (any(!vapply(columns, function(z) is.character(z) && length(z) == 1, logical(1)))) {
      stop("x, y, and fill must be single column names", call. = FALSE)
    }
    missing_columns <- setdiff(columns, names(data))
    if (length(missing_columns) > 0) {
      stop(
        "columns not found in data: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }
    if (!is.numeric(data[[fill]])) {
      stop("fill must name a numeric column", call. = FALSE)
    }

    keys <- paste(data[[y]], data[[x]], sep = "\r")
    if (anyDuplicated(keys)) {
      stop("long-form data must contain one value per x/y combination", call. = FALSE)
    }

    row_levels <- unique(as.character(data[[y]]))
    col_levels <- unique(as.character(data[[x]]))
    result <- matrix(
      NA_real_,
      nrow = length(row_levels),
      ncol = length(col_levels),
      dimnames = list(row_levels, col_levels)
    )
    result[cbind(match(data[[y]], row_levels), match(data[[x]], col_levels))] <-
      data[[fill]]
  } else {
    result <- as.matrix(data)
    if (!is.numeric(result)) {
      stop("wide data must contain only numeric values", call. = FALSE)
    }
  }

  if (length(result) == 0 || nrow(result) == 0 || ncol(result) == 0) {
    stop("data must contain at least one row and one column", call. = FALSE)
  }
  if (!any(is.finite(result))) {
    stop("data must contain at least one finite value", call. = FALSE)
  }
  if (is.null(rownames(result))) {
    rownames(result) <- paste0("row", seq_len(nrow(result)))
  }
  if (is.null(colnames(result))) {
    colnames(result) <- paste0("column", seq_len(ncol(result)))
  }

  result
}

standardize_heatmap <- function(data, scale) {
  if (scale == "none") {
    return(data)
  }

  margin <- if (scale == "row") 1 else 2
  result <- t(apply(data, margin, function(values) {
    center <- mean(values, na.rm = TRUE)
    spread <- stats::sd(values, na.rm = TRUE)
    if (!is.finite(spread) || spread == 0) {
      return(ifelse(is.na(values), NA_real_, 0))
    }
    (values - center) / spread
  }))
  if (margin == 2) {
    result <- t(result)
  }
  dimnames(result) <- dimnames(data)
  result
}

format_heatmap_value <- function(value, digits) {
  ifelse(
    is.na(value),
    "",
    formatC(value, format = "f", digits = digits)
  )
}

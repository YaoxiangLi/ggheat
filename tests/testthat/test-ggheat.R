heat_matrix <- function() {
  matrix(
    c(1, 3, 2, 5, 4, 6),
    nrow = 2,
    dimnames = list(c("A", "B"), c("x", "y", "z"))
  )
}

test_that("wide matrices create a ggplot", {
  plot <- ggheat(heat_matrix(), show_values = TRUE)

  expect_s3_class(plot, "ggplot")
  expect_named(plot$data, c("row", "column", "value"))
  expect_equal(nrow(plot$data), 6)
})

test_that("row scaling centers each row", {
  plot <- ggheat(heat_matrix(), scale = "row")
  values <- split(plot$data$value, plot$data$row)

  expect_true(all(vapply(values, function(x) abs(mean(x)) < 1e-10, logical(1))))
})

test_that("long-form data and clustering are supported", {
  data <- data.frame(
    sample = rep(c("x", "y", "z"), 2),
    feature = rep(c("A", "B"), each = 3),
    value = c(1, 2, 3, 6, 5, 4)
  )

  plot <- ggheat(
    data,
    x = "sample",
    y = "feature",
    fill = "value",
    cluster_rows = TRUE,
    cluster_cols = TRUE
  )
  expect_s3_class(plot, "ggplot")
})

test_that("malformed inputs fail clearly", {
  expect_error(
    ggheat(data.frame(x = "a", y = "b"), x = "x"),
    "supplied together"
  )
  expect_error(
    ggheat(data.frame(x = "a", y = "b", value = "c"), x = "x", y = "y", fill = "value"),
    "numeric"
  )

  duplicated <- data.frame(x = c("a", "a"), y = c("b", "b"), value = 1:2)
  expect_error(
    ggheat(duplicated, x = "x", y = "y", fill = "value"),
    "one value"
  )
})

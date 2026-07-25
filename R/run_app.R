#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  required <- c("shiny", "golem", "config")
  missing <- required[
    !vapply(required, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    stop(
      "The optional app requires: ",
      paste(missing, collapse = ", "),
      ". Install them before calling `run_app()`.",
      call. = FALSE
    )
  }

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

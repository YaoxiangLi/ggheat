#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  data <- shiny::reactive({
    path <- if (is.null(input$data_file)) {
      system.file("extdata", "example.csv", package = "ggheat")
    } else {
      input$data_file$datapath
    }

    value <- utils::read.csv(path, check.names = FALSE)
    if (ncol(value) > 1 && !is.numeric(value[[1]])) {
      rownames(value) <- make.unique(as.character(value[[1]]))
      value <- value[-1]
    }
    value
  })

  output$heatmap <- shiny::renderPlot({
    ggheat(
      data(),
      scale = input$scale,
      cluster_rows = isTRUE(input$cluster_rows),
      cluster_cols = isTRUE(input$cluster_cols),
      show_values = isTRUE(input$show_values)
    )
  })
}

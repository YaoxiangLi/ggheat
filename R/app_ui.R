#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      shiny::titlePanel("ggheat"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput(
            "data_file",
            "Upload numeric CSV (optional)",
            accept = c(".csv", "text/csv")
          ),
          shiny::selectInput(
            "scale",
            "Standardize",
            choices = c("None" = "none", "Rows" = "row", "Columns" = "column")
          ),
          shiny::checkboxInput("cluster_rows", "Cluster rows", FALSE),
          shiny::checkboxInput("cluster_cols", "Cluster columns", FALSE),
          shiny::checkboxInput("show_values", "Show values", FALSE)
        ),
        shiny::mainPanel(
          shiny::plotOutput("heatmap", height = "700px")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "ggheat"
    )
  )
}

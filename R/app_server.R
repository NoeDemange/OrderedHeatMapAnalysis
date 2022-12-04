#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

app_server <- function(input, output, session) {
  # Your application server logic
}

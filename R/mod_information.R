#' information UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_information_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Information", status = "info", solidHeader = TRUE, collapsible = FALSE, width = 12,
          tags$div(
                   "Thanks to No\u00e9 Demange (program developer)",
                   br(),
                   "We are grateful to the INRAE MIGALE bioinformatics facility (MIGALE, INRAE, 2020. Migale bioinformatics Facility, doi: 10.15454/1.5572390655343293E12) for providing help and storage resources.",
                   br(),
                   "Contact the maintainer of the app, Guillaume Sapriel: ", icon("envelope"), tags$b("guillaume.sapriel@uvsq.fr"),
                   br(), br(),
                   "R packages used for this app :",
                   br(),
                   "-ade4",
                   br(),
                   "-circlize",
                   br(),
                   "-cluster",
                   br(),
                   "-ComplexHeatmap",
                   br(),
                   "-config",
                   br(),
                   "-dendextend",
                   br(),
                   "-DendSer",
                   br(),
                   "-dynamicTreeCut",
                   br(),
                   "-ecp",
                   br(),
                   "-golem",
                   br(),
                   "-gplots",
                   br(),
                   "-graphics",
                   br(),
                   "-grDevices",
                   br(),
                   "-grid",
                   br(),
                   "-InteractiveComplexHeatmap",
                   br(),
                   "-seriation",
                   br(),
                   "-shiny",
                   br(),
                   "-shinycssloaders",
                   br(),
                   "-shinydashboard",
                   br(),
                   "-shinyFeedback",
                   br(),
                   "-stats",
                   br(),
                   "-tools",
                   br(),
                   "-tseries",
                   br(),
                   "-utils",
                   br(),
                   "-viridis",
                   br(),
                   "-Rcpp",
          )
        ),
    )
  )
}

#' information Server Functions
#'
#' @noRd
mod_information_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_information_ui("information_1")

## To be copied in the server
# mod_information_server("information_1")

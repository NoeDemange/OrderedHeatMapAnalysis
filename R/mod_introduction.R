#' introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_introduction_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      box(title = "OrderedHeatMapAnalysis (OHMA)", status = "info", solidHeader = TRUE, collapsible = FALSE, width = 12,
          tags$div(
            tags$h2("OrderedHeatMapAnalysis (OHMA)"),
            "is a direct data analysis framework allowing to simultaneously visualize and analyze the structure of complex datasets.",
            br(),
            "An optimized seriation of rows and columns of the input data table is performed, resulting in a mapping of the whole dataset into an ordered heatmap. Following analysis of the ordered heatmap structure directly highlights submatrix of regularly ordered data. Subsequently, an exhaustive identification of biculsters laying in the subspaces of the dataset can be performed, and their mutual relationships can easily be characterized. This method allows a straitforward and deep exploration of all dimensions of the dataset.",
            br(),br(),
            "Contact, Guillaume Sapriel: ", icon("envelope"), tags$b("guillaume.sapriel@uvsq.fr"),
            br(),br()),
          HTML('<center><figure><img src = "www/Intro1.png" width ="600" alt = "Heatmap partitioning"><img src = "www/Intro2.png" width ="600" alt = "Interactive Submatrix selection"></figure></center>'),
          column(3,tags$div(
            br(),
            tags$h4(
              tags$a(href="https://www.uvsq.fr/", "Université Versailles St Quentin", target="_blank"),
            ),
            tags$figure(
              tags$img(
                src = "www/Logo-UVSQ-RF3.png",
                width = 400,
                alt = "Logo UVSQ"
                ),
              ),
            ),
          ),
          column(6,tags$div(
            br(),
            tags$h4(
              "Atelier de bio informatique - ",
              tags$a(href="http://isyeb.mnhn.fr/fr", "Institut de Systématique, Évolution, Biodiversité - UMR 7205", target="_blank"),
            ),
            ),
          ),
      ),
    )
  )
}

#' introduction Server Functions
#'
#' @noRd
mod_introduction_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_introduction_ui("introduction_1")

## To be copied in the server
# mod_introduction_server("introduction_1")

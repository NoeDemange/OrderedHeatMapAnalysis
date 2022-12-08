#' data_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Data Processing",status = "primary",solidHeader = TRUE,
          helpText(h3("Data type")),
          radioButtons(ns("typ_data"),"",choices = c( #Ajouter des informations d'aides explications
            "Binary",
            "Numerical"),
            selected = "Binary",inline = TRUE),
#Ajout mise à jour pour afficher choix pour donner binaire ou donner numérique, voir mastering shiny 10.2.1 Conditional UI
#Tabs pour afficher mode binary ou mode numerical
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("Binary",
                     helpText(h3("Filtering")),
                     helpText("Enter for rows and columns the maximum number of ones (max)
                              and the minimum number of ones (min).
                              If you do not want to make any changes, enter zero."),
                     column(6,
                            helpText(h5("Line")),
                            numericInput(ns("fl_min"), "min", value = 0),
                            numericInput(ns("fl_max"), "max", value = 0)
                     ),
                     column(6,
                            helpText(h5("Column")),
                            numericInput(ns("fc_min"), "min", value = 0),
                            numericInput(ns("fc_max"), "max", value = 0)
                     ),
                     helpText(h3("Distance calculation")),
            ),
            tabPanel("Numerical",
                     helpText("En Création"),
                     # numericInput("min", "min", value = 0),
                     # numericInput("max", "max", value = 1)
            )
          ),
        helpText(h3("Hierarchical clustering")),
        selectInput(ns('inHC'),"Clustering hierarchique", c("ward.D","ward.D2",
                                                            "single","complete",
                                                            "average","mcquitty",
                                                            "median","centroid","diana")),
        helpText(h3("Seriation")),
        radioButtons(ns('ser'),"Seriation", choices = c("Oui","Non"), selected="Oui", inline = TRUE),
        width = 12
      )
    )
  )
}

#' data_processing Server Functions
#'
#' @noRd
mod_data_processing_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$typ_data, {
      updateTabsetPanel(inputId = "params", selected = input$typ_data)
    })




  })
}

## To be copied in the UI
# mod_data_processing_ui("data_processing_1")

## To be copied in the server
# mod_data_processing_server("data_processing_1")

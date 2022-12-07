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
          radioButtons(ns("typ_data"),"Data type",choices = c( #Ajouter des informations d'aides explications
            "Binary",
            "Numerical"),
            selected = "Binary",inline = TRUE),
#Ajout mise à jour pour afficher choix pour donner binaire ou donner numérique, voir mastering shiny 10.2.1 Conditional UI
#Tabs pour afficher mode binary ou mode numerical
          parameter_tabs <- tabsetPanel(
            id = ns("params"),
            type = "hidden",
            tabPanel("Binary",
                     numericInput("mean", "mean", value = 1),
                     numericInput("sd", "standard deviation", min = 0, value = 1)
            ),
            tabPanel("Numerical",
                     numericInput("min", "min", value = 0),
                     numericInput("max", "max", value = 1)
            )
          ),
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

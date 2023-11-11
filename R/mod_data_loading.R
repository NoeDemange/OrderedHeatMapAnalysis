#' data_loading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import tools
#' @import utils
#' @importFrom shiny NS tagList
#' @import shinyFeedback
mod_data_loading_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    fluidPage(
      box(title = "Data",status = "primary",solidHeader = TRUE,
          footer = tags$div(
            "* demo dataset from :  Martin Romei, Guillaume Sapriel, Pierre Imbert, Th\u00e9o Jamay, Jacques Chomilier, Guillaume Lecointre, Mathilde Carpentier, Protein folds as synapomorphies of the tree of life, Evolution, Volume 76, Issue 8, 1 August 2022, Pages 1706-1719, ",
            tags$a(href="https://doi.org/10.1111/evo.14550", "https://doi.org/10.1111/evo.14550", target="_blank"),
          ),
          helpText(
            h4("Choose your dataset (csv formatted. Names of samples/observations and variables/features on first column and row respectively). Validate, and go to data processing.")
          ),
          radioButtons(ns("data"),"",choices = c(
            "demo (Folds/Species)*",
            "Your Dataset (.csv)"),
            selected = "demo (Folds/Species)*",inline = TRUE),
          br(),
          fileInput(ns("file"), "Import", accept = c(".csv", ".rds")),
          radioButtons(ns("sep"),"csv separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),
                       selected = ","),
          actionButton(ns("val"), "Validate"),
          width = 12
      )
    )
  )
}

#' data_loading Server Functions
#'
#' @noRd
#'
# mod_data_loading_server <- function(input, output, session, r){
mod_data_loading_server <- function(id,r=r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    r$df <- eventReactive(input$val,{
      if(input$data == "demo (Folds/Species)*"){
        datf <- OrderedHeatMapAnalysis::my_dataset
        return(datf)
      }else if(input$data == "Your Dataset (.csv)"){
        req(input$file)
        hideFeedback(inputId="file")
        if(tools::file_ext(input$file$name)=="csv"){
          tryCatch({
            datf <- utils::read.csv(input$file$datapath,
                                  header = TRUE,
                                  sep = input$sep,
                                  row.names =1
          )
          showFeedbackSuccess(inputId="file")
          return(datf)
          }, error = function(e) {
            #debug_msg(e$message)
            showFeedback(inputId = "file", text = e$message, color = "#d9534f",
                         icon = shiny::icon("exclamation-sign", lib = "glyphicon"),
                         session = shiny::getDefaultReactiveDomain())
          })
        }else{
          stop("This is not a .csv")
        }
      }
    })
  })

}

## To be copied in the UI
# mod_data_loading_ui("data_loading_1")

## To be copied in the server
# mod_data_loading_server("data_loading_1")

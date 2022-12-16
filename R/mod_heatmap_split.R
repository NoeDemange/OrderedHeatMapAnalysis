#' heatmap_split UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinycssloaders
#' @import ComplexHeatmap
#' @importFrom shiny NS tagList
mod_heatmap_split_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Settings", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Split")),
          column(3,
                 radioButtons(ns("split_row"), "Row",
                              choices = list("Yes", "No"),
                              selected = "Yes",inline = TRUE
                 )
          ),
          column(9,
                 radioButtons(ns("split_col"), "Column",
                              choices = list("Yes", "No"),
                              selected = "Yes",inline = TRUE
                 )
          ),
          helpText(h3("Heatmap")),
          selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                              "cividis","rocket","mako","turbo"),selected="magma"),
          selectInput(ns("bg_color"),"Background color",c("white","black"),selected="white"),
          textInput(ns("legend_name"),"Enter a legend name",value = "legendname"),
          actionButton(ns("val_a1"), "valider"),
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_simple"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=10
      )
    )
  )
}

#' heatmap_split Server Functions
#'
#' @noRd
mod_heatmap_split_server <- function(id, r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_heatmap_split_ui("heatmap_split_1")

## To be copied in the server
# mod_heatmap_split_server("heatmap_split_1")

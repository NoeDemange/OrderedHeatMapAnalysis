#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ui <- shinydashboard::dashboardPage(skin = "purple",
                        dashboardHeader(title = "BSH"),
                        dashboardSidebar(
                          sidebarMenu(
                            menuItem("Dataset", tabName = "dataset", icon = icon("fas fa-file-arrow-down")),
                            menuItem("Data Processing", tabName = "tdon", icon = icon("fas fa-table")),
                            menuItem("Heatmap", tabName = "ht_simp", icon = icon("fas fa-chess-board")),
                            menuItem("Splitted Heatmap", tabName = "ht_split", icon = icon("fas fa-bar-chart")),
                            style = "font-size:18px"
                          )
                          # ,
                          # br(),
                          # br(),
                          # br(),
                          # br(),
                          # br(),
                          # br(),
                          # br(),
                          # br(),
                          # column(12,
                          #        downloadButton(outputId = "down", label = "Download the plot", style="color:#000000; display: block"),
                          #        textInput("fname", "Nom du fichier", value = "Plot"),
                          #        radioButtons("ext","Type de fichier", c("png","pdf"), selected = "pdf", inline = TRUE)
                          # )
                        ),
                        dashboardBody(
                          fluidRow(
                            tabItems(
                              tabItem(tabName="dataset",
                                     mod_data_loading_ui("data_loading_1")
                              ),
                              tabItem(tabName="tdon",
                                      mod_data_processing_ui("data_processing_1")
                              ),
                              tabItem(tabName= "ht_simp",
                                      mod_heatmap_simple_ui("heatmap_simple_1")

                              )
                            )
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
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext="png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "biseriatedheatmaps"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
suppressMessages(suppressWarnings(library(utils)))
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
                            menuItem("Data Analysis Heatmap", tabName = "ht_analysis", icon = icon("fas fa-tasks")),
                            menuItem("Information", tabName = "information", icon = icon("fas fa-info-circle")),
                            style = "font-size:18px"
                          )
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
                              ),
                              tabItem(tabName= "ht_split",
                                      mod_heatmap_split_ui("heatmap_split_1")
                              ),
                              tabItem(tabName= "ht_analysis",
                                      mod_heatmap_analysis_ui("heatmap_analysis_1")
                              ),
                              tabItem(tabName= "information",
                                      mod_information_ui("information_1")
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

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

options(shiny.maxRequestSize = 30 * 1024^2)

source("internal/analysis_ui.R")
source("internal/upload_ui.R")

PUBLIC_DATA_SET <- data.frame(
  ID = c(1:2),
  name =
    c(
      "Analysis of the infant gut microbiome reveals metabolic functional roles associated with healthy infants and infants with atopic dermatitis using metaproteomics",
      "ITS2 Sequencing and Targeted Meta-Proteomics of Infant Gut Mycobiome Reveal the Functional Role of Rhodotorula sp. during Atopic Dermatitis Manifestation"
    ),
  type = c("16S", "ITS"),
  link = c(
    "#",
    "#"
  )
)

#' Main UI analysis
#'
ui_analysis <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2(textOutput("project_id")),
      menuItem("Overview", tabName = "analysis_overview"),
      menuItem("Analysis",
        startExpanded = TRUE,
        menuSubItem("Alpha diversity", tabName = "analysis_alpha"),
        menuSubItem("Beta diversity", tabName = "analysis_beta"),
        menuSubItem("Taxonomic analysis", tabName = "analysis_taxa")
      )
    ),
    mainPanel(
      tabItems(
        # First tab content
        tabItem(
          tabName = "analysis_overview",
          ui_overview("O1")
        ),
        tabItem(
          tabName = "analysis_alpha",
          ui_alpha("A1")
        ),
        tabItem(
          tabName = "analysis_beta",
          ui_beta("B1")
        ),
        tabItem(
          tabName = "analysis_taxa",
          ui_taxa("T1")
        )
      )
    )
  )
)

# # Main page
# ui <- navbarPage(
#   "SYSMIOME!",
#   tabPanel(
#     "Analysis",
#     ui_analysis
#   ),
#   tabPanel(
#     "New project",
#     ui_new_project("mod_newproject")
#   ),
#   tabPanel(
#     "Public datasets",
#     fluidPage(
#       dataTableOutput("publictable")
#     )
#   ),
#   tabPanel(
#     "Debug",
#   ),
#   navbarMenu(
#     "About",
#     tabPanel(
#       "SYSMIOME"
#     ),
#     tabPanel(
#       "Projects",
#     )
#   )
# )


ui <- dashboardPage(
  dashboardHeader(
    title = "SYSBIOME"
  ),
  dashboardSidebar(
    menuItem("Analysis", tabName = "ui_analysis"),
    menuItem("New project", tabName = "ui_newproject"),
    menuItem("Public datasets", tabName = "ui_publicdata"),
    menuItem("About",
      menuSubItem("SYSMIOME", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("ui_newproject", ui_new_project("NEW1")),
      tabItem("ui_analysis", ui_analysis),
      tabItem("ui_publicdata", fluidPage(
        dataTableOutput("publictable")
      ))
    )
  )
)



server <- function(input, output, session) {
  # Check for projectID
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["project"]])) {
      updateTextInput(session, "InputLabel_A", value = query[["project"]])
    }
    # if (!is.null(query[['paramB']])) {
    #     updateTextInput(session, "InputLabel_A", value = query[['paramB']])
    # }
  })


  output$publictable <- renderDataTable(PUBLIC_DATA_SET)
  sv_overview("O1")
  sv_alpha("A1")
  sv_beta("B1")
  sv_taxa("T1")
  sv_new_project("mod_newproject")
  output$project_id <- renderText({PROJECT_ID})
}


shinyApp(ui, server)
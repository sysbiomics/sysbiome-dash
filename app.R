library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

library(logger)

options(shiny.maxRequestSize = 30 * 1024^2)

source("R/analysis_ui.R")
source("R/upload_ui.R")

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
  selectInput("project_selector", "Project Id",
              c("Projsmall" = "PRJNA709129_data",
                "Projbig" = "PRJNAXXX_DATA")),
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
          ui_overview(id = "O1")
        ),
        tabItem(
          tabName = "analysis_alpha",
          ui_alpha(id = "A1")
        ),
        tabItem(
          tabName = "analysis_beta",
          ui_beta(id = "B1")
        ),
        tabItem(
          tabName = "analysis_taxa",
          ui_taxa(id = "T1")
        )
      )
    )
  )
)

header <-  dashboardHeader(
    title = "SYSBIOME",
    dropdownMenu(type = "notifications",
      notificationItem(
      text = "5 new users today",
      icon("users")))
  )

ui <- dashboardPage(
  header,
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
  # session$BASE_FOLDER <- "/public_data"
  # session$PROJECT_ID <- "PRJNA709129_data"
  # session$PROJECT_FOLDER <- file.path(BASE_FOLDER, PROJECT_ID)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[["project"]])) {
      updateTextInput(session, "InputLabel_A", value = query[["project"]])
    }
  })

  output$publictable <- renderDataTable(PUBLIC_DATA_SET)
  sv_overview(id = "O1")
  sv_alpha(id = "A1")
  sv_beta(id = "B1")
  sv_taxa(id = "T1")
  sv_new_project("mod_newproject")
  output$project_id <- renderText({input$project_selector})
}

shinyApp(ui, server)




# Doing MODAL
# library(shiny)
# if (interactive()) {
#   shinyApp(
#     ui <- fluidPage(
#       actionButton("reset", "RESET", style="simple", size="sm", color = "warning"),
#       verbatimTextOutput(outputId = "text")
#     ),
#     server = function(input, output, session) {
#       l <- reactiveValues()
#       observeEvent(input$reset, {
#         # display a modal dialog with a header, textinput and action buttons
#         showModal(modalDialog(
#           tags$h2('Please enter your personal information'),
#           textInput('name', 'Name'),
#           textInput('state', 'State'),
#           footer=tagList(
#             actionButton('submit', 'Submit'),
#             modalButton('cancel')
#           )
#         ))
#       })
      
#       # only store the information if the user clicks submit
#       observeEvent(input$submit, {
#         removeModal()
#         l$name <- input$name
#         l$state <- input$state
#       })
      
#       # display whatever is listed in l
#       output$text <- renderPrint({
#         if (is.null(l$name)) return(NULL)
#         paste('Name:', l$name, 'and state:', l$state)
#       })
#     }
#   )
# }
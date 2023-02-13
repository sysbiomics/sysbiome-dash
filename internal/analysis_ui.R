library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)

BASE_FOLDER <- "/public_data"
PROJECT_ID <- "PRJNA709129_data"
PROJECT_FOLDER <- file.path(BASE_FOLDER, PROJECT_ID)



#
#' Overview
#

read_overview <- function(folder) {
  dat_asv_dim <- readr::read_tsv(file.path(folder, "asv.tab"), show_col_types = F) %>% dim()
  dat_met_dim <- readr::read_tsv(file.path(folder, "metadata.tsv"), show_col_types = F) %>%
    rename_with(.cols = 1, ~"ID_sample") %>%
    dim()

  return(list(
    asv_dim = dat_asv_dim,
    met_dim = dat_met_dim
  ))
}

ui_overview <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(tags$style(paste0("#", ns("project_id"), "{color: blue;
                          font-size: 20px;
                          font-style: bold;
                          }"))),
    textOutput(ns("project_id")),
    dataTableOutput(ns("overviewtable"))
  )
}

sv_overview <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      dat_overview <- reactive({
        dat_overview <- read_overview(PROJECT_FOLDER)
        # data.frame(
        #   Number_of_sample = dat_overview$met_dim
        # )
        tibble(
          `Number of samples` = dat_overview$met_dim[[1]],
          `Number of ASV` = dat_overview$asv_dim[[2]],
          `Metadata` = dat_overview$met_dim[[2]]
        )
      })

      output$overviewtable <- renderDataTable({
        dat_overview()
      })
      output$project_id <- renderText({
        paste0("Project:", PROJECT_ID)
      })
    }
  )
}

#
# Alpha
#

#' Read alpha diversity files and metadata
#'
#' @param folder A project folder
read_alpha <- function(folder) {
  dat_a <- readr::read_tsv(file.path(folder, "alpha", "alpha_wide.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")
  dat_m <- readr::read_tsv(file.path(folder, "metadata.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")

  return(list(
    alpha = dat_a,
    meta = dat_m
  ))
}


#' Plot alpha diversity
#'
#' @param .datlist
plot_alpha <- function(.datlist, measures, fml, simplified_name = F) {
  .dat_alpha <- .datlist$alpha %>%
    tidyr::pivot_longer(-ID_sample, names_to = "alpha_measure") %>%
    dplyr::filter(alpha_measure %in% measures)

  .dat_meta <- .datlist$meta

  .dat_alpha %>%
    dplyr::left_join(.dat_meta, by = "ID_sample") %>%
    ggplot(aes(x = Date, y = value)) +
    geom_boxplot() +
    facet_wrap(~alpha_measure, scales = "free_y")
}


# Label isn't neccessary, but just for show that I can use function.
ui_alpha <- function(id, label = "S1") {
  ns <- NS(id)

  fluidPage(

    # tags$head(tags$style(paste0("#", ns("project_id"), "{color: blue; font-size: 20px; font-style: bold;}"))),
    # textOutput(ns("project_id")),
    selectInput(
      inputId = ns("alphadiv_measure"),
      label = "Measure(s)",
      choices = c(
        "Shannon" = "diversity_shannon",
        "Simpson" = "diversity_gini_simpson",
        "invSimpson" = "diversity_inverse_simpson",
        "Chao1 (richness)" = "chao1",
        "ObservedOTUs" = "observed"
      ),
      selected = c("diversity_shannon", "diversity_gini_simpson", "diversity_inverse_simpson"),
      multiple = TRUE
    ),
    plotOutput(ns("plot_alpha")),
    selectInput(
      inputId = ns("export_data"),
      label = "Export plot as",
      choices = c(
        "PNG" = "png",
        "PDF" = "pdf"
      ),
    ),
    actionButton(
      inputId = ns("export_data_button"),
      label = "Export"
    ),
    tableOutput(ns("table_alpha"))
  )
}

sv_alpha <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      dat_alpha <- reactive({
        read_alpha(PROJECT_FOLDER)
      })
      output$plot_alpha <- renderPlot(
        {
          dat_alpha() %>%
            plot_alpha(input$alphadiv_measure)
        },
        res = 96
      )
      # output$project_id <- renderText({paste0("Project:", PROJECT_ID)})
    }
  )
}

#
# / Alpha
#

#
# Beta
#

read_beta <- function(folder) {
  dat_pcoa <- readr::read_tsv(file.path(folder, "beta", "pcoa.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")
  dat_nmds <- readr::read_tsv(file.path(folder, "beta", "nmds.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")

  list(
    pcoa = dat_pcoa,
    nmds = dat_nmds
  )
}

ui_beta <- function(id, label = "S2") {
  ns <- NS(id)
  fluidPage(
    selectInput(
      inputId = ns("beta_measure"),
      label = "Type of analysis",
      choices = c(
        "PCOA" = "pcoa",
        "PCA" = "pca"
      ),
      selected = c("PCOA"),
      multiple = FALSE
    ),
    fluidRow(
      column(4, imageOutput(ns("b1"))),
      column(4, imageOutput(ns("b2")))
    ),
    selectInput(
      inputId = ns("export_data"),
      label = "Export plot as",
      choices = c(
        "PNG" = "png",
        "PDF" = "pdf"
      ),
    ),
    actionButton(
      inputId = ns("export_data_button"),
      label = "Export"
    ),
    plotOutput(ns("plot_beta")),
  )
}

sv_beta <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      dat_beta <- reactive({
        read_beta(PROJECT_FOLDER)
      })

      output$b1 <- renderImage(
        {
          img <- "/public_data/demo/beta-diversity_a-family.png"
          list(src = img, alt = "This is alternate text", width = "70%", height = "70%")
        },
        deleteFile = FALSE
      )
      output$b2 <- renderImage(
        {
          img <- "/public_data/demo/beta-diversity-pca_a-family.png"
          list(src = img, alt = "This is alternate text", width = "70%", height = "70%")
        },
        deleteFile = FALSE
      )
    }
  )
}


#
# / Beta
#

#
# Taxonomy
#
read_taxa <- function(folder) {
  dat_asv <- readr::read_tsv(file.path(folder, "asv.tab"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")
  dat_taxa <- readr::read_tsv(file.path(folder, "taxonomy.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ASV_ID")
  dat_meta <- readr::read_tsv(file.path(folder, "metadata.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")

  # Use phyloseq later

  return(list(
    taxa = dat_t,
    asv = dat_asv,
    meta = dat_meta
  ))
}

ui_taxa <- function(id, label = "S2") {
  ns <- NS(id)
  fluidPage(
    imageOutput(ns("t1")),
    imageOutput(ns("t2"))
  )
}

sv_taxa <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$t1 <- renderImage(
        {
          img <- "/public_data/demo/clustering-phylum.png"
          list(src = img, alt = "This is alternate text")
        },
        deleteFile = FALSE
      )
      output$t2 <- renderImage(
        {
          img <- "/public_data/demo/firmicute-bacteriodtes_ratio-nosig.png"
          list(src = img, alt = "This is alternate text")
        },
        deleteFile = FALSE
      )
    }
  )
}


#
# / Taxonomy
#
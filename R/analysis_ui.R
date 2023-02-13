library(shiny)
library(shinyjs)
library(shinydashboard)
library(cowplot)
library(ggplot2)

# GLOBAL
# Need to move to global soon. The problem is this one get evaluate way to soon?
BASE_FOLDER <- "/public_data"
PROJECT_ID <- "PRJNA709129_data"
PROJECT_FOLDER <- file.path(BASE_FOLDER, PROJECT_ID)

source("R/IO.R")

theme_set(theme_minimal())

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
      reactiveValues()
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
    # Change PROJECT_FOLDER to input
    function(input, output, session) {
      dat_alpha <- reactive({
        read_alpha(PROJECT_FOLDER)
      })

      measures <- function() input$alphadiv_measure
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
# Beta diversity
#

# setClass("beta_analysis", slots=list(pcoa="tbl_df", nmds="tbl_df"), contains = class(tibble()))
# new()

read_beta <- function(folder) {
  dat_pcoa <- readr::read_tsv(file.path(folder, "beta", "pcoa.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")
  dat_nmds <- readr::read_tsv(file.path(folder, "beta", "nmds.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")
  dat_meta <- readr::read_tsv(file.path(folder, "metadata.tsv"), show_col_types = F) %>% rename_with(.cols = 1, ~"ID_sample")


  list(
    pcoa = dat_pcoa,
    nmds = dat_nmds
  )
}

ui_beta <- function(id, label = "S2") {
  ns <- NS(id)
  fluidPage(
    titlePanel("Hello Shiny!"),
    fluidRow(
      column(4, selectInput(
        inputId = ns("beta_measure"),
        label = "Type of analysis",
        choices = c(
          "PCOA" = "pcoa",
          "NMDS" = "nmds"
        ),
        selected = c("PCOA"),
        multiple = FALSE)
      ),
      column(4, selectInput(
        inputId = ns("dist_measure"),
        label = "Distance",
        choices = c(
          "Bray" = "bray",
          "Jaccard" = "jaccard"
        ),
        selected = c("Bray"),
        multiple = FALSE)
      ),
      plotOutput(ns("plot_beta")),
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
  )
}

sv_beta <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      dat_beta <- reactive({
        read_beta(PROJECT_FOLDER)
      })

      output$plot_beta <- renderPlot({
        plotType <- input$beta_measure
        dat <- dat_beta() %>% purrr::pluck(plotType)
        x_axis <- colnames(dat)[2]
        y_axis <- colnames(dat)[3]

        ggplot(data=dat, aes(x=.data[[x_axis]], y=.data[[y_axis]])) + geom_point()

      })
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

  ps_dat <- LoadFolder(folder)

  # Use phyloseq later

  return(list(
    ps = ps_dat,
    taxa = dat_taxa,
    asv = dat_asv,
    meta = dat_meta
  ))
}

ui_taxa <- function(id, label = "S2") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4, selectInput(
        inputId = ns("tax_level"),
        label = "Type of analysis",
        choices = c(
          "Phylum" = "phylum",
          "Family" = "family"
        ),
        selected = c("Phylum"),
        multiple = FALSE)
      ),
      column(4, selectInput(
        inputId = ns("tax_group"),
        label = "Group",
        choices = NULL,
        multiple = FALSE)
      ),
    ),
    plotOutput(ns("plot_bar"))
  )
}


# Need for taxonomy wranggle
library(microbiome)

sv_taxa <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      choices_group <- reactive({c("perturbation", "Date", "others")})

      # Update group selection
      # https://stackoverflow.com/questions/40152857/how-to-dynamically-populate-dropdown-box-choices-in-shiny-dashboard
      observeEvent(choices_group(), {
        updateSelectInput(session = session, inputId = "tax_group", choices = choices_group())
      })

      dat_ps <- reactive({
        LoadFolder(PROJECT_FOLDER)
      })

      output$plot_bar <- renderPlot({
        tlevel <- input$tax_level
        # str(tlevel)
        dat_ps() %>%
          aggregate_taxa(tlevel) %>%
          microbiome::transform("compositional") %>%
          FastMelt("Date") %>%
          ggplot() + aes(x = ID_sample, y = abn, fill = .data[[tlevel]]) + geom_bar(stat="identity")
      })
    }
  )
}

#
# / Taxonomy
#

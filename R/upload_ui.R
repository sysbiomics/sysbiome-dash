#
# Manage upload function here
#

#
# Meta data validation
#
metadata_validation <- function(){

}

ui_new_project <- function(id, label = "S3") {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fluidPage(
        textInput(
          inputId = ns("project_name"),
          label = "Project name",
          value = "My project"
        ),
        textInput(
          inputId = ns("summiter_email"),
          label = "Email",
          value = "YourEmail@gmail.com"
        ),
        fileInput(
          inputId = ns("project_metadata"),
          label = "Project metadata",
          accept = c(
            ".csv",
            ".tsv"
          )
        ),
        fileInput("fastq_files",
          "Raw sequences (FASTQ)",
          multiple = TRUE,
          accept = c(
            ".fq.gz",
            "application/zip",
            "application/x-zip-compressed",
            "application/x-compressed",
            "multipart/x-zip",
            "text/gzip"
          )
        ),
        actionButton(
          inputId = ns("project_submit"),
          label = "Submit"
        ),
        tags$a(href="www.rstudio.com", "Click here!"),
        tags$a(href="www.rstudio.com", "Click here!"),
        actionButton(
          inputId = ns("project_sample"),
          label = "Example input"
        ),
        actionButton(
          inputId = ns("debug_submit"),
          label = "Debug"
        ),
      )
    ),
    mainPanel(
      h1("Input file statistics:"),
    )
  )
}

sv_new_project <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$project_submit, {
        # Create project folder
        project_folder <- file.path("/incoming_data", input$project_name)
        dir.create(project_folder)

        # Save metadata
        writeLines(input$project_file, file.path(project_folder, "metadata.tsv"))

        # Save fastq files
        for (i in 1:length(input$fastq_files)) {
          writeLines(input$fastq_files[[i]], file.path(project_folder, paste0("fastq_", i, ".gz")))
        }

        # Send email
        send_mail(
          from = ""
        )
      })

      
    }
  )
}

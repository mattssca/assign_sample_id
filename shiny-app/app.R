library(shiny)
library(dplyr)
library(tibble)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(DT)
library(AssignSampleIDs)

ui <- fluidPage(
  titlePanel("Assign Sample IDs"),
  theme = bs_theme(bootswatch = "minty"),
  helpText("Upload a tab-delimited TXT file with your sample data."),
  p("This app assigns unique sample IDs, annotate tumor Information and allows you to download the results."),
  h4("Instructions"),
  tags$small(
    tags$ul(
      tags$li("Upload your data file (with the required columns Lab ID, Personal ID and Date)."),
      tags$li("For each required data field, choose the correct column from the corresponding drop-down menu."),
      tags$li("Click 'Assign Sample IDs' to process."),
      tags$li("Optional: check the Full Return checkbox to keep all columns."),
      tags$li("Optional: upload a previous batch to ensure consistent sample IDs."),
      tags$li("Download your results, or view them in the page.")
    )
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("full_return", "Full Return (all columns)", value = FALSE),
      fileInput("file", "Upload TXT File", accept = ".txt"),
      fileInput("prev_batch", "Upload Previous Batch (optional)", accept = ".txt"),
      numericInput("start_id", "Starting Sample ID", value = 1, min = 1),
      uiOutput("col_selectors"),
      actionButton("run", "Assign Sample IDs")
    ),
    mainPanel(
      downloadButton("download", "Download Results"),
      br(),
      DTOutput("result")
    )
  )
)

server <- function(input, output, session) {
  user_data <- reactive({
    req(input$file)
    read.table(input$file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  })

  prev_batch_df <- reactive({
    req(input$prev_batch)
    read.table(input$prev_batch$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  })

  output$col_selectors <- renderUI({
    req(user_data())
    cols <- names(user_data())
    choices <- c("Select column" = "", cols)
    tagList(
      pickerInput("lab_id_col", "Lab ID column", choices = choices, selected = "",
                  options = list(title = "Select column")),
      pickerInput("personal_id_col", "Personal ID column", choices = choices, selected = "",
                  options = list(title = "Select column")),
      pickerInput("date_col", "Date of Sample column", choices = choices, selected = "",
                  options = list(title = "Select column"))
    )
  })

  observe({
    all_selected <- input$lab_id_col != "" &&
      input$personal_id_col != "" &&
      input$date_col != ""
    all_unique <- length(unique(c(input$lab_id_col, input$personal_id_col, input$date_col))) == 3
    shinyjs::toggleState("run", all_selected && all_unique)
  })

  result <- eventReactive(input$run, {
    req(user_data(), input$start_id)
    selected_cols <- c(input$lab_id_col, input$personal_id_col, input$date_col)
    data <- user_data()

    # Check for empty or duplicate selections
    if (any(selected_cols == "")) {
      showNotification("Please select all columns.", type = "error")
      return(NULL)
    }
    if (length(unique(selected_cols)) != 3) {
      showNotification("Please select three different columns.", type = "error")
      return(NULL)
    }

    # ---- BEGIN: Informative content checks ----
    if (!all(grepl("KF\\d+", as.character(data[[input$lab_id_col]])))) {
      showNotification("The selected Lab ID column does not appear to contain valid lab IDs (should contain 'KF' and numbers).", type = "error")
      return(NULL)
    }
    if (!all(grepl("^\\d{8}-\\d{4}$", as.character(data[[input$personal_id_col]])))) {
      showNotification("The selected Personal ID column does not appear to contain valid personal IDs (should be in the format ########-####).", type = "error")
      return(NULL)
    }
    if (any(is.na(as.Date(as.character(data[[input$date_col]]), format = "%Y-%m-%d")))) {
      showNotification("The selected Date column does not appear to contain valid dates (should be in the format YYYY-MM-DD).", type = "error")
      return(NULL)
    }
    # ---- END: Informative content checks ----

    previous_batch <- NULL
    if (!is.null(input$prev_batch)) {
      previous_batch <- prev_batch_df()
    }

    out <- tryCatch({
      assign_sample_id(
        this_data = data,
        start_id = input$start_id,
        lab_id_col = input$lab_id_col,
        personal_id_col = input$personal_id_col,
        date_col = input$date_col,
        previous_batch = previous_batch,
        verbose = FALSE,
        return_full = input$full_return
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })

    out
  })

  output$result <- renderDT({
    req(result())
    invalid <- attr(result(), "invalid_rows")
    if (is.null(invalid) || nrow(invalid) == 0) {
      datatable(data.frame(Message = "No invalid samples found."), options = list(dom = 't'))
    } else {
      datatable(invalid, options = list(pageLength = 10))
    }
  })

  output$download <- downloadHandler(
    filename = function() "assigned_sample_ids.txt",
    content = function(file) {
      valid <- result()
      # Remove the attribute so only valid data is written
      attr(valid, "invalid_rows") <- NULL
      write.table(valid, file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

shinyApp(ui, server)

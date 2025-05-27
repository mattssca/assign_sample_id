library(shiny)
library(dplyr)
library(tibble)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(AssignSampleIDs)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  helpText("Upload a tab-delimited TXT file with your sample data."),
  useShinyjs(),
  titlePanel("Assign Sample IDs and Annotate Tumor Information"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload TXT File", accept = ".txt"),
      numericInput("start_id", "Starting Sample ID", value = 1, min = 1),
      uiOutput("col_selectors"),
      actionButton("run", "Assign Sample IDs")
    ),
    mainPanel(
      tableOutput("result"),
      downloadButton("download", "Download Results")
    )
  )
)

server <- function(input, output, session) {
  user_data <- reactive({
    req(input$file)
    read.table(input$file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
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
    # Lab ID: must contain 'KF' and digits
    if (!all(grepl("KF\\d+", as.character(data[[input$lab_id_col]])))) {
      showNotification("The selected Lab ID column does not appear to contain valid lab IDs (should contain 'KF' and numbers).", type = "error")
      return(NULL)
    }
    # Personal ID: must look like 8 digits, dash, 4 digits
    if (!all(grepl("^\\d{8}-\\d{4}$", as.character(data[[input$personal_id_col]])))) {
      showNotification("The selected Personal ID column does not appear to contain valid personal IDs (should be in the format ########-####).", type = "error")
      return(NULL)
    }
    # Date: must be parseable as YYYY-MM-DD
    if (any(is.na(as.Date(as.character(data[[input$date_col]]), format = "%Y-%m-%d")))) {
      showNotification("The selected Date column does not appear to contain valid dates (should be in the format YYYY-MM-DD).", type = "error")
      return(NULL)
    }
    # ---- END: Informative content checks ----

    out <- tryCatch({
      assign_sample_id(
        this_data = data,
        start_id = input$start_id,
        lab_id_col = input$lab_id_col,
        personal_id_col = input$personal_id_col,
        date_col = input$date_col,
        verbose = FALSE
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })

    # Only return lab_id and sample_id columns
    if (!is.null(out)) {
      out <- out %>% select(lab_id, sample_id)
    }
    out
  })

  output$result <- renderTable({
    req(result())
    result()
  })

  output$download <- downloadHandler(
    filename = function() "assigned_sample_ids.txt",
    content = function(file) {
      write.table(result(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )
}

shinyApp(ui, server)

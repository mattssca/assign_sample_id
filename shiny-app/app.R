library(shiny)
library(dplyr)
library(tibble)
library(shinyjs)
library(AssignSampleIDs)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Assign Sample IDs and Annotate Tumor Information"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload TXT File", accept = ".txt"),
      numericInput("start_id", "Starting Sample ID", value = 100, min = 1),
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
  
  # Dynamically generate column selectors after file upload
  output$col_selectors <- renderUI({
    req(user_data())
    cols <- names(user_data())
    tagList(
      selectInput("lab_id_col", "Lab ID column", choices = cols, selected = "lab_id"),
      selectInput("personal_id_col", "Personal ID column", choices = cols, selected = "personal_id"),
      selectInput("date_col", "Date of Sample column", choices = cols, selected = "date_of_sample")
    )
  })

  observe({
  shinyjs::toggleState("run", 
    !is.null(input$lab_id_col) && 
    !is.null(input$personal_id_col) && 
    !is.null(input$date_col)
  )
})
  
  result <- eventReactive(input$run, {
    req(user_data(), input$start_id, input$lab_id_col, input$personal_id_col, input$date_col)
    tryCatch({
      assign_sample_id(
        this_data = user_data(),
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
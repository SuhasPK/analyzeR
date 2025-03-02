# Module UI
readRUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("read_data_button"), "Read Data"),
    h6("Dataset Summary"),
    DTOutput(ns("dataset_summary")),
    h6("Statistics Summary"),
    DTOutput(ns("stats_summary")),
    h6("Frequency Table for Categorical Data"),
    DTOutput(ns("frequency_table"))
  )
}

# Module Server
readRServer <- function(input, output, session, dataset) {
  ns <- session$ns
  
  observeEvent(input$read_data_button, {
    showModal(modalDialog(
      title = "Read Data",
      "Do you want readR to read the data?",
      footer = tagList(
        modalButton("No"),
        actionButton(ns("go_to_readR"), "Yes")
      )
    ))
  })
  
  observeEvent(input$go_to_readR, {
    removeModal()
    
    output$dataset_summary <- renderDT({
      req(dataset())
      
      # List all the field names and their data types
      summary_df <- data.frame(
        Variable = names(dataset()),
        Type = sapply(dataset(), class)
      )
      
      datatable(summary_df, options = list(
        pageLength = 10,
        lengthMenu = c(10, 20, 50),
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ), class = "display compact cell-border stripe hover")
    })
    
    output$stats_summary <- renderDT({
      req(dataset())
      data <- dataset()
      
      summary_stats <- data.frame(
        Variable = names(data),
        Mean = sapply(data, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 3) else NA),
        Median = sapply(data, function(x) if(is.numeric(x)) round(median(x, na.rm = TRUE), 3) else NA),
        SD = sapply(data, function(x) if(is.numeric(x)) round(sd(x, na.rm = TRUE), 3) else NA),
        Range = sapply(data, function(x) if(is.numeric(x)) paste(round(min(x, na.rm = TRUE), 3), "-", round(max(x, na.rm = TRUE), 3)) else NA),
        IQR = sapply(data, function(x) if(is.numeric(x)) round(IQR(x, na.rm = TRUE), 3) else NA),
        Missing = sapply(data, function(x) sum(is.na(x)))
      )
      
      datatable(summary_stats, options = list(
        pageLength = 10,
        lengthMenu = c(10, 20, 50),
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-left', targets = '_all')
        ),
        dom = 't',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ), class = "display compact cell-border stripe hover")
    })
    
    output$frequency_table <- renderDT({
      req(dataset())
      data <- dataset()
      
      freq_tables <- lapply(data, function(x) if((is.factor(x) || is.character(x)) && !any(is.na(x))) table(x) else NULL)
      non_null_tables <- freq_tables[!sapply(freq_tables, is.null)]
      
      if (length(non_null_tables) > 0) {
        freq_df <- do.call(rbind, lapply(names(non_null_tables), function(var) {
          freq <- non_null_tables[[var]]
          data.frame(
            Variable = var,
            Category = names(freq),
            Frequency = round(as.vector(freq), 3),
            stringsAsFactors = FALSE
          )
        }))
        
        datatable(freq_df, options = list(
          pageLength = 10,
          lengthMenu = c(10, 20, 50),
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-left', targets = '_all')
          ),
          dom = 't',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        ), class = "display compact cell-border stripe hover")
      } else {
        return(NULL)
      }
    })
  })
}

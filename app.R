library(shiny)
library(readxl)
library(DT)
library(bslib)
library(tools)
library(writexl)
library(dplyr)

ui <- page_fluid(
  title = "Data Search and Edit Tool",

  #Setting card style
  tags$style(HTML("
    .card-header {
      text-align: center !important;
      font-size: 24px !important;
      font-weight: 600 !important;
    }
  ")),

  card(
    card_header("Upload Data"),
    fileInput("file", "Choose CSV or Excel file", accept = c(".csv", ".xls", ".xlsx")),
    selectInput("delim", "CSV Delimiter:", c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
    height="350px"
  ),

  uiOutput("dataUI")
)

server <- function(input, output, session) {
  values <- reactiveValues(
    data = NULL,
    filtered_data = NULL,
    filtered_indices = NULL,
    original_file_name = NULL,
    file_ext = NULL
  )

  # Load uploaded file
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    values$original_file_name <- tools::file_path_sans_ext(input$file$name)
    values$file_ext <- ext

    values$data <- if(ext == "csv") {
      read.csv(input$file$datapath, sep = input$delim, stringsAsFactors = FALSE)
    } else if(ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath)
    } else {
      showNotification("Unsupported file format", type = "error")
      return(NULL)
    }

    values$filtered_data <- values$data
    values$filtered_indices <- seq_len(nrow(values$data))
  })

  # Create UI elements once data is loaded
  output$dataUI <- renderUI({
    req(values$data)

    tagList(
      card(
        card_header("Search Data"),
        layout_sidebar(
          sidebar = sidebar(
            width = 350,

            # Column selection
            selectInput("filterColumns", "Filter by Columns:",
                        choices = colnames(values$data), selected = NULL, multiple = TRUE),

            # Dynamic field selectors
            uiOutput("fieldSelectors"),

            # Text search
            hr(), h4("Text Search"),
            textInput("searchText", "Search Term:"),
            selectInput("searchColumns", "Search in Columns:",
                        choices = c("All Columns", colnames(values$data)),
                        selected = "All Columns", multiple = TRUE),
            actionButton("searchBtn", "Search", class = "btn-primary"),

            # Row filtering
            hr(), h4("Filter by Row Numbers"),
            textInput("rowFilter", "Enter Row Numbers:", placeholder = "e.g., 1-5, 8, 10-12"),
            helpText("Specify individual rows or ranges separated by commas"),
            actionButton("applyRowFilter", "Apply Row Filter", class = "btn-primary"),

            #Reset Filters
            actionButton("resetBtn", "Reset All Filters", class = "btn-secondary"),

            # Replace functionality
            hr(), h4("Replace Text"),
            textInput("replaceText", paste0("Replace ","'Search Term'"," With:")),
            helpText("Search term to be replaced is case sensitive"),
            actionButton("replaceBtn", "Replace All", class = "btn-warning"),

            # Row Management
            hr(), h4("Row Management"),
            actionButton("addRowBtn", "Add New Row", class = "btn-success", width = "100%"),
            actionButton("removeRowsBtn", "Remove Selected Rows", class = "btn-danger", width = "100%")
          ),
          card(
            card_header("Results"),
            helpText("Double-click on cells to manually edit content"),
            DTOutput("resultsTable"),
            downloadButton("downloadBtn", "Download Edited Data")
          )
        )
      )
    )
  })

  # Generate field selectors
  output$fieldSelectors <- renderUI({
    req(values$data, input$filterColumns)

    if (length(input$filterColumns) == 0) return(NULL)

    field_selectors <- lapply(input$filterColumns, function(col) {
      unique_values <- sort(unique(as.character(values$data[[col]])))
      selectizeInput(
        inputId = paste0("select_", col),
        label = paste("Select", col, "values:"),
        choices = unique_values,
        selected = unique_values,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })

    tagList(
      h4("Field Selection"),
      field_selectors,
      actionButton("applyFieldFilters", "Apply Field Filters", class = "btn-primary")
    )
  })

  # Parse row numbers
  parseRowNumbers <- function(rowInput, maxRows) {
    if (is.null(rowInput) || rowInput == "") return(NULL)

    parts <- trimws(unlist(strsplit(rowInput, ",")))
    row_indices <- c()

    for (part in parts) {
      if (grepl("-", part)) {
        range_parts <- as.numeric(trimws(unlist(strsplit(part, "-"))))
        if (length(range_parts) == 2 && !any(is.na(range_parts))) {
          row_indices <- c(row_indices, range_parts[1]:range_parts[2])
        }
      } else {
        num <- as.numeric(part)
        if (!is.na(num)) row_indices <- c(row_indices, num)
      }
    }

    valid_indices <- row_indices[row_indices > 0 & row_indices <= maxRows]
    if (length(valid_indices) == 0) return(NULL)

    return(unique(sort(valid_indices)))
  }

  # Apply row filter
  observeEvent(input$applyRowFilter, {
    req(values$data, input$rowFilter)

    row_indices <- parseRowNumbers(input$rowFilter, nrow(values$data))

    if (is.null(row_indices)) {
      showNotification("Invalid row numbers. Please check your input.", type = "warning")
      return()
    }

    values$filtered_data <- values$data[row_indices, , drop = FALSE]
    values$filtered_indices <- row_indices

    showNotification(paste("Filtered to", length(row_indices), "rows"), type = "message")
  })

  # Apply field filters
  observeEvent(input$applyFieldFilters, {
    req(values$data, input$filterColumns)

    if (length(input$filterColumns) == 0) {
      values$filtered_data <- values$data
      values$filtered_indices <- seq_len(nrow(values$data))
      return()
    }

    filtered_data <- values$data

    for (col in input$filterColumns) {
      selected_values <- input[[paste0("select_", col)]]
      if (!is.null(selected_values) && length(selected_values) > 0) {
        filtered_data <- filtered_data[filtered_data[[col]] %in% selected_values, , drop = FALSE]
      }
    }

    if (nrow(filtered_data) > 0) {
      # Get indices in original data
      filtered_indices <- which(sapply(seq_len(nrow(values$data)), function(i) {
        row <- values$data[i, , drop = FALSE]
        any(apply(filtered_data, 1, function(fd_row) all(fd_row == row)))
      }))

      values$filtered_data <- filtered_data
      values$filtered_indices <- filtered_indices
    } else {
      values$filtered_data <- data.frame()
      values$filtered_indices <- integer(0)
      showNotification("No matching records found", type = "warning")
    }
  })

  # Text search
  observeEvent(input$searchBtn, {
    req(values$filtered_data, input$searchText)
    search_term <- tolower(input$searchText)

    search_cols <- if(length(input$searchColumns) == 0 || "All Columns" %in% input$searchColumns) {
      colnames(values$filtered_data)
    } else {
      intersect(input$searchColumns, colnames(values$filtered_data))
    }

    # Find matching rows
    matching_rows <- which(apply(values$filtered_data, 1, function(row) {
      any(sapply(search_cols, function(col) {
        grepl(search_term, tolower(as.character(row[col])), fixed = TRUE)
      }))
    }))

    if (length(matching_rows) > 0) {
      values$filtered_data <- values$filtered_data[matching_rows, , drop = FALSE]
      values$filtered_indices <- values$filtered_indices[matching_rows]
    } else {
      values$filtered_data <- data.frame()
      values$filtered_indices <- integer(0)
      showNotification("No matching records found", type = "warning")
    }
  })

  # Reset filters
  observeEvent(input$resetBtn, {
    values$filtered_data <- values$data
    values$filtered_indices <- seq_len(nrow(values$data))
    updateSelectInput(session, "filterColumns", selected = character(0))
    updateTextInput(session, "rowFilter", value = "")
    updateTextInput(session, "searchText", value = "")
  })

  # Replace functionality
  observeEvent(input$replaceBtn, {
    req(values$filtered_data, input$searchText, input$replaceText)

    search_term <- input$searchText
    replace_term <- input$replaceText

    replace_cols <- if(length(input$searchColumns) == 0 || "All Columns" %in% input$searchColumns) {
      colnames(values$filtered_data)
    } else {
      intersect(input$searchColumns, colnames(values$filtered_data))
    }

    replace_count <- 0

    for (i in seq_len(nrow(values$filtered_data))) {
      for (col in replace_cols) {
        if (is.character(values$filtered_data[[col]])) {
          original_value <- values$filtered_data[i, col]
          new_value <- gsub(search_term, replace_term, original_value, fixed = TRUE)

          if (original_value != new_value) {
            replace_count <- replace_count + 1
            values$filtered_data[i, col] <- new_value
            original_index <- values$filtered_indices[i]
            values$data[original_index, col] <- new_value
          }
        }
      }
    }

    if (replace_count > 0) {
      showNotification(paste0("Replaced ", replace_count, " instances of '",
                              search_term, "' with '", replace_term, "'"), type = "message")
    } else {
      showNotification("No replacements made", type = "warning")
    }
  })

  # Add row button handler
  observeEvent(input$addRowBtn, {
    req(values$data)
    showModal(modalDialog(
      title = "Add New Row",

      # Generate inputs for each column in the data
      lapply(colnames(values$data), function(col) {
        col_type <- class(values$data[[col]])[1]

        if (col_type %in% c("numeric", "integer", "double")) {
          numericInput(paste0("new_", col), label = col, value = 0)
        } else if (col_type == "logical") {
          checkboxInput(paste0("new_", col), label = col, value = FALSE)
        } else {
          textInput(paste0("new_", col), label = col, value = "")
        }
      }),

      footer = tagList(
        actionButton("cancelAddRow", "Cancel", class = "btn-secondary"),
        actionButton("confirmAddRow", "Add", class = "btn-success")
      )
    ))
  })

  # Handle add row confirmation
  observeEvent(input$confirmAddRow, {
    new_row <- sapply(colnames(values$data), function(col) {
      input[[paste0("new_", col)]]
    })

    # Add the new row to both data frames
    values$data <- rbind(values$data, new_row)

    # Only add to filtered data if it meets current filter criteria
    # For simplicity, we'll add it to filtered data and update indices
    values$filtered_data <- rbind(values$filtered_data, new_row)
    values$filtered_indices <- c(values$filtered_indices, nrow(values$data))

    removeModal()
    showNotification("New row added successfully", type = "message")
  })

  # Cancel add row modal
  observeEvent(input$cancelAddRow, {
    removeModal()
  })

  # Remove rows functionality
  observeEvent(input$removeRowsBtn, {
    req(values$filtered_data)
    selected_rows <- input$resultsTable_rows_selected

    if (length(selected_rows) > 0) {
      # First, get the original indices to remove
      original_indices_to_remove <- values$filtered_indices[selected_rows]

      # Remove from filtered data
      values$filtered_data <- values$filtered_data[-selected_rows, , drop = FALSE]
      values$filtered_indices <- values$filtered_indices[-selected_rows]

      # Remove from original data
      # Need to handle in descending order to avoid index shifting problems
      original_indices_to_remove <- sort(original_indices_to_remove, decreasing = TRUE)
      for (idx in original_indices_to_remove) {
        values$data <- values$data[-idx, , drop = FALSE]
      }

      # Update filtered indices to account for removed rows
      values$filtered_indices <- match(values$filtered_indices,
                                       setdiff(seq_len(nrow(values$data) + length(original_indices_to_remove)),
                                               original_indices_to_remove))

      showNotification(paste("Removed", length(selected_rows), "row(s)"), type = "message")
    } else {
      showNotification("No rows selected for removal", type = "warning")
    }
  })

  # Display results
  output$resultsTable <- renderDT({
    req(values$filtered_data)

    DT::datatable(
      values$filtered_data,
      editable = TRUE,
      selection = "multiple",
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50, 100),
        scrollX = TRUE
      ),
      class = "cell-border stripe"
    )
  })

  # Handle cell edits
  observeEvent(input$resultsTable_cell_edit, {
    info <- input$resultsTable_cell_edit
    row_index <- values$filtered_indices[info$row]
    col_name <- colnames(values$filtered_data)[info$col]

    values$filtered_data[info$row, info$col] <- info$value
    values$data[row_index, col_name] <- info$value
  })

  # Download functionality
  output$downloadBtn <- downloadHandler(
    filename = function() {
      # Generate filename based on the original file
      if (values$file_ext %in% c("xls", "xlsx")) {
        paste0(values$original_file_name, "_edited.xlsx")
      } else {
        paste0(values$original_file_name, "_edited.csv")
      }
    },
    content = function(file) {
      if (values$file_ext %in% c("xls", "xlsx")) {
        writexl::write_xlsx(values$data, file)
      } else {
        write.csv(values$data, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)

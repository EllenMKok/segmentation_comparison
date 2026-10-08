library(shiny)
library(readxl)
library(dplyr)
library(writexl)
library(stringdist)
library(DT)

ui <- fluidPage(
  
  titlePanel("Segmentation Comparison"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Person 1"),
      textInput("name1", "Name:", value = ""),
      fileInput(
        "file1",
        "Upload segmented file:",
        accept = c(".xlsx", ".xls")
      ),
      
      hr(),
      
      h4("Person 2"),
      textInput("name2", "Name:", value = ""),
      fileInput(
        "file2",
        "Upload segmented file:",
        accept = c(".xlsx", ".xls")
      ),
      
      hr(),
      
      actionButton(
        "align",
        "Align segmentations",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      uiOutput("overlap_percentage"),
      
      hr(),
      
      downloadButton(
        "download",
        "Download aligned data"
      )
    ),
    
    mainPanel(
      
      h3("Alignment"),
      
      uiOutput("alignment_info"),
      
      DTOutput("alignment_table")
    )
  )
)


server <- function(input, output, session) {
  
  
  # ------------------------------------------------------------
  # Read Person 1 data
  # ------------------------------------------------------------
  
  data1 <- reactive({
    
    req(input$file1)
    
    read_excel(input$file1$datapath) %>%
      mutate(
        Response_ID = as.character(Response_ID),
        Split_ID = as.character(Split_ID),
        Text = as.character(Text)
      ) %>%
      select(
        Response_ID,
        Split_ID,
        Text
      )
  })
  
  
  # ------------------------------------------------------------
  # Read Person 2 data
  # ------------------------------------------------------------
  
  data2 <- reactive({
    
    req(input$file2)
    
    read_excel(input$file2$datapath) %>%
      mutate(
        Response_ID = as.character(Response_ID),
        Split_ID = as.character(Split_ID),
        Text = as.character(Text)
      ) %>%
      select(
        Response_ID,
        Split_ID,
        Text
      )
  })
  
  
  # ------------------------------------------------------------
  # Align one response
  # ------------------------------------------------------------
  
  align_response <- function(df1, df2) {
    
    n <- nrow(df1)
    m <- nrow(df2)
    
    cost <- matrix(
      Inf,
      nrow = n + 1,
      ncol = m + 1
    )
    
    cost[1, 1] <- 0
    
    
    # Cost of aligning a segment with a blank
    gap1 <- sapply(
      df1$Text,
      function(x) {
        stringdist(
          x,
          "",
          method = "lv"
        )
      }
    )
    
    gap2 <- sapply(
      df2$Text,
      function(x) {
        stringdist(
          x,
          "",
          method = "lv"
        )
      }
    )
    
    
    # First column
    if (n > 0) {
      
      for (i in seq_len(n)) {
        cost[i + 1, 1] <- cost[i, 1] + gap1[i]
      }
    }
    
    
    # First row
    if (m > 0) {
      
      for (j in seq_len(m)) {
        cost[1, j + 1] <- cost[1, j] + gap2[j]
      }
    }
    
    
    # Dynamic programming
    if (n > 0 && m > 0) {
      
      for (i in seq_len(n)) {
        
        for (j in seq_len(m)) {
          
          match_cost <- stringdist(
            df1$Text[i],
            df2$Text[j],
            method = "lv"
          )
          
          diagonal <- cost[i, j] + match_cost
          up <- cost[i, j + 1] + gap1[i]
          left <- cost[i + 1, j] + gap2[j]
          
          cost[i + 1, j + 1] <- min(
            diagonal,
            up,
            left
          )
        }
      }
    }
    
    
    # ----------------------------------------------------------
    # Trace back through matrix
    # ----------------------------------------------------------
    
    i <- n
    j <- m
    
    alignment <- list()
    
    
    while (i > 0 || j > 0) {
      
      if (i > 0 && j > 0) {
        
        match_cost <- stringdist(
          df1$Text[i],
          df2$Text[j],
          method = "lv"
        )
        
        diagonal <- cost[i, j] + match_cost
        up <- cost[i, j + 1] + gap1[i]
        left <- cost[i + 1, j] + gap2[j]
        
        
        # Prefer diagonal when tied
        if (
          diagonal <= up &&
          diagonal <= left
        ) {
          
          alignment[[length(alignment) + 1]] <- list(
            i = i,
            j = j
          )
          
          i <- i - 1
          j <- j - 1
          
        } else if (up <= left) {
          
          alignment[[length(alignment) + 1]] <- list(
            i = i,
            j = NA
          )
          
          i <- i - 1
          
        } else {
          
          alignment[[length(alignment) + 1]] <- list(
            i = NA,
            j = j
          )
          
          j <- j - 1
        }
        
      } else if (i > 0) {
        
        alignment[[length(alignment) + 1]] <- list(
          i = i,
          j = NA
        )
        
        i <- i - 1
        
      } else {
        
        alignment[[length(alignment) + 1]] <- list(
          i = NA,
          j = j
        )
        
        j <- j - 1
      }
    }
    
    
    alignment <- rev(alignment)
    
    
    # ----------------------------------------------------------
    # Create aligned data
    # ----------------------------------------------------------
    
    result <- lapply(
      alignment,
      function(x) {
        
        text1 <- if (is.na(x$i)) {
          NA_character_
        } else {
          df1$Text[x$i]
        }
        
        text2 <- if (is.na(x$j)) {
          NA_character_
        } else {
          df2$Text[x$j]
        }
        
        
        # TRUE when both texts exist and
        # Levenshtein distance is <= 2
        text_identical <- if (
          !is.na(text1) &&
          !is.na(text2)
        ) {
          
          stringdist(
            text1,
            text2,
            method = "lv"
          ) <= 2
          
        } else {
          
          FALSE
        }
        
        
        data.frame(
          
          Response_ID = ifelse(
            n > 0,
            df1$Response_ID[1],
            df2$Response_ID[1]
          ),
          
          Person1_Split_ID = if (is.na(x$i)) {
            NA_character_
          } else {
            df1$Split_ID[x$i]
          },
          
          Person1_Text = text1,
          
          Person2_Split_ID = if (is.na(x$j)) {
            NA_character_
          } else {
            df2$Split_ID[x$j]
          },
          
          Person2_Text = text2,
          
          Text_identical = text_identical,
          
          stringsAsFactors = FALSE
        )
      }
    )
    
    
    bind_rows(result)
  }
  
  
  # ------------------------------------------------------------
  # Perform alignment
  # ------------------------------------------------------------
  
  aligned_data <- eventReactive(
    input$align,
    {
      
      req(data1())
      req(data2())
      
      
      common_ids <- intersect(
        unique(data1()$Response_ID),
        unique(data2()$Response_ID)
      )
      
      
      validate(
        need(
          length(common_ids) > 0,
          "The two files do not have any matching Response_IDs."
        )
      )
      
      
      all_results <- lapply(
        common_ids,
        function(id) {
          
          df1 <- data1() %>%
            filter(Response_ID == id)
          
          df2 <- data2() %>%
            filter(Response_ID == id)
          
          align_response(
            df1,
            df2
          )
        }
      )
      
      
      bind_rows(all_results)
    }
  )
  
  
  # ------------------------------------------------------------
  # Store manual TRUE/FALSE changes
  # ------------------------------------------------------------
  
  manual_matches <- reactiveVal(NULL)
  
  
  # ------------------------------------------------------------
  # Final data including manual changes
  # ------------------------------------------------------------
  
  final_aligned_data <- reactive({
    
    df <- aligned_data()
    
    req(df)
    
    manual <- manual_matches()
    
    if (!is.null(manual)) {
      df$Text_identical <- manual
    }
    
    df
  })
  
  
  # ------------------------------------------------------------
  # Toggle TRUE/FALSE when clicked
  # ------------------------------------------------------------
  
  observeEvent(
    input$toggle_match,
    {
      
      df <- aligned_data()
      
      req(df)
      
      current <- manual_matches()
      
      if (is.null(current)) {
        current <- df$Text_identical
      }
      
      row <- as.integer(input$toggle_match)
      
      current[row] <- !current[row]
      
      manual_matches(current)
    }
  )
  
  
  # ------------------------------------------------------------
  # Calculate and display overlap percentages
  # ------------------------------------------------------------
  
  output$overlap_percentage <- renderUI({
    
    df <- final_aligned_data()
    
    req(df)
    
    
    # Number of TRUE matches
    n_true <- sum(
      df$Text_identical,
      na.rm = TRUE
    )
    
    
    # Number of non-empty segments for Person 1
    n_person1 <- sum(
      !is.na(df$Person1_Text) &
        trimws(df$Person1_Text) != ""
    )
    
    
    # Number of non-empty segments for Person 2
    n_person2 <- sum(
      !is.na(df$Person2_Text) &
        trimws(df$Person2_Text) != ""
    )
    
    
    # Calculate percentages
    percentage1 <- if (n_person1 > 0) {
      100 * n_true / n_person1
    } else {
      NA_real_
    }
    
    percentage2 <- if (n_person2 > 0) {
      100 * n_true / n_person2
    } else {
      NA_real_
    }
    
    
    percentages <- sort(
      c(percentage1, percentage2)
    )
    
    h4(
      paste0(
        "The percentage segmentation overlap is ",
        round(percentages[1]),
        " - ",
        round(percentages[2]),
        "%"
      )
    )
    
    
    
  })
  
  
  # ------------------------------------------------------------
  # Alignment information
  # ------------------------------------------------------------
  
  output$alignment_info <- renderUI({
    
    req(aligned_data())
    
    h4(
      paste0(
        "Comparing ",
        input$name1,
        " and ",
        input$name2,
        " — ",
        length(
          unique(
            aligned_data()$Response_ID
          )
        ),
        " responses"
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Interactive alignment table
  # ------------------------------------------------------------
  
  output$alignment_table <- renderDT({
    
    df <- final_aligned_data()
    
    req(df)
    
    
    # Convert TRUE/FALSE to clickable HTML buttons
    df$Text_identical <- vapply(
      seq_len(nrow(df)),
      function(i) {
        
        value <- ifelse(
          df$Text_identical[i],
          "TRUE",
          "FALSE"
        )
        
        colour <- ifelse(
          df$Text_identical[i],
          "green",
          "red"
        )
        
        
        paste0(
          "<button ",
          "class='match-button' ",
          "data-row='", i, "' ",
          "style='",
          "color:", colour, ";",
          "font-weight:bold;",
          "border:none;",
          "background:none;",
          "cursor:pointer;",
          "'>",
          value,
          "</button>"
        )
      },
      character(1)
    )
    
    
    datatable(
      
      df,
      
      escape = FALSE,
      
      rownames = FALSE,
      
      selection = "none",
      
      options = list(
        pageLength = 25,
        ordering = FALSE
      ),
      
      callback = JS(
        "
        table.on(
          'click',
          'button.match-button',
          function() {
            
            var row = $(this).data('row');
            
            Shiny.setInputValue(
              'toggle_match',
              row,
              {priority: 'event'}
            );
          }
        );
        "
      )
    )
  })
  
  
  # ------------------------------------------------------------
  # Download
  # ------------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      
      name1 <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        trimws(input$name1)
      )
      
      name2 <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        trimws(input$name2)
      )
      
      paste0(
        "aligned_",
        name1,
        "_",
        name2,
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      write_xlsx(
        final_aligned_data(),
        file
      )
    }
  )
}


shinyApp(
  ui,
  server
)
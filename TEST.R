
# ======================
# MyBudgetAI Lite
# Utility bill dashboard
# ======================

library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(httr2)
library(jsonlite)

# >> connect to database

con <- dbConnect(SQLite(), "mybudgetai_lite_v2.db")

# >> create bills table

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS bills (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    category TEXT NOT NULL,
    bill_month TEXT NOT NULL,
    bill_year INTEGER NOT NULL,
    amount REAL NOT NULL,
    UNIQUE(category, bill_month, bill_year)
  )
")
# >> 3 main utilities

utilities <- c("Water", "Electricity", "Internet")

# >> use current date automatically

current_year <- as.integer(format(Sys.Date(), "%Y"))
current_month <- format(Sys.Date(), "%B")

# >> convert a month and year into a number for sorting (Jan2026 first before Feb2026)

get_month_number <- function(month, year) {
  year * 12 + match(month, month.name)
}

# >> format amounts as php
peso <- function(amount) {
  paste0("₱", formatC(
    amount,
    format = "f",
    digits = 2,
    big.mark = ","
  ))
}

# ======================
# UI HELPER
# ======================

# >> asks GEMINI AI to explain a utility bill forecast
get_ai_explanation <- function(
    utility,
    bill_history,
    estimate,
    method
) {
  
  # >> read the API key saved in .Renviron
  api_key <- Sys.getenv("GEMINI_API_KEY")
  
  # >> stop if  API key cannot be found
  if (api_key == "") {
    return("Gemini API key not found. Check your .Renviron file.")
  }
  
  # >> bill amounts into history text
  history_text <- paste(
    paste0("PHP ", bill_history),
    collapse = ", "
  )
  
  # >> give Gemini instructions for its explanation
  prompt <- paste(
    "You are a helpful utility bill analyst.",
    "Explain this forecast in 2 to 3 simple sentences.",
    "Utility:", utility,
    "Recent bills:", history_text,
    "Estimated next bill: PHP", round(estimate, 2),
    "Calculation method:", method,
    "Describe the pattern in the bill amounts.",
    "Do not claim to know why a bill changed.",
    "Label possible causes as possibilities, not facts.",
    "Remind the user that this is only an estimate."
  )
  
  # >> send the prompt to Gemini and receive its answer
  tryCatch({
    
    response <- request(
      "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:generateContent"
    ) |>
      req_headers(
        `x-goog-api-key` = api_key
      ) |>
      req_body_json(list(
        contents = list(
          list(
            parts = list(
              list(text = prompt)
            )
          )
        )
      )) |>
      req_perform()
    
    # >> read Gemini's response
    result <- resp_body_json(response)
    
    # >> get the explanation text
    answer <- result$candidates[[1]]$content$parts[[1]]$text
    
    # >> return a message if no text was received
    if (is.null(answer) || length(answer) == 0) {
      return("Gemini did not return an explanation. Try again.")
    }
    
    # >> send the explanation back to the app
    answer
    
  }, error = function(e) {
    "AI explanation unavailable. Check your API key, internet connection, or model access."
  })
}



# ================
# USER INTERFACE
# ================

ui <- fluidPage(
  
  titlePanel("MyBudgetAI Lite"),
  
  p("Your simple utility bill analytics dashboard"),
  
  sidebarLayout(
    
    # >> LEFT: add a bill and filter dashboard
    sidebarPanel(
      
      h3("Add a Bill"),
      
      selectInput(
        "category",
        "Utility Category",
        choices = utilities
      ),
      
      selectInput(
        "bill_month",
        "Bill Month",
        choices = month.name,
        selected = current_month
      ),
      
      selectInput(
        "bill_year",
        "Bill Year",
        choices = setNames(current_year, current_year),
        selected = current_year
      ),
      
      numericInput(
        "amount",
        "Bill Amount (PHP)",
        value = NULL,
        min = 0.01,
        step = 50
      ),
      
      actionButton(
        "save_bill",
        "Save Bill",
        class = "btn-primary"
      ),
      
      hr(),
      
      h3("Dashboard Filter"),
      
      selectInput(
        "filter_category",
        "Show bills for",
        choices = c("All Utilities", utilities),
        selected = "All Utilities"
      ),
      
      p(
        "Each utility keeps only its latest",
        strong("3 distinct bill months."),
        "Older bills are automatically removed."
      )
    ),
    
    # >>RIGHT: dashboard tabs
    mainPanel(
      
      tabsetPanel(
        
        # >>> TAB 1: DASHBOARD
        tabPanel(
          "Dashboard",
          
          br(),
          
          fluidRow(
            column(
              6,
              wellPanel(
                h4("Average Bill"),
                h3(textOutput("average_bill"))
              )
            ),
            
            column(
              6,
              wellPanel(
                h4("Bills Recorded"),
                h3(textOutput("bills_recorded"))
              )
            )
          ),
          
          h3("Monthly Trends"),
          plotOutput("monthly_plot"),
          
          h3("Total Spending by Utility"),
          plotOutput("category_total_plot"),
          
          h3("Average Bill by Utility"),
          plotOutput("category_average_plot")
        ),
        
        # >>> TAB 2: BILL HISTORY
        tabPanel(
          "Bill History",
          
          br(),
          
          p("Your saved bills appear here, newest month first."),
          
          tableOutput("bill_history"),
          
          br(),
          
          h4("Delete a Bill"),
          
          selectInput(
            "delete_id",
            "Choose a bill to delete",
            choices = c("No bills available" = "")
          ),
          
          actionButton(
            "delete_bill",
            "Delete Selected Bill",
            class = "btn-danger"
          )
        ),
        
        # >>> TAB 3: FORECAST
        tabPanel(
          "Bill Forecast",
          
          br(),
          
          p(
            "These are simple estimates based on recent bill history, ",
            "not guaranteed predictions."
          ),
          
          tableOutput("forecast_table")
        )
      )
    )
  )
)

# ===================
# SERVER: APP LOGIC
# ===================

server <- function(input, output, session) {
  
  # >> refresh the dashboard whenever a bill is added or deleted
  refresh_data <- reactiveVal(0)
  
  # >> read the saved bills from SQLite
  all_bills <- reactive({
    
    refresh_data()
    
    data <- dbReadTable(con, "bills")
    
    if (nrow(data) > 0) {
      
      data$month_number <- match(data$bill_month, month.name)
      
      data$month_index <- get_month_number(
        data$bill_month,
        data$bill_year
      )
      
      data$period <- as.Date(
        sprintf(
          "%04d-%02d-01",
          data$bill_year,
          data$month_number
        )
      )
      
      data <- data[order(data$month_index, decreasing = TRUE), ]
    }
    
    data
  })
  
  # >> apply selected utility filter
  filtered_bills <- reactive({
    
    data <- all_bills()
    
    if (input$filter_category != "All Utilities") {
      
      data <- data[
        data$category == input$filter_category,
      ]
    }
    
    data
  })

  # ================
  # SAVE A BILL
  # ================
  
  observeEvent(input$save_bill, {
    
    # >> check amount before saving
    if (
      is.null(input$amount) ||
      is.na(input$amount) ||
      input$amount <= 0
    ) {
      
      showNotification(
        "Please enter a bill amount greater than zero.",
        type = "error"
      )
      
      return()
    }
    
    # >> save using parameters instead of building
    # >> an SQL query by joining text together
    saved <- tryCatch({
      
      dbExecute(
        con,
        paste(
          "INSERT INTO bills",
          "(category, bill_month, bill_year, amount)",
          "VALUES (?, ?, ?, ?)"
        ),
        params = list(
          input$category,
          input$bill_month,
          as.integer(input$bill_year),
          input$amount
        )
      )
      
      TRUE
      
    }, error = function(e) {
      
      showNotification(
        paste(
          "Could not save the bill.",
          "A bill for this utility and month may already exist."
        ),
        type = "error"
      )
      
      FALSE
    })
    
    if (saved) {
      
      # >> keep only the latest three bill months for each utility
      data <- dbReadTable(con, "bills")
      
      data$month_index <- get_month_number(
        data$bill_month,
        data$bill_year
      )
      
      for (utility in utilities) {
        
        utility_bills <- data[
          data$category == utility,
        ]
        
        utility_bills <- utility_bills[
          order(
            utility_bills$month_index,
            decreasing = TRUE
          ),
        ]
        
        if (nrow(utility_bills) > 3) {
          
          # >> bills older than the latest three
          old_ids <- utility_bills$id[-(1:3)]
          
          for (bill_id in old_ids) {
            
            dbExecute(
              con,
              "DELETE FROM bills WHERE id = ?",
              params = list(bill_id)
            )
          }
        }
      }
      
      refresh_data(refresh_data() + 1)
      
      showNotification(
        "Bill saved! Your utility history has been updated.",
        type = "message"
      )
    }
  })
  
  # ==============
  # SUMMARY CARDS
  # ==============
  
  output$average_bill <- renderText({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      return(peso(0))
    }
    
    peso(mean(data$amount))
  })
  
  output$bills_recorded <- renderText({
    
    nrow(filtered_bills())
  })
  
  # ===================
  # BILL HISTORY TABLE
  # ====================
  
  output$bill_history <- renderTable({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      return(data.frame(
        Message = "No bills saved yet."
      ))
    }
    
    display_data <- data.frame(
      Category = data$category,
      Month = data$bill_month,
      Year = data$bill_year,
      Amount = vapply(data$amount, peso, character(1)),
      ID = data$id
    )
    
    display_data
  })
  
  # >> update the delete dropdown whenever history changes
  observe({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      
      choices <- c("No bills available" = "")
      
    } else {
      
      labels <- paste(
        data$category,
        data$bill_month,
        data$bill_year,
        vapply(data$amount, peso, character(1)),
        sep = " - "
      )
      
      choices <- setNames(data$id, labels)
    }
    
    updateSelectInput(
      session,
      "delete_id",
      choices = choices
    )
  })
  
  # =================
  # DELETE A BILL
  # =================
  
  observeEvent(input$delete_bill, {
    
    if (
      is.null(input$delete_id) ||
      input$delete_id == ""
    ) {
      
      showNotification(
        "Please select a bill to delete.",
        type = "error"
      )
      
      return()
    }
    
    dbExecute(
      con,
      "DELETE FROM bills WHERE id = ?",
      params = list(as.integer(input$delete_id))
    )
    
    refresh_data(refresh_data() + 1)
    
    showNotification("Bill deleted.")
  })
  
  # =======================
  # MONTHLY SPENDING TREND
  # =======================
  
  output$monthly_plot <- renderPlot({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      
      plot.new()
      text(0.5, 0.5, "Add bills to see your monthly trends.")
      return()
    }
    
    monthly_totals <- aggregate(
      amount ~ period,
      data = data,
      FUN = sum
    )
    
    ggplot(
      monthly_totals,
      aes(x = period, y = amount)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_x_date(date_labels = "%b %Y") +
      scale_y_continuous(
        labels = function(x) paste0("₱", x)
      ) +
      labs(
        x = "Bill Month",
        y = "Total Expenses",
        title = "Monthly Utility Spending"
      ) +
      theme_minimal()
  })
  
  # ============================
  # TOTAL SPENDING BY UTILITY
  # ============================
  
  output$category_total_plot <- renderPlot({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      
      plot.new()
      text(0.5, 0.5, "No bills to compare yet.")
      return()
    }
    
    totals <- aggregate(
      amount ~ category,
      data = data,
      FUN = sum
    )
    
    ggplot(
      totals,
      aes(x = category, y = amount)
    ) +
      geom_col() +
      scale_y_continuous(
        labels = function(x) paste0("₱", x)
      ) +
      labs(
        x = "Utility",
        y = "Total Amount",
        title = "Total Spending by Utility"
      ) +
      theme_minimal()
  })
  
  # ============================
  # AVERAGE BILL BY UTILITY
  # -===========================
  
  output$category_average_plot <- renderPlot({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      
      plot.new()
      text(0.5, 0.5, "No bills to calculate averages yet.")
      return()
    }
    
    averages <- aggregate(
      amount ~ category,
      data = data,
      FUN = mean
    )
    
    ggplot(
      averages,
      aes(x = category, y = amount)
    ) +
      geom_col() +
      scale_y_continuous(
        labels = function(x) paste0("₱", x)
      ) +
      labs(
        x = "Utility",
        y = "Average Amount",
        title = "Average Bill by Utility"
      ) +
      theme_minimal()
  })
  
  # =================
  # BILL FORECAST
  # =================
  
  output$forecast_table <- renderTable({
    
    data <- filtered_bills()
    
    if (nrow(data) == 0) {
      
      return(data.frame(
        Message = "Add bills to see estimates."
      ))
    }
    
    forecast_rows <- list()
    
    for (utility in unique(data$category)) {
      
      history <- data[
        data$category == utility,
      ]
      
      # >> Oldest to newest for calculating changes
      history <- history[
        order(history$month_index),
      ]
      
      # >> Only use the latest three bills
      history <- tail(history, 3)
      
      number_of_bills <- nrow(history)
      
      latest_amount <- tail(history$amount, 1)
      
      latest_month <- tail(history$month_index, 1)
      
      if (number_of_bills == 1) {
        
        # >> 1 bill, use it as the starting estimate
        estimate <- latest_amount
        
        method <- "Latest bill baseline"
        
      } else {
        
        # >> calculate how much the bill changes per month
        month_gaps <- diff(history$month_index)
        
        amount_changes <- diff(history$amount)
        
        monthly_changes <- amount_changes / month_gaps
        
        average_change <- mean(monthly_changes)
        
        # >> Estimate the next month by adding the average change
        estimate <- max(
          0,
          tail(history$amount, 1) + average_change
        )
        
        
        if (number_of_bills == 2) {
          
          method <- "Recent monthly change"
          
        } else {
          
          method <- "Average recent monthly changes"
          
        }

      }
      
      explanation <- get_ai_explanation(
        utility,
        history$amount,
        estimate,
        method
      )
      
      forecast_rows[[utility]] <- data.frame(
        Category = utility,
        `Bills Used` = number_of_bills,
        `Latest Bill` = peso(latest_amount),
        `Estimated Next Bill` = peso(estimate),
        `Calculation Method` = method,
        Explanation = explanation,
        check.names = FALSE
      )
    }
    
    do.call(rbind, forecast_rows)
  })
}

# ======================
# START THE APP
# ======================

shinyApp(ui = ui, server = server)




# === APP RESET ====
# con <- dbConnect(SQLite(), "mybudgetai_lite_v2.db")
# DBI::dbIsValid(con)
# DBI::dbExecute(con, "DELETE FROM bills")
# DBI::dbGetQuery(con, "SELECT * FROM bills")
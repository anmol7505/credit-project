library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)

# Load Data
df <- read.csv("credit_card_transactions.csv")

# ---------------- AI Agent Function ----------------

ai_agent <- function(question, data){
  
  q <- tolower(question)
  
  if(grepl("highest spending category", q)){
    
    res <- data %>%
      group_by(merchant_category) %>%
      summarise(total = sum(transaction_amount, na.rm=TRUE)) %>%
      arrange(desc(total)) %>%
      head(1)
    
    return(paste("Highest spending category is:", res$merchant_category,
                 "with total spending", round(res$total,2)))
  }
  
  if(grepl("average transaction", q)){
    
    avg <- mean(data$transaction_amount, na.rm=TRUE)
    
    return(paste("Average transaction amount is", round(avg,2)))
  }
  
  if(grepl("top country", q)){
    
    res <- data %>%
      group_by(merchant_country) %>%
      summarise(total = sum(transaction_amount, na.rm=TRUE)) %>%
      arrange(desc(total)) %>%
      head(1)
    
    return(paste("Top merchant country is", res$merchant_country))
  }
  
  if(grepl("average age", q)){
    
    age <- mean(data$customer_age, na.rm=TRUE)
    
    return(paste("Average customer age is", round(age,1)))
  }
  
  if(grepl("total transactions", q)){
    
    return(paste("Total transactions:", nrow(data)))
  }
  
  return("AI Agent: Sorry, I cannot answer that question yet.")
}

# ---------------- UI ----------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Credit Card Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-bar")),
      menuItem("AI Assistant", tabName = "ai", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # -------- GRAPHS --------
      
      tabItem(tabName = "graphs",
              
              fluidRow(
                box(plotOutput("plot1"), width=6),
                box(plotOutput("plot2"), width=6)
              ),
              
              fluidRow(
                box(plotOutput("plot3"), width=6),
                box(plotOutput("plot4"), width=6)
              ),
              
              fluidRow(
                box(plotOutput("plot5"), width=6),
                box(plotOutput("plot6"), width=6)
              ),
              
              fluidRow(
                box(plotOutput("plot7"), width=6),
                box(plotOutput("plot8"), width=6)
              ),
              
              fluidRow(
                box(plotOutput("plot9"), width=6),
                box(plotOutput("plot10"), width=6)
              )
      ),
      
      # -------- AI TAB --------
      
      tabItem(tabName="ai",
              
              fluidRow(
                
                box(
                  title="Ask AI About the Data",
                  width=12,
                  
                  textInput("question","Ask a question about the dataset"),
                  
                  actionButton("ask","Ask AI"),
                  
                  br(),br(),
                  
                  verbatimTextOutput("ai_answer")
                  
                )
              )
              
      )
    )
  )
)

# ---------------- SERVER ----------------

server <- function(input, output) {
  
  # ----- Graphs -----
  
  output$plot1 <- renderPlot({
    df %>%
      group_by(merchant_category) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(merchant_category, total_spent)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme_minimal() +
      labs(title="Spending by Category")
  })
  
  output$plot2 <- renderPlot({
    df %>%
      group_by(customer_gender) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(customer_gender, total_spent)) +
      geom_bar(stat="identity", fill="orange") +
      theme_minimal() +
      labs(title="Spending by Gender")
  })
  
  output$plot3 <- renderPlot({
    df %>%
      group_by(card_type) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(card_type, total_spent)) +
      geom_bar(stat="identity", fill="green") +
      theme_minimal() +
      labs(title="Spending by Card Type")
  })
  
  output$plot4 <- renderPlot({
    df %>%
      group_by(is_international) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(is_international, total_spent)) +
      geom_bar(stat="identity", fill="red") +
      theme_minimal() +
      labs(title="International vs Domestic")
  })
  
  output$plot5 <- renderPlot({
    df %>%
      group_by(customer_income_bracket) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(customer_income_bracket, total_spent)) +
      geom_bar(stat="identity", fill="purple") +
      theme_minimal() +
      labs(title="Income Bracket Spending")
  })
  
  output$plot6 <- renderPlot({
    df %>%
      group_by(customer_segment) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(customer_segment, total_spent)) +
      geom_bar(stat="identity", fill="brown") +
      theme_minimal() +
      labs(title="Customer Segment")
  })
  
  output$plot7 <- renderPlot({
    ggplot(df, aes(transaction_amount)) +
      geom_histogram(bins=30, fill="darkblue") +
      theme_minimal() +
      labs(title="Transaction Amount Distribution")
  })
  
  output$plot8 <- renderPlot({
    ggplot(df, aes(customer_age, transaction_amount)) +
      geom_point(color="darkgreen") +
      theme_minimal() +
      labs(title="Age vs Transaction Amount")
  })
  
  output$plot9 <- renderPlot({
    df %>%
      group_by(merchant_country) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(merchant_country, total_spent)) +
      geom_bar(stat="identity", fill="darkred") +
      theme_minimal() +
      labs(title="Merchant Country Spending")
  })
  
  output$plot10 <- renderPlot({
    df %>%
      group_by(transaction_type) %>%
      summarise(total_spent = sum(transaction_amount, na.rm = TRUE)) %>%
      ggplot(aes(transaction_type, total_spent)) +
      geom_bar(stat="identity", fill="gold") +
      theme_minimal() +
      labs(title="Purchase vs Refund")
  })
  
  # -------- AI Agent --------
  
  observeEvent(input$ask,{
    
    answer <- ai_agent(input$question, df)
    
    output$ai_answer <- renderText({
      answer
    })
    
  })
  
}

# Run App
shinyApp(ui = ui, server = server)
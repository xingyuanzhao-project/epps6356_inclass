# Assignment 6 - Unified Shiny Application
# Author: Xingyuan Zhao
# Course: EPPS6356
# Tasks: B (Built-in Datasets) + D (CFR Data)

library(shiny)
library(ggplot2)
library(dplyr)

# Load CFR data for Task D
cfr_data <- read.csv("CFR_country_total_with_ISO2 (1).csv")
cfr_data <- cfr_data[cfr_data$iso3 != "Grand Total" & cfr_data$iso3 != "(blank)", ]

# ==============================================================================
# UNIFIED SHINY APP - TASKS B & D
# ==============================================================================

ui <- fluidPage(
  
  # Custom styling
  tags$style(HTML("
    body {
      background-color: #f8f9fa;
      color: #212529;
    }
    h1, h2, h3, h4 {
      font-family: 'Arial', sans-serif;
    }
    .shiny-input-container {
      color: #000000;
    }
    .nav-tabs > li > a {
      font-family: 'Arial', sans-serif;
      font-size: 16px;
    }
  ")),
  
  # App title
  titlePanel("Assignment 6: Data Visualization with Shiny"),
  
  hr(),
  
  # Main navigation tabs
  tabsetPanel(
    id = "main_tabs",
    
    # ===========================================================================
    # TASK B: Built-in Datasets (mtcars, USArrests, uspop)
    # ===========================================================================
    tabPanel("Task B: Built-in Datasets",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 h4("Task B Controls"),
                 
                 # Dataset selector
                 selectInput("dataset",
                             "Choose Dataset:",
                             choices = c("mtcars", "USArrests", "uspop"),
                             selected = "mtcars"),
                 
                 hr(),
                 
                 # Conditional panels for dataset-specific inputs
                 conditionalPanel(
                   condition = "input.dataset == 'mtcars'",
                   h5("mtcars Options"),
                   selectInput("mtcars_x", "X-axis:",
                               choices = c("mpg", "cyl", "disp", "hp", "wt", "qsec"),
                               selected = "wt"),
                   selectInput("mtcars_y", "Y-axis:",
                               choices = c("mpg", "cyl", "disp", "hp", "wt", "qsec"),
                               selected = "mpg"),
                   selectInput("mtcars_chart", "Chart Type:",
                               choices = c("Scatter Plot", "Box Plot"),
                               selected = "Scatter Plot")
                 ),
                 
                 conditionalPanel(
                   condition = "input.dataset == 'USArrests'",
                   h5("USArrests Options"),
                   selectInput("arrests_var", "Variable:",
                               choices = c("Murder", "Assault", "UrbanPop", "Rape"),
                               selected = "Murder"),
                   sliderInput("arrests_top", "Top N States:",
                               min = 5, max = 50, value = 20)
                 ),
                 
                 conditionalPanel(
                   condition = "input.dataset == 'uspop'",
                   h5("uspop Options"),
                   radioButtons("uspop_chart", "Chart Type:",
                                choices = c("Line Chart", "Bar Chart"),
                                selected = "Line Chart")
                 ),
                 
                 hr(),
                 
                 # Color picker
                 selectInput("color_b", "Color:",
                             choices = c("Blue" = "#3498db", "Red" = "#e74c3c", 
                                         "Green" = "#2ecc71", "Purple" = "#9b59b6"),
                             selected = "#3498db")
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   tabPanel("Plot", 
                            plotOutput("plot_b", height = "500px")),
                   tabPanel("Summary", 
                            verbatimTextOutput("summary_b")),
                   tabPanel("Data", 
                            tableOutput("table_b"))
                 )
               )
             )
    ),
    
    # ===========================================================================
    # TASK D: CFR Cyber Attacks Data
    # ===========================================================================
    tabPanel("Task D: CFR Cyber Attacks",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 h4("Task D Controls"),
                 
                 # Number of countries
                 sliderInput("top_n",
                             "Number of Countries:",
                             min = 5, max = 50, value = 20, step = 5),
                 
                 # Chart type
                 radioButtons("viz_type",
                              "Chart Type:",
                              choices = c("Horizontal Bar" = "hbar",
                                          "Vertical Bar" = "vbar",
                                          "Lollipop" = "lollipop",
                                          "Distribution" = "hist"),
                              selected = "hbar"),
                 
                 # Color scheme
                 selectInput("color_d",
                             "Color:",
                             choices = c("Red" = "#e74c3c",
                                         "Blue" = "#3498db",
                                         "Green" = "#2ecc71",
                                         "Orange" = "#e67e22",
                                         "Purple" = "#9b59b6"),
                             selected = "#e74c3c"),
                 
                 hr(),
                 
                 # Statistics
                 wellPanel(
                   h5("Statistics"),
                   textOutput("total_attacks"),
                   textOutput("total_countries"),
                   textOutput("top_country")
                 )
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   tabPanel("Visualization",
                            plotOutput("plot_d", height = "600px")),
                   tabPanel("Data Table",
                            tableOutput("table_d")),
                   tabPanel("Summary",
                            verbatimTextOutput("summary_d"))
                 )
               )
             )
    ),
    
    # ===========================================================================
    # ABOUT TAB
    # ===========================================================================
    tabPanel("About",
             fluidRow(
               column(12,
                      h3("Assignment 6: Shiny Applications"),
                      hr(),
                      
                      h4("Task B: Built-in Datasets"),
                      p("Interactive visualization of three R built-in datasets:"),
                      tags$ul(
                        tags$li(tags$b("mtcars:"), "Motor Trend Car Road Tests"),
                        tags$li(tags$b("USArrests:"), "Violent Crime Rates by US State"),
                        tags$li(tags$b("uspop:"), "US Population by Decade (1790-1970)")
                      ),
                      
                      h4("Task D: CFR Cyber Attacks Data"),
                      p("Analysis of global cyber attacks data from the Council on Foreign Relations."),
                      tags$ul(
                        tags$li("Total countries tracked: ", nrow(cfr_data)),
                        tags$li("Total cyber attacks recorded: ", sum(cfr_data$total_attacks)),
                        tags$li("Data includes ISO2 and ISO3 country codes")
                      ),
                      
                      hr(),
                      p("Author: Xingyuan Zhao"),
                      p("Course: EPPS6356 Data Visualization")
               )
             )
    )
  )
)

server <- function(input, output) {
  
  # ===========================================================================
  # TASK B SERVER LOGIC
  # ===========================================================================
  
  # Reactive dataset for Task B
  data_b <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "USArrests" = USArrests,
           "uspop" = data.frame(Year = as.numeric(names(uspop)), 
                                Population = as.numeric(uspop)))
  })
  
  # Task B plot
  output$plot_b <- renderPlot({
    
    data <- data_b()
    
    if (input$dataset == "mtcars") {
      
      if (input$mtcars_chart == "Scatter Plot") {
        ggplot(data, aes_string(x = input$mtcars_x, y = input$mtcars_y)) +
          geom_point(size = 4, color = input$color_b, alpha = 0.7) +
          geom_smooth(method = "lm", color = "#34495e", se = TRUE) +
          theme_minimal() +
          labs(title = paste("mtcars:", input$mtcars_y, "vs", input$mtcars_x),
               x = input$mtcars_x, y = input$mtcars_y) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      } else {
        ggplot(data, aes_string(y = input$mtcars_y)) +
          geom_boxplot(fill = input$color_b, alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("mtcars: Distribution of", input$mtcars_y),
               y = input$mtcars_y) +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      }
      
    } else if (input$dataset == "USArrests") {
      
      data$State <- rownames(data)
      top_states <- data %>% 
        arrange(desc(.data[[input$arrests_var]])) %>% 
        head(input$arrests_top)
      
      ggplot(top_states, aes_string(x = "reorder(State, -`input$arrests_var`)", 
                                     y = input$arrests_var)) +
        geom_bar(stat = "identity", fill = input$color_b, alpha = 0.8) +
        theme_minimal() +
        labs(title = paste("USArrests: Top", input$arrests_top, "States by", input$arrests_var),
             x = "State", y = input$arrests_var) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      
    } else if (input$dataset == "uspop") {
      
      if (input$uspop_chart == "Line Chart") {
        ggplot(data, aes(x = Year, y = Population)) +
          geom_line(color = input$color_b, size = 1.5) +
          geom_point(color = input$color_b, size = 4) +
          theme_minimal() +
          labs(title = "uspop: U.S. Population Growth (1790-1970)",
               x = "Year", y = "Population (millions)") +
          theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
          scale_y_continuous(labels = scales::comma)
      } else {
        ggplot(data, aes(x = factor(Year), y = Population)) +
          geom_bar(stat = "identity", fill = input$color_b, alpha = 0.8) +
          theme_minimal() +
          labs(title = "uspop: U.S. Population by Decade",
               x = "Year", y = "Population (millions)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
      }
    }
  })
  
  # Task B summary
  output$summary_b <- renderPrint({
    summary(data_b())
  })
  
  # Task B table
  output$table_b <- renderTable({
    head(data_b(), 20)
  }, rownames = TRUE)
  
  # ===========================================================================
  # TASK D SERVER LOGIC
  # ===========================================================================
  
  # Reactive top N data for Task D
  data_d <- reactive({
    cfr_data %>%
      arrange(desc(total_attacks)) %>%
      head(input$top_n)
  })
  
  # Task D plot
  output$plot_d <- renderPlot({
    
    data <- data_d()
    
    if (input$viz_type == "hbar") {
      # Horizontal bar chart
      ggplot(data, aes(x = total_attacks, y = reorder(ISO2, total_attacks))) +
        geom_bar(stat = "identity", fill = input$color_d, alpha = 0.8) +
        geom_text(aes(label = total_attacks), hjust = -0.2, size = 4) +
        theme_minimal() +
        labs(title = paste("Top", input$top_n, "Countries by Cyber Attacks"),
             x = "Total Attacks", y = "Country (ISO2)") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 11),
              panel.grid.major.y = element_blank())
      
    } else if (input$viz_type == "vbar") {
      # Vertical bar chart
      ggplot(data, aes(x = reorder(ISO2, -total_attacks), y = total_attacks)) +
        geom_bar(stat = "identity", fill = input$color_d, alpha = 0.8) +
        geom_text(aes(label = total_attacks), vjust = -0.5, size = 4) +
        theme_minimal() +
        labs(title = paste("Top", input$top_n, "Countries by Cyber Attacks"),
             x = "Country (ISO2)", y = "Total Attacks") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      
    } else if (input$viz_type == "lollipop") {
      # Lollipop chart
      ggplot(data, aes(x = total_attacks, y = reorder(iso3, total_attacks))) +
        geom_segment(aes(x = 0, xend = total_attacks, 
                        y = iso3, yend = iso3),
                    color = input$color_d, size = 1, alpha = 0.5) +
        geom_point(size = 5, color = input$color_d, alpha = 0.8) +
        theme_minimal() +
        labs(title = paste("Top", input$top_n, "Countries - Lollipop Chart"),
             x = "Total Attacks", y = "Country (ISO3)") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              panel.grid.major.y = element_blank())
      
    } else if (input$viz_type == "hist") {
      # Distribution histogram
      ggplot(cfr_data, aes(x = total_attacks)) +
        geom_histogram(bins = 30, fill = input$color_d, color = "white", alpha = 0.8) +
        theme_minimal() +
        labs(title = "Distribution of Cyber Attacks Across All Countries",
             x = "Number of Attacks", y = "Frequency (Countries)") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    }
  })
  
  # Task D table
  output$table_d <- renderTable({
    data_d() %>%
      select(ISO2, iso3, total_attacks) %>%
      rename("Country (ISO2)" = ISO2,
             "Country (ISO3)" = iso3,
             "Total Attacks" = total_attacks)
  })
  
  # Task D summary
  output$summary_d <- renderPrint({
    cat("CFR Cyber Attacks Data Summary\n")
    cat("==============================\n\n")
    summary(cfr_data$total_attacks)
  })
  
  # Task D statistics
  output$total_attacks <- renderText({
    paste("Total Attacks:", format(sum(cfr_data$total_attacks), big.mark = ","))
  })
  
  output$total_countries <- renderText({
    paste("Countries: ", nrow(cfr_data))
  })
  
  output$top_country <- renderText({
    top <- cfr_data %>% arrange(desc(total_attacks)) %>% head(1)
    paste("Most Targeted:", top$ISO2, "-", top$total_attacks, "attacks")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

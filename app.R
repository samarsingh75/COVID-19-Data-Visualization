# COVID-19 WHO Dashboard (Simplified Global View)

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Load WHO data
covid_data <- read_csv("WHO-COVID-19-global-table-data.csv")

# Clean column names
names(covid_data) <- gsub("[- ]", "_", names(covid_data))

# Rename key columns if needed
names(covid_data)[names(covid_data) == "Cases___cumulative_total"] <- "Cases_cumulative_total"
names(covid_data)[names(covid_data) == "Deaths___cumulative_total"] <- "Deaths_cumulative_total"

# Separate global data and remove it from the country list
global_row <- covid_data %>% filter(Name == "Global")
covid_data <- covid_data %>% filter(Name != "Global")

# Verify required columns exist
required_cols <- c("Name", "Cases_cumulative_total", "Deaths_cumulative_total")
if(!all(required_cols %in% names(covid_data))) {
  stop("After renaming, your CSV must include: Name, Cases_cumulative_total, Deaths_cumulative_total")
}



# UI
ui <- fluidPage(
  titlePanel("COVID-19 Global Dashboard (WHO Data)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select Options"),
      selectInput("country", "Choose a Country:",
                  choices = sort(unique(covid_data$Name)),
                  selected = "India"),
      selectInput("metric", "Choose Data Type:",
                  choices = c("Total Cases" = "Cases_cumulative_total",
                              "Total Deaths" = "Deaths_cumulative_total"),
                  selected = "Cases_cumulative_total"),
      hr(),
      h5("Developed using R Shiny and ggplot2"),
      h6("Data Source: World Health Organization (WHO)")
    ),
    
    mainPanel(
      tabsetPanel(
        # Global Overview
        tabPanel("Global Overview",
                 h3("Worldwide COVID-19 Summary", align = "center"),
                 fluidRow(
                   column(6,
                          div(style = "background-color:#3498db; color:white; 
                                    border-radius:10px; padding:15px; text-align:center;",
                             
                               h4(icon("globe"), " Total Cases", style="font-size:16px;"),
                              h3(format(global_row$Cases_cumulative_total, big.mark = ","), 
                                 style="font-size:20px; font-weight:bold;")
                          )),
                   column(6,
                          div(style = "background-color:#e74c3c; color:white; 
                                    border-radius:10px; padding:15px; text-align:center;",
                              h4(icon("skull"), " Total Deaths", style="font-size:16px;"),
                              h3(format(global_row$Deaths_cumulative_total, big.mark = ","), 
                                 style="font-size:20px; font-weight:bold;")
                          ))
                 ),
                 br(),
                 h4("Global COVID-19 Breakdown", align = "center"),
                 plotOutput("globalPie", height = "350px")
        ),
        
        # 🏳 Country Overview
        tabPanel("🏳 Country Overview",
                 h3("COVID-19 Overview for Selected Country"),
                 plotOutput("pieChart", height = "350px"),
                 tableOutput("countryStats")),
        
        #  Global Comparison
        tabPanel(" Global Comparison",
                 h3("Top 25 Countries Comparison"),
                 plotOutput("barPlot", height = "500px"))
      )
    
    )
  )
)

# -------------------------------------------------------
# SERVER
# -------------------------------------------------------
server <- function(input, output) {
  
  # Global Pie Chart
  output$globalPie <- renderPlot({
    global_data <- data.frame(
      Category = c("Total Cases", "Total Deaths"),
      Count = c(global_row$Cases_cumulative_total, global_row$Deaths_cumulative_total)
    )
    
    ggplot(global_data, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      labs(title = "Global COVID-19 Breakdown")
  })
  
  # 🏳 Country Pie Chart
  output$pieChart <- renderPlot({
    selected <- covid_data %>% filter(Name == input$country)
    
    pie_data <- data.frame(
      Category = c("Total Cases", "Total Deaths"),
      Count = c(selected$Cases_cumulative_total, selected$Deaths_cumulative_total)
    )
    
    ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      labs(title = paste("COVID-19 Breakdown in", input$country))
  })
  
  # 🏳 Country Data Table
  output$countryStats <- renderTable({
    covid_data %>%
      filter(Name == input$country) %>%
      select(Country = Name,
             `Total Cases` = Cases_cumulative_total,
             `Total Deaths` = Deaths_cumulative_total)
  })
  
  #  Top 25 Comparison
  output$barPlot <- renderPlot({
    metric_col <- sym(input$metric)
    top25 <- covid_data %>% arrange(desc(!!metric_col)) %>% slice(1:25)
    
    ggplot(top25, aes(x = reorder(Name, !!metric_col), y = !!metric_col, fill = Name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste("Top 25 Countries by",
                         ifelse(input$metric == "Cases_cumulative_total", "Total Cases", "Total Deaths")),
           x = "Country", y = "Count") +
      theme(legend.position = "none")
  })
}

# Run App
shinyApp(ui = ui, server = server)

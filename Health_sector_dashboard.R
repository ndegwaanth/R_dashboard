library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)

# Sample data (simulate health records)
set.seed(123)
health_data <- tibble(
  PatientID = 1:500,
  Age = sample(10:80, 500, replace = TRUE),
  Gender = sample(c("Male", "Female"), 500, replace = TRUE),
  Region = sample(c("Nairobi", "Mombasa", "Kisumu", "Nakuru"), 500, replace = TRUE),
  Disease = sample(c("Malaria", "Diabetes", "Hypertension", "Flu"), 500, replace = TRUE),
  BMI = round(rnorm(500, 25, 5), 1),
  VisitDate = sample(seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day"), 500, replace = TRUE)
)

# Sample health center coordinates
health_centers <- tibble(
  Name = c("Kenyatta Hospital", "Mombasa Clinic", "Kisumu Health Center", "Nakuru General"),
  Region = c("Nairobi", "Mombasa", "Kisumu", "Nakuru"),
  Lat = c(-1.2921, -4.0435, -0.0917, -0.3031),
  Lng = c(36.8219, 39.6682, 34.7680, 36.0800)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Health Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("heartbeat")),
      menuItem("Map", tabName = "map", icon = icon("map-marker-alt")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      hr(),
      selectInput("gender", "Gender", choices = c("All", unique(health_data$Gender)), selected = "All"),
      selectInput("region", "Region", choices = c("All", unique(health_data$Region)), selected = "All")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("totalPatients"),
          valueBoxOutput("avgBMI"),
          valueBoxOutput("topDisease")
        ),
        fluidRow(
          box(title = "Disease Trend Over Time", width = 6, plotlyOutput("diseaseTrend")),
          box(title = "BMI Distribution", width = 6, plotlyOutput("bmiPlot"))
        )
      ),
      tabItem(tabName = "map",
        box(title = "Health Centers", width = 12, leafletOutput("healthMap", height = 500))
      ),
      tabItem(tabName = "table",
        box(title = "Patient Records", width = 12, dataTableOutput("patientTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Filtered data
  filteredData <- reactive({
    data <- health_data
    if (input$gender != "All") data <- data %>% filter(Gender == input$gender)
    if (input$region != "All") data <- data %>% filter(Region == input$region)
    data
  })

  # KPIs
  output$totalPatients <- renderValueBox({
    valueBox(nrow(filteredData()), "Total Patients", icon = icon("users"), color = "blue")
  })

  output$avgBMI <- renderValueBox({
    avg_bmi <- round(mean(filteredData()$BMI, na.rm = TRUE), 1)
    valueBox(avg_bmi, "Average BMI", icon = icon("weight"), color = "purple")
  })

  output$topDisease <- renderValueBox({
    top_disease <- filteredData() %>%
      count(Disease, sort = TRUE) %>%
      top_n(1, n) %>%
      pull(Disease)
    valueBox(top_disease, "Most Common Disease", icon = icon("heartbeat"), color = "red")
  })

  # Plots
  output$diseaseTrend <- renderPlotly({
    trend <- filteredData() %>%
      mutate(Month = format(VisitDate, "%Y-%m")) %>%
      count(Month, Disease) %>%
      arrange(Month)

    p <- ggplot(trend, aes(x = Month, y = n, color = Disease)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Monthly Disease Trend", x = "Month", y = "Number of Cases")

    ggplotly(p)
  })

  output$bmiPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = BMI, fill = Gender)) +
      geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
      theme_minimal() +
      labs(title = "BMI Distribution", x = "BMI", y = "Count")
    ggplotly(p)
  })

  # Map
  output$healthMap <- renderLeaflet({
    leaflet(health_centers) %>%
      addTiles() %>%
      addCircleMarkers(~Lng, ~Lat, label = ~Name, radius = 8, color = "darkred", fillOpacity = 0.8)
  })

  # Data table
  output$patientTable <- renderDataTable({
    filteredData()
  })
}

# Run the app
shinyApp(ui, server)

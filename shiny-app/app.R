#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(networkD3)

# this function takes a vector of countries
# and a vector of indicators
# and returns the data frame with only
# these countries' specified indicator values
select_countries <- function(countries, indicator) {
  indicators <- c(indicator, "year", "location", "conflictid")
  countries.data <- data.frame()
  for (i in countries) {
    countries.data <- rbind(countries.data, 
                                (WBD.SES.conflict[which(WBD.SES.conflict$location == i),
                                  indicators]))
    countries.data <- countries.data[!is.na(countries.data[,indicator]),]
  }
countries.data
}

# this function takes an indicator integer (from checkbox)
# and turns it into the corrseponding data frame name
string_converter <- function(indicator) {
  df.ind <- NULL
  if (indicator == "Health Expenditure") {
    df.ind <- "health_expenditure"
  } else if (indicator == "Fertility Rate") {
    df.ind <- "fertility_rate"
  } else if (indicator == "Life Expectancy Female") {
    df.ind <- "life_expectancy_female"
  } else if (indicator == "Mortality Rate Under 5") {
    df.ind <- "mortality_rate_under5"
  } else if (indicator == "Child Employment") {
    df.ind <- "children_employment"
  } else if (indicator == "Labor Force Female") {
    df.ind <- "labor_force_female"
  } else if (indicator == "Labor Force Participation Rate") {
    df.ind <- "labor_force_participation_rate"
  } else if (indicator == "Gini Index") {
    df.ind <- "gini_index"
  } else if (indicator == "Refugee Origin") {
    df.ind <- "refugee_origin"
  } else if (indicator == "Refugee Asylum") {
    df.ind <- "refugee_asylum"
  } else if (indicator == "Sanitation Access") {
    df.ind <- "sanitation_access"
  } else if (indicator == "Water Access") {
    df.ind <- "water_access"
  } else if (indicator == "Electricity Access") {
    df.ind <- "electricity_access"
  } else if (indicator == "Slums Population") {
    df.ind <- "slums_population"
  } else if (indicator == "SES") {
    df.ind <- "SES"
  } else if (indicator == "fertility_rate") {
    df.ind <- "Fertility Rate"
  } else if (indicator == "life_expectancy_female") {
    df.ind <- "Life Expectancy Female"
  } else if (indicator == "mortality_rate_under5") {
    df.ind <- "Mortality Rate Under 5"
  } else if (indicator == "children_employment") {
    df.ind <- "Child Employment"
  } else if (indicator == "labor_force_female") {
    df.ind <- "Labor Force Female"
  } else if (indicator == "labor_force_participation_rate") {
    df.ind <- "Labor Force Participation Rate"
  } else if (indicator == "gini_index") {
    df.ind <- "Gini Index"
  } else if (indicator == "refugee_origin") {
    df.ind <- "Refugee Origin"
  } else if (indicator == "refugee_asylum") {
    df.ind <- "Refugee Asylum"
  } else if (indicator == "sanitation_access") {
    df.ind <- "Sanitation Access"
  } else if (indicator == "water_access") {
    df.ind <- "Water Access"
  } else if (indicator == "electricity_access") {
    df.ind <- "Electricity Access"
  } else if (indicator == "slums_population") {
    df.ind <- "Slums Population"
  } else if (indicator == "health_expenditure") {
    df.ind <- "Health Expenditure"
  }
  df.ind
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualizing Armed Conflicts"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      selectInput("plot", "Visualization Type", 
                  choices=c("World Bank Indicators","Conflict concentration", "Network plot")),
      hr(),
      conditionalPanel(
        condition = "input.plot == 'World Bank Indicators'",
        sliderInput("range", "Range:",
                    min = 1960, max = 2015,
                    value = c(1995,2015), sep = ""),
        hr(),
        fluidRow(
          column(4, verbatimTextOutput("range"))
        ),
        selectizeInput("countries","Country selector", choices = country.name, multiple = TRUE, selected = c("India","Canada")),
        
        hr(),
        fluidRow(column(3, verbatimTextOutput("value"))),
        selectInput("indicators", "Indicators:", 
                    choices=c("Health Expenditure", "Fertility Rate", "Life Expectancy Female", "Mortality Rate Under 5", "Child Employment", "Labor Force Female", "Labor Force Participation Rate", "Gini Index", "Refugee Origin", "Refugee Asylum", "Sanitation Access", "Water Access", "Electricity Access", "Slums Population", "SES")),
        hr(),
        helpText("Indicator data from the World Bank.")
      ),
      conditionalPanel(
        condition = "input.plot == 'Network plot'",
        sliderInput("number", "Minimum Number of Conflicts:",
                    min = 0, max = 270,
                    value = 30),
        sliderInput("opacity", "Node Opacity", 0.6, min = 0.1,
                    max = 1, step = .1),
        sliderInput("font", "Font Size", 11, min = 8,
                    max = 15, step = .5)
      )
      
    ),
    
    
    # Show a plot of the indicators
    mainPanel(
      conditionalPanel(
        condition = "input.plot == 'World Bank Indicators'",
        plotOutput("indPlot"),
        hr(),
        helpText("Line thickness indicates more incidences of conflict in a country.")
      ),
      conditionalPanel(
        condition = "input.plot == 'Conflict concentration'",
        plotlyOutput("conflictMap")
      ),
      conditionalPanel(
        condition = "input.plot == 'Network plot'",
        simpleNetworkOutput("networkPlot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$indPlot <- renderPlot({
    # get year range from slider input
    year.range <- { input$range }
    lower <- year.range[1]
    upper <- year.range[2]
    
    # get selected indicators
    indicator <- string_converter(input$indicators)
    # get selected countries
    countries <- input$countries
    # get selected countries' specified indicators
    countries.data <- select_countries(countries, indicator)
 
    
    countries.data <- countries.data[which(countries.data$year >= lower & countries.data$year <= upper),]
    countries.data <- merge(countries.data, map.count.data, by="location", all=FALSE)
    # draw the line graph with the specified number of bins
    if (!is.null(countries) && !is.null(countries.data)) {
      ggplot() + geom_line(data=countries.data, aes(x = countries.data$year, y = (countries.data[,indicator]),col=countries.data$location, size = countries.data$count))  + labs(title = "Trends Over Time", x = "Year", y = input$indicators, color = "Countries") + scale_size_continuous(range=c(1.2,3), guide=FALSE) 
    }
    
  })
  
  output$conflictMap <- renderPlotly({
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(map.count.data) %>%
      add_trace(
        z = ~count, color = ~count, colors = 'Reds', locationmode = 'country names',
        text = ~location, locations = ~location, marker = list(line = l)
      ) %>%
      colorbar(title = '# Conflicts') %>%
      layout(
        title = 'Number of Conflicts Contested in Countries<br>from 1946-2016',
        geo = g
      )
    
  })
  
  output$networkPlot <- renderSimpleNetwork({
    minimum.count.data <- conflict.freqs[which(conflict.freqs$count >= input$number),1]
    network.data <- merge(minimum.count.data,conflict.subset)
    simpleNetwork(network.data, linkColour = "#fe82b4",charge= -10, nodeColour = "#000", fontSize = input$font, opacity = input$opacity, zoom = TRUE)
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


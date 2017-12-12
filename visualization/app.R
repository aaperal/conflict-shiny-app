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
    countries.data <- countries.data[!is.na(indicator),]
  }
countries.data
}

# this function takes an indicator integer (from checkbox)
# and turns it into the corrseponding data frame name
to_df_string <- function(indicator) {
  df.ind <- NULL
  if (indicator == 1) {
    df.ind <- "health_expenditure"
  } else if (indicator == 2) {
    df.ind <- "fetility_rate"
  } else if (indicator == 3) {
    df.ind <- "life_expectancy_female"
  } else if (indicator == 4) {
    df.ind <- "mortality_rate_under5"
  } else if (indicator == 5) {
    df.ind <- "children_employment"
  } else if (indicator == 6) {
    df.ind <- "labor_force_female"
  } else if (indicator == 7) {
    df.ind <- "labor_force_Participation_rate"
  } else if (indicator == 8) {
    df.ind <- "gini_index"
  } else if (indicator == 9) {
    df.ind <- "refugee_origin"
  } else if (indicator == 10) {
    df.ind <- "refugee_asylum"
  } else if (indicator == 11) {
    df.ind <- "sanitation_access"
  } else if (indicator == 12) {
    df.ind <- "water_access"
  } else if (indicator == 13) {
    df.ind <- "electricity_access"
  } else if (indicator == 14) {
    df.ind <- "slums_population"
  }
  df.ind
}

# helper function that returns the necessary
# geom_line strings
geom_string <- function(countries, countries.data) {
  browser()
  string <- list()
  country.data <- data.frame()
  for (i in countries) {
   country.data <- countries.data[which(countries.data$location == i),]
   string <- c(string, list(geom_line(aes(y=countries.data[which(countries.data$location == i),]$health_expenditure))))
  }
  string
}

plot_countries <- function(countries, countries.data) {
  num.countries <- length(countries)
  color.palette <- brewer.pal(num.countries, "Set3")
  # need to extract indicator vector for each country
  # for (i in countries) {
  #   indicator.vector <- na.omit(WBD.SES.conflict[
  #     which(WBD.SES.conflict$location == country),
  #     c("fetility_rate", "year")]) 
  # }
  #browser()
  geoms <- geom_string(countries, countries.data)
  graph <- ggplot(countries.data, aes(countries.data$year)) + geoms + xlab("Year") + ylab("Health Expenditure")
  graph
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
                    value = c(1990,2015)),
        hr(),
        fluidRow(
          column(4, verbatimTextOutput("range"))
        ),
        selectizeInput("countries","Country selector", choices = country.name, multiple = TRUE, selected = "Afghanistan"),
        
        hr(),
        fluidRow(column(3, verbatimTextOutput("value"))),
        selectInput("indicators", "Indicators:", 
                    choices=colnames(WBD.SES.conflict[,c(29:42,45)])),
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
    indicator <- input$indicators
  
    # get selected countries
    countries <- input$countries
    # get selected countries' specified indicators
    #browser()
    countries.data <- select_countries(countries, indicator)
 
    #countries.conflicts <- map.data2[which(map.data2 == countries),]
    
    # only display the years in range
    
    #countries.conflicts <- countries.conflicts[which(countries.conflicts$year >= lower & countries.conflicts$year <= upper),]
    countries.data <- countries.data[which(countries.data$year >= lower & countries.data$year <= upper),]
    #browser()
    countries.data <- merge(countries.data, map.count.data, by="location", all=FALSE)
    # draw the line graph with the specified number of bins
    #plot_countries(countries, countries.data)
    if (!is.null(countries)) {
      ggplot() + geom_line(data=countries.data, aes(x = countries.data$year, y = (countries.data[,indicator]),col=countries.data$location, size = countries.data$count))  + labs(title = "Trends Over Time", x = "Year", y = indicator, color = "Countries") + scale_size_continuous(range=c(1.2,3), guide=FALSE) 
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
      colorbar(title = 'Number of Conflicts', tickprefix = '') %>%
      layout(
        title = 'Number of Conflicts Contested in Country<br>Conflicts from 1946-2016',
        geo = g
      )
    
    # get conflict locations and ids
    #conflicts.loc.id <- WBD.SES.conflict[which(unique(WBD.SES.conflict$conflictid)), "location"]
    
  })
  
  output$networkPlot <- renderSimpleNetwork({
    minimum.count.data <- conflict.freqs[which(conflict.freqs$count >= input$number),1]
    network.data <- merge(minimum.count.data,conflict.subset)
    simpleNetwork(network.data, linkColour = "#fe82b4",charge= -10, nodeColour = "#000", fontSize = input$font, opacity = input$opacity, zoom = TRUE)
    
    
    
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


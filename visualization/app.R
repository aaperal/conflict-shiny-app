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
library(googleVis)

# this function takes a vector of countries
# and a vector of indicators
# and returns the data frame with only
# these countries' specified indicator values
select_countries <- function(countries, indicators) {
  indicators <- c(indicators, "year", "location")
  countries.data <- data.frame()
  for (i in countries) {
    countries.data <- rbind(countries.data, 
                                na.omit(WBD.SES.conflict[which(WBD.SES.conflict$location == i),
                                  indicators]))
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
ui <- navbarPage("Navbar!",
   tabPanel("About",
            fluidRow(
              column(6,
                     includeMarkdown("about-page.rmd")
              ),
              column(3,
                     class="img-polaroid",
                     img(src="global-war-pic.jpeg",
                         height = 300, width = 400),
                     tags$small(
                       "Source: How the Second World War Turned Into a Global War?",
                       a(href="http://www.historydiscussion.net/wars/how-the-second-world-war-turned-into-a-global-war/808",
                         "Link")
                     )
                )
            )
   ),                
          
  tabPanel("Plot",
    # Application title
    titlePanel("Visualizing Armed Conflicts"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      sidebarPanel(
        sliderInput("range", "Range:",
                    min = 1960, max = 2015,
                    value = c(1960,2015)),
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
      
      mainPanel(
        plotOutput("indPlot")
      )
    )
  ),
        
  #Show a plot of the indicators
  tabPanel("Map",
    mainPanel(
        plotlyOutput("conflictFreq")
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
    countries.data <- select_countries(countries, indicator)
    
    # only display the years in range
    countries.data <- countries.data[which(countries.data$year >= lower & countries.data$year <= upper),]
    
    # draw the line graph with the specified number of bins
    #plot_countries(countries, countries.data)
    if (!is.null(countries)) {
      ggplot(countries.data, aes(x = countries.data$year, col=countries.data$location)) + geom_line(aes(y=countries.data[,indicator])) + labs(title = "Trends Over Time", x = "Year", y = indicator, color = "Countries")
    }
    
  })
  
  output$conflictFreq <- renderPlotly({
    # get conflict locations and ids
    conflicts.loc.id <- WBD.SES.conflict[which(unique(WBD.SES.conflict$conflictid)), "location"]
    
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


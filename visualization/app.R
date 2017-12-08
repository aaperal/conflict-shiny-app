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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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
      selectizeInput('text','Country selector', choices = country.name, multiple = TRUE, selected = "Afghanistan"),
      checkboxGroupInput("checkGroup", label = ("Indicators"), 
                         choices = list("Health Expenditure" = 1, "Fertility Rate" = 2, "Life Expectancy Female" = 3, "Mortality Rate Under 5" = 4, 
                                        "Children Employment" = 5, "Labor Force Female" = 6, "Labor Force Participation Rate" = 7, 
                                        "Gini Index" = 7, "Refugee Origin" = 8, "Refugee Asylum" = 9, "Sanitation Access" = 10,
                                        "Water Access" = 11, "Electricity Access" = 12, "Slums Population" = 13),
                         selected = 1),
      
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- WBD.SES.conflict[, 30] # fertility rate
    x    <- na.omit(x)
    year.range <- { input$range }
    lower <- year.range[1]
    upper <- year.range[2]
   
    country <- input$text
    indicator.vector <- na.omit(WBD.SES.conflict[which(WBD.SES.conflict$location == country), c("fetility_rate", "year")])
    # only display the years in range
    indicator.vector <- indicator.vector[which(indicator.vector$year >= lower & indicator.vector$year <= upper),]
    
    # draw the line graph with the specified number of bins
    ggplot(indicator.vector, aes(x = indicator.vector$year)) + geom_line(aes(y=indicator.vector$fetility_rate)) + xlab("Year") + ylab("fertility rate")
    #hist(x, main = paste("Histogram of fertility rates"), xlab = "Fertiliy Rate", breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # output$linePlot <- renderPlot({
  #   ctry <- input$text
  #   y    <- WBD.SES.conflict$ctry
  #   y
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)


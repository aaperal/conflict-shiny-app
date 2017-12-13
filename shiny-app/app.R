#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install required apps
#uncomment the next line to run
#install.packages(c('shiny','ggplot2','RColorBrewer','plotly','networkD3','ggthemes','markdown'))

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(networkD3)
library(ggthemes)
library(markdown)

library(readr)
SES.data <- read_csv("GLOB.SES.csv")

require(plyr)
require(WDI)
library(WDI)

# health expenditure, total (% of GDP) indicator = SH.XPD.TOTL.ZS
# fertility rate, tota (births per total) indicator = SP.DYN.TFRT.IN
# life expectancy at birth, female (years) indicator = SP.DYN.LE00.FE.IN
# life expectancy at birth, total (years) indicator = SP.DYN.LE00.IN
# mortality rate, under-5 (per 1000 live births) indicator = SH.DYN.MORT
# children in employment, total (% of children 7-14) indicator = SL.TLF.0714.ZS
# labor force, female (% of total labor force) indicator = SL.TLF.TOTL.FE.ZS
# labor force participation rate, female (% of female population ages 15+)(modeled ILO estimate) indicator = SL.TLF.CACT.FE.ZS
# GINI index (World Bank estimate) indicator = SI.POV.GINI
# Refugee population by country or territory of origin () indicator = SM.POP.REFG.OR
# Refugee population by country or territory of asylum () indicator = SM.POP.REFG
# Improved sanitation facilities (% of population with access) indicator = SH.STA.ACSN
# Improved water source (% of population with access) indicator = SH.H2O.SAFE.ZS
# Access to electricity (% of population) indicator = EG.ELC.ACCS.ZS
# Population living in slums (% of urban population) indicator = EN.POP.SLUM.UR.ZS


#stored the required variables in a vector to access all at once
ind = c("health_expenditure" = "SH.XPD.TOTL.ZS",
        "fertility_rate" = "SP.DYN.TFRT.IN",
        "life_expectancy_female" = "SP.DYN.LE00.FE.IN",
        "mortality_rate_under5" = "SH.DYN.MORT",
        "children_employment" = "SL.TLF.0714.ZS",
        "labor_force_female" = "SL.TLF.TOTL.FE.ZS",
        "labor_force_participation_rate" ='SL.TLF.CACT.FE.ZS',
        "gini_index" = "SI.POV.GINI",
        "refugee_origin" = "SM.POP.REFG.OR",
        "refugee_asylum" = "SM.POP.REFG",
        "sanitation_access" = "SH.STA.ACSN",
        "water_access" = "SH.H2O.SAFE.ZS",
        "electricity_access" = "EG.ELC.ACCS.ZS",
        "slums_population" = "EN.POP.SLUM.UR.ZS"
)

WBD.data = WDI(indicator = ind, start=1960, end=2015)

#renamed variables to be more appropriate
rnm = c("SH.XPD.TOTL.ZS" = "health_expenditure",
        "SP.DYN.TFRT.IN" = "fertility_rate",
        "SP.DYN.LE00.FE.IN" = "life_expectancy_female",
        "SH.DYN.MORT" = "mortality_rate_under5",
        "SL.TLF.0714.ZS" = "children_employment",
        "SL.TLF.TOTL.FE.ZS" = "labor_force_female",
        "SL.TLF.CACT.FE.ZS" = "labor_force_participation_rate",
        "SI.POV.GINI" = "gini_index",
        "SM.POP.REFG.OR" = "refugee_origin",
        "SM.POP.REFG" = "refugee_asylum",
        "SH.STA.ACSN" = "sanitation_access",
        "SH.H2O.SAFE.ZS" = "water_access",
        "EG.ELC.ACCS.ZS" = "electricity_access",
        "EN.POP.SLUM.UR.ZS" = "slums_population"
)

WBD.data2 <- plyr::rename(WBD.data, rnm)
head(WBD.data2)

#downloaded and retrieved the conflict data set
library(readr)
conflict.data <- read_csv("ucdp-prio-acd-171.csv")
conflict.data$location <- as.character(conflict.data$location)

difference <- setdiff(SES.data$country, WBD.data2$country)
difference
# we need to rename a couple of countries in both the SES dataset
# and the WB dataset. We normalize country names so that we can 
# merge the two datasets.

# renames in SES
# rename Lao to Laos 
SES.data$country[SES.data$country == "Lao"] <- "Laos"
# rename Congo, Dem Rep to Congo, Dem. Rep.
SES.data$country[SES.data$country == "Congo, Dem Rep"] <- "Congo, Dem. Rep."
# Trinidad & Tobago to Trinidad and Tobago
SES.data$country[SES.data$country == "Trinidad & Tobago"] <- "Trinidad and Tobago"

# renames in WBD

# rename Congo, Rep. to Congo
WBD.data2$country[WBD.data2$country == "Congo, Rep."] <- "Congo"
# rename Egypt, Arab Rep. to Egypt
WBD.data2$country[WBD.data2$country == "Egypt, Arab Rep."] <- "Egypt"
# rename Gambia, The to Gambia
WBD.data2$country[WBD.data2$country == "Gambia, The"] <- "Gambia"
# rename Hong Kong SAR, China to Hong Kong
WBD.data2$country[WBD.data2$country == "Hong Kong SAR, China"] <- "Hong Kong"
# rename Iran, Islamic Rep. to Iran
WBD.data2$country[WBD.data2$country == "Iran, Islamic Rep."] <- "Iran"
# rename Kyrgyz Republic to Kyrgyzstan
WBD.data2$country[WBD.data2$country == "Kyrgyz Republic"] <- "Kyrgyzstan"
# rename Korea, Rep. to South Korea
WBD.data2$country[WBD.data2$country == "Korea, Rep."] <- "South Korea"
# rename Lao PDR to Laos
WBD.data2$country[WBD.data2$country == "Lao PDR"] <- "Laos"
# rename Macao SAR, China to Macao
WBD.data2$country[WBD.data2$country == "Macao SAR, China"] <- "Macao"
# rename Russian Federation to Russia
WBD.data2$country[WBD.data2$country == "Russian Federation"] <- "Russia"
# rename Slovak Republic to Slovakia
WBD.data2$country[WBD.data2$country == "Slovak Republic"] <- "Slovakia"
# rename Syrian Arab Republic to Syria
WBD.data2$country[WBD.data2$country == "Syrian Arab Republic"] <- "Syria"
# rename Venezuela, RB to Venezuela
WBD.data2$country[WBD.data2$country == "Venezuela, RB"] <- "Venezuela"
# rename Yemen, Rep. to Yemen
WBD.data2$country[WBD.data2$country == "Yemen, Rep."] <- "Yemen"
# rename Korea, Dem. People's Rep to North Korea
WBD.data2$country[WBD.data2$country == "Korea, Dem. People’s Rep."] <- "North Korea"
# rename Korea, Rep. to South Korea
WBD.data2$country[WBD.data2$country == "Korea, Rep."] <- "South Korea"

difference <- setdiff(SES.data$country, WBD.data2$country)
difference


#merging to the SES data set and the WBD data set
WBD.SES <- merge(WBD.data2, SES.data, by=c("country", "year"), all=TRUE)

#function to duplicate multiple countries with the same values 
replicate_fcn <- function(location.vector) {
  conflict.expanded <- data.frame()
  split <- unlist(strsplit(location.vector, ", "))
  len_split <- length(split)   
  
  #find the row which has the stringed values 
  rownum <- unlist(which(conflict.data$location == location.vector))
  rowlength <- length(rownum)
  
  #replicate the specific row based on the row number 
  for(i in 1:rowlength){
    conflict.expanded <- rbind(conflict.expanded, 
                               conflict.data[rep(rownum[i],each=len_split),])
  }
  #sort by year
  conflict.expanded <- conflict.expanded[order(conflict.expanded$year),]  
  
  for(j in seq(1,nrow(conflict.expanded),1)){
    mod_val <- j %% len_split
    if ((mod_val) == 0) {
      mod_val <- len_split
    }
    conflict.expanded$location[j] <- split[mod_val]
  }
  
  #we now delete the original unreplicated rows from the dataset
  #numrow[1] has the first row number of the unreplicated row
  del.start <- rownum[1]
  #the last row
  del.end <- rownum[rowlength]
  del.list <- seq(del.start,del.end,1)
  #delete unreplicated rows
  conflict.data <- conflict.data[-del.list,]
  
  #append replicated rows to end of dataset
  conflict.data <- rbind(conflict.data, conflict.expanded)
  conflict.data
}

# at this point conflict.expanded will be a data frame
# with one set of expanded rows
# we want to now add this set of expanded rows to
# conflict.data and repeat the process for all the rows
# that need duplication

difference <- setdiff(conflict.data$location, WBD.SES$country)

#values which we have to change either by replicating or renaming
difference

#replicate all necessary values
conflict.data <- replicate_fcn(difference[3])
conflict.data <- replicate_fcn(difference[4])
conflict.data <- replicate_fcn(difference[6])
conflict.data <- replicate_fcn(difference[9])
conflict.data <- replicate_fcn(difference[10])
conflict.data <- replicate_fcn(difference[12])
conflict.data <- replicate_fcn(difference[13])
conflict.data <- replicate_fcn(difference[15])
conflict.data <- replicate_fcn(difference[17])
conflict.data <- replicate_fcn(difference[18])
conflict.data <- replicate_fcn(difference[19])
conflict.data <- replicate_fcn(difference[20])
conflict.data <- replicate_fcn(difference[22])
conflict.data <- replicate_fcn(difference[23])
conflict.data <- replicate_fcn(difference[25])
conflict.data <- replicate_fcn(difference[26])
conflict.data <- replicate_fcn(difference[27])
conflict.data <- replicate_fcn(difference[28])
conflict.data <- replicate_fcn(difference[30])
conflict.data <- replicate_fcn(difference[31])
conflict.data <- replicate_fcn(difference[32])
conflict.data <- replicate_fcn(difference[33])
conflict.data <- replicate_fcn(difference[34])
conflict.data <- replicate_fcn(difference[35])
conflict.data <- replicate_fcn(difference[36])
conflict.data <- replicate_fcn(difference[38])
conflict.data <- replicate_fcn(difference[39])
conflict.data <- replicate_fcn(difference[40])
conflict.data <- replicate_fcn(difference[41])
conflict.data <- replicate_fcn(difference[42])
conflict.data <- replicate_fcn(difference[43])
conflict.data <- replicate_fcn(difference[44])
conflict.data <- replicate_fcn(difference[45])
conflict.data <- replicate_fcn(difference[46])
conflict.data <- replicate_fcn(difference[47])
conflict.data <- replicate_fcn(difference[48])
conflict.data <- replicate_fcn(difference[49])
conflict.data <- replicate_fcn(difference[51])
conflict.data <- replicate_fcn(difference[54])
conflict.data <- replicate_fcn(difference[55])
conflict.data <- replicate_fcn(difference[56])
conflict.data <- replicate_fcn(difference[58])
conflict.data <- replicate_fcn(difference[59])
conflict.data <- replicate_fcn(difference[60])
conflict.data <- replicate_fcn(difference[61])
conflict.data <- replicate_fcn(difference[62])
conflict.data <- replicate_fcn(difference[63])
conflict.data <- replicate_fcn(difference[65])

#rename Cambodia (Kampuchea) to "Cambodia"
conflict.data$location[conflict.data$location == "Cambodia (Kampuchea)"] <- "Cambodia"
#rename "Russia (Soviet Union)" to "Russia"
conflict.data$location[conflict.data$location == "Russia (Soviet Union)"] <- "Russia"
#rename "Madagascar (Malagasy)" to "Madagascar"
conflict.data$location[conflict.data$location == "Madagascar (Malagasy)"] <- "Madagascar"
#rename North/South Yemen to Yemen
conflict.data$location[conflict.data$location == "Yemen (North Yemen)"] <- "Yemen"
conflict.data$location[conflict.data$location == "South Yemen"] <- "Yemen"
#rename North/South Vietnam to "Vietnam"
conflict.data$location[conflict.data$location == "Vietnam (North Vietnam)"] <- "Vietnam"
conflict.data$location[conflict.data$location == "South Vietnam"] <- "Vietnam"
#rename "United States of America" to "United States"
conflict.data$location[conflict.data$location == "United States of America"] <- "United States"
#rename "Myanmar (Burma)" to "Myanmar"
conflict.data$location[conflict.data$location == "Myanmar (Burma)"] <- "Myanmar"
#rename "DR Congo (Zaire)" to "Congo, Dem. Rep."
conflict.data$location[conflict.data$location == "DR Congo (Zaire)"] <- "Congo, Dem. Rep."
#rename "Rumania" to "Romania"
conflict.data$location[conflict.data$location == "Rumania"] <- "Romania"
#rename "Zimbabwe (Rhodesia)" to "Zimbabwe"
conflict.data$location[conflict.data$location == "Zimbabwe (Rhodesia)"] <- "Zimbabwe"
#rename "Serbia (Yugoslavia)" to "Serbia"
conflict.data$location[conflict.data$location == "Serbia (Yugoslavia)"] <- "Serbia"
#rename "Bosnia-Herzegovina" to "Bosnia and Herzegovina"
conflict.data$location[conflict.data$location == "Bosnia-Herzegovina"] <- "Bosnia and Herzegovina"
#rename "Ivory Coast" to "Cote d'Ivoire"
conflict.data$location[conflict.data$location == "Ivory Coast"] <- "Cote d'Ivoire"
#rename "Brunei" to "Brunei Darussalam"
conflict.data$location[conflict.data$location == "Brunei"] <- "Brunei Darussalam"

#issues with "Taiwan" and "Hyderabad"

#values which we have issues with
difference <- setdiff(conflict.data$location, WBD.SES$country)
difference

#merging to the WBD/SES data set and the conflict set
WBD.SES.conflict <- merge(conflict.data, WBD.SES, by.y=c("country", "year"), by.x=c("location", "year"), all=TRUE)

require(dplyr)
# more wrangling
# remove values in the "location" columns that we don't want or need

setdiff(WBD.SES.conflict$location, conflict.data$location)

to.remove <- c("Early-demographic dividend", "East Asia & Pacific (IDA & IBRD countries)", "East Asia & Pacific (excluding high income)", "Europe & Central Asia (excluding high income)", "Europe & Central Asia (IDA & IBRD countries)", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "High income", "IBRD only", "IDA & IBRD total", "IDA blend", "IDA only", "IDA total", "Late-demographic dividend", "Latin America & Caribbean (excluding high income)", "Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)", "Middle income", "Not classified", "OECD members", "Other small states", "Pacific island small states", "Post-demographic dividend", "Pre-demographic dividend", "Small States", "South Asia (IDA & IBRD)", "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)", "Upper middle income")
WBD.SES.conflict <- WBD.SES.conflict[ !WBD.SES.conflict$location %in% to.remove,]


# convert encoding for countries to UTF-8
country.name <- as.character(unique(WBD.SES.conflict[,1]))
country.name <- iconv(country.name, "", "UTF-8", "?")

country.name

# creating the world map
library(rworldmap)
library(plyr)
# Data wrangling to make data plottable on world map
# We count the number of times a conflict has occurred
# in a given location
map.data <- WBD.SES.conflict[,c("location","year","conflictid","iso2c")]
map.data2 <- map.data[which(!is.na(map.data$conflictid)),]
map.count.data <- map.data2 %>%
  group_by(location) %>%
  dplyr::summarize(count=n())
country.names <- data.frame(unique(map.data[,c("location")]))
colnames(country.names) <- c("location")
map.count.data <- merge(country.names, map.count.data, by = "location", all=TRUE)
map.count.data[is.na(map.count.data)] <- 0
map.count.data[,1] <- iconv(map.count.data[,1], "", "UTF-8", "?")

# create the network map

# More data wrangling.
# Here we count the number of times an entity
# has engaged in a conflict.
conflict.subset <- conflict.data[,c("sidea","side b")]
colnames(conflict.subset) <- c("sidea","sideb")
temp.data <- conflict.subset %>%
  group_by(sidea) %>%
  dplyr::summarize(count=n())
temp.data2 <- conflict.subset %>%
  group_by(sideb) %>%
  dplyr::summarize(count=n())
colnames(temp.data2) <- c("sidea","count")
conflict.freqs <- rbind(temp.data,temp.data2)
rm(temp.data,temp.data2)




#APP CREATION





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
ui <- navbarPage("",
 tabPanel("About",
          fluidRow(
            column(6,
                   includeMarkdown("about-page.md")
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
 
 tabPanel("Map",
          mainPanel(
            plotlyOutput("conflictMap")
          )
 ),
 
 tabPanel("Network",
          titlePanel("Visualizing Networks of Conflicts"),
          
          sidebarLayout(
            sidebarPanel(
              sliderInput("number", "Minimum Number of Conflicts:",
                          min = 0, max = 270,
                          value = 30),
              sliderInput("opacity", "Node Opacity", 0.6, min = 0.1,
                          max = 1, step = .1),
              sliderInput("font", "Font Size", 11, min = 8,
                          max = 15, step = .5),
              helpText("Nodes are various entities in conflict and edges indicate that the entities have been in conflict")
            ),
            mainPanel(
              simpleNetworkOutput("networkPlot")
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
                        value = c(1995,2015), sep = ""),
            hr(),
            fluidRow(
              column(4, verbatimTextOutput("range"))
            ),
            selectizeInput("countries","Country selector", choices = country.name, multiple = TRUE, 
                           selected = c("India","Canada")),
            
            hr(),
            fluidRow(column(3, verbatimTextOutput("value"))),
            selectInput("indicators", "Indicators:", 
                        choices=c("Health Expenditure", "Fertility Rate", "Life Expectancy Female", 
                                  "Mortality Rate Under 5", "Child Employment", "Labor Force Female", 
                                  "Labor Force Participation Rate", "Gini Index", "Refugee Origin", 
                                  "Refugee Asylum", "Sanitation Access", "Water Access", "Electricity Access", 
                                  "Slums Population", "SES")),
            hr(),
            helpText("Indicator data from the World Bank."),
            helpText("Line thickness indicates increased incidences of conflicts.")
          ),
        mainPanel(
          plotOutput("indPlot")
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
      ggplot(countries.data, aes(year,countries.data[,indicator])) + 
        geom_point(aes(col=countries.data$location, size = countries.data$count)) + 
        geom_line(aes(col=countries.data$location, size = countries.data$count)) + 
        labs(title = "Trends Over Time", x = "Year", y = input$indicators, color = "Countries") + 
        scale_size_continuous(range=c(.5,3), guide=FALSE) + theme_hc(bgcolor = "darkunica") +
        scale_fill_hc("darkunica") 
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
    simpleNetwork(network.data, linkColour = "#fe82b4",charge= -10, nodeColour = "#000", 
                  fontSize = input$font, opacity = input$opacity, zoom = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DT)
#install.packages('rjson')
library(rjson)
#install.packages("countrycode")
library("countrycode")

data <- fromJSON(file = 'data_cia2.json')
df <- tibble()
for (i in 1:length(data)){
  df = bind_rows(df, data[[i]])}
colnames(df)
variables <- list("Expenditure on education"= 'expenditure', "Youth unemployment rate" = 'youth_unempl_rate', "Net migration rate" = 'net_migr_rate', "Electricity from fossil fuels" = 'electricity_fossil_fuel', "Area"= 'area', "Population growth rate" = 'pop_growth_rate', "Life expectancy at birth"= 'life_expectancy')
world_map<-map_data("world")
world_map$ISO3<-countrycode::countrycode(sourcevar=world_map$region,
                                         origin="country.name",
                                         destination = "iso3c", nomatch = NA)
df_world <- left_join(df, world_map, by = 'ISO3')
# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  titlePanel("CIA World Factbook 2020"),
  textOutput('subtitle'),
  br(),
  
  mainPanel(
    tabsetPanel(
      tabPanel(title = 'Univariate Analysis',
               sidebarLayout(
                 
                 # ── Sidebar: all inputs ──────────────────────────────────────────────────
                 sidebarPanel(
                   selectInput('variable', 'Select a variable:', choices = variables, selected = "Expenditure on education"),
                   actionButton('view', 'View raw data'),
                   DTOutput('raw'),
                   width = 5),
                     
                     mainPanel(
                       tabsetPanel(
                         tabPanel(
                           title = "Map",
                           br(),
                           plotOutput("Map")),
                         
                         tabPanel(
                           title = "Global analysis",
                           br(),
                           DTOutput("table")),
                         
                         tabPanel(
                           title = "Analysis per continent" ) 
                      ),
                      width = 7
                    )
                  )
                ),
      tabPanel(title = "Multivariate Analysis", 
               sidebarLayout(
                 sidebarPanel(
               selectInput('variable_1', 'Select variable 1:', choices = variables, selected = "Expenditure on education"),
               selectInput('variable_2', 'Select variable 2:', choices = variables, selected = "Youth unemployment rate"),
               selectInput('variable_scale', 'Scale points by:', choices = variables, selected = "Area")
              ),
              mainPanel(plotOutput("scatter"))))
              
    ),width = 12))

    
    # ── Main panel: all outputs ──────────────────────────────────────────────



# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  selected_var <- reactive({
    return(input$variable)
  })
  output$subtitle <- renderText({'This is a shiny app to visualize the variables from the CIA 2020 factbook!'})
  observeEvent(input$view, {
    output$raw <- DT::renderDT(df_world[c('country', 'continent', input$variable)], options = list(pageLength = 15))
  })
  output$Map <- renderPlot({ggplot(df_world, aes(x=long,y=lat, group=group, fill =  input$variable))+#.data[[selected_var()]]))+
    geom_polygon(colour="white")})
  
  
} # end server


# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)


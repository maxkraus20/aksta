library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
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
variables <- list("Expenditure on education"= 'expenditure', "Youth unemployment rate" = 'youth_unempl_rate', "Net migration rate" = 'net_migr_rate', "Electricity from fossil fuels" = 'electricity_fossil_fuel', "Life expectancy at birth"= 'life_expectancy')
scales <- list("Area"= 'area', "Population" = 'population')
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
                   selectInput('variable', 'Select a variable:', choices = variables, selected = "expenditure"),
                   actionButton('view', 'View raw data'),
                   DTOutput('raw'),
                   width = 5),
                     
                     mainPanel(
                       tabsetPanel(
                         tabPanel(
                           title = "Map",
                           br(),
                           plotlyOutput("Map")),
                         
                         tabPanel(
                           title = "Global analysis",
                           br(),
                           plotlyOutput("hist_global"),
                           br(),
                           plotlyOutput("boxplot_global")),
                         
                         tabPanel(
                           title = "Analysis per continent",
                           br(),
                           plotlyOutput("density_continent"),
                           br(),
                           plotlyOutput("boxplot_continent"))
                      ),
                      width = 7
                    )
                  )
                ),
      tabPanel(title = "Multivariate Analysis", 
               sidebarLayout(
                 sidebarPanel(
               selectInput('variable_1', 'Select variable 1:', choices = variables, selected = "expenditure"),
               selectInput('variable_2', 'Select variable 2:', choices = variables, selected = "youth_unempl_rate"),
               selectInput('variable_scale', 'Scale points by:', choices = scales, selected = "area")
              ),
              mainPanel(plotlyOutput("scatter"))))
              
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
  var1 <- reactive({return(input$variable_1)})
  var2 <- reactive({return(input$variable_2)})
  vars <- reactive({return(input$variable_scale)})
  
  # Univariate Map  ────────────────────────────────────────────────────────────
  output$Map <- renderPlotly({ggplot(df_world, aes(x=long,y=lat, group=group, fill =  .data[[selected_var()]]))+#.data[[selected_var()]]))+
    geom_polygon(colour="white")})
  
  #
  output$scatter <- renderPlotly({
    p <- ggplot(df, aes(x = .data[[var1()]], y = .data[[var2()]])) +
      geom_point(aes(size = .data[[vars()]], color = continent)) +
      geom_smooth(aes(color = continent), method = 'loess', se = FALSE, show.legend = FALSE) +
      labs(x = names(variables)[variables == var1()],
           y = names(variables)[variables == var2()]) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Univariate Global analysis  ────────────────────────────────────────────────
  output$scatter <- renderPlotly({
    p <- ggplot(df, aes(x = .data[[var1()]], y = .data[[var2()]])) +
      geom_point(aes(size = .data[[vars()]], color = continent), alpha = 0.7) +
      geom_smooth(aes(color = continent), method = 'loess', se = FALSE, show.legend = FALSE) +
      labs(x = names(variables)[variables == var1()],
           y = names(variables)[variables == var2()],
           size = names(scales)[scales == vars()],
           title = paste(names(variables)[variables == var1()], "vs", 
                         names(variables)[variables == var2()])) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Multivariate
  output$hist_global <- renderPlotly({
    p <- ggplot(df, aes(x = .data[[selected_var()]])) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = "steelblue", alpha = 0.5, color = "white") +
      geom_density(color = "darkblue", linewidth = 0.5, fill = "darkblue", alpha = 0.3) +
      labs(x = names(variables)[variables == selected_var()],
           title = paste("Distribution of", names(variables)[variables == selected_var()])) +
      theme_minimal()
    ggplotly(p)
  })
  
  #  Univariate Continent analysis
  output$boxplot_continent <- renderPlotly({
    p <- ggplot(df, aes(x = continent, y = .data[[selected_var()]], fill = continent)) +
      geom_boxplot(alpha = 0.7) +
      labs(x = "Continent", y = names(variables)[variables == selected_var()],
           title = paste("Boxplot of", names(variables)[variables == selected_var()], "by Continent")) +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$density_continent <- renderPlotly({
    p <- ggplot(df, aes(x = .data[[selected_var()]], fill = continent, color = continent)) +
      geom_density(alpha = 0.3) +
      labs(x = names(variables)[variables == selected_var()],
           title = paste("Density of", names(variables)[variables == selected_var()], "by Continent")) +
      theme_minimal()
    ggplotly(p)
  })
  
} # end server


# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)


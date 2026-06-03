library(shiny)
library(ggplot2)
library(dplyr)


# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  titlePanel(""),
  
  sidebarLayout(
    
    # ── Sidebar: all inputs ──────────────────────────────────────────────────
    sidebarPanel(
    ), 
    
    
    # ── Main panel: all outputs ──────────────────────────────────────────────
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          title = "Plot",
          br(),
          plotOutput("scatter")        
        ),
        
        tabPanel(
          title = "Summary table",
          br(),
          tableOutput("table")         
        ),
        
        tabPanel(
        )
        
      ) 
      
    ) 
    
  ) 
  
) 


# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  
  
} # end server


# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)

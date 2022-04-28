#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(plotly)
load(url("https://github.com/adriochoa15/Project2.git/Jane_Austen/words.csv"))




ui <- bootstrapPage(
    
    selectInput(inputId = "n_breaks",
                label = "Number of bins in histogram (approximate):",
                choices = c(10, 20, 35, 50),
                selected = 20),
    
    checkboxInput(inputId = "individual_obs",
                  label = strong("Show individual observations"),
                  value = FALSE),
    
    checkboxInput(inputId = "density",
                  label = strong("Show density estimate"),
                  value = FALSE),
    
    plotOutput(outputId = "main_plot", height = "300px"),
    
    # Display this only if the density is shown
    conditionalPanel(condition = "input.density == true",
                     sliderInput(inputId = "bw_adjust",
                                 label = "Bandwidth adjustment:",
                                 min = 0.2, max = 2, value = 1, step = 0.2)
    )
    
)



server <- function(input, output) {
    
    output$main_plot <- renderPlot({
        
        hist(faithful$eruptions,
             probability = TRUE,
             breaks = as.numeric(input$n_breaks),
             xlab = "Frequency (words)",
             main = "Social Media Accronym Frequency")
        
        if (input$individual_obs) {
            rug(faithful$eruptions)
        }
        
        if (input$density) {
            dens <- density(faithful$eruptions,
                            adjust = input$bw_adjust)
            lines(dens, col = "blue")
        }
        
    })
}
shinyApp(ui = ui, server = server)

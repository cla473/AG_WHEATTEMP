#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#----------------------------------------------------------------------------------
# Generates a graph showing the values specified as a heat map on the map of Australia
#----------------------------------------------------------------------------------
# Sample Usage:
#     Rscript Process_AvgTemp.R -y 2014
#----------------------------------------------------------------------------------
#rm(list =ls())


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Grain Filling period \nEarly Season Wheat Varieties"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(5, offset=1,
             sliderInput("years1", "Select the first Year:", sep="",
                         min =1957, max = 2016, value = 1964)
      ),
      column(5, offset=1,
             sliderInput("years2", "Select another Year:", sep="",
                         min =1957, max = 2016, value = 2009)
      )
   ),
   
   fluidRow(
       column(5, offset = 1,
              textOutput("selectedValue1"),
              h2("Average Temperatures"),
              imageOutput("avgTemp1"),
              h2("Maximum Temperatures"),
              imageOutput("maxTemp1"),
              h2("Day Counts"),
              imageOutput("dayCount1"),
              h2("Days with Temperatures >= 32"),
              imageOutput("daysGTE321")
       ),
      column(5, offset = 1,
             textOutput("selectedValue2"),
             h2("Average Temperatures"),
             imageOutput("avgTemp2"),
             h2("Maximum Temperatures"),
             imageOutput("maxTemp2"),
             h2("Day Counts"),
             imageOutput("dayCount2"),
             h2("Days with Temperatures >= 32"),
             imageOutput("daysGTE322")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    avgTemp_dir <- "/OSM/CBR/AG_WHEATTEMP/work/avgTemp/"
    maxTemp_dir <- "/OSM/CBR/AG_WHEATTEMP/work/maxTemp/"
    dayCount_dir <- "/OSM/CBR/AG_WHEATTEMP/work/dayCount/"
    daysGTE32_dir <- "/OSM/CBR/AG_WHEATTEMP/work/daysGTE32/"
    
    output$selectedValue1 <- renderText({
        paste("The first year is", as.character(input$years1))
    })
    
    output$avgTemp1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        avgTempFile1 <- normalizePath(file.path(avgTemp_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the avgTempFile and alt text
        list(src = avgTempFile1, width=500,
             alt = paste("Min Temp Year", input$years1))
    }, deleteFile = FALSE)    
    
        
    output$maxTemp1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        maxTempFile1 <- normalizePath(file.path(maxTemp_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = maxTempFile1, width=500,
             alt = paste("Max Temp Year", input$years1))
    }, deleteFile = FALSE)    

    output$dayCount1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        dayCountFile1 <- normalizePath(file.path(dayCount_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = dayCountFile1, width=500,
             alt = paste("Day Count Year", input$years1))
    }, deleteFile = FALSE)    

    output$daysGTE321 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        daysGTE32File1 <- normalizePath(file.path(daysGTE32_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = daysGTE32File1, width=500,
             alt = paste("daysGTE32 year", input$years1))
    }, deleteFile = FALSE)    
    
    output$selectedValue2 <- renderText({
        paste("The other year is", as.character(input$years2))
    })
    
    output$avgTemp2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        avgTempFile2 <- normalizePath(file.path(avgTemp_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the avgTempFile and alt text
        list(src = avgTempFile2, width=500,
             alt = paste("Min Temp Year", input$years2))
    }, deleteFile = FALSE)    
    
    
    output$maxTemp2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        maxTempFile2 <- normalizePath(file.path(maxTemp_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = maxTempFile2, width=500,
             alt = paste("Max Temp Year", input$years2))
    }, deleteFile = FALSE)    
    
    output$dayCount2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        dayCountFile2 <- normalizePath(file.path(dayCount_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = dayCountFile2, width=500,
             alt = paste("Day Count Year", input$years2))
    }, deleteFile = FALSE)    
    
    output$daysGTE322 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        daysGTE32File2 <- normalizePath(file.path(daysGTE32_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = daysGTE32File2, width=500,
             alt = paste("daysGTE32 year", input$years2))
    }, deleteFile = FALSE)        
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style(type = "text/css", "
      .irs-grid-text {font-size: 12pt;}
      .irs-min {font-size: 12pt;}
      .irs-max {font-size: 12pt;}
      .irs-single {font-size: 12pt;}
    "),
   
   # Application title
   titlePanel("Grain Filling period \nEarly Season Wheat Varieties"),
   
   hr(),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
       splitLayout(cellWidths=c(600, 600), 
                   cellArgs = list(style="height:120px; padding-left: 25px;"),
                   sliderInput("years1", "Select the first Year:", sep="", width="450px", min=1957, max=2016, value=1964), 
                   sliderInput("years2", "Select another Year:", sep="", width="450px", min=1957, max=2016, value=2009))
   ),
   
   fluidRow(
       column(11, h3(textOutput("aveTemplbl")))
   ),
   fluidRow(
       splitLayout(cellWidths=c(600, 600), 
                   cellArgs = list(style="height:520px;"),
                   imageOutput("avgTemp1"), 
                   imageOutput("avgTemp2"))
   ),
   
   fluidRow(
       column(11, h3(textOutput("maxTemplbl")))
   ),
   fluidRow(
       splitLayout(cellWidths=c(600, 600), 
                   cellArgs = list(style="height:520px;"),
                   imageOutput("maxTemp1"), 
                   imageOutput("maxTemp2"))
   ),
   
   fluidRow(
       column(11, h3(textOutput("dayCountlbl")))
   ),
   fluidRow(
       splitLayout(cellWidths=c(600, 600), 
                   cellArgs = list(style="height:520px;"),
                   imageOutput("dayCount1"), 
                   imageOutput("dayCount2"))
   ),
   
   fluidRow(
       column(11, h3(textOutput("daysGTE32lbl")))
   ),
   fluidRow(
       splitLayout(cellWidths=c(600, 600), 
                   cellArgs = list(style="height:520px;"),
                   imageOutput("daysGTE321"), 
                   imageOutput("daysGTE322"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #avgTemp_dir <- "/OSM/CBR/AG_WHEATTEMP/work/shiny/AG_WheatTemperature/www/avgTemp/"
    avgTemp_dir <- "./www/avgTemp/"
    maxTemp_dir <- "./www/maxTemp/"
    dayCount_dir <- "./www/dayCount/"
    daysGTE32_dir <- "./www/daysGTE32/"
    imageWidth <- 600
    #imageHeight <- 500
    
    output$aveTemplbl <- renderText({
        paste0("Average Temperature:  ", as.character(input$years1), " vs ", as.character(input$years2))
    })
    
    output$avgTemp1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        avgTempFile1 <- normalizePath(file.path(avgTemp_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the avgTempFile and alt text
        list(src = avgTempFile1, width=imageWidth,
             alt = paste("Min Temp Year", input$years1))
    }, deleteFile = FALSE)    

    output$avgTemp2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        avgTempFile2 <- normalizePath(file.path(avgTemp_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the avgTempFile and alt text
        list(src = avgTempFile2, width=imageWidth, 
             alt = paste("Min Temp Year", input$years2))
    }, deleteFile = FALSE)    
    

    output$maxTemplbl <- renderText({
        paste0("Maximum Temperature:  ", as.character(input$years1), " vs ", as.character(input$years2))
    })

    output$maxTemp1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        maxTempFile1 <- normalizePath(file.path(maxTemp_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = maxTempFile1, width=imageWidth,
             alt = paste("Max Temp Year", input$years1))
    }, deleteFile = FALSE)    

    output$maxTemp2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        maxTempFile2 <- normalizePath(file.path(maxTemp_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = maxTempFile2, width=imageWidth,
             alt = paste("Max Temp Year", input$years2))
    }, deleteFile = FALSE)    

        
    output$dayCountlbl <- renderText({
        paste0("Day Counts:  ", as.character(input$years1), " vs ", as.character(input$years2))
    })
    
    output$dayCount1 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        dayCountFile1 <- normalizePath(file.path(dayCount_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = dayCountFile1, width=imageWidth,
             alt = paste("Day Count Year", input$years1))
    }, deleteFile = FALSE)    

    output$dayCount2 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        dayCountFile2 <- normalizePath(file.path(dayCount_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = dayCountFile2, width=imageWidth,
             alt = paste("Day Count Year", input$years2))
    }, deleteFile = FALSE)    
    
    
    output$daysGTE32lbl <- renderText({
        paste0("Days with Temperature >= 32:  ", as.character(input$years1), " vs ", as.character(input$years2))
    })

    output$daysGTE321 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        daysGTE32File1 <- normalizePath(file.path(daysGTE32_dir, paste('map_', input$years1, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = daysGTE32File1, width=imageWidth,
             alt = paste("daysGTE32 year", input$years1))
    }, deleteFile = FALSE)    

    output$daysGTE322 <- renderImage({
        # When input$n is 3, filename is ./images/image3.jpeg
        daysGTE32File2 <- normalizePath(file.path(daysGTE32_dir, paste('map_', input$years2, '.png', sep='')))
        
        # Return a list containing the maxTempFile and alt text
        list(src = daysGTE32File2, width=imageWidth,
             alt = paste("daysGTE32 year", input$years2))
    }, deleteFile = FALSE)        
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


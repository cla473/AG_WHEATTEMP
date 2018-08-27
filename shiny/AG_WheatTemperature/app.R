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
   sidebarLayout(
      sidebarPanel(
         sliderInput("years", "Select the Year:", sep="",
                     min =2000, max = 2016, value = 2001)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          textOutput("selectedValue"),
          textOutput("selectedFile"),
          h2("Average Temperatures"),
          uiOutput("avgTemp"),
          h2("Maximum Temperatures"),
          htmlOutput("maxTemp")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    avgTemp_dir <- "/OSM/CBR/AG_WHEATTEMP/work/avgTemp/"
    maxTemp_dir <- "/OSM/CBR/AG_WHEATTEMP/work/maxTemp/"
    dayCount_dir <- "/OSM/CBR/AG_WHEATTEMP/work/dayCount/"

    filename <- reactive({
        #paste0(avgTemp_dir, "map_", as.character(input$years), ".png")
        paste0("map_", as.character(input$years), ".png")
    })    

    output$selectedValue <- renderText({
        paste("The year selected is", as.character(input$years))
    })
    
    output$selectedFile <- renderText({
        filename()
    })

    output$avgTemp <- renderUI({
        tags$img(src = filename(), width=500)
    })    
    #output$maxTemp <- renderText({
    #    c('<img src="\OSM\CBR\AG_WHEATTEMP\work\maxTemp\map_2016.png">')
    #})    
}

# Run the application 
shinyApp(ui = ui, server = server)


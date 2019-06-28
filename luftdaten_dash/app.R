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
library(lubridate)
library(gridExtra)

#utility functions
sensor_url<-function(required_date,sensor_id){
    paste0("https://archive.luftdaten.info/",
           required_date,
           "/",
           required_date,
           "_sds011_sensor_",
           sensor_id,
           ".csv")
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Luftdaten Sensor Longdituinal Data"),

    p("Find sensor IDs by clicking on hexes on this map. You'll have to copy/paste."),
    a("Luftdaten Map",href="https://deutschland.maps.luftdaten.info"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("sensor", label = "Sensor ID:", value=22951, min=0, step=1),
            dateRangeInput("dates", label = "Date range:", start=today()-weeks(1))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({
        
        withProgress(message = 'Making plot', value = 0, {
        
            start_day=ymd(input$dates[1])
            stop_day=ymd(input$dates[2])
            sensor_of_choice<-input$sensor
            
            myurl<-sensor_url(start_day,sensor_of_choice)
            
            files<-tibble(day=seq(start_day,stop_day,by='days'))
            
            incProgress(1/3, detail = "Checking files")
            
            files %<>%
                mutate(url=sensor_url(day,sensor_of_choice))
            
            files %<>%
                group_by(day,url) %>%
                do(exists=!http_error(.$url))
            
            incProgress(2/3, detail = "Downloading data")
            
            sensor_data<-files %>%
                filter(exists==TRUE) %>%
                select(url) %>%
                as_vector() %>%
                map(~read_delim(file=.x,delim=";")) %>%
                bind_rows()
        
            P1_plot<-sensor_data %>%
                ggplot(aes(x=timestamp,y=P1))+
                geom_point()+
                geom_smooth()
            
            P2_plot<-sensor_data %>%
                ggplot(aes(x=timestamp,y=P2))+
                geom_point()+
                geom_smooth()
            
            grid.arrange(P1_plot,P2_plot,nrow=2)
            
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

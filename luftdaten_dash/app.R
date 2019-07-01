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
library(magrittr)
library(httr)

#function to build the url of the required archived .csv file from the sensor ID and date
sensor_url<-function(required_date,sensor_id){
    paste0("https://archive.luftdaten.info/",
           required_date,
           "/",
           required_date,
           "_sds011_sensor_",
           sensor_id,
           ".csv")
}

# Define UI for application that gets luftdaten archive data and plots it
ui <- fillPage(

    # Application title
    titlePanel("Luftdaten Sensor Longdituinal Data"),

    #some cursory instructions
    p("Find sensor IDs by clicking on hexes on",
      a("Luftdaten Map",href="https://deutschland.maps.luftdaten.info"),
      ". You'll have to copy/paste."),
    p("If you specify a large date range, it may take a while to pull the data!"),
    p("The choice of deault sensor ID is arbitrary."),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            #sensor ID
            numericInput("sensor", label = "Sensor ID:", value=22951, min=0, step=1),
            #required date range
            dateRangeInput("dates", label = "Date range:", start=today()-weeks(1))
        ),

        # Show a plot of particle density by time
        mainPanel(
           plotOutput("timePlot",height="800")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timePlot <- renderPlot({
        
        #this takes a while, so we have a progress bar
        withProgress(message = 'Making plot', value = 0, {
        
            #variables that define the query
            start_day=ymd(input$dates[1])
            stop_day=ymd(input$dates[2])
            sensor_of_choice<-input$sensor
            
            #generates a sequence of days from the start to stop date
            files<-tibble(day=seq(start_day,stop_day,by='days'))
            
            incProgress(1/3, detail = "Checking files")
            
            #build a url for each day (each day's data is a seperate file)
            files %<>%
                mutate(url=sensor_url(day,sensor_of_choice))
            
            #cehck if data exists for each day
            files %<>%
                group_by(day,url) %>%
                do(exists=!http_error(.$url))
            
            incProgress(2/3, detail = "Downloading data")
            
            #where data is present, download it
            sensor_data<-files %>%
                filter(exists==TRUE) %>%
                select(url) %>%
                as_vector() %>%
                map(~read_delim(file=.x,delim=";")) %>%
                bind_rows()
        
            #work out which day of the week each date is
            sensor_data %<>%
                mutate(day_of_week=wday(timestamp,label=TRUE))
            
            #plot time series
            P1_plot<-sensor_data %>%
                ggplot(aes(x=timestamp,y=P1,colour=day_of_week))+
                geom_point(size=0.001)+
                labs(title="PM10 (particles less than 10 microns)")+
                scale_colour_brewer(type="div",palette="Spectral")
            
            P2_plot<-sensor_data %>%
                ggplot(aes(x=timestamp,y=P2,colour=day_of_week))+
                geom_point(size=0.001)+
                labs(title="PM2.5 (particles less than 2.5 microns)")+
                scale_colour_brewer(type="div",palette="Spectral")
            
            grid.arrange(P1_plot,P2_plot,nrow=2)
            
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

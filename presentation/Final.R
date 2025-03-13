library(tidyverse)
library(janitor)
library(naniar)
library(ggmap)
library(shiny)
library("tidyverse")
library("naniar")
library("janitor")
library("stringr")
library(tidyverse)
library(janitor)
library(naniar)
library(tidyverse)
library(skimr)
library(janitor)
library(palmerpenguins)
library(gtools)
library(RColorBrewer)
library(paletteer)
library(ggthemes)


locations <- readr::read_csv("data/Health_Facility_General_Information.csv") %>% 
  clean_names()

cardiac <- readr::read_csv("data/cardiac-surgery.csv") %>% 
  clean_names() %>% 
  filter(facility_id!=0) %>% 
  mutate(hospital_name = ifelse(hospital_name == 'Albany Med. Ctr', 'Albany Medical Center', hospital_name), 
         hospital_name = ifelse(hospital_name == 'Buffalo General Med Ctr', 'Buffalo General Hosp', hospital_name), 
         hospital_name = ifelse(hospital_name == 'Bronx-Lebanon-Cncourse', 'Bronx-Lebanon-Concourse', hospital_name), 
         hospital_name = ifelse(hospital_name == 'Brookdale Hosp Med Ctr', 'Brookdale Univ Hosp Med Ctr', hospital_name))

locations <- locations %>% 
  group_by(facility_id) %>% 
  summarise(across(everything(), first)) 

cardiac <- left_join(cardiac, locations, by="facility_id")

year_data <- cardiac %>% 
  filter(str_detect(year_of_hospital_discharge, "-", negate = TRUE))

register_stadiamaps("e2de824a-0995-4a51-915c-33c14c061e7b", write = FALSE) 

lat <- c(40.58, 44.70) 
long <- c(-78.87, -72.98)
bbox <- make_bbox(long, lat, f = 0.03)

maptotal <- get_stadiamap(bbox, maptype = "stamen_terrain", zoom=7) 

x <- cardiac %>% 
  arrange(hospital_name) %>% 
  group_by(facility_id, hospital_name, facility_latitude, facility_longitude) %>% 
  summarise(number_of_cases=sum(number_of_cases), average_moratality_rate=mean(observed_mortality_rate), .groups = 'keep') %>% 
  mutate(total_deaths=average_moratality_rate/100*number_of_cases) %>% 
  mutate(facility_latitude=as.numeric(facility_latitude), facility_longitude=as.numeric(facility_longitude))

lat1 <- c(40.58, 41.2) 
long1 <- c(-74.5, -72.98)
bbox1 <- make_bbox(long1, lat1, f = 0.03)

mapnyc <- get_stadiamap(bbox1, maptype = "stamen_terrain", zoom=8) 

library(shinydashboard)
library(shinyjs)
library(ggplot2)

ui <- dashboardPage(
  skin = "purple",
  
  
  dashboardHeader(
    title = "Cardiac Dashboard",
    titleWidth = 250
  ),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cases", 
               tabName = "cases", 
               icon = icon("folder-open")),
      menuItem("Mortality Rate", 
               tabName = "dashboard", 
               icon = icon("skull")),
      menuItem("Procedures", 
               tabName = "widgets", 
               icon = icon("stethoscope")),
      menuItem("Maps", 
               tabName = "map", 
               icon = icon("earth-americas")),
      menuItem("NYC Maps", 
               tabName = "nyc", 
               icon = icon("hotel"))
    ),
    useShinyjs()  
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box {
          border-radius: 8px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }
        .box-header {
          background-color: #6a1b9a;
          color: white;
          border-radius: 8px 8px 0 0;
        }
        .box-body {
          background-color: #f9f9f9;
        }
        .box-footer {
          background-color: #e1bee7;
        }
        .selectize-input {
          border-radius: 5px;
          border: 1px solid #ddd;
        }
        .control-box {
          margin-top: 20px;
        }
      "))
    ),
    tabItems(
      
      tabItem(tabName = "cases",
              h1(strong("Cases Based on Cardiac Surgery in New York Hospitals"), style = "font-size:24px; text-align:center; margin-bottom: 20px;"),
              fluidRow(
                box(plotOutput("plot3a", height = 250), status = "primary", solidHeader = T, width = 6, title = "Cases per Year"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("a", "Select Hospital:", choices = unique(year_data$hospital_name))
                )
              ),
              fluidRow(
                box(plotOutput("plot3b", height = 250), status = "primary", solidHeader = T, width = 6, title = "Cases vs Mortality Rate"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("b", "Select Year:", choices = unique(year_data$year_of_hospital_discharge))
                )
              )
      ),
      

      tabItem(tabName = "dashboard", 
              h1(strong("Mortality Rate Analysis Based on Cardiac Surgery Results"), style = "font-size:24px; text-align:center; margin-bottom: 20px;"),
              fluidRow(
                box(plotOutput("plot1a", height = 250), status = "primary", solidHeader = T, width = 6, title = "Mortality Rate by Region"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("region", "Select Region:", choices = unique(year_data$region))
                )
              ),
              fluidRow(
                box(plotOutput("plot1b", height = 250), status = "primary", solidHeader = T, width = 6, title = "Mortality Rate by Year"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("year", "Select Year:", choices = unique(year_data$year_of_hospital_discharge))
                )
              ),
              fluidRow(
                box(plotOutput("plot1c", height = 250), status = "primary", solidHeader = T, width = 6, title = "Mortality Rate Over Time"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("hospital", "Select Hospital:", choices = unique(year_data$hospital_name))
                )
              )
      ),
      

      tabItem(tabName = "widgets",
              h1(strong("Procedure Analysis in New York Hospitals"), style = "font-size:24px; text-align:center; margin-bottom: 20px;"),
              fluidRow(
                box(plotOutput("plot2a", height = 250), status = "primary", solidHeader = T, width = 6, title = "Procedures by X Axis"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    radioButtons("x", "Select X Axis:", choices = c("year_of_hospital_discharge", "region"), selected = "year_of_hospital_discharge")
                )
              ),
              fluidRow(
                box(plotOutput("plot2b", height = 250), status = "primary", solidHeader = T, width = 6, title = "Mortality Rate by Procedure"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("p", "Select Year:", choices = unique(cardiac$year_of_hospital_discharge))
                )
              ),
              fluidRow(
                box(plotOutput("plot2c", height = 250), status = "primary", solidHeader = T, width = 6, title = "Procedure Cases per Hospital"),
                box(title = "Controls", status = "info", solidHeader = T, width = 6,
                    selectInput("h", "Select Hospital:", choices = unique(year_data$hospital_name))
                )
              )
      ),
      

      tabItem(tabName = "map",
              h1(strong("Overall Maps Based on Dataset"), style = "font-size:24px; text-align:center; margin-bottom: 20px;"),
              fluidRow(
                box(plotOutput("plot4a", height = 450), status = "primary", solidHeader = T, width = 6, title = "Map 1"),
                box(plotOutput("plot4b", height = 450), status = "primary", solidHeader = T, width = 6, title = "Map 2")
              ),
              fluidRow(
                box(plotOutput("plot4c", height = 450), status = "primary", solidHeader = T, width = 6, title = "Map 3"),
                box(plotOutput("plot4d", height = 450), status = "primary", solidHeader = T, width = 6, title = "Map 4")
              )
      ),
      

      tabItem(tabName = "nyc",
              h1(strong("Maps Zoomed in on New York City"), style = "font-size:24px; text-align:center; margin-bottom: 20px;"),
              fluidRow(
                box(plotOutput("plot5a", height = 450), status = "primary", solidHeader = T, width = 6, title = "NYC Map 1"),
                box(plotOutput("plot5b", height = 450), status = "primary", solidHeader = T, width = 6, title = "NYC Map 2")
              ),
              fluidRow(
                box(plotOutput("plot5c", height = 450), status = "primary", solidHeader = T, width = 6, title = "NYC Map 3")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  output$plot1a <- renderPlot({
    
    year_data %>%
      filter(region == input$region) %>%
      ggplot(aes(x = observed_mortality_rate))+ 
      geom_density(color = "black", fill = "steelblue", alpha = 0.6)+
      labs(title = "Observed Mortality Rate Distribution by Region",
           x = "Mortality Rate",
           y = "Proportion of Hospitals in the Region")
  })
  
  output$plot1b <- renderPlot({
    
    year_data %>%
      filter(year_of_hospital_discharge == input$year) %>%
      ggplot(aes(x = observed_mortality_rate))+ 
      geom_density(color = "black", fill = "steelblue", alpha = 0.6)+
      labs(title = "Observed Mortality Rate Distribution by Year",
           x = "Mortality Rate",
           y = "Proportion of Hospitals")
  })
  
  output$plot1c <- renderPlot({
    
    year_data %>% 
      filter(hospital_name == input$hospital) %>% 
      group_by(year_of_hospital_discharge, hospital_name) %>% 
      summarize(avg_mortality = mean(observed_mortality_rate), .groups = 'keep') %>% 
      ggplot(aes(x = as.factor(year_of_hospital_discharge), y = avg_mortality, group = hospital_name))+
      geom_line()+
      geom_point(na.rm = T)+
      labs(title = "Mortality Rate Over Time",
           x = "Year",
           y = "Mortality Rate")
  })
  
  
  output$plot2a <- renderPlot({ #this is a bigger picture of the two plots shown below
    
    year_data %>%
      ggplot(aes_string(x= input$x, fill = "procedure"))+
      geom_bar(position="dodge", alpha=0.8, color="black")+
      coord_flip() + 
      labs(x= "Procedure Type", y= "New York Region", fill= "Procedure")
    
  })
  
  output$plot2b <- renderPlot({
    
    cardiac %>% 
      filter(year_of_hospital_discharge == input$p) %>% 
      group_by(procedure) %>% 
      summarize(avg_mortality = mean(observed_mortality_rate)) %>% 
      ggplot(aes(x = reorder(procedure, avg_mortality), y = avg_mortality))+
      geom_col(color = "black", fill = "lightblue")+
      coord_flip()+
      labs(title = "Average Observed Mortality Rate by Procedure",
           x = "Procedure",
           y = "Mortality Rate")+
      theme_light(base_size = 14)
    
  })
  
  output$plot2c <- renderPlot({
    
    year_data %>% 
      filter(hospital_name == input$h) %>% 
      group_by(procedure, hospital_name) %>% 
      summarize(procedure_cases = sum(number_of_cases), .groups = "keep") %>% 
      ggplot(aes(x = procedure, y = procedure_cases))+
      geom_col(color = "black", fill = "lightblue")+
      coord_flip() + 
      labs(title = "Number of Cases per Procedure",
           x = "Procedure",
           y = "Number of Cases")+
      theme_light(base_size = 14)
    
  })
  
  output$plot3a <- renderPlot({
    
    year_data %>% 
      filter(hospital_name == input$a) %>% 
      group_by(year_of_hospital_discharge) %>% 
      summarize(total_cases_a = sum(number_of_cases)) %>% 
      ggplot(aes_string(x="year_of_hospital_discharge", y = "total_cases_a"))+
      geom_col(alpha=0.8, color="black", fill = "steelblue")+
      labs(title = "Number of Cases per Year",
           x= "Year", 
           y= "Number of Cases")+
      theme_light(base_size = 14)
    
  })
  
  
  output$plot3b <- renderPlot({
    
    year_data %>% 
      filter(year_of_hospital_discharge == input$b) %>% 
      group_by(facility_id, procedure) %>% 
      summarize(total_procedures = sum(number_of_cases), avg_mortality = mean(observed_mortality_rate)) %>%
      ggplot(aes(x = total_procedures, y = avg_mortality))+
      geom_point(color = "olivedrab", na.rm = T) +
      labs(title = "Average Mortality Rate compared with Procedure Numbers ",
           x = "Total Number of Procedures",
           y = "Mortality Rate")+
      theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 5))+
      theme_classic()+
      scale_x_log10()+
      geom_smooth(method=lm, se=F, formula = 'y ~ x')
    
  })
  
  output$plot4a <- renderPlot({
    
    ggmap(maptotal) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, size = 1.5), na.rm= T) + 
      labs(x= "Longitude", y= "Latitude", title="Cardiac Locations")
    
  })
  
  output$plot4a <- renderPlot({
    ggmap(maptotal) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, size = 2, color = average_moratality_rate), alpha = 0.8, na.rm = T) +
      scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Cardiac Locations")
  })
  
  output$plot4b <- renderPlot({
    ggmap(maptotal) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, color  = number_of_cases, size = 2), alpha = 0.8, na.rm = T) + 
      scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Cases Distribution") 
  })
  
  output$plot4c <- renderPlot({
    ggmap(maptotal) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, color  = average_moratality_rate, size = 2), alpha = 0.8, na.rm = T) + 
      scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Mortality Rate") 
  })
  
  output$plot4d <- renderPlot({
    ggmap(maptotal) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, color  = total_deaths, size = 2), alpha = 0.8, na.rm = T) + 
      scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Total Deaths") 
  })
  
  output$plot5a <- renderPlot({
    
    ggmap(mapnyc) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, colour  = average_moratality_rate,size = 2.5), na.rm= T) + scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Average Mortality Rate by Hospital")
    
  })
  
  output$plot5b <- renderPlot({
    
    ggmap(mapnyc) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, colour  = number_of_cases, size = 2.5), na.rm= T) + 
      scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Total Number of Cases")
    
  })
  
  output$plot5c <- renderPlot({
    
    ggmap(mapnyc) + 
      geom_point(data = x, aes(facility_longitude, facility_latitude, colour  = total_deaths, size = 2.5), na.rm= T) + 
      scale_color_gradient(low = "steelblue", high = "midnightblue") +
      labs(x= "Longitude", y= "Latitude", title="Total Number of Deaths")
    
  })
  
}

shinyApp(ui, server)




# load packages ----
library(shiny)
library(palmerpenguins)
library(tidyverse)
library(DT)
library(here)
library(shinyWidgets)
library(janitor)

# read in data ----
weather <- read.csv(here("myapp", "data", "table_28.csv"))
temp_summary <- readRDS("data/temp_month_summary.rds")

# user interface ----

# body mass slider input ----
body_mass_slider <- sliderInput(inputId = "body_mass",
                                label = "Select a range of body masses (g):",
                                min = 2700, max = 6300, value = c(3000, 4000))

# species picker input ----
island_picker_input <- pickerInput(inputId = "island_id", label = "Select an island:",
                                   choices = c("Torgersen", "Dream", "Biscoe"),
                                   selected = c("Torgersen", "Dream", "Biscoe"),
                                   multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE))

# Bin slider input ----
bin_slider <- sliderInput(inputId = "bin_num", label = "Select number of bins:",
                          min = 1, max = 100, value = 25) 

# Month selector
month_select <- checkboxGroupInput(inputId = "month", 
                                   label = "Choose a month(s):", 
                                   choices = c("January", "February", "March", "April", 
                                               "May", "June", "July", "August", 
                                               "September", "October", "November", "December"), 
                                   selected = c("January", "February")
)


ui <- fluidPage(
  
  # app title ----
  tags$h1("Wylie's App"), #h1("text here")
  
  # app subtitle ----
  p(strong("This is so much fun!")),
  
  navbarPage(
    
    "Exploring Antarctic Penguins & Weather",
    tabPanel("Background",
             em("Data taken from the Palmer station. There are three species of penguin there.")),
    tabPanel("Antarctic Penguins",
             tabsetPanel(
               tabPanel("Scatterplot",
                        em(
                          
                          body_mass_slider,
                          
                          # body mass output ----
                          plotOutput(outputId = "bodyMass_scatterPlot")
                          
                        )),
               tabPanel("Histogram",
                        em(
                          
                          island_picker_input,
                          bin_slider,
                          
                          # flipper length histogram output ----
                          plotOutput(outputId = "bodyMass_histogram")
                          
                          )))),
    tabPanel("Antarctic Weather",
             em("some widget to explore weather data here")),
    tabPanel("Explore the Data",
             tabsetPanel(
               tabPanel("Penguin Data",
                        em(
                          
                          # DT table output
                          DT::dataTableOutput(outputId = "penguin_DT")
                          
                        )),
               tabPanel("Palmer Station Weather Data",
                        em(
                          
                          month_select,
                          
                          # DT table output for weather data
                          DT::dataTableOutput(outputId = "tempr_DT")
                          
                        ))))
    
  )
)






# break ----








# server instructions ----
server <- function(input, output) {
  
  # filter body masses ----
  body_mass_df <- reactive({
    
    penguins %>% 
      filter(body_mass_g %in% input$body_mass[1]:input$body_mass[2])
    
  })
  
  # Filter species ----
  island_df <- reactive({
    
    validate(

    # validate() tests a condition, returns error if test fails-
    # need() takes an exp that returns T or F + chr string to return if exp is FALSE

      need(length(input$island_id) > 0, "Please select at least one island to visualize.")
    )
    
    penguins %>% 
      filter(island == input$island_id)
    
  })
  
  # filter weather data by month ----
  month_df <- reactive({
    
    temp_summary %>% 
      filter(month_name %in% input$month_select)
    
  })
  
  # Render DT table
  output$penguin_DT <- DT::renderDataTable({
    
    DT::datatable(na.omit(penguins),
                  caption = tags$caption(style = 'caption-side: top; text-align: left;',
                                         'Table 1: ', 
                                         tags$em('Size measurements for adult foraging penguins near Palmer Station, Antarctica')),
                  options = list(pageLength = 5))
    
  })
  
  # Render DT table for weather data
  output$weather_DT <- DT::renderDataTable({
    
    DT::datatable(weather,
                  caption = tags$caption(style = 'caption-side: top; text-align: left;',
                                         'Table 1: ', 
                                         tags$em('Weather Data')),
                  options = list(pageLength = 5))
    
  })
  
  # render the temperature data table ----
  output$temp_DT <- DT::renderDataTable({
    DT::datatable(month_df(),
                  class = 'cell-border stripe',
                  colnames = c('Year', "Month", 'Mean Air Temp.', 'Max. Air Temp.', 'Min. Air Temp.'),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 2: ', tags$em('Mean, maximum, and minimum monthly air temperatures (°C) recorded at Palmer Station, Antarctica from 1989 - 2019.'))) 
    
  })
  
  # Render scatterplot ----
  output$bodyMass_scatterPlot <- renderPlot({
    
    ggplot(na.omit(body_mass_df()), 
           aes(x = flipper_length_mm,
               y = bill_length_mm,
               color = species,
               shape = species)) +
      geom_point() +
      scale_color_manual(values = c("Adelie" = "#FEA346", 
                                    "Chinstrap" = "#B251F1", 
                                    "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19,
                                    "Chinstrap" = 17,
                                    "Gentoo" = 15)) +
      labs( x = "Flipper Length (mm)",
            y = "Bill Length (mm)",
            color = "Penguin Species",
            shape = "Penguin Species")
    
  })
  
  # Render histogram ----
  output$bodyMass_histogram <- renderPlot({
    
    ggplot(na.omit(island_df()),
           aes(x = flipper_length_mm,
               fill = species)) +
      geom_histogram(alpha = 0.6, bins = input$bin_num) +
      scale_fill_manual(values = c("Adelie" = "#FEA346", 
                                    "Chinstrap" = "#B251F1", 
                                    "Gentoo" = "#4BA4A4")) +
      labs(x = "Body Mass (g)",
           y = "Count",
           color = "Penguin Species")
    
  })
}

# combine ui and server into app ----
shinyApp(ui = ui, server = server)
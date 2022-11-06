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
library(leaflet)
library(htmlwidgets)

all_modzcta <- readRDS("all_modzcta.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 NYC Trends by ZCTA"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tags$a(href="https://github.com/nychealth/coronavirus-data"),
          h5("All data metrics are aggregated by week (categorized by week). 
             All data sourced from NYC Dep of Health"),
          selectInput("date",
                      "Select a data (week ending in):",
                      choices = unique(all_modzcta$week_ending)
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("Case Rate", leafletOutput("cases")),
             tabPanel("Test Rate", leafletOutput("tests")),
             tabPanel("Percent Positive", leafletOutput("pctpos")),
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    week_zcta <- reactive({
      w <- all_modzcta %>% filter(week_ending == input$date)
      return(w)
    })
    
    output$cases <- renderLeaflet({
      pal <- colorBin(palette = "YlGn", 9, domain = all_modzcta$caserate)
      
      labels = sprintf(
        "<strong>%s</strong><br/>%g cases per 100,000 people",
        week_zcta()$MODZCTA, week_zcta()$caserate) %>%
        lapply(htmltools::HTML)
      
      week_zcta() %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(-73.9, 40.7, zoom = 10) %>%
        addPolygons(label = labels,
                    stroke = FALSE,
                    smoothFactor = .5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~ pal(week_zcta()$caserate),
                    highlightOptions = highlightOptions(weight = 5,
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE)) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~ caserate,
                  title = "Cases Per 100,000",
                  opacity = 0.7)
    })
    
    output$tests <- renderLeaflet({
      pal <- colorBin(palette = "PuBu", 9, domain = all_modzcta$testrate)
      
      labels = sprintf(
        "<strong>%s</strong><br/>%g tests per 100,000 people",
        week_zcta()$MODZCTA, week_zcta()$testrate) %>%
        lapply(htmltools::HTML)
      
      week_zcta() %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(-73.9, 40.7, zoom = 10) %>%
        addPolygons(label = labels,
                    stroke = FALSE,
                    smoothFactor = .5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~ pal(week_zcta()$testrate),
                    highlightOptions = highlightOptions(weight = 5,
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE)) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~ testrate,
                  title = "Cases Per 100,000",
                  opacity = 0.7)
    })
    
    output$pctpos <- renderLeaflet({
      pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$pctpos)
      
      labels = sprintf(
        "<strong>%s</strong><br/>%g tests per 100,000 people",
        week_zcta()$MODZCTA, week_zcta()$pctpos) %>%
        lapply(htmltools::HTML)
      
      week_zcta() %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet() %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(-73.9, 40.7, zoom = 10) %>%
        addPolygons(label = labels,
                    stroke = FALSE,
                    smoothFactor = .5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~ pal(week_zcta()$pctpos),
                    highlightOptions = highlightOptions(weight = 5,
                                                        fillOpacity = 1,
                                                        color = "black",
                                                        opacity = 1,
                                                        bringToFront = TRUE)) %>%
        addLegend("bottomright",
                  pal = pal,
                  values = ~ pctpos,
                  title = "Cases Per 100,000",
                  opacity = 0.7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

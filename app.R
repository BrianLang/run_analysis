library(shiny)
library(fit)
library(tidyverse)
library(leaflet)
library(sp)

# Define UI for data upload app ----
ui <- fluidPage(

    # App title ----
    titlePanel("Uploading Files"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", "Choose .fit File",
                      multiple = TRUE,
                      accept = c(".fit"))

        ),

        # Main panel for displaying outputs ----
        mainPanel(
            tableOutput("summary"),
            # Output: Data file ----
            tableOutput("record"),

            leafletOutput("map")
        )
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {

    fit_file <- reactive({
        fit::read.fit(input$file1$datapath)
    })


    output$summary <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)

        return(fit_file() %>%
                   .$session %>%
                   transmute(num_laps, start_time, total_calories,
                             kilometers = total_distance/1000,
                             minutes = total_elapsed_time/60))

    })
    #
    output$record <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)

        return(fit_file() %>% .$record %>% head())

    })

    output$map <- renderLeaflet({
        req(input$file1)

        fitdata <- fit_file()


        mp <- sp::SpatialPoints(coords = fitdata$record %>%
                              transmute(lng = position_long,
                                     lat = position_lat) %>% filter(!is.na(lat)))
        mp1 <- mapview::coords2Lines(coords = sp::coordinates(mp), ID = "A")


        return(leaflet::leaflet(mp1) %>%
            leaflet::addTiles() %>%
            leaflet::addPolylines(weight = 2))

        #
        #     geom_path(aes(x = lon, y = lat),
        #               data = fitdata$record %>%
        #                   mutate(lon = position_long,
        #                          lat = position_lat) %>%
        #                   filter(!is.na(lon)), size = 1, colour = 'blue')
    })

}
# Run the app ----
shinyApp(ui, server)

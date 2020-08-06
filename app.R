library(shiny)
library(fit)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sp)
library(mapview)

ui <- fluidPage(

    titlePanel("Visualize FIT file"),


    sidebarLayout(

        sidebarPanel(

            fileInput("file1", "Choose .fit File",
                      multiple = TRUE,
                      accept = c(".fit"))

        ),

        mainPanel(
            fluidRow(tableOutput("summary"), align="center"), ## header

            leafletOutput("map"), ## map with line

            plotOutput("figures"), ## speed vs distance
            hr(),
            tags$footer("Brian M. Lang 2020", align = "center")
        )

    )
)

# Define server logic to read selected file ----
server <- function(input, output) {


    ## user input file will be NULL to begin with, so use the example to start with, this will be shipped with the shiny app when uploaded to rstudio.io
    fit_file <- reactive({
        if (is.null(input$file1)) {
            fit::read.fit("./data/example.fit")
        } else {
            fit::read.fit(input$file1$datapath)
        }
    })

    output$summary <- renderTable({

        return(fit_file() %>%
                   .$session %>%
                   transmute(kilometers = total_distance/1000,
                             minutes = total_elapsed_time/60,
                             average_pace = (minutes)/kilometers,
                             total_calories))

    })
    #
    # output$record <- renderTable({
    #     # input$file1 will be NULL initially. After the user selects
    #     # and uploads a file, head of that data file by default,
    #     # or all rows if selected, will be shown.
    #     req(input$file1)
    #
    #     return(fit_file() %>% .$record %>% head())
    #
    # })

    output$map <- renderLeaflet({
        fitdata <- fit_file()


        mp <- sp::SpatialPoints(coords = fitdata$record %>%
                                    transmute(lng = position_long,
                                              lat = position_lat) %>%
                                    filter(!is.na(lat)))
        mp1 <- mapview::coords2Lines(coords = sp::coordinates(mp), ID = "A")


        return(leaflet::leaflet(mp1) %>%
                   leaflet::addTiles() %>%
                   leaflet::addPolylines(weight = 4, color = "black"))
    })

    output$figures <- renderPlot({
        fitdata <- fit_file()

        pdata <- fitdata$record %>%
            filter(!is.na(position_long)) %>%
            mutate(mpkm = 1000 / (speed * 60) )

        pace_dist <- pdata %>%
            ggplot(aes(distance/1000, mpkm)) +
            geom_line() +
            lims(y = c(0, 12)) +
            theme_minimal() +
            labs(y = "min/km", x = "distance (km)")

        pace_dist
    })

}

shinyApp(ui, server)

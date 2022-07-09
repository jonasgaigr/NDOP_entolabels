library(shiny)
library(tidyverse)
library(DT)
library(sf)
library(proj4)
library(raster)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Aplikace na generování lokalizačních štítků z Nálezové databáze ochrany přírody AOPK ČR"),

    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Vyberte soubor CSV",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            ),
            checkboxGroupInput("variable", "Údaje na štítku:",
                               c("Stát" = "country",
                                 "GPS" = "gps",
                                 "Obec" = "gear",
                                 "Pole síťového mapování 1. řádu" = "sitmap",
                                 "Habitat" = "habitat",
                                 "Autor" = "autor")
                               ),
            sliderInput("label_length", "Délka štítku (mm):",
                        min = 0, max = 50, value = 14),
            sliderInput("label_width", "Šířka štítku (mm):",
                        min = 0, max = 25, value = 7),
            radioButtons("output_format", "Export",
                         choices = list("docx" = 1, 
                                        "pdf" = 2),
                         selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$table <- renderDataTable({
    req(input$file)
    
    sites <- read.csv2(input$file$datapath) %>%
      dplyr::select(-PORADI) %>%
      dplyr::mutate(DATUM = lubridate::ymd(DATUM_OD)) %>%
      sf::st_as_sf(., 
                   coords = c("X", "Y"),
                   crs = "+init=epsg:5514") %>%
      sf::st_transform(., crs("+init=epsg:4326")) %>%
      dplyr::group_by(ID_NALEZ) %>%
      dplyr::mutate(GPS = paste(format(round(st_coordinates(geometry)[2], 6), nsmall = 6),
                                format(round(st_coordinates(geometry)[1], 6), nsmall = 6),
                                sep = ", ")) %>%
      sf::st_drop_geometry()
      
    DT::datatable(sites)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

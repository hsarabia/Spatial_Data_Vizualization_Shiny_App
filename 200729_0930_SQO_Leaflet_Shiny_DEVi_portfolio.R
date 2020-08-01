#Hiram Sarabia, MS
#Environmental Scientist
#San Diego Water Board
#Monitoring, Assessment, and Research Unit
#Goal: Build Shiny App to visualize SQO Spatial Data for San Diego Region 
#July 2020

#Install and or Update Required Libraries
if(!require(shiny)){install.packages("shiny")}
if(!require(htmlwidgets)){install.packages("htmlwidgets")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(rgdal)){install.packages("rgdal")}

#Load Required Packages
library(shiny)
library(htmlwidgets)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(rgdal)

#Set Working Directory
setwd("C:/R/ShinyApps/SQOMapping")
#Load Data
test <- read.csv('R9SQO_draftfinal.csv', stringsAsFactors = F)
# Dealing with year as "date": https://stackoverflow.com/questions/30255833/convert-four-digit-year-values-to-a-date-type
test$Year <- as.Date(as.character(test$Year), format = "%Y")
test$Year <- year(test$Year)

#Add Random Noise to Coordinates
#test$lat2 <- jitter(test$lat, factor = 0.0001)
#test$long2 <- jitter(test$long, factor = 0.0001)

#Print Button
jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 
#Shiny App User Interface
ui <- bootstrapPage(titlePanel(h2("San Diego Water Board - SQO Spatial Data Vizualization App by Hiram Sarabia", align = "center")),
    #Logo
    #titlePanel(title=div(img(src="index.jpg")),
    tags$head(tags$script(src = jsfile)),
    #HTML AbsolutePanel Background:  
    #tags$head(tags$style(
      #HTML('
      #     #controls {
      #     background-color: #dec4de;
      #     }')
      #)),
    tags$head(tags$style(
      HTML('#controls:hover{opacity: 0.95;}'))),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  #Absolute Panel Controls: https://github.com/rstudio/leaflet/issues/534
  #Opacity HTML tags
  #https://stackoverflow.com/questions/47697683/change-opacity-on-mouseover-in-r-shiny
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                width = 200, height = "auto",
  #absolutePanel(top = 80, right = 10,
  #              id="controls",
  #             style="z-index:500;",
  #             class = "panel panel-default",
  #              fixed = TRUE,
  #              draggable = TRUE, 
                #selectInput("wb", "Waterbody", 
                #            choices = test$Harbor_Are %>% unique() %>% sort())
                # ),
                selectInput("waterbody", "Waterbody", choices = c("Dana Point Harbor","Oceanside Harbor","Mission Bay","San Diego Bay","Region 9"), selected = 'Region 9'),
  
                selectInput("dataselect", "BIGHT ProjectID", choices = c("2008","2013","2008 & 2013"), selected = '2008 & 2013'),
                
                sliderInput("range",
                            "SQO Score Range", 
                            min(test$Result), max(test$Result),
                            value = range(test$Result), 
                            step = 1.0
                ),
                selectInput("colors", 
                            "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  ))

#Shiny App Server
server <- function(input, output, session) {
  
  #Reactive expression for the data subsetted by waterbody based on user selection
  filteredData <- reactive({
    req(input$waterbody)
    if (input$waterbody =='San Diego Bay'){
      data <- test[test$Harbor_Are == input$waterbody,]
    } else if(input$waterbody == 'Dana Point Harbor'){
      data <- test[test$Harbor_Are == input$waterbody,]
    } else if(input$waterbody == 'Oceanside Harbor'){
      data <- test[test$Harbor_Are == input$waterbody,]
    } else if(input$waterbody == 'Mission Bay'){
      data <- test[test$Harbor_Are == input$waterbody,]
    } else {
      data <- test
    }
  })
  
  # Reactive expression for the data subsetted by year based on user selection
  filteredData2 <- reactive({
    req(input$dataselect)
    if (input$dataselect =='2008'){
      data <- test[test$Year == input$dataselect,]
    } else if(input$dataselect == '2013'){
      data <- test[test$Year == input$dataselect,]
    } else {
      data <- test
    }
  })
  
  #Reactive expression for data to be subsetted by range based on user input
  filteredData3 <- reactive({
    req(input$dataselect)
    if (input$dataselect =='2008'){
      data <- test[test$Year == input$dataselect,]
      data[data$Result >= input$range[1] & data$Result <= input$range[2],]
    } else if(input$dataselect == '2013'){
      data <- test[test$Year == input$dataselect,]
      data[data$Result >= input$range[1] & data$Result <= input$range[2],]
    } else {
      data <- test
      data[data$Result >= input$range[1] & data$Result <= input$range[2],]
    }
  })
    
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, test$Result)
  })
  
  #Load Shapefile and perform spatial transform
  #https://stackoverflow.com/questions/42182879/polygon-shapefile-doesnt-render-in-leaflet-r
  setwd("C:/R/ShinyApps/SQOMapping/Shapefiles/")
  WMA <- readOGR(dsn = ".",
                layer = "SD_Bay_WMA",
                verbose = FALSE)
  WMA_latlon <- spTransform(WMA, CRS("+proj=longlat +datum=WGS84"))
  
  R9 <- readOGR(dsn = ".",
                 layer = "RB9_Region_Boundaries",
                 verbose = FALSE)
  R9_latlon <- spTransform(R9, CRS("+proj=longlat +datum=WGS84"))
  
  
  #Leaflet References:
  #https://rstudio.github.io/leaflet/shiny.html
  #Fix Z-index
  #https://stackoverflow.com/questions/43881007/how-to-addtiles-on-top-of-addpolygons-in-rs-leaflet
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data=filteredData()) %>% 
      #addProviderTiles(providers$Stamen.TonerLite,
                       #options = providerTileOptions(noWrap = TRUE)) %>%
      #Additional Tile Options
      #REF: https://rstudio.github.io/leaflet/basemaps.html
      #Leaflet provider demo: https://leaflet-extras.github.io/leaflet-providers/preview/
      #Combining Tiles
      #addProviderTiles(providers$MtbMap,
                      #options = providerTileOptions(opacity = 0.25)) %>%
      #addProviderTiles(providers$Esri.OceanBasemap,
                      #options = providerTileOptions(opacity = 1.00)) %>%
      #addProviderTiles(providers$Stamen.TonerLabels) %>%
      #addProviderTiles("Stamen.Terrain") %>%
      addMapPane("Tiles", zIndex = 200) %>% # shown below all layers 
      addMapPane("Stations", zIndex = 400) %>% # shown above Tiles
      addMapPane("Polygons", zIndex = 410) %>% # shown above Stations
      
      addProviderTiles("Esri.WorldImagery", group = "ESRI World Imagery", options = pathOptions(pane = "Tiles")) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group = "ESRI World Gray Canvas", options = pathOptions(pane = "Tiles")) %>%
      #addProviderTiles("Stamen.Terrain", group = "Stamen Terrain", options = pathOptions(pane = "Tiles")) %>%
      addProviderTiles("Esri.OceanBasemap", group = "ESRI Ocean Basemap", options = pathOptions(pane = "Tiles")) %>%
      #Polygons
      addPolygons(data = WMA_latlon, fill = F, stroke = T, color = "#03F", weight = 3, group = "WMA", options = pathOptions(pane ="Polygons")) %>%
      addPolygons(data = R9_latlon, fill = F, stroke = T, color = "#000000", weight = 3, group = "R9", options = pathOptions(pane ="Polygons")) %>%
      #Layer control guide: https://rstudio.github.io/leaflet/showhide.html
      addLayersControl(
        baseGroups = c("ESRI World Gray Canvas", "ESRI Ocean Basemap", "ESRI World Imagery"),
        overlayGroups = c("WMA","R9"), position = "topleft") %>%
      #Methods to Manipulate Map Widget: 
      #setView(-117.111222, 32.651666, zoom = 12) %>%
      #setView(~min(test$a), ~min(test$b), zoom = 12) %>%
      #Dynamic
      #https://stackoverflow.com/questions/40481794/change-setview-dynamically-according-to-select-box-in-r-shiny-app
      #setView(~min(long), ~min(lat), zoom = 12) %>%
      fitBounds(~min(long) - 0.025, ~min(lat) - 0.025, ~max(long) + 0.025, ~max(lat) + 0.025) %>%
      onRender(
        "function(el, x) {
        L.easyPrint({
        sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
        filename: 'SQOmap',
        exportOnly: true,
        hideControlContainer: true
        }).addTo(this);
  }"
      #fitBounds(-117.111222, 32.333333, -117.222111, 32.766727)
      #Raster: https://rstudio.github.io/leaflet/raster.html
      )
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~Result*50, weight = 1, color = "#777777",
                 fillColor = ~pal(Result), fillOpacity = 0.7, popup = ~paste(Site,", SQO=",Result), options = pathOptions(pane = "Stations")
      ) %>%
      addPolygons(data = R9_latlon, fill = F, stroke = T, color = "#000000", weight = 3, group = "R9", options = pathOptions(pane ="Polygons")) %>%
      addPolygons(data = WMA_latlon, fill = F, stroke = T, color = "#03F", weight = 3, group = "WMA", options = pathOptions(pane ="Polygons"))
 })
  
 observe({
   pal <- colorpal()
  
    leafletProxy("map", data = filteredData2()) %>%
      clearShapes() %>%
      addCircles(radius = ~Result*50, weight = 1, color = "#777777",
                 fillColor = ~pal(Result), fillOpacity = 0.7, popup = ~paste(Site,", SQO=",Result), options = pathOptions("Stations") 
      ) %>%
      addPolygons(data = R9_latlon, fill = F, stroke = T, color = "#000000", weight = 3, group = "R9", options = pathOptions(pane ="Polygons")) %>%
      addPolygons(data = WMA_latlon, fill = F, stroke = T, color = "#03F", weight = 3, group = "WMA", options = pathOptions(pane ="Polygons")) 
  })
 
 observe({
   pal <- colorpal()
   
   leafletProxy("map", data = filteredData3()) %>%
     clearShapes() %>%
     addCircles(radius = ~Result*50, weight = 1, color = "#777777",
                fillColor = ~pal(Result), fillOpacity = 0.7, popup = ~paste(Site,", SQO=",Result), options = pathOptions("Stations")
     ) %>%
     addPolygons(data = R9_latlon, fill = F, stroke = T, color = "#000000", weight = 3, group = "R9", options = pathOptions(pane ="Polygons")) %>%
     addPolygons(data = WMA_latlon, fill = F, stroke = T, color = "#03F", weight = 3, group = "WMA", options = pathOptions(pane ="Polygons")) 
 
   })
  # Use a separate observer to recreate the legend as needed.
  #Possible Fixes for printing legend
  #https://github.com/rowanwins/leaflet-easyPrint/issues/39
  #https://github.com/rstudio/leaflet/issues/615
  observe({
    proxy <- leafletProxy("map", data = test)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomleft", title = "SQO Score Scale", bins = 4,
                          pal = pal, values = ~Result, layerId="Legend"
      )
    }
  })
}

shinyApp(ui, server)

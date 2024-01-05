#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(leaflet)
library(dismo)
library(tidyr)
library(stringr)
library(bslib) #forgot to say - you need to install this for the themes I use

# Define UI for application - what features will the user encounter
# This maps to the structure of an HTML page
ui <- fluidPage(
    theme=bs_theme(bootswatch = "lumen"),
  navbarPage(title="Demos", 
               tabPanel("gbif",
                        h1("choose a species to map"), #header size 1 is an html object that R has mapped to a function h1()
                        # Sidebar with a map and download 
                        sidebarLayout(
                          sidebarPanel(
                            leafletOutput("spp_map"),
                            downloadButton("downloadData", "Download")
                          ),
                          
                          # user text input for a species to map...this probably would have looked better in the sidebar panel
                          mainPanel(
                            textInput("text", "input species data"),
                            submitButton("Map!"),
                          )),
               ), 
             # another tab maping a user's csv data (use tree_samp.csv)
              tabPanel("input tree data",
                           mainPanel(
                             
                             fileInput("file1", "Enter tree data as csv",
                                       accept=".csv"),
                             uiOutput("dropdown"),
                             submitButton("Map!"), # note we can reuse this map button user input element!
                             tableOutput("trees_df"),
                             leafletOutput("treeM")
                           )
                 
               )
             )
          
        )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
  #save the user's uploaded file in as a dataframe in the reactive function mydata()
  mydata<-reactive({
    inFile<- input$file1
    if (is.null(inFile))
      return(NULL)
    trees<- read.csv(inFile$datapath)
    return(trees)
  })
  
  #call that dataframe to visualize the first few entries
  output$trees_df<- renderTable(
    head(mydata())
  )
  
  #create a dropdown menu that filters the dataframe
  output$dropdown <- renderUI({
    df<-mydata()
    ents<-unique(df$entity)
    selectInput('city', 'select city', ents)
  })
  # observe event will wait for the input of the dropdown to occur. The map output will remain
  # a default blank "proxy" leaflet map, until the event is observed and the map button is pushed
  observeEvent(input$city, {
    tdf<-mydata()
    entity=as.character(input$city)
    treedff<-tdf[tdf$entity == entity ,]
    leafletProxy("treeM")%>%
      clearMarkers()%>%
      addCircleMarkers(lat=as.numeric(treedff$INTPTLA), lng=as.numeric(treedff$INTPTLO))
  })
  
  output$treeM<-renderLeaflet(
    leaflet()%>%
      addProviderTiles(providers$CartoDB.Positron)
  )
  
  #reactive function that will split the string of text given by the user into Genus and species for the gbif function
  locs_map<- reactive({
    organism<-as.character(input$text)
    spp<- str_split(organism, " ")
    spp_locs<-gbif(spp[[1]][1], spp[[1]][2]) # pulls species occurrence info from gbif
    
    return(spp_locs)
  })

  output$spp_map<- renderLeaflet({
    spp_locs<-locs_map()
    
    spp_map<- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addMarkers(lat=spp_locs$lat, spp_locs$lon)
      
  })
  
  # download button I looked up how to do on stack overflow 
  
  output$downloadData<- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
      
    },
    
    content = function(file){
      write.csv(locs_map(), file)
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

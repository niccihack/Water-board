#N.Hack
#03/19/2018
#State Water Resources Control Board

#Test shiny app build by playing with HAB data and recreating the tableau map seen here
#http://www.mywaterquality.ca.gov/habs/where/freshwater_events.html

library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)

habdat <- hab_incident_detail
habdat$fdate <- lubridate::date(habdat$`First Observed`)
habdat$ldate = lubridate::date(habdat$`Bloom Last Verified`)
habdat <- habdat %>% dplyr::mutate(color = dplyr::recode(`Typeof Sign`, 
                                           'Closed' = '#D00',
                                           'Danger' = '#F00',
                                           'Closed to Swimming' = '#F4E',
                                           'No Contact Advisory' = '#F09',
                                           'Caution and Danger' = '#F40',
                                           'Precaution: Rinse Off' = '#F60',
                                           'Caution' = '#F80',
                                           'Warning to Caution' = '#FA4',
                                           'Warning' = '#FC0',
                                           'Advisory' = '#FE0',
                                           'None' = '#FFFAFA',
                                           'Unknown' = '#a1a1a1')) %>% 
  dplyr::arrange()
#specify levels to get correct order in legend
habdat$`Typeof Sign` = factor(habdat$`Typeof Sign`, 
                                 levels = c('Closed',
                                            'Danger',
                                            'Closed to Swimming',
                                            'No Contact Advisory',
                                            'Caution and Danger',
                                            'Precaution: Rinse Off',
                                            'Caution',
                                            'Warning to Caution',
                                            'Warning',
                                            'Advisory',
                                            'None',
                                            'Unknown'))
# habdat$color = as.factor(habdat$color)

ui <- dashboardPage(
  dashboardHeader(
    title = "CA HAB Portal",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    div(style='display: inline-block;text-align:left;margin-left: 25px',
        sliderInput(inputId = 'date',
               label = "Date range",
               min = min(habdat$fdate),
               max = max(habdat$fdate),
               value = range(habdat$fdate)
               )),
    div(style='display: inline-block;text-align:left;margin-left: 25px',
        selectInput(inputId = 'county',
                label = "County",
                choices = c('All',sort(unique(habdat$`County Name`))))),
    tags$head(
      tags$style(HTML('#button{color:black}'))#makes button text black
    ),
    div(style='display: inline-block;padding:15px;margin-left: 25px',
       downloadButton('button','Download data')) #adds download button
   ),#closes sidebar
  dashboardBody(type = 'tabs',tags$style(type = "text/css", 
                             "#map {height: calc(100vh - 80px) !important;}"),
    tabsetPanel(tabPanel("Map",leafletOutput('map')),
                tabPanel('Data', DTOutput('viewData')))
  )# closes body
)#closes ui

# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  newdat <- reactive({
    subset <- dplyr::filter(habdat,fdate >= input$date[1] &
                              fdate <= input$date[2])#subset by date
    #subset by county including 'All' catagory
    if (input$county == 'All'){
      return(subset)
    }else{
      subset = dplyr::filter(subset, subset$`County Name` == input$county)
    }
    return(subset)# returns datatable of new values 
    }) #closes reactive
  
  output$viewData <- renderDT(
    newdat() %>% 
      dplyr::group_by(`Official Water Body Name`) %>%
      dplyr::summarise(County = `County Name`,
                       'Regional waterboard' = `Regional Water Board__1`,
                       'Water manager' = `Water Body Manager`,
                       'Hazard level' = `Typeof Sign`,
                       'First date observed' = as.character(fdate),
                       'Last date observed' = as.character(ldate),
                       'Are signs present?' = `Posted Sign Description`,
                       "Is the incident resolved?" = `IsIncidentResolved__1`,
                       'Details' = `Incident Information`
                       )
)#create output table of data above
  
  output$button <- downloadHandler( 
    filename= function(){
      paste("HAB",input$date[1],'to', input$date[2],".csv",sep = "")
    },
    content = function(file){
      write.csv(newdat(), file)
    }
  )#produces download button
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, 
                       options = providerTileOptions(
                         tileSize = c(255.93))) %>%
      setView(lng = mean(habdat$Longitude), 
              lat = mean(habdat$Latitude), 
              zoom = 6)
  })
  
  #Set up content for popups 
  popup <- function(name, lng, lat){
    selectedwater <- habdat[habdat$`Waterbody Name` == name,]#Tell app what record to find
    content <- as.character(tagList(
      tags$h4(as.character(selectedwater$`Official Water Body Name`)),
      tags$br(),
      tags$strong("Hazard level: "),
      sprintf(as.character(selectedwater$`Typeof Sign`)),
      tags$br(),
      tags$strong("Regional water board: "), 
      sprintf(as.character(selectedwater$`Regional Water Board__1`)),
      tags$br(),
      tags$strong('Land manager: '),
      sprintf(as.character(selectedwater$`Rec Land Manager`))
    ))
    leafletProxy('map', data = newdat()) %>% addPopups(lng = lng,lat = lat, content)
  }
    
    
  #Adds circle markers and legend for HAB signs
  #!!!!!Markers are not mapping accurately!!!!!!
  observe({
    leafletProxy('map') %>%
      clearShapes() %>%
      addCircles(data = newdat(),
                 lat = ~Latitude,
                 lng = ~Longitude,
                 color= ~color,
                 opacity = 1,
                 popup = popup,
                 layerId = newdat()$`Official Water Body Name`) %>% #need this id so popup knows where to find the data
      clearControls() %>% #need to clear controls otherwise it keeps adding more legends on top of the old ones
      addLegend("bottomleft", 
                colors = unique(newdat()[order(newdat()$`Typeof Sign`),]$color),
                values = ~`Typeof Sign`,
                labels = sort(unique(newdat()$`Typeof Sign`)),
                opacity = 1,
                title = 'HAB hazard level') %>%
      clearPopups()
      event <- input$map_shape_click #adds popup on click
      if(is.null(event)){
        return()
      }else{
        isolate({
        popup(event$id, event$lng, event$lat)# popup shows up on marker
      })
      }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


#Testing leaflet outside of shiny app
# leaflet(habdat) %>% 
#   addProviderTiles(providers$Hydda.Full) %>% #basemap from providers
#   setView(lng = mean(habdat$Longitude), 
#           lat = mean(habdat$latitude), 
#           zoom = 5) %>%
#   addCircles(lng = habdat$Longitude, 
#                lat = habdat$Latitude)
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
habdat = dplyr::rename(habdat, lng = `Longitude (Custom SQL Query)`, lat = latitude) 

ui <- dashboardPage(
  dashboardHeader(
    title = "CA FHAB Portal"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "menu", 
      menuItem("Home", tabName = 'home', icon = icon('home')),
      menuItem("Information", tabName = 'info', icon = icon('info-circle')),
      menuItem("Monitoring", tabName = 'monitoring', icon = icon('binoculars'))
    )
    
   ),#closes sidebar
  dashboardBody(
    tabItems(
      tabItem(tabName = 'home',
              fluidRow(
                valueBox(width=4,"Information","What is a HAB?", 
                         icon = icon('question'), href = 'http://www.mywaterquality.ca.gov/habs/what/index.html'),
                valueBox(width=4, "Report a bloom!","Have you seen a bloom?",
                         icon = icon('bullhorn'), color = 'yellow',href = 'http://www.mywaterquality.ca.gov/habs/do/index.html#how'),
                valueBox(width=4, "Advisories","Where are current blooms?", 
                    icon = icon('map'), color = 'red')
              ),
              fluidRow(
                box(width=12)
              )),
      tabItem(tabName = 'info',
              box(title = "What are HABs?", solidHeader = T,status = 'success',
                  tags$img(style = 'float: left;margin-right:15px;', 
                           src='hab1.jpg',
                           "At the base of the food chain in fresh, brackish, and marine systems are photosynthetic cyanobacteria and algae. Both single-celled microscopic and larger multicellular forms exist. When conditions are optimal, including light and temperature, levels of nutrients, and lack of water turbulence, cyanobacteria and some algae can quickly multiply into a harmful algal bloom (HAB). Some cyanobacteria and harmful algae can produce toxic chemicals, including cyanotoxins, domoic acid, and other algal toxins.")),
              box(title = "Why should I be concerned?", 
                  solidHeader = T,
                  status = 'warning',
                  tags$img(style = 'float: left;margin-right:15px;', 
                           src='dead_fish.jpg',
                           'Cyanobacteria and harmful algal blooms (HABs) can have negative impacts on the environment, people, pets, wildlife, or livestock, as well as the economy. Some HABs can produce large amounts of cyanotoxins or algal toxins, which can poison livestock, wildlife, and humans. Certain other types of cyanobacteria are nontoxic but can impart an unpleasant taste to water and fish as well as giving off an unpleasant smell as they die and decay. Cyanotoxins and algal toxins pose risks to the health and safety of people and pets recreating in water bodies, eating fish, and drinking water. They can accumulate in fish and shellfish to levels posing threats to people and wildlife consumers.',tags$br(), tags$br(),'The most researched group of freshwater HABs is cyanobacteria, or blue-green algae. These are problematic because they can impede recreational and beneficial uses of waterbodies by reducing aesthetics, lowering dissolved oxygen concentration, causing taste and odor problems in drinking water, and producing potent cyanotoxins, associated with illness and mortality in people, pets, livestock, and wildlife. Cyanobacteria blooms and their associated toxins have increased globally in geographic distribution, frequency, duration, and severity. Non-cyanobacteria HAB events have also increased, the most common of which is the golden haptophyte alga, which has caused fish kills in the east, mid-west and southern states, and Southern California.'))
              ),#close info tab
      tabItem(tabName = 'monitoring',
              fluidRow(
                box(
        status = 'danger', width = 12, solidHeader = T, title = "DISCLAIMER", "This map shows HAB events voluntarily reported to the State Water Board's Surface Water Ambient Monitoring Program. Data provided are for general information purposes ", tags$strong('only'), " and may contain errors. The exact location, extent and toxicity of the reported bloom may not be accurate and may not affect the entire waterbody. The data are subject to change as new information is received. Please check back for updates."),
             
      column(width = 9,type = 'tabs',tags$style(type = "text/css", 
                             "#map {height: calc(100vh - 90px) !important;}"),
             tabBox(
               width = NULL,
               title = "California Freshwater HABs", id = "tabset1",
               tabPanel("Map",leafletOutput('map')),
               tabPanel('Data', DTOutput('viewData')))
            ),#close column1
      
      column(width = 3,
             box(width = NULL,
                 solidHeader = T, status = 'primary', title = 'Inputs',
                 div(style='text-align:left;padding: 15px',
                     sliderInput(inputId = 'date',
                                 label = "Date range",
                                 min = min(habdat$fdate),
                                 max = max(habdat$fdate),
                                 value = range(habdat$fdate)
                     )),
                 div(style='text-align:left; padding: 15px',
                     selectInput(inputId = 'county',
                                 label = "County",
                                 choices = c('All',sort(unique(habdat$`County Name`))))),
                 tags$head(
                   tags$style(HTML('#button{color:black}'))#makes button text black
                 ),
                 div(style='text-align:left;padding: 15px',
                     downloadButton('button','Download data')) #adds download button
             ),#closes box
             
             box(width = NULL,
                 status = 'warning', title = "Directions",
               "Click on a point to get information about each bloom.",
               tags$br(),tags$br(),
               "Use the inputs above to narrow your selection by date and/or county.",
               tags$br(),tags$br(),
               "Click the 'Data' tab above the map to see more information about the current selection of HABs.",
               tags$br(),tags$br(),
               "Click the 'Download data' button to download the data for your selection of HABs.",
               tags$br(),tags$br(),
               tags$a("What do these advisories mean?",href='http://www.mywaterquality.ca.gov/habs/resources/index.html#recreational')) #close box
        )#close column2
       )#close row1
     )#close monitoring tab
    )#close tabItems
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
  
  # output$tabset1Selected <- renderText({
  #   input$tabset1
  # })
  # 
  
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
    leaflet(options = leafletOptions(minZoom = 6)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = mean(habdat$lng), 
              lat = mean(habdat$lat), 
              zoom = 6) %>%
      setMaxBounds(lng1 = -130.3933357,
                   lat1 = 43.108951,
                   lng2 = -109.729128,
                   lat2 = 32.144795)
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
      tags$strong("Last date observed: "),
      sprintf(as.character(selectedwater$ldate)),
      tags$br(),
      tags$strong("Regional water board: "), 
      sprintf(as.character(selectedwater$`Regional Water Board__1`)),
      tags$br(),
      tags$strong('Land manager: '),
      sprintf(as.character(selectedwater$`Rec Land Manager`))
    ))
    leafletProxy('map', data = newdat()) %>% addPopups(lng = lng,lat = lat, content)
  }
    
  #Attempt to have points on map show up when map rendered and not with inputs
  #from https://github.com/rstudio/leaflet/issues/242
 #outputOptions(output, 'map', suspendWhenHidden = F) 
  #Does nothing or throws errors
 
  #Adds circle markers and legend for HAB signs
  observe({
    leafletProxy('map') %>%
      clearShapes() %>%
      addCircles(data = newdat(), #Cannot use addcirclemarkers as popup info does not work
                 lat = ~lat,
                 lng = ~lng,
                 color= ~color,
                 opacity = 1,
                 radius = 5,
                 popup = popup,
                 layerId = newdat()$`Official Water Body Name`) %>% #need this id so popup knows where to find the data
      clearControls() %>% #need to clear controls otherwise it keeps adding more legends on top of the old ones
      addLegend("topleft", 
                colors = unique(newdat()[order(newdat()$`Typeof Sign`),]$color),
                values = ~`Typeof Sign`,
                labels = sort(unique(newdat()$`Typeof Sign`)),
                opacity = 1,
                title = 'HAB hazard level')%>%
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
}#close server


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
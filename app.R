#N.Hack
#03/19/2018
#State Water Resources Control Board

#Test shiny app build by playing with HAB data and recreating the tableau map seen here
#http://www.mywaterquality.ca.gov/habs/where/freshwater_events.html

library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(shiny.router)

habdat <- readxl::read_excel("~/HAB/hab_incident_detail.xlsx")
habdat$fdate <- lubridate::date(habdat$`First Observed`)
habdat$ldate = lubridate::date(habdat$`Bloom Last Verified`)
habdat = habdat %>% dplyr::mutate(
  advisory = dplyr::recode(`Typeof Sign`, 
                           'Closed' = 'Danger',
                           'Danger' = 'Danger',
                           'Closed to Swimming' = 'Danger',
                           'No Contact Advisory' = 'Danger',
                           'Caution and Danger' = 'Danger',
                           'Precaution: Rinse Off' = 'Warning',
                           'Caution' = 'Caution',
                           'Warning to Caution' = 'Warning',
                           'Warning' = 'Warning',
                           'Advisory' = 'Caution',
                           'None' = 'Suspect',
                           'Unknown' = 'Suspect'))
habdat <- habdat %>% dplyr::mutate(color = dplyr::recode(advisory, 
                                           'Danger' = 'red',
                                           'Caution' = 'yellow',
                                           'Warning' = 'orange',
                                           'Suspect' = '#D8BFD8')) 
#specify levels to get correct order in legend
habdat$advisory = factor(habdat$advisory,levels = c('Danger',
                                                    'Warning',
                                                    'Caution',
                                                    'Suspect'))
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
      menuItem("Monitoring", tabName = 'monitoring', icon = icon('binoculars'),badgeLabel = "new", badgeColor = "green"),
      menuItem("Report a bloom", tabName='report', icon = icon('bullhorn')),
      menuItem("Tool box", tabName = 'toolbox', icon=icon('wrench'))
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
                box(status = 'success',
                    solidHeader = T,
                    title = 'Portal Updates',
                    tags$head(HTML('link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"')),
                    tags$i(class = "fa fa-bell"),
                    tags$a('Check out the new California Freshwater HABs map!'),
                    width = 12),
                box(width=4,
                    height = 600,
                    status = 'primary',
                    solidHeader = T,
                    tags$strong(style = 'text-align:center;','California Freshwater HAB Reports'),tags$br(),'View an interactive map of recently reported freshwater HABs statewide.',tags$br(),tags$br(),
                    tags$img(style = 'height: 500px; width: 400px; display: block; margin-left: auto; margin-right:auto;',src="map_events.jpg")),
                box(width=4,
                    height = 600,
                    status = 'primary',
                    solidHeader = T,
                    tags$strong('California Marine HABs'),tags$br(),'Check out tons of information on ocean HABs statewide including an interactive map of recently reported marine HABs and the most recent incidents involving saltwater HABs.',tags$br(),
                    tags$img(style='height: 500px; width: 400px; display: block; margin-left: auto; margin-right:auto;',src='CAhab.jpg')),
                box(width=4,
                    height = 600,
                    status = 'primary',
                    solidHeader = T,
                    tags$strong('Observing Freshwater HABs using Satellites'),tags$br(),'View an interactive map of HABs in large California water bodies using satellite imagery.',tags$br(),
                    tags$img(style='height: 500px; width: 400px; display: block; margin-left: auto; margin-right:auto;', src='map_satellite_tuc.png'))
                )#close row
              ),#close tabItem,
      tabItem(tabName = 'info',
              fluidRow(
                column(width = 7,
              box(title = "What are HABs?", 
                  width = NULL,
                  collapsible = T,
                  solidHeader = T,status = 'success',
                  tags$img(style = 'float: left;margin-right:15px;', 
                           src='hab1.jpg',
                           "At the base of the food chain in fresh, brackish, and marine systems are photosynthetic cyanobacteria and algae. Both single-celled microscopic and larger multicellular forms exist. When conditions are optimal, including light and temperature, levels of nutrients, and lack of water turbulence, cyanobacteria and some algae can quickly multiply into a harmful algal bloom (HAB). Some cyanobacteria and harmful algae can produce toxic chemicals, including cyanotoxins, domoic acid, and other algal toxins.", tags$br(),tags$br(),"Cyanobacteria and algae are present in most freshwater and marine aquatic ecosystems, and perform many roles that are vital for ecosystem health. Cyanobacteria and algae provide organic matter and energy to higher trophic levels, such as aquatic insects and fish.")),
              box(title = "Why should I be concerned?", 
                  width = NULL,
                  collapsible = T,
                  solidHeader = T,
                  status = 'warning',
                  tags$img(style = 'float: left;margin-right:15px;', 
                           src='dead_fish.jpg',
                           'Cyanobacteria and harmful algal blooms (HABs) can have negative impacts on the environment, people, pets, wildlife, or livestock, as well as the economy. Some HABs can produce large amounts of cyanotoxins or algal toxins, which can poison livestock, wildlife, and humans. Certain other types of cyanobacteria are nontoxic but can impart an unpleasant taste to water and fish as well as giving off an unpleasant smell as they die and decay. Cyanotoxins and algal toxins pose risks to the health and safety of people and pets recreating in water bodies, eating fish, and drinking water. They can accumulate in fish and shellfish to levels posing threats to people and wildlife consumers.',tags$br(), tags$br(),'The most researched group of freshwater HABs is cyanobacteria, or blue-green algae. These are problematic because they can impede recreational and beneficial uses of waterbodies by reducing aesthetics, lowering dissolved oxygen concentration, causing taste and odor problems in drinking water, and producing potent cyanotoxins, associated with illness and mortality in people, pets, livestock, and wildlife. Cyanobacteria blooms and their associated toxins have increased globally in geographic distribution, frequency, duration, and severity. Non-cyanobacteria HAB events have also increased, the most common of which is the golden haptophyte alga, which has caused fish kills in the east, mid-west and southern states, and Southern California.', tags$br(), tags$br(),tags$a('What to know more?', href='https://www.cdc.gov/habs/pdf/habsphysician_card.pdf'))),
              box(title = "Where do they come from?",
                  width = NULL,
                  collapsible = T,
                  solidHeader = T,
                  status = 'primary',
                  tags$img(style = 'float: left;margin-right:15px;',
                           src = 'hab2.jpg',
                           "There are a large number of environmental factors that have been linked to bloom increases and toxin production. These include climate change, nutrient over-enrichment (nitrogen and phosphorus), higher temperatures, salinity, water residence time (stagnation), vertical lake stratification, organic matter enrichment, and high pH (more alkaline)."))
            ),#close column
            column(width=5,
                   box(width=NULL,
                       status = 'success',
                       h3("How to be safe")),
                   box(width = NULL,
                       status = 'primary',
                       h3("What are we doing to help?"))
                   )#close column
          )#close row
      ),#close info tab
      tabItem(tabName = 'monitoring',
              fluidRow(
                box(
        status = 'danger', width = 12, solidHeader = T, title = "DISCLAIMER", "This map shows voluntarily reported HAB events and is for general information purposes ", tags$strong('only')," and may contain errors. The exact location, extent and toxicity of the reported bloom may not be accurate and may not affect the entire waterbody. Due to monetary constraints not all water bodies are monitored on a regular basis so no advisory does not mean no HAB is present. The data are subject to change as new information is received. Please check back as blooms are updated daily."),
             
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
     ),#close monitoring tab
     tabItem(tabName = 'report',
             fluidRow(
               infoBox(width=4,"Call",HTML(paste('1 (916) 341 - 5357', tags$br(),'Toll free: 1 (844) 729 - 6466')) , 
                        icon = icon('phone')),
               infoBox(width=4, "Email","CyanoHAB.Reports@waterboards.ca.gov",
                        icon = icon('envelope'), color = 'teal'),
               infoBox(width=4, "Mobile","Download the bloomWatch App", 
                        icon = icon('mobile'), color = 'purple',href="https://cyanos.org/bloomwatch/", img(src='bloomWatch_logo_NoBackground.png', style = 'height: 50px; width: 100px; float: right; clear:right;margin-top:-30px;'))
             ),#close row
             box(
               width = 12,
               solidHeader = T,
               status = 'primary',
               title = "Fill out a Freshwater Bloom Incident Form",
               
               tags$img(style = 'float: left;margin-right:15px;',
                        src = 'dog.jpg'),
               h3("Fill out a form when reporting a bloom for the first time or updating the status on a current bloom. This includes a new animal or human illness or death.",tags$br(), tags$br(),"Reporting a harmful algal bloom or an animal or human illness associated with exposure to a bloom helps authorities understand where problems are occurring and to respond appropriately."),tags$br(), tags$br(),
               tags$head(tags$style(HTML(".small-box {height: 120px; padding: 15px;}"))),
               valueBox('Incident form','',icon = icon('edit'), href = "http://www.mywaterquality.ca.gov/habs/do/bloomreport.html")
             ),#close box
          fluidRow(
             tabBox(
               width = 12,
               side = 'right',
               title = 'How do I know it is a HAB?',
               tabPanel(
                 "ID Guides",
                 tags$style(HTML("img{padding:30px;")),
                 a(img(src='HABid.png',width = 250, height = 300, desc = "SWAMP Guide"), href='http://www.ccamp.net/Swamp/images/3/33/SOP-Visual_Guide_to_Observing_Blooms.pdf'),
                 a(href = 'https://pubs.usgs.gov/of/2015/1164/ofr20151164.pdf',img(src='usgsHABguide.png',width = 250, height = 300)),
                 a(img(src='ohio.png',width = 250, height = 300), href = 'http://epa.ohio.gov/portals/28/Documents/HAB/BloomCharacterizationGuide-DRAFT.pdf'),
                 a(img(src='kentucky.png',width = 250, heigth= 300), href= 'http://geaugaswcd.com/yahoo_site_admin/assets/docs/Kannan-Lenca-2012-Pond_Scum_Field_Guide_c_VF_V2.88153308.pdf'),
                 a(img(src='wash.png', width=250, height=300), href = 'https://cedar.wwu.edu/cgi/viewcontent.cgi?referer=http://www.mywaterquality.ca.gov/habs/do/index.html&httpsredir=1&article=1005&context=cedarbooks')
               ),#close guides tab
               tabPanel(
                 "Algae Keys",
                 tags$style(HTML("img{padding:30px;")),
                 a(img(src='phycokey.png', width = 250, height= 300),href='http://cfb.unh.edu/phycokey/phycokey.htm'
               ),
               a(img(src='ca-algae-key.png',width=250, height=300),href='http://dbmuseblade.colorado.edu/DiatomTwo/sbsac_site/key.html')
               )#close algae tab
               )#close tabbox
          )#close fluidRow
     )#close report tab
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
  
  
  output$viewData <- renderDT(
    newdat() %>% 
      dplyr::group_by(`Official Water Body Name`) %>%
      dplyr::summarise('County' = `County Name`,
                       'Regional waterboard' = `Regional Water Board__1`,
                       'Water manager' = `Water Body Manager`,
                       'Hazard level' = advisory,
                       'First date observed' = as.character(fdate),
                       'Last date observed' = as.character(ldate),
                       'Are signs present?' = `Posted Sign Description`,
                       "Is the incident resolved?" = `IsIncidentResolved__1`,
                       'Details' = `Incident Information`
                       )
)#create output table of data above
  
  output$button <- downloadHandler( 
    filename= function(){
      paste("HAB",input$date[1],'to', input$date[2],'in',input$county,"county.csv",sep = "")
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
      sprintf(as.character(selectedwater$advisory)),
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
                colors = unique(newdat()[order(newdat()$advisory),]$color),
                values = ~advisory,
                labels = sort(unique(newdat()$advisory)),
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
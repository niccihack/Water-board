#N.Hack
#03/19/2018
#State Water Resources Control Board

#Test shiny app build by playing with HAB data and recreating the tableau map seen here
#http://www.mywaterquality.ca.gov/habs/where/freshwater_events.html

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(rdrop2)

# Define the fields we want to save from the form
fields <- c("talkselect", "rank", "interest", "clarity")

# which fields are mandatory
fieldsMandatory <- "talkselect"

outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(file.path(outputDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- do.call(rbind, data)
  data
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    title = "Presentation survey",
    fluidRow(
      column(6,
             div(
               id='form',tags$hr(),
               selectInput("talkselect",h3("Select a talk"),
                           choices = list("Working with Nature Across the Land-Use Spectrum: a Holistic Approach to Ecological Resilience","Urban Ecology: Designing the Climate-Resilient Cities of the Future","California’s Future – Warmer, Drier and Wetter","Effects of Climate Uncertainty on the Development and Evaluation of Adaptation Strategies","Reexamination of Sediment Management in Newport Bay under Accelerating Sea-Level Rise","Use of Urban Vegetation As Climate Adaptation- Effective, and Low Cost When Done Well","Land Use Change, Fire, Cannabis","Erosion and Best Management Practices after Forest Fire","Drinking Water Quality Impacts of Watershed Fires: A Case Study","Panel Discussion: How to address water quality monitoring problems associated with fire?","Lightning Talks: Short presentations on innovative tech and special studies","Monitoring Bioaccumulation in California’s Changing Landscapes","Remote Sensing of Cyanobacteria Abundance: Next Steps in Utilizing Satellite Imagery and Data","Statewide Contaminant and Toxicity Monitoring Related to Land Use","From Open Data to Open Indicators","A Vision for More Effective Use of Biological Data in Water Resource Management", "Prioritizing Management Goals for Stream Biological Integrity Within the Context of Landscape Constraints","Using an Interactive Dashboard to Communicate Bioassessment Data", "eDNA Methods and Application for several State and Federally Listed Aquatic Species", "Panel Discussion: What are the Water Data Needs to Address the Management Decisions Related to Landscape Change?","Data Story and Visualization Panel Discussion")),
               sliderInput("rank", "Rank this talk from 0 to 5 with 5 being the best",0, 5,1, ticks = F),
               radioButtons("interest", label = "The subject of this talk was of interest to me", choices = list("Strongly agree"=1, "Somewhat agree"=2, "Neutral"=3,"Somewhat disagree"=4,"Strongly disagree"=5)),
               radioButtons("clarity", label = "The speaker did a good job of presenting information in an understandable way", choices = list("Strongly agree"=1, "Somewhat agree"=2, "Neutral"=3,"Somewhat disagree"=4,"Strongly disagree"=5)),
               actionButton("submit", "Submit", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )#close hidden div
               )#close hidden
             )#close div
      ),#close column
    
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thank you, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    )#close thank you message  
  #plotOutput('plot')
    )#close row
),#close page
  
  server = function(input, output, session) {
    
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    
    # Plotting values
    #values = reactiveValues(values = NULL)
    #observeEvent(input$interest, {
      #values$values <- read.csv(input$file$datapath)
    #})
    #output$plot <- renderPlot({
    #  barplot(input$submit$interest)
    #})     
  }
)




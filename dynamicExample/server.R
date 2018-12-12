#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Created on: ", format(Sys.time(), '%d %B, %Y'))
  })
  
  # render the header section
  output$head <- renderUI({
    lapply(headList, switchButtons)
  })
  
  # render sections as tab sections
  output$questions <- renderUI({
    sections <- lapply(sectionsList, renderSection)
    names(sections) <- NULL
    do.call(tabsetPanel, sections)
  })
  
  # stores the answers in a list
  answers <- reactive({
    reactiveValuesToList(input)
  })
  
  # Temporary output that shows the current answers for debugging
  output$answers <- renderPrint({
    answers()
  })
  
  # checks whether the report is complete
  isDownloadable <- reactive({
    isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
  })
  
  # whenever the input is complete, let's enable the download button
  observe({
    if(isDownloadable()){
      shinyjs::enable("report")
    } else{
      shinyjs::disable("report")
    }
  })
  
  # whenever the input is not complete, show the tooltip for explanation for the download button
  output$trigger <- renderUI({
   
    if(isDownloadable()){
      tags$script("$('#report').tooltip('hide');")
    } else{
      tags$script("$('#report').tooltip('show');")
    }

  })
  
  # This is a toy example how to do another validation alerts: for example, showing the user that the email is in a wrong format
  output$triggerEmail <- renderUI({
    email <- answers()$correspondingEmail
    if(email == "@" || isValidEmail(email)){
      tags$script("$('#correspondingEmail').tooltip('hide');")
    } else{
      tags$script("$('#correspondingEmail').tooltip('show');")
    }
  })
  
  # This section deals with the pdf generation
  output$report <- downloadHandler(
    
    filename = "report.pdf",
    
    content = function(file) {
      # Create the report file in a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      # create a string which copies the structure of the desired rmd file
      RmdFile <- composeRmd(answers = answers(), sectionsList = sectionsList, headList = headList, answerList = answerList)
      
      # print the Rmd document in the console (for debugging)
      #writeLines(RmdFile)
      
      # store the string as a temporary report.Rmd file 
      tempReport <- file.path(tempdir(), "report.Rmd")
      writeLines(RmdFile, con = tempReport)
      
      # knit the temporary document into a proper pdf (which will be called "report.pdf")
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})

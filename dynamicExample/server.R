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
  
  #### Send header and questions to UI ----
  output$currentTime <- renderText({
    #invalidateLater(1000, session)
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
    do.call(tabsetPanel, c(sections, id = "sections"))
  })
  
  #### Store answers, check whether checklist is complete ----
  # stores the answers in a list
  answers <- reactive({
    reactiveValuesToList(input)
  })
  
  #### Moving to the next or previous sections ----
  observeEvent(input$nextButton, {
    sectionId <- sapply(sectionsList, function(section) digest::digest(section$Name))
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection + 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
  })
  observeEvent(input$previousButton, {
    sectionId <- sapply(sectionsList, function(section) digest::digest(section$Name))
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection - 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
  })
  # disable moving next or previous for first and last sections
  observeEvent(input$sections, {
    sectionId <- sapply(sectionsList, function(section) digest::digest(section$Name))
    currSection <- which(sectionId == input$sections)
    if(currSection == 1){
      shinyjs::disable("previousButton")
    } else{
      shinyjs::enable("previousButton")
    }
    
    if(currSection == length(sectionId)){
      shinyjs::disable("nextButton")
    } else{
      shinyjs::enable("nextButton")
    }
  })
  # observeEvent(currSection(), {
  #   sectionId <- sapply(sectionsList, function(section) section$Name)
  # 
  #   updateTabsetPanel(session, "", selected = digest::digest(sectionId[currSection()]))
  # })
  # Temporary output that shows the current answers for debugging
  # output$answers <- renderPrint({
  #  answers()
  # })
  
  # checks whether the report is complete
  isDownloadable <- reactive({
    isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
  })
  
  #### Reactive animations ----
  # whenever the input is complete, let's enable the download button
  observe({
    if(isDownloadable()){
      shinyjs::enable("report")
      
      # and start animation every 4 sec
      invalidateLater(4000, session)
      shinyanimate::startAnim(session, "generatereport", type = "bounce")
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
  
  # changing icons when item is answered
  observe({
    items <- getItemList(sectionsList)
    
    for(item in items){
      session$sendCustomMessage(
        type = "toggleChecker",
        message = list(id = paste0(item, "Checker"), val = input[[item]], divId = paste0("div", item, "Checker"))
      )
    }
  
  })
  
  # validate title of the study
  observeEvent(input$studyTitle, {
    feedbackSuccess(
      inputId = "studyTitle",
      condition = input$studyTitle != "",
      text = NULL,
      color = "black"
    )
  })
  
  # validate author names
  observeEvent(input$authorNames, {
    feedbackSuccess(
      inputId = "authorNames",
      condition = input$authorNames != "",
      text = NULL,
      color = "black"
    )
  })
  
  
  # validate e-mail
  observeEvent(input$correspondingEmail, {
    if (input$correspondingEmail == "@"){

    } else if (isValidEmail(input$correspondingEmail)){
      feedbackSuccess(
        inputId = "correspondingEmail",
        condition = TRUE,
        text = " ",
        color = "black"
      )
    } else {
      feedbackWarning(
        inputId = "correspondingEmail",
        condition = TRUE,
        text = "Provided email appears invalid.",
        color = "black"
      )
    }
  })
  
  # validate link
  observeEvent(input$linkToRepository, {
    if (input$linkToRepository == ""){
      
    } else if (RCurl::url.exists(input$linkToRepository)){
      feedbackSuccess(
        inputId = "linkToRepository",
        condition = TRUE,
        text = NULL,
        color = "black"
      )
    } else {
      feedbackWarning(
        inputId = "linkToRepository",
        condition = TRUE,
        text = "The link cannot be accessed.",
        color = "black"
      )
    }
  })

  #### Working with report ----
  # Stash current Rmd if report dropdown is opened or save_as is changed  
  RmdFile <- reactive({
    dontrun <- input$generatereport
    composeRmd(answers = isolate(answers()),
               sectionsList = sectionsList, headList = headList, answerList = answerList,
               save.as = input$save.as)
  })
  
  # render Rmd file in show code modal panel
  output$code <- renderText({
    RmdFile()
  })
  
  # render previews
  output$generatePreview <- renderUI({
    input$preview
    RmdPath <- file.path(tempdir(), "report.Rmd")
    writeLines(RmdFile(), con = RmdPath)

    if(input$save.as %in% c("word", "rtf")){
      showNotification("Word and rtf files cannot be previewed in the browser, displaying markdown file",
                       type = "warning", closeButton = FALSE, duration = 7)
      includeMarkdown(RmdPath)
    } else{
      save.as <- ifelse(input$save.as == "word", "docx", input$save.as)
      out_file <- paste0("preview.", save.as)

      rmarkdown::render(RmdPath, output_file = out_file, output_dir = "www/doc",
                        envir = new.env(parent = globalenv()))
      src_file <- file.path("doc", out_file)
      tags$iframe(style = "height:600px; width:100%", src = src_file)
    }
  })

  #### Download ----
  # This section deals with the pdf generation
  output$report <- downloadHandler(
    
    
    filename = function() {
      save.as <- ifelse(input$save.as == "word", "doc", input$save.as)
      paste("report", save.as, sep = ".")
    },
    
    content = function(file) {
      # Create the report file in a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).

      # create a string which copies the structure of the desired rmd file
      RmdFile <- composeRmd(answers = answers(),
                            sectionsList = sectionsList, headList = headList, answerList = answerList,
                            save.as = input$save.as)
      
      # print the Rmd document in the console (for debugging)
      #writeLines(RmdFile)
      
      # store the string as a temporary report.Rmd file 
      tempReport <- file.path(tempdir(), "report.Rmd")
      writeLines(RmdFile, con = tempReport)
      
      # knit the temporary document into a proper pdf (which will be called "report.pdf/html/doc")
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv()))
      
      showNotification("Downloaded", type = "message", duration = 3, closeButton = FALSE)
    }
  )
  
})

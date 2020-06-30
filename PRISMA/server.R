library(shiny)

shinyServer(function(input, output, session) {
  
  #### Send header and questions to UI ----
  # output$currentTime <- renderText({
  #   #invalidateLater(1000, session)
  #   shinyBS::toggleModal(session, "intro")
  #   paste("Created on: ", format(Sys.time(), '%d %B, %Y'))
  # })
  
  # observe({
  #   shinyBS::toggleModal(session, "intro")
  # })
  
  # Deprecated rendering of sections (now created in global.R and passed in ui directly)
  # render the header section
  # output$head <- renderUI({
  #   headingHTML
  # })
  
  # render sections as tab sections
  # output$questions <- renderUI({
  #   sectionsHTML
  # })
  
  #### Store answers, check whether checklist is complete ----
  # stores the answers in a list
  answers <- reactive({
    reactiveValuesToList(input)
  })
  
  # #### Moving to the next or previous sections ----
  observeEvent(input$test, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection + 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
    shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
  })
  # observeEvent(input$previousButton, {
  #   sectionId <- sapply(sectionsList, function(section) section$Value)
  #   currSection <- which(sectionId == input$sections)
  #   nextSection <- currSection - 1
  #   updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
  #   shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
  # })
  # # disable moving next or previous for first and last sections
  # observeEvent(input$sections, {
  #   sectionId <- sapply(sectionsList, function(section) section$Value)
  #   currSection <- which(sectionId == input$sections)
  #   if(currSection == 1){
  #     shinyjs::hide("previousButton")
  #   } else{
  #     shinyjs::show("previousButton")
  #   }
  #   
  #   if(currSection == length(sectionId)){
  #     shinyjs::hide("nextButton")
  #   } else{
  #     shinyjs::show("nextButton")
  #   }
  # })

  # Temporary output that shows the current answers for debugging
  # Use to capture answers and pass to table
  output$answers <- renderTable({
  ll <- answers()[grepl("ind_m_.*_text\\b", names(answers()))]
    
  df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                   Obs = unlist(ll))
  sort(df$ID)
  df
  })
  
  
  # Carry across responses to the "Title" question in the Main checklist to the
  # PRISMA-A checklist
  observeEvent(input$ind_m_1, {
    updateRadioButtons(session, "ind_a_1", selected = input$ind_m_1)
  })
  
  observeEvent(input$ind_m_1_text, {
    updateRadioButtons(session, "ind_a_1_text", selected = input$ind_m_1_text)
  })
  
  #For testing - delete when done
  observeEvent(input$fill, {
    updateRadioButtons(session = session, "ind_a_1", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_2", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_3", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_4", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_5", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_6", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_7", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_8", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_9", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_10", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_11", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_12", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_1", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_3", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_4", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_5", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_6", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_7", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_8", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_9", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_10a", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_10b", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_11", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_12", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_13a", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_13b", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_13c", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_13d", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_13e", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_13f", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_14", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_15", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_16a", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_16b", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_17", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_18", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_19", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_20a", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_20b", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_20c", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_20d", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_21", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_22", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_23a", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_23b", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_23c", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_23d", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_24a", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_24b", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_24c", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_25", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_26", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_27", selected = "Yes")
    })
  
  # checks which sections are complete
  whichComplete <- reactive({
    isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
  })
  
  # checks whether the report is complete
  isDownloadable <- reactive({
    all(whichComplete())
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
    items <- getItemList(sectionsList, all = FALSE) # loop only on items
    
    for(item in items){
      
      session$sendCustomMessage(
        type = "toggleChecker",
        message = list(id = paste0(item, "Checker"), val = input[[item]], divId = paste0("div", item, "Checker"))
      )
    }
  
  })
  
  observeEvent(input$generatereport, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    
    #if(!isDownloadable()){
      for(section in sectionId){
        # output[[paste0("icon_", section)]] <- renderText({"table"})
      }  
    #}
  })
  
  observeEvent(input$generatereport, {
    items <- getItemList(sectionsList, all = FALSE)
    ans   <- isolate(answers())
    
    for(item in items){
      if(ans[item] == "" || is.null(ans[[item]])){
        shinyanimate::startAnim(session, paste0(item, "Checker"), type = "shake")
      }
      
      session$sendCustomMessage(
        type = "toggleCheckerColor",
        message = list(id = paste0(item, "Checker"), val = input[[item]], divId = paste0("div", item, "Checker"))
      )
    }
  })
 
  # Change icons in Section headings (three state option)
  observe({
    sectionValues <- sapply(sectionsList, function(sec) sec$Value)
    for(i in seq_along(sectionValues)){
      session$sendCustomMessage(
        type = "toggleSectionIcon",
        # as long as the user does not click "report", do not display aggresive feedback (-> val = "init")
        message = list(id = paste0(".icon", sectionValues[[i]]),
                       val = ifelse(input$generatereport == 0 && !whichComplete()[[i]],
                                    "init", whichComplete()[[i]])
                       )
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
      paste("Transparency Report", save.as, sep = ".")
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

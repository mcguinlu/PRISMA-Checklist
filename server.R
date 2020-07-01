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
  output$answers <- renderUI({

  })
  
  
  # Carry across responses to the "Title" question in the Main checklist to the
  # PRISMA-A checklist
  observeEvent(input$ind_m_1, {
    updateRadioButtons(session, "ind_a_1", selected = input$ind_m_1)
  })
  
  observeEvent(input$ind_m_1_text, {
    updateRadioButtons(session, "ind_a_1_text", selected = input$ind_m_1_text)
  })
  
  # #For testing - delete when done
  observeEvent(input$fill, {
    updateRadioButtons(session = session, "ind_a_2", selected = "No")
    updateRadioButtons(session = session, "ind_a_3", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_4", selected = "No")
    updateRadioButtons(session = session, "ind_a_5", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_6", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_7", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_8", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_9", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_10", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_11", selected = "Yes")
    updateRadioButtons(session = session, "ind_a_12", selected = "Yes")
    updateRadioButtons(session = session, "ind_m_1", selected = "Yes")
    updateTextInput(session = session, "ind_m_1_text", value = "Section 1")
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
    } else {
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
  

  #### Download ----
  # This section deals with the pdf generation
  output$report <- downloadHandler(
    
    filename = function() {
      save.as <- ifelse(input$save.as == "Word", "doc", input$save.as)
      paste("PRISMA Checklist", save.as, sep = ".")
    },
    
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading checklist"),
                          value = 0, {
      # Create the report file in a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempfile <- file.path(tempdir(), "reference.docx")
      
      if (input$save.as == "pdf") {
        file.copy("www/doc/report_pdf.Rmd", tempReport, overwrite = TRUE)
      } else {
        file.copy("www/doc/report_word.Rmd", tempReport, overwrite = TRUE)
        file.copy("www/doc/word-styles-reference-01.docx", tempfile, overwrite = TRUE)
      }

      ll <- answers()[grepl("ind_m_.*\\b", names(answers()))]
      df2 <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                        response = unlist(ll))
      ll <- answers()[grepl("ind_m_.*_text\\b", names(answers()))]
      df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                       text = unlist(ll))
      
      df$ID <- gsub("ind_m_", "",df$ID)
      df2$ID <- gsub("ind_m_", "",df2$ID)
      df$ID <- gsub("_text", "",df$ID)
      
      df <- merge(df2, df, by= "ID")
      colnames(df)[2] <- "Response"
      colnames(df)[3] <- "Text"
      
      df_m <- merge(df_m, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
      
      df_m <- df_m[order(df_m$seq),] %>%
        select(Domain,No,Label,Response,Text)
      
      
      ll <- answers()[grepl("ind_a_.*\\b", names(answers()))]
      df2 <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                        response = unlist(ll))
      ll <- answers()[grepl("ind_a_.*_text\\b", names(answers()))]
      df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                       text = unlist(ll))
      
      df$ID <- gsub("ind_a_", "",df$ID)
      df2$ID <- gsub("ind_a_", "",df2$ID)
      df$ID <- gsub("_text", "",df$ID)
      
      df <- merge(df2, df, by= "ID")
      colnames(df)[2] <- "Response"
      colnames(df)[3] <- "Text"
      
      df_a <- merge(df_a, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
      
      df_a <- df_a[order(df_a$seq),] %>%
        select(Domain,No,Label,Response,Text)
      
      # Set up parameters to pass to Rmd document
      params <- list(df_m = df_m, 
                     df_a = df_a)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      })  
    }
  )
  
})









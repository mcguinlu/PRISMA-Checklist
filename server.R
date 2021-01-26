shinyServer(function(input, output, session) {
  

# NAVIGATION --------------------------------------------------------------

  # Move to PRISMA-A tab from Item 2 of the Main checklist
  observeEvent(input$gotoAb, {
    sectionId <- sapply(sectionsList, function(section) section$Value)
    currSection <- which(sectionId == input$sections)
    nextSection <- currSection + 1
    updateTabsetPanel(session, "sections", selected = sectionId[[nextSection]])
    shinyjs::runjs("document.getElementById('scrollAnchor').scrollIntoView({behavior: 'smooth'});")
  })

  # Carry across responses to the "Title" question in the Main checklist to the
  # PRISMA-A checklist
  observeEvent(input$ind_m_1, {
    updateRadioButtons(session, "ind_a_1", selected = input$ind_m_1)
  })
  
  observeEvent(input$ind_m_1_text, {
    updateRadioButtons(session, "ind_a_1_text", selected = input$ind_m_1_text)
  })

  
# TESTING -----------------------------------------------------------------

  # For testing - delete when done
  observeEvent(input$fill, {
    updateRadioButtons(session = session, "ind_a_1", selected = "Not reported")
    updateRadioButtons(session = session, "ind_a_2", selected = "Not reported")
    updateRadioButtons(session = session, "ind_a_3", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_4", selected = "Not reported")
    updateRadioButtons(session = session, "ind_a_5", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_6", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_7", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_8", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_9", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_10", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_11", selected = "Reported")
    updateRadioButtons(session = session, "ind_a_12", selected = "Reported")
    updateRadioButtons(session = session, "ind_m_1", selected = "Reported")
    updateTextInput(session = session, "ind_m_1", value = "Section 1, Page 2")
    updateTextInput(session = session, "ind_m_3", value = "Section 4, Line 7")
    updateTextInput(session = session, "ind_m_4", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_5", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_6", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_7", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_8", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_9", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_10a", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_10b", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_11", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_12", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_13a", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_13b", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_13c", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_13d", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_13e", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_13f", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_14", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_15", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_16a", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_16b", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_17", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_18", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_19", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_20a", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_20b", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_20c", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_20d", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_21", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_22", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_23a", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_23b", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_23c", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_23d", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_24a", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_24b", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_24c", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_25", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_26", value = "Line XX-ZZ")
    updateTextInput(session = session, "ind_m_27", value = "Line XX-ZZ")
    })
  

# VALIDATION --------------------------------------------------------------

  # Checks which sections are complete, and enables download when they are
  # Three different set-ups: one for both, and one for each (Main ("_main") /
  # Abstract (_abs))
  
  # observe({
  #   shinyjs::disable("report")
  # })
  
  # Validation for both checklists
    whichComplete <- reactive({
      isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
    })
  
    isDownloadable <- reactive({
      all(whichComplete())
    })

  # Validation for Abstract checklist only
    whichComplete_abs <- reactive({
      isComplete(answers = answers(),
                 sectionsList = sectionsList[2],
                 headList = headList)
    })
    
    isDownloadable_abs <- reactive({
      all(whichComplete_abs())
    })
    

  # Validation for Main checklist only
    whichComplete_main <- reactive({
      isComplete(answers = answers(),
                 sectionsList = sectionsList[1],
                 headList = headList)
    })
    
    isDownloadable_main <- reactive({
      all(whichComplete_main())
    })

    # Enable download only for those that are complete
    
    observe({
      shinyjs::disable("report")
      if(isDownloadable() & input$report_type == "Main + Abstract"){
        shinyjs::enable("report")
      }
      if(isDownloadable_abs() & input$report_type == "_abs"){
        shinyjs::enable("report")
      }
      if(isDownloadable_main() & input$report_type == "_main"){
        shinyjs::enable("report")
      }
    })
  

# DYNAMIC FEEDBACK --------------------------------------------------------

  # Show exclamation beside items that are not complete
  output$trigger <- renderUI({
    if(isDownloadable()){
      tags$script("$('#report').tooltip('hide');")
    } else{
      tags$script("$('#report').tooltip('show');")
    }

  })
  
  # Change icon to tick when a question is answered
  observe({
    items <- getItemList(sectionsList, all = FALSE) # loop only on items
    
    for (item in items) {
      session$sendCustomMessage(type = "toggleChecker",
                                message = list(
                                  id = paste0(item, "Checker"),
                                  val = input[[item]],
                                  divId = paste0("div", item, "Checker")
                                ))
    }
    
  })
  
  # Change icons in Section headings
  observe({
    sectionValues <- sapply(sectionsList, function(sec)
      sec$Value)
    for (i in seq_along(sectionValues)) {
      session$sendCustomMessage(type = "toggleSectionIcon",
                                message = list(
                                  id = paste0(".icon", sectionValues[[i]]),
                                  val = ifelse(
                                    input$generatereport == 0 && !whichComplete()[[i]],
                                    "init",
                                    whichComplete()[[i]]
                                  )
                                ))
    }
  })
  

# CLEAN AND FORMAT ANSWERS ------------------------------------------------

  # Convert answers to list
  answers <- reactive({
    reactiveValuesToList(input)
  })
  
  # Create reactive values containing the dataframes produced from JSON in
  # global.R
  rv <- reactiveValues(df_m = df_m, df_a = df_a)
  
  # Once "Generate report" is clicked, create clean datasets
  observeEvent(input$generatereport,{
    
    # Create clean Main checklist 
    # Extract answers and text to dataframes
    ll <- answers()[grepl("ind_m_.*\\b", names(answers()))]
    df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                      response = unlist(ll))

    # Get item ID
    df$ID <- gsub("ind_m_", "",df$ID)

    # Merge to create dataframe containing ID, answer, answer text
    colnames(df)[2] <- "Location where item is reported"
    
    # Merge with dataframe containing question text
    df_m <- merge(df_m, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
    
    # Order by seq and select relevant columns
    df_m <- df_m[order(df_m$seq),] %>%
      select(Domain,No,Label,"Location where item is reported")
    
    colnames(df_m)[1] <- "Topic"
    colnames(df_m)[2] <- "No."
    colnames(df_m)[3] <- "Item"
    
    # Assign to reactive value
    rv$df_m <- df_m
    
    
    # Create clean abstract checklist dataframe (with answers)
    ll <- answers()[grepl("ind_a_.*\\b", names(answers()))]
    df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                      response = unlist(ll))

    # Get item ID
    df$ID <- gsub("ind_a_", "",df$ID)
    df$response <- gsub("Reported","Yes",df$response)
    df$response <- gsub("Not reported","No",df$response)
    
    # Merge to create dataframe containing ID, answer, answer text
    colnames(df)[2] <- "Reported?"

    # Merge with dataframe containing question text    
    df_a <- merge(df_a, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
    
    # Order by seq and select relevant columns
    df_a <- df_a[order(df_a$seq),] %>%
      select(Domain,No,Label,"Reported?")
    
    colnames(df_a)[1] <- "Topic"
    colnames(df_a)[2] <- "No."
    colnames(df_a)[3] <- "Item"
    
    # Assign to reactive value
    rv$df_a <- df_a
  })
  
  

# DOWNLOADS ---------------------------------------------------------------

  # Download report
  output$report <- downloadHandler(
    filename = function() {
      format <- ifelse(input$format == "word", "docx", "pdf")
      paste0("PRISMA Checklist.", format)
    },
    
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading checklist"),
                          value = 0.8,
                          {
                            tempReport <- file.path(tempdir(), "report.Rmd")
                            tempfile <- file.path(tempdir(), "reference.docx")
                            report_type <- ifelse(input$report_type == "Main + Abstract","",input$report_type)
                            
                            if (input$format == "PDF") {
                              file.copy(paste0("www/doc/report_pdf",report_type,"_",input$orient,".Rmd"), tempReport, overwrite = TRUE)
                            } else {
                              file.copy(paste0("www/doc/report_word",report_type,".Rmd"), tempReport, overwrite = TRUE)
                              file.copy(paste0("www/doc/word-styles-reference-",input$orient,".docx"),
                                        tempfile,
                                        overwrite = TRUE)
                            }

                            
                            # Render the report
                            rmarkdown::render(
                              tempReport,
                              output_file = file,
                              params = list(df_m = rv$df_m, # Main data 
                                            df_a = rv$df_a), # Abstract data
                              envir = new.env(parent = globalenv())
                            )
                            
                          })
    }
  )
  

## Download citations
  
  output$downloadbib <- downloadHandler(
    filename = function() {
      paste("citation", ".bib", sep = "")
    },
    content = function(file) {
      file.copy("www/prismacitation.bib", file)
    }
  )
  
  output$downloadris <- downloadHandler(
    filename = function() {
      paste("citation", ".ris", sep = "")
    },
    content = function(file) {
      file.copy("www/prismacitation.ris", file)
    }
  )
  
  
})









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
    updateTextInput(session = session, "ind_m_1_text", value = "Section 1, Page 2")
    updateTextInput(session = session, "ind_m_3_text", value = "A really long string to see if the reports look okay when there is a lot of text.")
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
  

# VALIDATION --------------------------------------------------------------

  # Checks which sections are complete, and enables download when they are
  # Three different set-ups: one for both, and one for each (Main ("_main") /
  # Abstract (_abs))
  
  # Validation for both checklists
    whichComplete <- reactive({
      isComplete(answers = answers(), sectionsList = sectionsList, headList = headList)
    })
  
    isDownloadable <- reactive({
      all(whichComplete())
    })
    
    observe({
      if(isDownloadable()){
        shinyjs::enable("report")
      } else {
        shinyjs::disable("report")
      }
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
    
    observe({
      if(isDownloadable_abs()){
        shinyjs::enable("report_abs")
      } else {
        shinyjs::disable("report_abs")
      }
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
    
    observe({
      if(isDownloadable_main()){
        shinyjs::enable("report_main")
      } else {
        shinyjs::disable("report_main")
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
    df2 <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                      response = unlist(ll))
    ll <- answers()[grepl("ind_m_.*_text\\b", names(answers()))]
    df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                     text = unlist(ll))
    
    # Get item ID
    df$ID <- gsub("ind_m_", "",df$ID)
    df2$ID <- gsub("ind_m_", "",df2$ID)
    df$ID <- gsub("_text", "",df$ID)
    
    # Merge to create dataframe containing ID, answer, answer text
    df <- merge(df2, df, by= "ID")
    colnames(df)[2] <- "Reported?"
    colnames(df)[3] <- "Page/Section"
    
    # Merge with dataframe containing question text
    df_m <- merge(df_m, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
    
    # Order by seq and select relevant columns
    df_m <- df_m[order(df_m$seq),] %>%
      select(Domain,No,Label,"Reported?","Page/Section")
    
    colnames(df_m)[1] <- "Topic"
    colnames(df_m)[2] <- "No."
    colnames(df_m)[3] <- "Item"
    
    # Assign to reactive value
    rv$df_m <- df_m
    
    
    # Create clean abstract checklist dataframe (with answers)
    ll <- answers()[grepl("ind_a_.*\\b", names(answers()))]
    df2 <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                      response = unlist(ll))
    ll <- answers()[grepl("ind_a_.*_text\\b", names(answers()))]
    df <- data.frame(ID = rep(names(ll), sapply(ll, length)),
                     text = unlist(ll))
    
    # Get item ID
    df$ID <- gsub("ind_a_", "",df$ID)
    df2$ID <- gsub("ind_a_", "",df2$ID)
    df$ID <- gsub("_text", "",df$ID)
    
    # Merge to create dataframe containing ID, answer, answer text
    df <- merge(df2, df, by= "ID")
    colnames(df)[2] <- "Reported?"
    colnames(df)[3] <- "Page/Section"
    
    # Merge with dataframe containing question text    
    df_a <- merge(df_a, df, by.x = "No", by.y = "ID", all.x = TRUE, sort = FALSE)
    
    # Order by seq and select relevant columns
    df_a <- df_a[order(df_a$seq),] %>%
      select(Domain,No,Label,"Reported?","Page/Section")
    
    colnames(df_a)[1] <- "Topic"
    colnames(df_a)[2] <- "No."
    colnames(df_a)[3] <- "Item"
    
    # Assign to reactive value
    rv$df_a <- df_a
  })
  
  

# DOWNLOADS ---------------------------------------------------------------

  # Download Main + Abstract report
  output$report <- downloadHandler(
    filename = function() {
      format <- ifelse(input$format == "Word", "docx", "pdf")
      paste0("PRISMA (Main + Abstract) Checklist.", format)
    },
    
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading checklist"),
                          value = 0.8,
                          {
                            tempReport <- file.path(tempdir(), "report.Rmd")
                            tempfile <- file.path(tempdir(), "reference.docx")
                            
                            if (input$format == "PDF") {
                              file.copy(paste0("www/doc/report_pdf_",input$orient,".Rmd"), tempReport, overwrite = TRUE)
                            } else {
                              file.copy("www/doc/report_word.Rmd", tempReport, overwrite = TRUE)
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
  
  # Download Main report only
  output$report_main <- downloadHandler(
    filename = function() {
      format <- ifelse(input$format == "Word", "docx", "pdf")
      paste0("PRISMA (Main) Checklist.", format)
    },
    
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading checklist"),
                          value = 0.8,
                          {
                            tempReport <- file.path(tempdir(), "report.Rmd")
                            tempfile <-
                              file.path(tempdir(), "reference.docx")
                            
                            if (input$format == "PDF") {
                              file.copy(paste0("www/doc/report_pdf_main_",input$orient,".Rmd"), tempReport, overwrite = TRUE)
                            } else {
                              file.copy("www/doc/report_word_main.Rmd", tempReport, overwrite = TRUE)
                              file.copy(paste0("www/doc/word-styles-reference-",input$orient,".docx"),
                                        tempfile,
                                        overwrite = TRUE)
                            }
                            
                            # Render the report
                            rmarkdown::render(
                              tempReport,
                              output_file = file,
                              params = list(df_m = rv$df_m), # Main data
                              envir = new.env(parent = globalenv())
                            )
                          })
    }
  )
  
  # Download Abstract report only
  output$report_abs <- downloadHandler(
    filename = function() {
      format <- ifelse(input$format == "Word", "docx", "pdf")
      paste0("PRISMA (Abstract) Checklist.", format)
    },
    
    content = function(file) {
      shiny::withProgress(message = paste0("Downloading checklist"),
                          value = 0.8,
                          {
                            tempReport <- file.path(tempdir(), "report.Rmd")
                            tempfile <-
                              file.path(tempdir(), "reference.docx")
                            
                            if (input$format == "PDF") {
                              file.copy(paste0("www/doc/report_pdf_abs_",input$orient,".Rmd"), tempReport, overwrite = TRUE)
                            } else {
                              file.copy("www/doc/report_word_abs.Rmd", tempReport, overwrite = TRUE)
                              file.copy(paste0("www/doc/word-styles-reference-",input$orient,".docx"),
                                        tempfile,
                                        overwrite = TRUE)
                            }
                            
                            # Render the report
                            rmarkdown::render(
                              tempReport,
                              output_file = file,
                              params = list(df_a = rv$df_a), # Abstract data
                              envir = new.env(parent = globalenv())
                            )
                          })
    }
  )
  

  
  
})









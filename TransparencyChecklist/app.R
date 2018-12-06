library(shiny)
library(tidyr)
library(pander)


shinyApp(
  ui = fluidPage(
    br(),
    titlePanel("TRANSPARENCY REPORT"),
    h4(textOutput("currentTime")),
 
    fluidRow(wellPanel(
      textInput(inputId = "studyTitle", label = h4("Title of the Study:"), value = ""),
      textInput(inputId = "authorNames", label = "All Authors [Given name(s) Surname;] *"),
      
      textInput(inputId = "correspondingEmail", label = "Corresponding author's email address *"),
      textInput(inputId = "linkToRepository", label = "Link to Project Repository *")
      
    )),
    
    title = 'Transparency report',
    mainPanel(
      helpText("After answering the checklist questions, please generate a pdf report and register it to an open-access repository where it can remain publicly accessible via the web. By adding the link of this report to your manuscript, you can inform the reader about the transparency and openness related factors of your study."),
      
      uiOutput("test"),
    
    
    downloadButton("report", "Generate report")
    )
  ),
  server = function(input, output, session) {
    
    # Displays the current date
    output$currentTime <- renderText({
      invalidateLater(1000, session)
      paste("Created on: ", Sys.Date())
    })
    
    #prepare question text
    questionsData <- read.csv("questions.csv", sep = ";")
    questionsData$Questions <- as.character(questionsData$Questions)
    questionsData$Answers <- sapply(strsplit(as.character(questionsData$Answers), ","), as.list, USE.NAMES = TRUE)
    names(questionsData$Answers) <-rownames(questionsData)
    
    #holy grail
    output$test <- renderUI({
      lapply(1:nrow(questionsData), function(i) {
        if(questionsData$Type[i]=="select"){
          if (i %in% c(2,3)){
            #setting the conditional relations up, e.g.: making 2,3 conditional upon Q1  
            conditionalPanel(
              condition = "input.ind1 == 'Yes'",
              selectInput(inputId = paste0("ind", i), questionsData$Questions[i], questionsData$Answers[i], width = "600px")
            )  
            
          }else{
            selectInput(inputId = paste0("ind", i), questionsData$Questions[i], questionsData$Answers[i], width = "600px")
          }
        }
      })
    })
    
    output$report <- downloadHandler(
      
      
      filename = "report.pdf",
      
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(answers = shiny::reactiveValuesToList(input), mainQuestions = questionsData$Questions)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
)

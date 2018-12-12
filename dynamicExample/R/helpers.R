## Functions which help us to create some structure of the shiny app
## and defines the buttons which appear horizontal

renderSection <- function(section){
  # creates a tab ( can be changed to a fluidrow if we do not want tabs)
  
  tabPanel(section$Name,
    br(),
    # create the header and an initial info about the section
    fluidRow(column(1),
             column(10,
                    #h3(section$Name),
                     strong(section$Label)
                    ),
             column(1)),

    # render all fields within this section
    lapply(section$Questions, customField),
    
    # a break line after each section
    fluidRow(hr())
  )
  
}

customField <- function(ind){
  # is the input is not question, it is assumed that it is some quidance text in between the items
  if(ind$Type == "text"){
    
    # the quidance text can itself be conditional
    if(is.null(ind$Depends)){
      fluidRow(column(1),
               column(10, br(), strong(ind$Label)),
               column(1))
    } else{
      conditionalPanel(condition = gsub(pattern = "\\.", replacement = "input.", ind$Depends),
                       fluidRow(column(1), 
                                column(10, br(), strong(ind$Label)),
                                column(1))
                       )
    }
  } else { # render questions
    customButton(ind)
  }
}


customButton <- function(ind){
    
  # Always display unconditional items  
  if(is.null(ind$Depends)){
    ind$Depends <- "true"
  } else { # or display depending on the state of the input
    ind$Depends <- gsub(pattern = "\\.", replacement = "input.", ind$Depends)
  }
  
  
  if(ind$Type != "comment"){ # when the item is not a comment, show the button in a 6:4 format (label: button)
    fluidPage( # wrapping into another fluid page makes a slight indentation of the questions from the text fields
    conditionalPanel(condition = ind$Depends,
                     fluidRow(column(1),
                              column(6, br(), ind$Label), # this makes the buttons appear horizonally aligned
                              column(4, switchButtons(ind)), # create a standard shiny button
                              column(1)
                     )
    )
    )
  } else{ # when the item is a comment, show the commentary section as a standard textArea stretched over the width of the panel
    #fluidPage(
      conditionalPanel(condition = ind$Depends,
                       fluidRow(column(1),
                                column(10, br(), strong(ind$Label), br(),
                                       tags$style(type = "text/css", "textarea {width:80%}"),
                                       tags$textarea(id = ind$Name, placeholder = ind$AnswerType,
                                                     rows = 5, class = "form-control")),
                                column(1)))
    #)
  }
}


switchButtons <- function(ind){
  # if the AnswerType is specified in the answerList object (from .json), the button options should be rendered from 
  # those options
  # otherwise, the AnswerType is passed directly to the options
  if(ind$AnswerType %in% names(answerList)){
    answers <- answerList[[ind$AnswerType]]
  } else{ 
    answers <- ind$AnswerType
  }
  
  # switch between different input types
  switch (ind$Type,
    "select" = pickerInput (inputId = ind$Name, label = "", choices = c("", answers),
                            selected = NULL, multiple = FALSE),
    "radio"  = radioButtons(inputId = ind$Name, label = "", choices = answers, selected = 0,
                            inline = TRUE),
    "textInput"   = textInput   (inputId = ind$Name, label = ind$Label, value = ind$AnswerType),
    "textArea" = textAreaInput(inputId = ind$Name, label = "", placeholder =  answers, rows = 6)
  )
}
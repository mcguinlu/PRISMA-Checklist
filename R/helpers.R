## Functions which help us to create some structure of the shiny app
## and defines the buttons which appear horizontal

renderSection <- function(section){
  # creates a tab ( can be changed to a fluidrow if we do not want tabs)
  
  tabPanel(title = section$Name,
           value = section$Value,
           icon  = tags$i(class = paste0("icon", section$Value, " fa fa-eye")),
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

customField <- function(ind) {
  # Renders domain headings (e.g Title), and special case of the Abstract item
  if (ind$Type == "text") {
    if (ind$Domain == "Abstract") {
      # Handles special case for the Abstract item of the main checklist, which
      # points you towards the Abstract checklist
      # Special case is indicated by Domain == Abstract
      fluidPage(fluidRow(
        column(2, br(), strong(ind$Domain)),
        column(1, br(), strong(ind$Qnumber), align = "middle"),
        column(4, br(), ind$Label),
        column(4, br(), actionLink("gotoAb", "Go to PRISMA-A"), align = "middle"),
        column(1)
      ))
    } else {
      # Adds row with 
      fluidPage(fluidRow(
        column(2, strong(ind$Domain)),
        column(4, strong(ind$Label)),
        column(2),
        column(4)
      ))
      
    }
    
  } else {
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
  
  
  if(ind$Type != "break"){ # when the item is not a comment, show the button in a 6:4 format (label: button)
    
    fluidPage( # wrapping into another fluid page makes a slight indentation of the questions from the text fields
                     fluidRow(column(2, br(), strong(ind$Domain)),
                              column(1, br(), strong(ind$Qnumber), align = "middle"),
                              column(4, br(), ind$Label, 
                                     a(ind$href, href = ind$href, target = "_blank"),
                                     ind$LabelEnd), # this makes the buttons appear horizonally aligned
                              column(4, switchButtons(ind), align = "middle"), #, create a standard shiny button
                              column(1, br(), # adds exclamation circle next to the item
                                     tags$div(
                                       id = paste0("div", ind$Name, "Checker"),
                                       title = "This question needs to be answered.",
                                       tags$i(id = paste0(ind$Name, "Checker"),
                                              class = 'fa fa-exclamation-circle')
                                       )
                                     )
    )
    )
  
  } else{
    # If the Type is a break, produce a horizontal rule.
    # Used to seperate the sections
    hr()
  }
}


switchButtons <- function(ind, type){
  # if the AnswerType is specified in the answerList object (from .json), the button options should be rendered from 
  # those options
  # otherwise, the AnswerType is passed directly to the options
  if(ind$AnswerType %in% names(answerList)){
    answers <- answerList[[ind$AnswerType]]
  } else{ 
    answers <- ind$AnswerType
  }
  
  # switch between different input types
  switch (ind$AnswerType,
    "YesNo"     = radioButtons(inputId = paste0(ind$Name), label = "", choices = answers, selected = character(0),
                               inline = TRUE),
    "Text" = textInput(inputId = paste0(ind$Name), label = "", value = "", placeholder = "Location where item is reported" ),
  )
}


getItemList <- function(sectionsList, all = TRUE){
  items <- unlist(sapply(sectionsList, function(section) sapply(section$Questions, function(item) item$Name)))
  
  if(all){
    return(items)
  } else {
    return(items[grep("ind", items)])
  }
}
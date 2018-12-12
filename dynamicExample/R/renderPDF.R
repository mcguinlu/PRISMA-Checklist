## functions to generate the rmd file--
composeRmd <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL){
  # returns a string

  # First, we create the YAML header of the rmd file
  headYaml <- 
"---
title: '&studyTitle'
subtitle: 'Transparency Report'
author: '&authorNames'
date: '&date'
output: pdf_document
---

Corresponding author's email address: [&correspondingEmail](&correspondingEmail)

Link to Project Repository: [&linkToRepository](&linkToRepository)
"
  
  # and fill the header with information taken from the question in the head
  date <- format(Sys.time(), '%d %B, %Y')
  headYaml <- gsub("&authorNames",        answers$authorNames,        headYaml)
  headYaml <- gsub("&studyTitle",         answers$studyTitle,         headYaml)
  headYaml <- gsub("&correspondingEmail", answers$correspondingEmail, headYaml)
  headYaml <- gsub("&linkToRepository",   answers$linkToRepository,   headYaml)
  headYaml <- gsub("&date",               date,                       headYaml)
  
  # We create sections of the rmd file
  sections <- sapply(sectionsList, composeSections, answers = answers)
  
  # combine everything together
  rmd <- paste(c(headYaml, sections), collapse = "\n")
  
  
  rmd
}

composeSections <- function(section, answers = NULL){
  # Creating a section
  
  # First, we sketch the outline of the section
  body <- 
"

\\section{&SectionName}

**&SectionLabel**


&Questions

\\newpage
"
  
  # Generate the individual questions and their answers
  questions <- sapply(section$Questions, composeQuestions, answers = answers)
  
  # Fill in the section Name, the text, and the generated questions
  body <- gsub("&SectionName", section$Name, body)
  body <- gsub("&SectionLabel", section$Label, body)
  body <- gsub("&Questions", paste(questions, collapse = " \n "), body)
  
  # Escape latex backslashes from the question generation
  body <- gsub("&escape&", "\\", body, fixed = TRUE) # double escaping screws latex code

  body
}

composeQuestions <- function(question, answers = answers){
  # This function takes a question (from the .json file), checks whether it is supposed to be shown
  # (based on the answers and the conditional statements from .json)
  # If it is supposed to be shown, the question and its answer is printed
  
  show <- TRUE
  
  # check whether the section is suppposed to be shown
  if(!is.null(question$Depends)){
    show <- gsub(".ind_", "answers$ind_", question$Depends)
    show <- eval(parse(text = show))
  }
  
  # if the question is not shown, return empty space (will screw up the appearance of the rmd file, but not the pdf)
  if(!show){
    return("")
  }
  
  # if the question is "Explain" -- additional comment following some question, render it as a comment
  if(question$Label == "Explain") {
    question$Type <- "comment"
  }
  
  # make answers bold, but if it is a comment, show it as a quote:
  if( !(question$Type %in% c("comment", "text"))){
    answer <- paste0(" &escape&textbf{", answers[[question$Name]], "} ")
  } else if(question$Type == "comment"){
    answer <- ifelse(answers[[question$Name]] == "", "No comments.", answers[[question$Name]]) # If the comment box is empty
    answer <- paste0(" &escape&begin{quote}\n", answer, "\n&escape&end{quote} ")
  }
  
  
  # differnt types of output
  if( !(question$Type %in% c("comment", "text")) ){ # a numbered list with answer at the right side
    body <- paste0(" ", question$Label, " &escape&hfill ", answer, " \n ")
  } else if(question$Type == "comment"){ # a block of quote
    
    if(question$Label == "Explain"){ # it it is additional question, do not show the label
      body <- paste0(" \n \n ", answer, " \n ")
    } else{ # if it is a comment, show a the label
      body <- paste0(" \n \n **", question$Label, "** ", " \n \n ", answer, " \n ")
    }
  } else if(question$Type == "text"){ # if it's not a question (does not have an answer), just print the text in bold
    body <- paste0(" **", question$Label, "** ", " \n ")
  } else {
    body <- ""
  }

  return(body)
}
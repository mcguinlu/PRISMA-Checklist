## functions to generate the rmd file--
composeRmd <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL, save.as = "pdf"){
  # returns a string

  switch (save.as,
    "pdf" = composePDF(answers = answers, sectionsList = sectionsList, headList = headList),
    "html" = composeHTML(answers = answers, sectionsList = sectionsList, headList = headList),
    "word" = composeDOC(answers = answers, sectionsList = sectionsList, headList = headList),
    "rtf" = composeRTF(answers = answers, sectionsList = sectionsList, headList = headList)
  )
}

composeHTML <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL){
  rmd <- composePDF(answers = answers, sectionsList = sectionsList, headList = headList)
  rmd <- gsub("pdf_document", "html_document", rmd)
  
  
  rmd <- gsub("\\newpage", "***", rmd, fixed = TRUE) # change pagebreak to line separation
  rmd <- gsub("\\hfill  \\textbf{", " **", rmd, fixed = TRUE) # change indentation of answers
  rmd <- gsub("}", "**", rmd, fixed = TRUE)
  
  
  return(rmd)
}

composeDOC <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL){
  rmd <- composePDF(answers = answers, sectionsList = sectionsList, headList = headList)
  rmd <- gsub("pdf_document", "word_document", rmd)
  
  rmd <- gsub("\\newpage", "***", rmd, fixed = TRUE) # change pagebreak to line separation
  rmd <- gsub("\\hfill  \\textbf{", " **", rmd, fixed = TRUE) # change indentation of answers
  rmd <- gsub("}", "**", rmd, fixed = TRUE)
  
  return(rmd)
}

composeRTF <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL){
  rmd <- composePDF(answers = answers, sectionsList = sectionsList, headList = headList)
  rmd <- gsub("pdf_document", "rtf_document", rmd)
  
  rmd <- gsub("\\newpage", "***", rmd, fixed = TRUE) # change pagebreak to line separation
  rmd <- gsub("\\hfill  \\textbf{", " **", rmd, fixed = TRUE) # change indentation of answers
  rmd <- gsub("}", "**", rmd, fixed = TRUE)
  
  return(rmd)
}


## functions to generate the rmd file--
composePDF <- function(answers = NULL, sectionsList = NULL, headList = NULL, answerList = NULL){
  # returns a string
  
  # First, we create the YAML header of the rmd file (be carefull about indentation, can automatically generate another header which screws everything)
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
  answers$studyTitle <- ifelse(answers$studyTitle == "", "Untitled", answers$studyTitle)
  
  headYaml <- gsub("&studyTitle",         answers$studyTitle,         headYaml)
  headYaml <- gsub("&authorNames",        answers$authorNames,        headYaml)
  headYaml <- gsub("&correspondingEmail", answers$correspondingEmail, headYaml)
  headYaml <- gsub("&linkToRepository",   answers$linkToRepository,   headYaml)
  headYaml <- gsub("&date",               date,                       headYaml)
  
  # fill in answers with "not answered" - important for generating the files
  bundleQuestions <- getItemList(sectionsList)
  not.answered <- !bundleQuestions %in% names(answers)
  answers[bundleQuestions[not.answered]] <- "Not answered"
  
  # We create sections of the rmd file
  sections <- sapply(sectionsList, composeSections, answers = answers)
  
  references <- renderReferences()
  # combine everything together
  rmd <- paste(c(headYaml, sections, references), collapse = "\n")
  
  
  rmd
}

composeSections <- function(section, answers = NULL){
  # Creating a section
  # \\section{&SectionName}
  # First, we sketch the outline of the section
  body <- 
"

## &SectionName

**&SectionLabel**


&Questions

\\newpage
"

  # Generate the individual questions and their answers
  questions <- sapply(section$Questions, composeQuestions, answers = answers)
  
  # Fill in the section Name, the text, and the generated questions
  body <- gsub("&SectionName", section$Name, body)
  body <- gsub("&SectionLabel", section$Label, body)
  body <- gsub("&Questions", paste(questions, collapse = " \n"), body)
  
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
  
  body <- 
"
&Label &Answer
"
  
  
  # if the question is "Explain" -- additional comment following some question, render it as a comment
  if(question$Label == "Explain") {
    question$Type <- "comment"
  }
  
  # make answers bold, but if it is a comment, show it as a quote:
  if( !(question$Type %in% c("comment", "text"))){
    answer <- paste0(" &escape&textbf{", answers[[question$Name]], "} ")
  } else if(question$Type == "comment"){
    answer <- ifelse(answers[[question$Name]] == "", "No comments.", answers[[question$Name]]) # If the comment box is empty
    answer <- paste0("\n\n> ", answer)
  } else{
    answer <- ""
  }
  
  # layout Labels:
  if( !(question$Type %in% c("comment", "text"))){
    label <- paste0(" ", question$Label, " &escape&hfill")
  } else if(question$Type == "text" || (question$Type == "comment" && question$Label != "Explain")){
    label <- paste0("**", question$Label, "**")
  } else{
    label <- ""
  }
  
  body <- gsub("&Label", label, body)
  body <- gsub("&Answer", answer, body)
  # 
  # 
  # # different types of output
  # if( !(question$Type %in% c("comment", "text")) ){ # a numbered list with answer at the right side
  #   body <- paste0(" ", question$Label, " &escape&hfill ", answer, " \n ")
  # } else if(question$Type == "comment"){ # a block of quote
  #   
  #   if(question$Label == "Explain"){ # it it is additional question, do not show the label
  #     body <- paste0(" \n \n ", answer, " \n ")
  #   } else{ # if it is a comment, show a the label
  #     body <- paste0(" \n \n **", question$Label, "** ", " \n \n ", answer, " \n ")
  #   }
  # } else if(question$Type == "text"){ # if it's not a question, just print the text in bold
  #   body <- paste0(" **", question$Label, "** ", " \n")
  # } else {
  #   body <- ""
  # }

  return(body)
}

renderReferences <- function(){
  "
## References
 
Aalbersberg, I. J., Appleyard, T., Brookhart, S., Carpenter, T., Clarke, M., Curry, S., ... & Freedman, L. (2018).
Making science transparent by default: Introducing the TOP Statement. [https://osf.io/sm78t](https://osf.io/sm78t)  


Simons, D. J., Shoda, Y., & Lindsay, D. S. (2017). 
Constraints on generality (COG): A proposed addition to all empirical papers.
*Perspectives on Psychological Science*, *12*(6), 1123-1128.
[DOI: 10.1177/1745691617708630](https://doi.org/10.1177/1745691617708630), [https://psyarxiv.com/w9e3r/](https://psyarxiv.com/w9e3r/)
"
}
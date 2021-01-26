isComplete <- function(answers = NULL, sectionsList = NULL, headList = NULL){
  # First, check whether the answers to the header questions are:
  # complete and valid
  
  # If yes, then check all sections
  # This function escapes early, so that whenever even one question is not filled in, it returns FALSE
  # (does not loop over everything, unless everything is valid)
  
  # When the app is being initialized, do not test anything
  # if(length(answers) == 8){
  #   return(FALSE)
  # }
  
  # Check the head and return FALSE if not filled in correctly (check only the name project name and authors)
  # completeHead <- sapply(headList[1:2], function(question){
  #   gsub(" ", "", answers[[question$Name]]) != ""
  # })
  # 
  # if(!all(completeHead)){
  #   return(FALSE)
  # }
  
  # do not require e-mail anymore
  #validEmail <- isValidEmail(answers$correspondingEmail)
  
  #if(!validEmail){
  #  return(FALSE)
  #}
  
  # Check questions and return TRUE if filled in sufficiently
  completeSection <- sapply(sectionsList, function(section){
    # we need to for loop (not lapply) across the questions so that we are able to break early
    # if we encounter a single question that has not been answered
    
    complete <- TRUE
    for(i in seq_along(section$Questions)){
      
      complete <- isCompleteQuestion(section$Questions[[i]], answers)
      
      # if a question is not complete, escape with stored FALSE
      if(!complete){
        break
      }
    }
    
    complete
  })
  
  # Check whether all sections are complete
  #if(!all(completeSection)){
  return(completeSection)
  #}
  
  
  # the report is complete if and only if all shown questions are filled in appropriately
  #return(TRUE)
}

isCompleteQuestion <- function(question, answers){
  
  # if it's not even a question (comment, text), skip to another question
  # if a comment is supposed to be mandatory, change Type = 'comment' to Type = 'textArea' in .json
  if(question$Type %in% c("comment", "text", "break")){
    return(TRUE)
  }
  
  # the current question is supposed to be shown, and so it needs to be in answers; otherwise, the question is not completed
  if(is.null(answers[[question$Name]])){
    return(FALSE)
  } 
  
  # and the answer should not be an empty string
  if(gsub(" ", "", answers[[question$Name]]) == ""){
    return(FALSE)
  }
  
  # if all checks out (question is shown and answered), then it is complete
  return(TRUE)
}
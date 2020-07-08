library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shinyFeedback)
library(shinyanimate)
library(shinythemes)
library(shinycssloaders)
library(bsplus)
library(jsonlite)
library(RCurl) # for checking whether url.exists
library(digest)
library(knitr)
library(markdown)
library(dplyr)
library(flextable)
library(tidyr)
library(officer)

source("R/helpers.R")
source("R/validation.R")
source("R/renderPDF.R")

# First, we load the .json, which defines the structure of the application
questions <- jsonlite::read_json(path = "data/questions.json")
answers <- jsonlite::read_json(path = "data/answers.json")

df_m <- data.table::rbindlist(questions$Sections$`PRISMA MAIN CHECKLIST`$Questions, fill=TRUE) %>%
  filter(Type != "break") %>%
  select(Domain, Qnumber, Label) %>%
  mutate(Domain = ifelse(Domain == "",NA, Domain)) %>%
  fill(Domain) %>%
  mutate(seq = seq(1:nrow(.)))

colnames(df_m)[2] <- "No"

df_a <- data.table::rbindlist(questions$Sections$ABSTRACT$Questions, fill=TRUE) %>%
  filter(Type != "break") %>%
  select(Domain, Qnumber, Label) %>%
  mutate(Domain = ifelse(Domain == "",NA, Domain)) %>%
  fill(Domain) %>%
  mutate(seq = seq(1:nrow(.)))

colnames(df_a)[2] <- "No"

headList <- questions$Head
sectionsList <- questions$Sections
answerList <- answers$Answers

# Name the questions (ind_1 ... ind_n) - this slightly reduces the tedious filling in of question numbers in .json
# and reduces the likelihood of a manual mistake.
sectionsList <- lapply(sectionsList, function(Sec){
  Sec$Questions <- lapply(Sec$Questions,  function(x,arg1) {

    if(is.null(x$Name)){ # create names for questions in format ind_number of question
      x <- c(x, Name = paste0("ind_",arg1,"_", x$Qnumber))

    }

    x
  }, arg1 = Sec$Indicator)

  Sec$Value <- digest::digest(Sec$Name)

  Sec
})


# write html code for sections prior opening the app
sectionsHTML <- lapply(sectionsList, renderSection)
names(sectionsHTML) <- NULL
sectionsHTML <- do.call(tabsetPanel, c(sectionsHTML, id = "sections"))

# write html code for heading
headHTML <- lapply(headList, switchButtons)

# PACKAGE CALLS -----------------------------------------------------------

library(shiny) # Web Application Framework for R
library(shinyWidgets) # Custom Inputs Widgets for Shiny
library(shinyjs) # Easily Improve the User Experience of Your Shiny Apps in Seconds
library(shinyBS) # Twitter Bootstrap Components for Shiny
library(shinyFeedback) # Display User Feedback in Shiny Apps
library(shinyanimate) # Animation for 'shiny' Elements
library(shinythemes) # Themes for Shiny
library(shinycssloaders) # Add CSS Loading Animations to 'shiny' Outputs
library(bsplus) # Adds Functionality to the R Markdown + Shiny Bootstrap Framework
library(jsonlite) # A Robust, High Performance JSON Parser and Generator for R
library(RCurl) # General Network (HTTP/FTP/...) Client Interface for R 
library(digest) # Create Compact Hash Digests of R Objects
library(knitr) # A General-Purpose Package for Dynamic Report Generation in R
library(markdown) # Render Markdown with the C Library 'Sundown'
library(dplyr) # A Grammar of Data Manipulation
library(flextable) # Functions for Tabular Reporting
library(tidyr) # Tidy Messy Data
library(officer) # Manipulation of Microsoft Word and PowerPoint Documents
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax


# SOURCE LOCAL SCRIPTS ----------------------------------------------------

source("R/helpers.R")
source("R/validation.R")

# PRELIMINARY SET-UP ------------------------------------------------------

# First, we load the .json, which defines the structure of the application
questions <- jsonlite::read_json(path = "data/questions.json")
answers <- jsonlite::read_json(path = "data/answers.json")

# Convert Main questions from JSON to dataframe for use in the exports
df_m <- data.table::rbindlist(questions$Sections$`PRISMA MAIN CHECKLIST`$Questions, fill = TRUE) %>%
  filter(Type != "break") %>%
  select(Domain, Qnumber, Label) %>%
  mutate(Domain = ifelse(Domain == "",NA, Domain)) %>%
  mutate(seq = seq(1:nrow(.)))

colnames(df_m)[2] <- "No"

# Convert Abstract questions from JSON to dataframe for use in the exports
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

# Name the questions (ind_m_1 ... ind_m_n) 
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


# Produce the HTML code for the checklists prior to opening app
sectionsHTML <- lapply(sectionsList, renderSection)
names(sectionsHTML) <- NULL
sectionsHTML <- do.call(tabsetPanel, c(sectionsHTML, id = "sections"))

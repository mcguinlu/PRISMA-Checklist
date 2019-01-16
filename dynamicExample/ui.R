#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #### Load various .js files enabling interactivity ----
  useShinyjs(), # this is for enabling/disabling buttons from shinyjs
  useShinyFeedback(), # enabling/disabling feedback from shinyFeedback
  withAnim(), # enable animations from shinyanimate
  # showing icons for required items
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        src = "js/toggleChecker.js"
      )
    )
  ),
  # hack to load font-awesome when Shiny loads
  tags$div(
    style = "display: none;",
    shiny::icon("user")
  ),
  
  
  #### Application outline ----
  # Application title
  headerPanel(column(12, "TRANSPARENCY REPORT", align = "center"), windowTitle = "Transparency Report"),

  br(), br(), br(), br(), br(),
  
  # The header (basic information about the paper and authors)
  fluidRow(
    column(1),
    column(10,
      wellPanel(strong(textOutput("currentTime")), br(), uiOutput("head"))),
    column(1)
  ),
  
  # Show initial instructions:
  fluidRow(
    column(1),
    column(10, 
           h4("Please select an answer for each item below. If you want to elaborate on your answers, you can do so in the comment box that follows each section.")
           ),
    column(1)
  ),
  
  br(), br(),
  # Show questions
  uiOutput("questions"),

  ##### Report menu (downloading) ----
  absolutePanel(
    dropdown(
      h4("Generate Report"),
      pickerInput(inputId = "save.as", label = "Format", 
                  choices = c("pdf", "html", "word", "rtf"), 
                  multiple = FALSE, width = 'auto', inline = FALSE),
      div(style = "display:inline-block",
        actionBttn(inputId = "preview", label = "Preview", icon = icon("eye"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE),# br(), br(), 
        actionBttn(inputId = "showcode", label = "Show code", icon = icon("code"),
                   style = "simple",
                   color = "primary",
                   size = "xs",
                   no_outline = FALSE)
        ), br(), br(),
      downloadButton('report', 'Download report', class = "downbutt"),
      
      icon = icon("file-alt"), up = TRUE, tooltip = tooltipOptions(title = "Report"), style = "unite",
      size = "lg", inputId = "generatereport", width = "100%"),
    
    bottom = "2.5%", fixed = TRUE, width = "25%"),
  
  #tags$head(tags$style(".downbutt{background-color:#add8e6;} .downbutt{color: #337ab7;}")),
  
  shinyBS::bsModal(id = "previewer", title = "Preview", trigger = "preview", size = "large",
                   uiOutput("generatePreview")),
  
  shinyBS::bsModal(id = "codeshower", title = "Code", trigger = "showcode", size = "large",
                   verbatimTextOutput("code")),
  
  shinyBS::bsTooltip(id = "report",
                     title = "A report can be downloaded after all questions in each section have been answered.",
                     # Please, respond to all displayed items to download the pdf report (comments are optional).
                     trigger = "manual",
                     placement = "right"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
  br(),
  fluidRow(column(1),
           column(2, actionButton("previousButton", "Go to previous section", icon = icon("arrow-circle-left"))),
           column(6),
           column(2, actionButton("nextButton", "Go to next section", icon = icon("arrow-circle-right"))),
           column(1)
           ),
  br()
  
  # temporary (for debugging): showing the current status of the answers
  # ,br(),
  # verbatimTextOutput("answers")
  )
)

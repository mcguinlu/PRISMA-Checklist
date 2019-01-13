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
  useShinyjs(), # this is for enabling/disabling buttons
  useShinyFeedback(),
  
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
           h4("Please select one answer for each item regarding your study.
If you want to elaborate on your answers, you can do so in the comment box that follows each section.")
           ),
    column(1)
  ),
  
  br(), br(),
  # Show questions
  uiOutput("questions"),

  # The download button with a tooltip
  br(),
  fluidRow(column(1), column(2, downloadButton("report", "Generate Report")), column(9)),
  # fluidRow(
  #   column(1),
  #   column(11,
  #   div(id = "display",
  #       div(id="downloadButtonMouseCatcher",
  #           style="position:absolute; z-index: 1; top: 0px; bottom: 0px; left: 0px; right: 0px;")
  #       ),
  #       downloadButton("report", "Generate Report")
  #   )
  # ),
  
  shinyBS::bsTooltip(id = "report",
                     title = "A pdf report can be generated after all questions in each section have been answered.",
                     # Please, respond to all displayed items to download the pdf report (comments are optional).
                     trigger = "manual",
                     placement = "bottom"),
  uiOutput("trigger") # this trigger displays or hides the explaining tooltip
  
  # temporary (for debugging): showing the current status of the answers
  # br(),
  # verbatimTextOutput("answers")
  )
)

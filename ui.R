library(shiny)

shinyUI(fluidPage(title = "",
                  theme = shinytheme("yeti"),
  #### Load various .js files enabling interactivity ----
  useShinyjs(), # this is for enabling/disabling buttons from shinyjs
  useShinyFeedback(), # enabling/disabling feedback from shinyFeedback
  withAnim(), # enable animations from shinyanimate

  # showing icons for required items
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$script(
        src = "js/toggleChecker.js"
      ),
      shiny::tags$script(
        src = "js/toggleSectionIcon.js"
      ),
      shiny::includeHTML("google-analytics.html"),
    )
  ),
  # hack to load font-awesome when Shiny loads
  tags$div(
    style = "display: none;",
    shiny::icon("user")
  ),

  #### Application outline ----
  # Application title
  headerPanel(column(12, "PRISMA 2020 Checklist", align = "center"),
              windowTitle = "PRISMA 2020 Checklist"),

  br(),
  
  # uiOutput("answers"),
  

  # Show initial instructions:
  fluidRow(
    column(1),
    column(10,
           includeMarkdown("www/doc/briefIntro.Rmd"), br(),  actionButton("fill","Complete with sample answers"),

           ),
    column(1)
  ),

  br(), br(),
  tags$div(id = "scrollAnchor"), # for scrolling up

  # SHow questions
  fluidRow(
    column(1),
    column(2,align="left",
  actionButton(inputId = "triggerIntro", label = "Additional information", icon = icon("info-circle"))
    ),
    column(6),
  column(2,align="right",
         dropdown(animate = FALSE,
           h4("Generate & Download Report"),
           pickerInput(inputId = "save.as", label = "Format",
                       choices = c("Word",
                                   "PDF"
                                   #"html",
                                   #"rtf"
                                   ),
                       multiple = FALSE, selected = "Word", width = 'auto', inline = FALSE),
           br(),
           downloadButton('report', 'Download Main + Abstract'),
           br(),
           br(),
           downloadButton('report_main', 'Download Main only'),
           br(),
           br(),
           downloadButton('report_abs', 'Download Abstract only'),
           

           icon = icon("file-alt"), up = FALSE, label = "Generate Report",
           size = "default", inputId = "generatereport")
  ),
  column(1)
  ),
  
  br(),
  
  fluidRow(
    column(1),
    column(10,
  sectionsHTML
    ),
    column(1)
  ),

br(), br(),

  # Show tooltip which says that the download is not ready
  shinyBS::bsTooltip(id = "generatereport",
                     title = "A report can be downloaded after all questions in each section have been answered.",
                     trigger = "manual",
                     placement = "top"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip

  # info modal
  shinyBS::bsModal(id = "intro", title = "About", trigger = "triggerIntro", size = "large",
                   includeMarkdown("www/doc/helpText.Rmd"),
                   br(),
                   tags$a(tags$img(src = "img/GitHub-Mark-32px.png"), 
                          href = "https://github.com/mcguinlu/PRISMA-Checklist", 
                          target = "_blank"))


  )
)

library(shiny)

shinyUI(fluidPage(title = "",
                  theme = shinytheme("cerulean"),
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
        src = "js/toggleCheckerColor.js"
      ),
      shiny::tags$script(
        src = "js/toggleSectionIcon.js"
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
  headerPanel(column(12, "PRISMA 2020 Checklist", align = "center"),
              windowTitle = "PRISMA 2020 Checklist"),

  br(),
  
  tableOutput("answers"),
  
  actionButton("fill","Fill"),
  
  # Show initial instructions:
  fluidRow(
    column(1),
    column(10,
           includeMarkdown("www/doc/briefIntro.Rmd")
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
                       choices = c("pdf", "html", "word", "rtf"),
                       multiple = FALSE, width = 'auto', inline = FALSE),
           div(style = "display:inline-block",
               actionBttn(inputId = "showcode", label = "Show code", icon = icon("code"),
                          style = "simple",
                          color = "primary",
                          size = "xs",
                          no_outline = FALSE)
           ), br(), br(),
           downloadButton('report', 'Download', class = "downbutt"),
           
           icon = icon("file-alt"), up = FALSE, label = "Generate Report",
           size = "default", inputId = "generatereport")  ),
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

  # Switching between sections
  # fluidRow(column(2),
  #          column(2, align = "center",
  #                 actionButton("previousButton", "Go to previous section", icon = icon("arrow-circle-left"))),
  #          column(4),
  #          column(2, align = "center",
  #                 actionButton("nextButton", "Go to next section", icon = icon("arrow-circle-right"))),
  #          column(2)
  # ),
  br(), br(),

  # Open window for a code
  shinyBS::bsModal(id = "codeshower", title = "Code", trigger = "showcode", size = "large",
                   shinycssloaders::withSpinner(verbatimTextOutput("code"))),

  # Show tooltip which says that the download is not ready
  shinyBS::bsTooltip(id = "report",
                     title = "A report can be downloaded after all questions in each section have been answered.",
                     trigger = "manual",
                     placement = "bottom"),
  uiOutput("trigger"), # this trigger displays or hides the explaining tooltip
  br(), br(),

  # info modal
  shinyBS::bsModal(id = "intro", title = "About", trigger = "triggerIntro", size = "large",
                   includeMarkdown("www/doc/helpText.Rmd"),
                   br(),
                   tags$a(tags$img(src = "img/GitHub-Mark-32px.png"), 
                          href = "https://github.com/mcguinlu/PRISMA-Checklist", 
                          target = "_blank"))


  )
)

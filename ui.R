shinyUI(
  fluidPage(
    title = "",
    theme = shinytheme("yeti"),
    useShinyjs(),
    useShinyFeedback(),
    withAnim(),

    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "js/toggleChecker.js"),
        shiny::tags$script(src = "js/toggleSectionIcon.js"),
        shiny::includeHTML("google-analytics.html"),
      )
    ),
    
    tags$div(style = "display: none;",
             shiny::icon("user")),
    
    headerPanel(
      column(12, "PRISMA 2020 Checklist", align = "center"),
      windowTitle = "PRISMA 2020 Checklist"
    ),
    
    br(),

    fluidRow(
      column(1),
      column(
        10,
        includeMarkdown("www/doc/briefIntro.Rmd"),
        br(),
        actionButton("fill", "Complete with sample answers"),
        
      ),
      column(1)
    ),
    
    br(),
    br(),
    tags$div(id = "scrollAnchor"),

    fluidRow(
      column(1),
      column(
        2,
        align = "left",
        actionButton(
          inputId = "triggerIntro",
          label = "About & Citation Info",
          icon = icon("info-circle")
        )
      ),
      column(6),
      column(
        2,
        align = "center",
        dropdown(right = FALSE,
          animate = FALSE,
          h4("Generate & Download Report"),
          selectInput(
            inputId = "report_type",
            label = "Report type",
            choices = c("Main + Abstract",
                        "Main only" = "_main",
                        "Abstract only" = "_abs"),
            multiple = FALSE,
            selected = "Main + Abstract",
            width = "auto"
          ),
          
          selectInput(
            inputId = "format",
            label = "Format",
            choices = c("Word" = "word",
                        "PDF"),
                        multiple = FALSE,
                        selected = "Word",
                        width = "auto"
            ),
          selectInput(
            inputId = "orient",
            label = "Orientation",
            choices = c("Portrait",
                        "Landscape"),
            multiple = FALSE,
            selected = "Portrait",
            width = 'auto'
          ),
          br(),
          
            downloadButton('report','Download Checklist'),
          helpText("The \"Download Checklist\" button will only become active once you have responsed to all relevant items."),
            icon = icon("file-alt"),
            up = FALSE,
            label = "Generate Report",
            size = "default",
            inputId = "generatereport"

          )
        ),
        column(1)
      ),
      
      br(),
      
      fluidRow(column(1),
               column(10,
                      sectionsHTML),
               column(1)),
      
      br(),
      br(),
      
      shinyBS::bsTooltip(
        id = "generatereport",
        title = "A report can be downloaded after all questions in each section have been answered.",
        trigger = "manual",
        placement = "top"
      ),
      uiOutput("trigger"),
      
      shinyBS::bsModal(
        id = "intro",
        title = "About & Citation Info",
        trigger = "triggerIntro",
        size = "large",
        h3(strong("Citation Information")),
        includeMarkdown("www/doc/helpText.Rmd"),
        br(),
        div(
        downloadButton("downloadbib", "Download citation (.bib)"),
        downloadButton("downloadris", "Download citation (.ris)"), style="text-align: center;"),
        br(),
        hr(),
        h3(strong("Found a bug?")),
        p(
          "Please ",
          a(href = "mailto:luke.mcguinness@bristol.ac.uk?cc=matthew.page@monash.edu&Subject=PRISMA2020%20Checklist%20Query", "email us!")
        ),
        br(),
        hr(),
        h3(strong("About")),
        p(
          "This application was developed by ",
          a(href = "https://www.lukemcguinness.com/", "Luke McGuinness,"),
          " adapted from a similar application created for the",
          a(href = "https://github.com/BalazsAczel/TransparencyChecklist", "Transparency Checklist."),
          "The full source code for this application is available via the ",
          a(href = "https://github.com/mcguinlu/PRISMA-Checklist", "GitHub repository.")
        ),
        br(),
        
        
        div(img(
          tags$a(
            tags$img(src = "img/GitHub-Mark-32px.png"),
            href = "https://github.com/mcguinlu/PRISMA-Checklist",
            target = "_blank"
          )
        ), style = "text-align: center;")

      )
      
      
    )
  )
  
library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    title = 'Radio buttons in a table',
    DT::dataTableOutput('foo'),
    verbatimTextOutput('sel')
  ),
  server = function(input, output, session) {
    m = matrix(
      as.character(1:3), nrow = 2, ncol = 3, byrow = TRUE,
      dimnames = list(c("Study Protocol", "Analysis plan"), c("Yes", "No", "N/A"))
    )
    for (i in seq_len(nrow(m))) {
      m[i, ] = sprintf(
        '<input type="radio" name="%s" value="%s"/>',
       rownames(m)[i],  m[i, ]
      )
    }
   # rownames(m)[1] = '111'   # I've changed here !!!
    m
    output$foo = DT::renderDataTable(
      m, escape = FALSE, selection = 'none', server = FALSE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS("table.rows().every(function(i, tab, row) {
                          var $this = $(this.node());
                          $this.attr('id', this.data()[0]);
                          $this.addClass('shiny-input-radiogroup');
    });
                          Shiny.unbindAll(table.table().node());
                          Shiny.bindAll(table.table().node());")
    )
    output$sel = renderPrint({
    str(sapply(rownames(m), function(i) input[[i]]))
   })
  }
)

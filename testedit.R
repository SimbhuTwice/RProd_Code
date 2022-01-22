library(shiny)
library(DT)
library(shinydashboard)
library(scales)
library(flextable)
library(tidyverse)
library(data.table)
library(gt)
library(DT)
library(gtExtras)
library(shinythemes)
library(janitor)
library(lubridate)


callback <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "$.contextMenu({",
  "  selector: '#' + id + ' td.factor input[type=text]',",
  "  trigger: 'hover',",
  "  build: function($trigger, e){",
  "    var levels = $trigger.parent().data('levels');",
  "    if(levels === undefined){",
  "      var colindex = table.cell($trigger.parent()[0]).index().column;",
  "      levels = table.column(colindex).data().unique();",
  "    }",
  "    var options = levels.reduce(function(result, item, index, array){",
  "      result[index] = item;",
  "      return result;",
  "    }, {});",
  "    return {",
  "      autoHide: true,",
  "      items: {",
  "        dropdown: {",
  "          name: 'Edit',",
  "          type: 'select',",
  "          options: options,",
  "          selected: 0",
  "        }",
  "      },",
  "      events: {",
  "        show: function(opts){",
  "          opts.$trigger.off('blur');",
  "        },",
  "        hide: function(opts){",
  "          var $this = this;",
  "          var data = $.contextMenu.getInputValues(opts, $this.data());",
  "          var $input = opts.$trigger;",
  "          $input.val(options[data.dropdown]);",
  "          $input.trigger('change');",
  "        }",
  "      }",
  "    };",
  "  }",
  "});"
)

createdCell <- function(levels){
  if(missing(levels)){
    return("function(td, cellData, rowData, rowIndex, colIndex){}")
  }
  quotedLevels <- toString(sprintf("\"%s\"", levels))
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
    "}"
  )
}


salr<-read_csv("testsales.csv")
####Transformation
salr$Department <- as.factor(salr$Department)
salr$ProductName <- as.factor(salr$ProductName )
salr$Date = Sys.Date() 
###


ui <- fluidPage(
  titlePanel("CRPL Physical Stock Check App"),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
    ),
    tags$script(
      src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
    )
  ),
  DTOutput("dtable"),
  br(),
  DT::dataTableOutput("updated.df")
)


server <- function(input, output){
  output[["dtable"]] <- renderDT({
    datatable(
      salr, editable = TRUE, callback = JS(callback),
      style = "auto",class = 'cell-border stripe',
      filter = 'top',
     escape = TRUE,
      rownames = TRUE,
      caption = 'Cheers Physical Stock Check',
      extensions = 'Buttons',
      options = list( 
       dom = "Blfrtip",
       buttons = 
         list( list(
           extend = "collection",
            buttons = c("csv", "excel"),
            text = "Download",
           paging = TRUE,
           searching = TRUE,
           fixedColumns = TRUE,
           autoWidth = TRUE,
           ordering = TRUE
         ) )


       #  dom = 'Bfrtip',
       # # dom='tB',
       #  buttons = c('csv','pdf'),
        # columnDefs = list(
        #   list(
        #     targets = 1,
        #     className = "factor",
        #     createdCell = JS(createdCell(c(levels(salr$Department), "another level")))
        #   ),
        #   list(
        #     targets = 2,
        #     className = "factor",
        #     createdCell = JS(createdCell(c(levels(salr$ProductName), "another level")))
        #   )
        # )
      )
    )
  }, server = FALSE)
  
  
  observeEvent(input$cheers.df_data_cell_edit, {
    salr[input$cheers.df_data_cell_edit$row,input$cheers.df_data_cell_edit$col] <<- input$chers.df_data_cell_edit$value
  })
  
  view_fun<-eventReactive(input$viewBtn,{
    if(is.null(input$saveBtn)||input$saveBtn==0)
    {
      returnValue()
    }
    else
    {
      DT::datatable(salr,selection = 'none')
    }
    
  })
  
  
  observeEvent(input$saveBtn,{
    write.csv(salr,'test.csv')
  })
  
  output$updated.df<-renderDataTable({
    view_fun()
  }
  )
}

shinyApp(ui, server)
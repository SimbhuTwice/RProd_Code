

library(shinydashboard)
library(shiny)
library(scales)
library(readxl)
library(rmarkdown)
library(flextable)
library(tidyverse)
library(data.table)
library(gt)
library(DT)
library(gtExtras)
library(shinythemes)
library(janitor)
library(lubridate)
library(emo)
library(reactable) # to make table
library(htmltools) # for html components
library(reactablefmtr) # easier implementation of reactable
library(teamcolors) # for logos
library(janitor) 
library(crosstalk)
library(knitr)



disinf<- read_csv('https://raw.githubusercontent.com/SimbhuTwice/sicare/main/disinf.csv')


disinf_tf <- disinf%>%
  clean_names()%>%
  select(Client=client_name,Category=category,Area=location,Responsible=responsibilty,
         Completed_On=completed_on,Type=service_type,Amount=amount,Payment_Status = payment_status,
         Source=source)%>%
  mutate(Completed_On=dmy(Completed_On))%>%
  arrange(desc(Completed_On))

data <- SharedData$new(disinf_tf)



render <- c(
  "function(data, type, row){",
  "  if(type === 'display'){",
  "    var a = '<a href=\"https://twice.bitrix24.com/company/personal/user/200/tasks/task/view/' + row[1]+'/' + '\">' + data + '</a>';",
  "    return a;",
  "  } else {",
  "    return data;",
  "  }",
  "}"
)





#navbarPage
ui <- navbarPage("SI Care Dashboard",
                 theme = shinythemes::shinytheme("flatly"),  # <--- Specify theme here
                 tabPanel("Daily Summary ",value=1, gt_output(outputId = "raw_data"),height = px(1600),
                          width = px(1600)),
                 tabPanel("Service Details",bscols(
                   
                   # widths = c(3, 9),
                   widths = c(7,3,11),
                   
                   filter_checkbox("paymentstatus", "Filter by Payment Status", data, ~Payment_Status),
                   #filter_select("respo", "Search By Responsible", data, ~Responsible),
                   filter_select("workType", "Search By Work Type", data, ~Type),
                   #h2("SI Care Service Level Details"),
                   reactableOutput("renderedReporttdp")
                 )),
                 tabPanel("Revenue Summary ",value=2, gt_output(outputId = "revenue_data"),height = px(1600),
                          width = px(1600)),
                 tabPanel("Download Data ",                            DT::dataTableOutput("cheers.df_data"),
                          br(),
                          #actionButton("viewBtn","View"),
                          br(),
                         # actionButton("saveBtn","Save"),
                          br())
)


server <- function(input, output) {
  
  gt_tbl1 <-reactive({
    disinf_tf
  })
  
  
  gt_tbl_react <- reactiveVal(NULL)
  gt_tbl_react(gt_tbl1)
  
  output$cheers.df_data<-renderDataTable(
    disinf_tf,selection = 'none', editable = TRUE, 
    rownames = TRUE,
    extensions = 'Buttons',
    
    options = list(
      paging = FALSE,
      searching = TRUE,
      fixedColumns = FALSE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    ),
    
    class = "display"
  )
  
  output$raw_data <-
    render_gt({
      gt_tbl1()%>%
        gt::gt() %>% 
        # tab_row_group(
        #   group = "PestControl",
        #   rows = Type =="Pest Control")%>%
        # tab_row_group(
        #   group = "Fumigation",
        #   rows = Type =="Fumigation") %>% 
        # tab_row_group(
        #   group = "AC Services",
        #   rows = Type =="Ac Work")%>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),   locations = cells_column_labels(everything())
        )%>%
        gt::opt_table_lines()%>%
        gt_theme_538()%>%
        tab_header(title = md("**SICare Service Summary**"),
                   subtitle = paste0("As on ",max(disinf_tf$Completed_On)))%>%
        tab_spanner(columns = vars(Amount:Payment_Status),
                    label = md("**Accounts Details**"))%>%
        gt_add_divider(Client, color = "grey", weight = px(1)) %>% 
        tab_source_note(md("**Data Source**: SI Care")) %>% 
        # trim gives smaller range of colors
        # so the green and purples are not as dark
        gt_color_rows(Amount, palette = "ggsci::blue_material")%>%
        data_color(
          columns = vars(Type),
          colors = scales::col_factor(
            palette = paletteer::paletteer_d(
              n = 3, palette = "colorblindr::OkabeIto"
            ) %>% as.character(),
            domain = NULL
          )
        )%>%
        tab_footnote(
          footnote = md("There are Amounts represented as ***zero*** as that is for the group companies"),
          locations = cells_column_labels(
            columns = Amount
          ))%>%
        tab_footnote(
          footnote = md("Payment Status is ***self*** meaning its done for our group companies"),
          locations = cells_column_labels(
            columns = Payment_Status
          ))%>%
        tab_style(
          style = cell_text(color = "red"),
          locations = cells_body(
            columns = vars(Payment_Status),
            rows = Payment_Status == "Pending"
          )
        )%>%
        tab_style(
          style = cell_text(color = "#7882A4"),
          locations = cells_body(
            columns = vars(Payment_Status),
            rows = Payment_Status == "Self"
          )
        )%>%
        tab_style(
          style = cell_text(color = "#2EB086"),
          locations = cells_body(
            columns = vars(Payment_Status),
            rows = Payment_Status == "Received"
          )
        )%>%
        grand_summary_rows(
          columns = Amount,
          fns = list(
            TOTAL = ~sum(.)
          ),
          formatter = fmt_number,
          decimals = 0
        )
      
    })
  
  ##Revenue
  output$revenue_data <-
    render_gt({
      
      disinf_tf%>%
        group_by(Type)%>%
        summarise(Revenue=sum(Amount))%>%
        arrange(desc(Revenue))%>%
        select(ServiceType=Type,Revenue)%>%
        gt::gt() %>%
        grand_summary_rows(
          columns = Revenue,
          fns = list(
            TotalRevenue = ~sum(.)
          ),
          formatter = fmt_number,
          decimals = 0
        )%>%
        #gt::opt_table_lines()%>%
        gt_plt_dot(column = Revenue, category_column = ServiceType,  max_value = 60000,
                   palette = c("#1C7947", "#90AACB", "#63d64a", "#C56824", "#4fabf7")) %>%
        #gt_theme_538()%>%
        gt_theme_nytimes()%>%
        #gt_theme_nytimes() %>% 
        tab_header(title = md("**SI Care Revenue Summary**"),
                   subtitle = paste0("As on ",max(disinf_tf$Completed_On)))%>%
        # trim gives smaller range of colors
        # so the green and purples are not as dark
        gt_color_rows(Revenue, palette = "ggsci::blue_material")%>%
        cols_width(ServiceType ~ px(300), 2 ~ px(90))%>%
        tab_source_note(md("**Data Source**: SI Care"))
      
    })
  ##Revenue EOF
  output$renderedReporttdp<-   renderReactable({
    tbl<-  
      reactable(data, 
                compact    = FALSE, # for minimum row height
                filterable = FALSE, # for individual column filters
                striped    = TRUE, # banded rows
                searchable = TRUE,
                resizable  = TRUE)
    
    
  })
  
  
}

shinyApp(ui=ui,server=server)

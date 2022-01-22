

library(shinydashboard)
library(shiny)
library(scales)
library(readxl)
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

tdis <- read_csv("TD.csv",skip=2)



tdis <- tdis%>%
  clean_names()

tdis_td <- tdis%>%
  mutate(po_date=dmy(po_date))%>%
  filter(!is.na(po_date))%>%
  select(company,BitrixId = task_no,Material=material_full_description,Qty=po_qty,Customer=customer_name,po_date,Amount=total_amount,remarks)

tdis_ts <- tdis%>%
  mutate(po_date=dmy(po_date))%>%
  filter(!is.na(po_date))%>%
  select(Customer=customer_name,Material=material_full_description,Qty=po_qty,StockInHand = stock_in_hand,Delivered=qty_delivered,OrderPlaced=balance_ordered_placed,po_date,
         Amount=total_amount,Company=company)


tdis_tf <- tdis%>%
  mutate(po_date=dmy(po_date),actualtarget=7)%>%
  filter(!is.na(po_date))%>%
  select(company,task_no,material_full_description,po_qty,actualtarget,customer_name,po_date,
         stock_in_hand,qty_delivered,total_amount,balance_ordered_placed,remarks)%>%
  #mutate(customer_po_date=dmy(customer_po_date))%>%
  mutate(AgeDays=Sys.Date()-po_date,material_full_description=tolower(material_full_description))%>%
  mutate(type = case_when(
    # str_detect(material_full_description,"ups and battery") ~ paste0("ups ", " car-battery"),
    str_detect(material_full_description,"ups") ~ "ups",
    str_detect(material_full_description,"rack") ~ "inbox",
    str_detect(material_full_description,"battery") ~ "car-battery",
    str_detect(material_full_description,"ups|battery") ~ paste0("ups","car-battery"),
    TRUE ~ "car-battery"
  ),
  .after = task_no
  ) %>% 
  mutate(Days_Delayed = AgeDays , .after = task_no)


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

  
  ui <- navbarPage("TDIG Dashboard",
                  theme = shinythemes::shinytheme("flatly"),  # <--- Specify theme here
                  tabPanel("TD Implementation Summary ",value=1, gt_output(outputId = "raw_data"),height = px(1600),
                           width = px(1600)),
                  tabPanel("TD Implementation Details",value=2,DTOutput("DT1")),
                  tabPanel("TD Stocks",value=3,gt_output(outputId = "GT1"),height = px(1600),
                           width = px(1600))
                  
  )
  
  
  server <- function(input, output) {
    
    gt_tbl1 <-reactive({
      tdis_tf %>%
        arrange(desc(Days_Delayed))%>%
        select(customer_name,Days_Delayed,type,total_amount,po_date,material_full_description,
               actualtarget,customer_name,AgeDays,company)
    })
    
    
    gt_tbl_react <- reactiveVal(NULL)
    gt_tbl_react(gt_tbl1)

    output$raw_data <-
      render_gt({
        gt_tbl1()%>%
          gt::gt() %>% 
          tab_row_group(
            group = "TD Infra",
            rows = company =="TDIS") %>% 
          tab_row_group(
            group = "TDGlobal",
            rows = company =="TDG")%>%
          gt_plt_bullet(column =AgeDays , target = actualtarget,colors = c("#E1341E","#1ECBE1")) %>% 
          gt_fa_column(column = type) %>%
          cols_hide(
            columns = c(
              company
            )
          )%>%
          #gt_theme_guardian()%>%
          gt_theme_espn()%>%
          #gt_theme_nytimes() %>% 
          fmt_symbol_first(column = Days_Delayed, suffix = "Days", decimals = 0) %>%
          cols_label(
            actualtarget = html(
              "<span style='color:#1ECBE1;'>ActualTarget</span> vs <span style='color:#E1341E;'>Today</span>"
            )
          ) %>% 
          tab_header(
            title = paste0("TD Implementation Ageing As on - ",Sys.Date()),
            subtitle ="Considering 7 Days as our target date from PO "
          ) %>% 
          #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
          gt_add_divider(type, color = "grey", weight = px(1)) %>% 
          tab_source_note(md("**Data Source**: TDIS")) %>% 
          tab_options(
            table.border.bottom.color = "grey",
            table.width = px(500)
          )%>%
          grand_summary_rows(
            columns = total_amount,
            fns = list(
              TOTAL = ~sum(.)
            ),
            formatter = fmt_number,
            decimals = 0
          ) %>%
          tab_style(
            style = list(
              cell_text(style = "italic"),
              cell_fill(color = "lightblue")
            ),
            locations = cells_grand_summary(
              columns = total_amount,
              rows = 1)
          )
        
      })
    
    output$DT1 <-
      renderDT({
        datatable(tdis_td, rownames = FALSE, 
                      options = list(
                        columnDefs = list(
                          list(targets = 1, render = JS(render)),
                          list(targets = "_all", className = "dt-center")
                        )
                      )
        )
      })
    
    output$GT1 <-
      render_gt({
        tdis_ts%>%
          gt()%>%
          tab_row_group(
            group = "TD Infra",
            rows = Company =="TDIS") %>% 
          tab_row_group(
            group = "TDGlobal",
            rows = Company =="TDG")%>%
          tab_style(
            style = list(
              cell_text(weight = "bold")
            ),   locations = cells_column_labels(everything())
          )%>%
          gt::opt_table_lines()%>%
          gt_theme_538()%>%
          #gt_theme_nytimes() %>% 
          tab_header(title = md("**TD Stock Summary**"),
                     subtitle = paste0("As on ",Sys.Date()))%>%
          tab_spanner(columns = vars(StockInHand:OrderPlaced),
                      label = md("**Stock Details**"))%>%
          # trim gives smaller range of colors
          # so the green and purples are not as dark
          gt_color_rows(Amount, palette = "ggsci::blue_material")%>%
          data_color(
            columns = vars(StockInHand),
            colors = scales::col_numeric(
              palette = c(
                "#FF6464", "#FFE162", "#91C483"),
              domain = NULL)
          )%>%
          tab_footnote(
            footnote = "Stock Available in Inventory.",
            locations = cells_column_labels(
              columns = StockInHand
            ))%>%
          tab_footnote(
            footnote = "Order Placed with Supplier.",
            locations = cells_column_labels(
              columns = OrderPlaced
            ))
        
      })
  }

  shinyApp(ui=ui,server=server)

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

#tdis <- read_csv("TD.csv",skip=1)

#rmarkdown::render("sipl_1.Rmd")


tdis<- read_csv('https://raw.githubusercontent.com/SimbhuTwice/TDGIS/main/TD.csv',skip=1)

tdr <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/TDGIS/main/tdr.csv')

tdp <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/TDGIS/main/tdp.csv')




tdis <- tdis%>%
  clean_names()

tdis_td <- tdis%>%
  filter(!is.na(po_date))%>%
  filter(!is.na(total_amount))%>%
  mutate(po_date=dmy(po_date))%>%
  select(company,BitrixId = task_no,Material=material_full_description,Qty=po_qty,Customer=customer_name,po_date,Amount=total_amount,remarks,status)

tdis_ts <- tdis%>%
  filter(!is.na(po_date))%>%
  filter(!is.na(total_amount))%>%
  mutate(po_date=dmy(po_date))%>%
  select(Customer=customer_name,Material=material_full_description,Qty=po_qty,StockInHand = stock_in_hand,Delivered=qty_delivered,OrderPlaced=balance_ordered_placed,po_date,
         Amount=total_amount,Company=company,status)


tdis_tf <- tdis%>%
  mutate(po_date=dmy(po_date),actualtarget=7)%>%
  filter(!is.na(po_date))%>%
  filter(!is.na(total_amount))%>%
  filter(status!="Delivered")%>%
  select(company,task_no,material_full_description,po_qty,actualtarget,customer_name,po_date,
         stock_in_hand,qty_delivered,total_amount,balance_ordered_placed,remarks,status)%>%
  #mutate(customer_po_date=dmy(customer_po_date))%>%
  #mutate(AgeDays=Sys.Date()-po_date,material_full_description=tolower(material_full_description))%>%
  mutate(material_full_description=tolower(material_full_description))%>%
  mutate(AgeDays = case_when(
    as.numeric(Sys.Date()-po_date)<= 7 ~ 0,
    TRUE ~ as.numeric(Sys.Date()-po_date-7)
  ))%>%
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



tdr_tf<-tdr%>%
  clean_names()%>%
  filter(!is.na(date))%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(name=snakecase::to_upper_camel_case(name))%>%
  mutate(date=dmy(date),due_on=dmy(due_on),name=as.factor(name))%>%
  select(date,due_on,invoice_no,name,bill_value,pending_value,final_value=final_pending_value,company,
         Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days,LastUpdated=lud)%>%
  filter(!str_detect(name,"Npr"))

tdr_tf<-tdr_tf%>%
  left_join(
    tdr_tf%>%
      mutate(final_value=as.numeric(final_value))%>%
      mutate(bill_value=as.numeric(bill_value))%>%
      filter(invoice_no=="ADVANCE")%>%
      group_by(invoice_no,company,name)%>%
      summarise(Advance_Paid = sum(Days_30to60,Days_60to90,Days_GT90),by="name"))

tdr_tf$Advance_Paid <- replace_na(tdr_tf$Advance_Paid,0)

tdr_npr<-tdr%>%
  clean_names()%>%
  filter(!is.na(date))%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(name=snakecase::to_upper_camel_case(name))%>%
  mutate(date=dmy(date),due_on=dmy(due_on),name=as.factor(name))%>%
  select(date,due_on,invoice_no,name,bill_value,pending_value,final_value=final_pending_value,company,
         Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days,LastUpdated=lud)%>%
  filter(str_detect(name,"Npr"))


tdr_tf%>%
  group_by(name,company)%>%
  summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),billdate=min(date))%>%
  mutate(Actual_Due=as.numeric(due_on-billdate))


tdr_tf2<-tdr_tf%>%
  mutate(LastUpdated=dmy(LastUpdated))%>%
  group_by(name,company,LastUpdated,Advance_Paid)%>%
  summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),
            billdate=min(date),Days_LT30=sum(Days_LT30),Days_30to60=sum(Days_30to60),
            Days_60to90=sum(Days_60to90),Days_GT90=sum(Days_GT90))%>%
  mutate(Actual_Due=as.numeric(due_on-billdate))%>%
  mutate(AgeDays = case_when(
    as.numeric(Sys.Date()-billdate)<= Actual_Due ~ 0,
    TRUE ~ as.numeric(Sys.Date()-billdate-Actual_Due)
  ))%>%
  mutate(Days_Delayed = AgeDays , .after = name)%>%
  mutate(name=as.character(name))%>%
  ungroup()%>%
  select(name,company,Advance_Paid,BillValue,PendingValue,Days_Delayed,due_on,LastUpdated)


tdp_tf<-  tdp%>%
  clean_names()%>%
  filter(!is.na(date))%>%
  # mutate(lut=dmy(lut))%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(lut=dmy(lut),value=as.numeric(value),date=dmy(date),due_on=dmy(due_on))%>%
  mutate(name=snakecase::to_upper_camel_case(name))%>%
  mutate(name=as.factor(name),
         value=str_trim(str_replace(value,"Cr|Dr","")),
         final=str_trim(str_replace(final,"Cr|Dr","")),
         final=str_trim(str_replace_all(final,",","")),
         final=str_trim(str_replace(final,"-","0")))%>%
  select(name,date,due_on,invoice_no,name,bill_value=value,pending_value=pending,final_value=final,company,
         Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days,LastUpdated=lut)
# mutate(bill_value=as.numeric(gsub(",","",bill_value,fixed=TRUE)))%>%
# mutate(final_value=as.numeric(gsub(",","",final_value,fixed=TRUE)))


tdp_tf<-tdp_tf%>%
  left_join(
    tdp_tf%>%
      mutate(final_value=as.numeric(final_value))%>%
      mutate(bill_value=as.numeric(bill_value))%>%
      filter(invoice_no=="ADVANCE")%>%
      group_by(invoice_no,company,name)%>%
      summarise(Advance_Paid = sum(Days_30to60,Days_60to90,Days_GT90),by="name"))


tdp_tf$Advance_Paid <- replace_na(tdp_tf$Advance_Paid,0)


tdp_tf2<-tdp_tf%>%
  mutate(final_value=as.numeric(final_value))%>%
  mutate(bill_value=as.numeric(bill_value))%>%
  group_by(company,LastUpdated,name,Advance_Paid)%>%
  summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),
            billdate=min(date),Days_LT30=sum(Days_LT30),Days_30to60=sum(Days_30to60),
            Days_60to90=sum(Days_60to90),Days_GT90=sum(Days_GT90))%>%
  mutate(Actual_Due=as.numeric(due_on-billdate))%>%
  mutate(AgeDays = case_when(
    as.numeric(Sys.Date()-billdate)<= Actual_Due ~ 0,
    TRUE ~ as.numeric(Sys.Date()-billdate-Actual_Due)
  ))%>%
  mutate(Days_Delayed = AgeDays , .after = name)%>%
  mutate(name=as.character(name))%>%
  ungroup()%>%
  select(name,company,Advance_Paid,BillValue,PendingValue,Days_Delayed,due_on,LastUpdated)






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
ui <- navbarPage("TD Dashboard",
                 theme = shinythemes::shinytheme("flatly"),  # <--- Specify theme here
                 tabPanel("Implementation Summary ",value=1, gt_output(outputId = "raw_data"),height = px(1600),
                          width = px(1600)),
                 tabPanel("Implementation Details",value=2,DTOutput("DT1")),
                 tabPanel("Receivables Summary ",value=1, gt_output(outputId = "tdr"),height = px(1600),
                          width = px(1600)),
                 tabPanel("Receivables Details",reactableOutput("renderedReport")),
                 tabPanel("Payables Summary ",value=1, gt_output(outputId = "tdp"),height = px(1600),
                          width = px(1600)),
                 tabPanel("Payables Details",reactableOutput("renderedReporttdp")),
                 navbarMenu("More",
                            tabPanel("Stocks Details",value=3,gt_output(outputId = "GT1"),height = px(1600),
                                     width = px(1600)),
                            tabPanel("NPR",value=4,gt_output(outputId = "NPR"),height = px(1600),
                                     width = px(1600)),
                            tabPanel("Download Data",  sidebarLayout(
                              sidebarPanel(
                                selectInput("dataset", "Choose a dataset:", 
                                            choices = c("Payables", "Receivables")),
                                radioButtons("filetype", "File type:",
                                             choices = c("csv", "tsv")),
                                downloadButton('downloadData', 'Download')
                              ),
                              mainPanel(
                                tableOutput('table')
                              )
                            ),value=5,gt_output(outputId = "Download"),height = px(1600),
                                     width = px(1600)))
                 
)


server <- function(input, output) {
  
  gt_tbl1 <-reactive({
    tdis_tf %>%
      arrange(desc(Days_Delayed))%>%
      select(customer_name,Days_Delayed,type,total_amount,po_date,material_full_description,
             actualtarget,customer_name,AgeDays,company)
  })
  
  gt_tbl2 <-reactive({
    tdr_tf%>%
      group_by(name,company)%>%
      summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),billdate=min(date))%>%
      mutate(Actual_Due=as.numeric(due_on-billdate))%>%
      mutate(AgeDays = case_when(
        as.numeric(Sys.Date()-billdate)<= Actual_Due ~ 0,
        TRUE ~ as.numeric(Sys.Date()-billdate-Actual_Due)
      ))%>%
      mutate(Days_Delayed = AgeDays , .after = name)%>%
      mutate(name=as.character(name))%>%
      ungroup()
  })
  
  gt_tbl3 <-reactive({
    tdp_tf%>%
      group_by(name,company)%>%
      mutate(bill_value=as.numeric(bill_value),final_value=as.numeric(final_value))%>%
      summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),billdate=min(date))%>%
      mutate(Actual_Due=as.numeric(due_on-billdate))%>%
      mutate(AgeDays = case_when(
        as.numeric(Sys.Date()-billdate)<= Actual_Due ~ 0,
        TRUE ~ as.numeric(Sys.Date()-billdate-Actual_Due)
      ))%>%
      mutate(Days_Delayed = AgeDays , .after = name)%>%
      mutate(name=as.character(name))%>%
      ungroup()
  })
  
  gt_tbl_react <- reactiveVal(NULL)
  gt_tbl_react(gt_tbl1)
  gt_tbl_react(gt_tbl2)
  gt_tbl_react(gt_tbl3)
  
  output$raw_data <-
    render_gt({
      gt_tbl1()%>%
        gt::gt() %>% 
        tab_row_group(
          group = "TD Infra",
          rows = company =="TDIS") %>% 
        tab_row_group(
          group = "TD Global",
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
          title = paste0("TD Implementation Summary As on - ",Sys.Date()),
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
  
  #Stock Details
  output$GT1 <-
    render_gt({
      tdis_ts%>%
        gt()%>%
        tab_row_group(
          group = "TD Infra",
          rows = Company =="TDIS") %>% 
        tab_row_group(
          group = "TD Global",
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
  
  
  ##NPR
  
  
  output$NPR <-
    render_gt({
      tdr_npr %>%
        select(Date=date,Due=due_on,Name=name,Pending_Amount=pending_value,
               Final_Amount=final_value,Company=company)%>%
        gt()%>%
        tab_row_group(
          group = "TD Infra",
          rows = Company =="TDIS") %>% 
        tab_row_group(
          group = "TD Global",
          rows = Company =="TDG")%>%
        tab_style(
          style = list(
            cell_text(weight = "bold")
          ),   locations = cells_column_labels(everything())
        )%>%
        gt::opt_table_lines()%>%
        gt_theme_538()%>%
        #gt_theme_nytimes() %>% 
        tab_header(title = md("**NPR Summary**"),
                   subtitle = paste0("Clients are categorized as NPR as their payment receivables are challenging - 
                                       As on ",Sys.Date()))%>%
        tab_spanner(columns = vars(Pending_Amount:Final_Amount),
                    label = md("**Accounting Details**"))%>%
        # trim gives smaller range of colors
        # so the green and purples are not as dark
        gt_color_rows(Final_Amount, palette = "ggsci::blue_material")%>%
        grand_summary_rows(
          columns = Final_Amount,
          fns = list(
            TOTAL = ~sum(.)
          ),
          formatter = fmt_number,
          decimals = 0
        )
      
    })
  
  
  
  output$renderedReport<-   renderReactable({
    tbl<-  reactable(
      data       = tdr_tf2,elementId = "company",
      compact    = FALSE, # for minimum row height
      filterable = TRUE, # for individual column filters
      striped    = TRUE, # banded rows
      searchable = TRUE,
      resizable  = TRUE, # for resizable column widths
      columns    = list(
        name   = colDef(name = "Name",  width = 270,  align = "center"),
        company  = colDef(name = "Company",width = 90, align = "center"),
        BillValue = colDef(name = "Bill Value",width = 90, align = "center"), 
        PendingValue  = colDef(name = "Pending Value",      width = 120, align = "center"),
        due_on    = colDef(name = "Due Date",        width = 120, align = "center"),
        Days_Delayed    = colDef(name = "Days Delayed",        width = 120, align = "center"),
        LastUpdated    = colDef(name = "Last Updated",        width = 120, align = "center"),
        Advance_Paid = colDef(width = 120, align = "center",style = function(value) {
          normalized <- (value - min(tdr_tf2$Advance_Paid)) / (max(tdr_tf2$Advance_Paid) - min(tdr_tf2$Advance_Paid))
          color <- orange_pal(normalized)
          list(background = color)
        })
        
      ),
      details = function(index) { 
        sec_lvl = tdr_tf[tdr_tf$name == tdr_tf2$name[index]&tdr_tf$company == tdr_tf2$company[index],] %>%
          filter(!invoice_no=="ADVANCE")%>%
          select(invoice_no,Days_LT30:Days_GT90)
        reactable(data       = sec_lvl,
                  compact    = TRUE, 
                  filterable = TRUE,
                  bordered   = TRUE, 
                  resizable  = TRUE,
                  columns    = list(
                    invoice_no    = colDef(name = "Invoice #",    width = 150, align = "center"),
                    Days_LT30    = colDef(name = "< 30 Days",    width = 90, align = "center"),
                    Days_30to60      = colDef(name = "30 to 60 Days",width = 90, align = "center"),
                    Days_60to90      = colDef(name = "60 to 90 Days", width = 90, align = "center"),
                    Days_GT90         = colDef(name = "> 90 Days",    width = 90, align = "center")
                  )
        )
      }
    )
    
    # htmltools::tags$iframe(src = "sipl_1.html", width = '100%',  height = 1000,  style = "border:none;")
    
    # HTML(markdown::markdownToHTML(knitr::knit('sipl_1.md', quiet = TRUE)))
  })
  orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ff9500"))(x), maxColorValue = 255)
  output$renderedReporttdp<-   renderReactable({
    tbl<-  reactable(
      data       = tdp_tf2,elementId = "company",
      compact    = FALSE, # for minimum row height
      filterable = TRUE, # for individual column filters
      striped    = TRUE, # banded rows
      searchable = TRUE,
      resizable  = TRUE, # for resizable column widths
      columns    = list(
        name   = colDef(name = "Name",  width = 270,  align = "center"),
        company  = colDef(name = "Company",width = 90, align = "center"),
        BillValue = colDef(name = "Bill Value",width = 90, align = "center"), 
        PendingValue  = colDef(name = "Pending Value",      width = 120, align = "center"),
        Days_Delayed    = colDef(name = "Days Delayed",        width = 120, align = "center"),
        due_on    = colDef(name = "Due On",        width = 120, align = "center"),
        LastUpdated    = colDef(name = "Last Updated",        width = 120, align = "center"),
        Advance_Paid = colDef(width = 120, align = "center",style = function(value) {
          normalized <- (value - min(tdp_tf2$Advance_Paid)) / (max(tdp_tf2$Advance_Paid) - min(tdp_tf2$Advance_Paid))
          color <- orange_pal(normalized)
          list(background = color)
        })
        
      ),
      
      details = function(index) { 
        sec_lvl = tdp_tf[tdp_tf$name == tdp_tf2$name[index]&tdp_tf$company == tdp_tf2$company[index], ] %>%
          filter(!invoice_no=="ADVANCE")%>%
          select(invoice_no,Days_LT30:Days_GT90)
        reactable(data       = sec_lvl,
                  compact    = TRUE, 
                  filterable = TRUE,
                  bordered   = TRUE, 
                  resizable  = TRUE,
                  columns    = list(
                    invoice_no    = colDef(name = "Invoice #",    width = 150, align = "center"),
                    Days_LT30    = colDef(name = "< 30 Days",    width = 90, align = "center"),
                    Days_30to60      = colDef(name = "30 to 60 Days",width = 90, align = "center"),
                    Days_60to90      = colDef(name = "60 to 90 Days", width = 90, align = "center"),
                    Days_GT90         = colDef(name = "> 90 Days",    width = 90, align = "center")
                  )
        )
      }
    )
    
    # htmltools::tags$iframe(src = "sipl_1.html", width = '100%',  height = 1000,  style = "border:none;")
    
    # HTML(markdown::markdownToHTML(knitr::knit('sipl_1.md', quiet = TRUE)))
  })
  
  
  
  
  
  
  
  ##TDR Table
  output$tdr <- render_gt({
    gt_tbl2() %>%
      arrange(desc(AgeDays))%>%
      select(Client=name,Days_Delayed,BillValue,PendingValue,billdate,Actual_Due,AgeDays,company)%>%
      gt::gt() %>% 
      tab_row_group(
        group = "TD Infra",
        rows = company =="TDIS") %>% 
      tab_row_group(
        group = "TD Global",
        rows = company =="TDG")%>%
      gt_plt_bullet(column =AgeDays , target = Actual_Due,colors = c("#E1341E","#1ECBE1")) %>% 
      # gt_fa_column(column = type) %>%
      cols_hide(
        columns = c(
          company
        )
      )%>%
      #gt_theme_538()%>%
      #gt_theme_guardian()%>%
      #gt_theme_espn()%>%
      gt_theme_nytimes() %>% 
      fmt_symbol_first(column = Days_Delayed, suffix = " Days", decimals = 0) %>%
      cols_label(
        Actual_Due = html(
          "<span style='color:#1ECBE1;'>Actual Due</span> vs <span style='color:#E1341E;'>Today</span>"
        )
      ) %>% 
      tab_header(
        title = paste0("Receivables Summary As on - ",Sys.Date()),
        subtitle = "Considering the actual due based on the clients"
      ) %>% 
      #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
      gt_add_divider(Days_Delayed, color = "#7897AB", weight = px(1)) %>% 
      tab_source_note(md("**Data Source**: TD")) %>% 
      tab_options(
        table.border.bottom.color = "grey",
        table.width = px(500)
      )%>%
      grand_summary_rows(
        columns = BillValue,
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
          columns = BillValue,
          rows = 1)
      )
    
    
    
  })
  
  ####TD Payables
  
  output$tdp <- render_gt({
    gt_tbl3() %>%
      arrange(desc(AgeDays))%>%
      select(Client=name,Days_Delayed,BillValue,PendingValue,billdate,Actual_Due,AgeDays,company)%>%
      gt::gt() %>% 
      tab_row_group(
        group = "TD Infra",
        rows = company =="TDIS") %>% 
      tab_row_group(
        group = "TD Global",
        rows = company =="TDG")%>%
      gt_plt_bullet(column =AgeDays , target = Actual_Due,colors = c("#E1341E","#1ECBE1")) %>% 
      # gt_fa_column(column = type) %>%
      cols_hide(
        columns = c(
          company
        )
      )%>%
      #gt_theme_538()%>%
      #gt_theme_guardian()%>%
      #gt_theme_espn()%>%
      gt_theme_nytimes() %>% 
      fmt_symbol_first(column = Days_Delayed, suffix = " Days", decimals = 0) %>%
      cols_label(
        Actual_Due = html(
          "<span style='color:#1ECBE1;'>Actual Due</span> vs <span style='color:#E1341E;'>Today</span>"
        )
      ) %>% 
      tab_header(
        title = paste0("Payab les Summary As on - ",Sys.Date()),
        subtitle = "Considering the actual due based on the clients"
      ) %>% 
      #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
      gt_add_divider(Days_Delayed, color = "#7897AB", weight = px(1)) %>% 
      tab_source_note(md("**Data Source**: TD")) %>% 
      tab_options(
        table.border.bottom.color = "grey",
        table.width = px(500)
      )%>%
      grand_summary_rows(
        columns = BillValue,
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
          columns = BillValue,
          rows = 1)
      )
    
    
    
  })
  
  
  datasetInput <- reactive({
    # Fetch the appropriate data object, depending on the value
    # of input$dataset.
    switch(input$dataset,
           "Payables" = tdp_tf2,
           "Receivables" = tdr_tf2)
  })
  
  output$Download <- renderTable({
    datasetInput()
  })
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$dataset, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
}

shinyApp(ui=ui,server=server)

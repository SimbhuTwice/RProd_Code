
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

#"/Users/simbhud"
#devtools::install_github("hadley/emo")
#shiny app for pur vs sales october 20 2021

### Pre Work before shiny deployment###

# 
# sales_nov <- read_csv("Salnov2.csv",skip=5)
# pur_nov <- read_csv("Purnov2.csv",skip=6)

#write to csv for shiny upload updated nov 15 with nov 14 data
#write_csv(sales_nov,"shinynovsales.csv")

#write_csv(pur_nov,"shinynovpur.csv")
# 
#   pur_oct%>%
#     head()%>%View()
#   
#   sales_nov <- sales_nov %>%
#     filter(StoreName!="Teynampet")%>%
#     mutate(BillDate=dmy(BillDate),
#            BillMonth=month(BillDate),
#            BillYear=year(BillDate))%>%
#     select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,Amount,BillTime,BillMonth,BillYear)
#  
#   
#   
#   pur_nov <- pur_nov %>%
#     mutate(GRNDate=dmy(GRNDate),
#            mnths=month(GRNDate),
#            BillYear=year(GRNDate))%>%
#     select(Department,GRNDate,SupplierName,StoreName,ProductName,Quantity,
#       MRP,Amount,PurchasePrice,mnths)


###Pre Work ends

sales_nov<-read_csv("shinydecsales.csv",show_col_types = FALSE)

pur_nov <- read_csv("shinydecpur.csv",show_col_types = FALSE)

chrs_acc <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/cheers1.csv',skip=2)

chrs_acc <- chrs_acc%>%clean_names()%>%
    select(date,particulars,instrument_date,credit,cheque_in_hand_issued_on)

#chrs_acc2 <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/cheers2.csv',skip=2)

# chrs_acc2<- chrs_acc2%>%
#     clean_names()%>%
#     select(date,particulars,instrument_date,credit,bill_date,payment_type,cheque_in_hand_issued_on)
# 

chrs_sum <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/summary.csv',skip=1)

chrs_sum <- chrs_sum%>%
    clean_names()%>%
  select(s_no:total)%>%
  filter(!is.na(s_no))

# Define UI for application that draws a histogram
sname <- as.data.frame(unique(sales_nov$StoreName))


vend_bullet_crpl <- read_csv("crpl_pending.csv")

vend_bullet_crpl <- vend_bullet_crpl%>%
    mutate(Purchased=dmy(Purchased))

#Cheque Addition Dec 21 2021
cheque_cleared <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/band.csv',skip=10)

cheque_cleared_dt <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/band.csv',skip=10)



cheque_viz <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/cheque.csv')

cheque_viz <- cheque_viz%>%mutate(Date=dmy(Date))
#Added jan 20 , 2022 for automatic sales and purchase


###This is added for cheque cleared 

cheque_cleared_dt <- cheque_cleared_dt%>%
  clean_names()%>%
  mutate(ref_cheque_no=as.numeric(ref_cheque_no))%>%
  filter(str_length(ref_cheque_no)< 4)%>%
  mutate(date=dmy(date),debit=str_replace(debit,",",""),
         credit=str_replace(credit,",",""),
         balance=str_replace(balance,",",""))%>%
  mutate(debit=as.numeric(debit),
         credit=as.numeric(credit),
         balance=as.numeric(balance))%>%
  select(date,description,ref_cheque_no,debit)

sal_daily <- sales_nov%>%
  select(Date=BillDate,BillYear,Amount)%>%
  filter(Date >'2021-11-30')%>%
  # filter(BillYear>2021)%>%
  group_by(Date)%>%
  dplyr::summarise(Total = round(sum(Amount)))


pur_daily <- pur_nov%>%
  select(Date=GRNDate,Amount)%>%
  group_by(Date)%>%
  filter(Date>'2021-11-30',Amount>0)%>%
  dplyr::summarise(Total = round(sum(Amount)))

# cheque_viz$Pur<-cheque_viz%>%left_join(pur_daily,by='Date')%>%
#   mutate(Pur=Total.y)%>%pull(Pur)
# 
# cheque_viz$Sales<- cheque_viz%>%left_join(sal_daily,by='Date')%>%
#   mutate(Sales=Total.y)%>%select(Sales)%>%pull(Sales)


cheque_viz_p<-pur_daily%>%left_join(cheque_viz,by='Date')%>%
  mutate(Pur=Total.x)

cheque_viz_s<-sal_daily%>%left_join(cheque_viz,by='Date')%>%
  mutate(Sales=Total.x)


######Update Ends jan 20 ,2022


##Added Jan 21 2022 for taking cheque from bank statement


cheque_cleared <- cheque_cleared%>%
  clean_names()%>%
  mutate(debit=str_replace(debit,",",""),
         credit=str_replace(credit,",",""),
         balance=str_replace(balance,",",""))%>%
  mutate(debit=as.numeric(debit),
         credit=as.numeric(credit),
         balance=as.numeric(balance))

cheque_cleared <- cheque_cleared%>%
  mutate(ref_cheque_no=as.numeric(ref_cheque_no))%>%
  filter(str_length(ref_cheque_no)< 4)%>%
  select(Date=date,debit)%>%
  group_by(Date)%>%
  mutate(Date=dmy(Date))%>%
  dplyr::summarise(amount=sum(debit))

cheque_viz_c <-  cheque_viz%>%left_join(cheque_cleared,by='Date')

cheque_viz<-cheque_viz_p%>%inner_join(cheque_viz_s,by='Date')%>%
  left_join(cheque_cleared,by='Date')%>%
  clean_names()%>%
  select(Date=date,Pur=pur_x,CC=amount,Sales=sales_y,Pur_Prop=pur_prop_y,CC_Prop=cc_prop_y)





######End of Bank updae#####
cheque_viz <- cheque_viz%>%
    mutate(Sales=round(Sales))%>%
    mutate(CC=replace_na(CC,0),Pur=replace_na(Pur,0),Sales=replace_na(Sales,0))%>%
    group_by(Date)%>%
  mutate(Total=round(sum(Pur+CC+Sales)))%>%
    mutate(Pur_Prop=round(Pur/Total,2)*100,CC_Prop=round(CC/(Total),2)*100,Sale_Prop=round(Sales/(Total),2)*100)%>%
    ungroup()


cheq_viz_join <- cheque_viz%>%
    group_by(Date)%>%
    mutate(
        X25=round(get('Pur_Prop')),
        X50=round(get('CC_Prop')),
        X100=round(get('Sale_Prop'))
    )%>%
    arrange(-X100)%>%
    summarise(Salary = list(c(X25,X50,X100)))


# Set color palette
#pal_salary <- c('#264653','#e9c46a','#e76f51')
pal_salary <- c('#fb8500', '#5B84B1FF', '#3a6b35')

##Cheque Addition ends

colnames(sname)<-"sname"
sidebar <- dashboardSidebar(width = 350,
                            conditionalPanel(condition="input.tabselected==1",
                                             selectInput(
                                                 inputId = "select_vendor",
                                                 label = "Select Vendor",
                                                 choices = sort(unique(sales_nov$SupplierName)),
                                                 selected = "All",
                                                 multiple = FALSE
                                             ),
                                             # sliderInput("rateThreshold", paste0("Slide the Month to retrieve data updated till : ",max(sales_nov$BillDate)),
                                             #             min = 0, max = 12, value = 6, step = 1
                                             # )
                                             #Updated Jan 1 2022 : changed from liderinout to Date Range inout for the date year  issue.
                                             dateRangeInput("daterange0", "Select only the start date to see the results until till date:",
                                                            start = Sys.Date()-90,
                                                            end = Sys.Date())
                                             
                            ),
                            
                            conditionalPanel(condition="input.tabselected==2",
                                             selectInput("branches", "Select the branch to get its details:",
                                                         
                                                         c( "SVA" = "cheers Ayanavaram",
                                                             "AnnaNagar" = "Cheers Annanagar",
                                                             "Nungambakkam" = "Cheers Nungambakkam",
                                                             "Adyar" = "Adyar"))
                            ),
                            
                            conditionalPanel(condition="input.tabselected==6",
                                             dateRangeInput("daterange1", "Select the Date range to see the details:",
                                                            start = Sys.Date()-7,
                                                            end = Sys.Date()-1))
                 
)


# Header ----
header <- dashboardHeader(title="Cheers Purchase,Sales & Accounts",titleWidth = 350)

# Body ----
body <- dashboardBody(
    mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("Data",value=1, gt_output(outputId = "raw_data"),width = 12),
                    tabPanel("Detailed Analysis",value=2, gt_output(outputId = "table")),
                    tabPanel("Cheques in Hand",value=3,DTOutput("acc1")),
                    tabPanel("Cheques Cleared", value=4,DTOutput("acc2")),
                    #Cheques Summary has been transformed from tableoutput to Gt output Jan 4 2022
                    #tabPanel("Cheques Summary",value=5, DTOutput("summary")),
                    #Added GT version of Cheque summary
                    tabPanel("Cheques Summary",value=5, gt_output(outputId = "summary")),
                   # tabPanel("CRPL Pending",value=6,gt_output(outputId = "crpl"),height = 400,width = 12),
                   tabPanel("Cheques Cleared vs Pur vs Sales",value=6,gt_output(outputId = "cheque"),height = px(800),
                            width = px(800)),
                    id="tabselected"
        )
    )
)
ui <- dashboardPage(header, sidebar, body)

server <- function(input,
                   output
) {
    
    
    
    gt_tbl <-reactive({
        sales_nov%>%
            filter(SupplierName==input$select_vendor)%>%
            filter(StoreName==input$branches)%>%
            filter(BillDate >= ymd(input$daterange0[1]) & BillDate <= ymd(input$daterange0[2]))%>%
            #filter(BillMonth>=input$rateThreshold)%>%
            select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
                   Amount,BillTime,mnths=BillMonth,BillYear)%>%
            group_by(ProductName,Department)%>%
            summarise(sales=sum(Amount))%>%
            arrange(desc(sales))%>%inner_join(pur_nov%>%
                                                  filter(SupplierName==input$select_vendor)%>%
                                                  #filter(mnths>=input$rateThreshold)%>%
                                                  filter(GRNDate >= ymd(input$daterange0[1]) & GRNDate <= ymd(input$daterange0[2]))%>%
                                                  select(Department,GRNDate,StoreName,SupplierName,ProductName,
                                                         Quantity,MRP,Amount,PurchasePrice,mnths)%>%
                                                  group_by(ProductName,Department)%>%
                                                  summarise(purchased=sum(Amount))%>%
                                                  arrange(desc(purchased)), by="ProductName")%>%
            select(  ProductName ,purchased,sales)%>%
            group_by( ProductName)%>%
            summarise(purchase=round(sum(purchased)),sold = round(sum(sales)))%>%
            arrange(desc(purchase))
    })
    
    
    gt_tbl2 <-reactive({
        sales_nov%>%
            filter(SupplierName==input$select_vendor)%>%
            #filter(BillMonth>input$rateThreshold)%>%
            filter(BillDate >= ymd(input$daterange0[1]) & BillDate <= ymd(input$daterange0[2]))%>%
            
            select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
                   Amount,BillTime,BillMonth,BillYear)%>%
            group_by(Department,SupplierName)%>%
            summarise(sales=round(sum(Amount)))%>%
            arrange(desc(sales))%>%inner_join(pur_nov%>%
                                                  filter(SupplierName ==input$select_vendor)%>%
                                                 # filter(mnths>input$rateThreshold)%>%
                                                  filter(GRNDate >= ymd(input$daterange0[1]) & GRNDate <= ymd(input$daterange0[2]))%>%
                                                  select(Department,GRNDate,StoreName,SupplierName,ProductName,
                                                         Quantity,MRP,Amount,PurchasePrice,mnths)%>%
                                                  group_by(Department,SupplierName)%>%
                                                  summarise(purchased=round(sum(Amount)))%>%
                                                  arrange(desc(purchased)), by="Department")%>%
            select(Department,Vendor=SupplierName.x,purchased,sales)%>%
            arrange(desc(purchased))
    })
    
    
    gt_tbl3 <- reactive({
        vend_bullet_crpl %>%
            select("DISTRIBUTOR"=VendorName,Products,CreditDays,AgeDays,Amount,Purchased,"Recurring_Vendor"=Existing)%>%
            arrange(desc(AgeDays))%>%
            filter(Amount>10000)%>%
            filter(AgeDays>50)%>%
            mutate(type = case_when(
                Products %in% c("BEVRAGES") ~ "wine-bottle",
                Products %in% c("SNACKA AND BISCUITS", "PACKED FOODS") ~ "cookie-bite",
                #Products %in% c("", "PEDIA SURE BEVERAGES") ~ "baby",
                Products %in% c("FROZEN AND DAIRY") ~ "ice-cream",
                Products %in% c("HEALTH AND BEAUTY") ~ "diagnoses",
                Products %in% c("HOME NEEDS","GROCERIES","Groceries","HUL","FRUITS AND VEGTABLES") ~ "shopping-cart",
                TRUE ~ "home"
            ),
            .after = DISTRIBUTOR
            ) %>% 
            mutate(Days_Delayed = AgeDays , .after = type)
        
        
    })
    
    
    gt_tbl4 <-reactive({
      cheque_viz%>%
            left_join(cheq_viz_join)%>%
            select(Date,"Purchase"=Pur,Sales,"Cheque_Cleared"=CC,Total,Salary)%>%
           filter(Date >= ymd(input$daterange1[1]) & Date <= ymd(input$daterange1[2])) 

    })
    
    gt_tbl_react <- reactiveVal(NULL)
    gt_tbl_react(gt_tbl)
    gt_tbl_react(gt_tbl2)
    gt_tbl_react(gt_tbl3)
    gt_tbl_react(gt_tbl4)
    
    
    output$table <-
        # render_gt(
        #   expr = gt_tbl,
        #   height = px(600),
        #   width = px(600)
        # )
        
        
        render_gt({
            gt_tbl() %>%gt()%>%gt_theme_espn()%>%gt_color_rows(purchase:sold, palette = "ggsci::blue_material")%>%
                tab_header(
                    title = paste0("Cheers Purchase vs Sales Details (Product Wise) Updated till ",max(sales_nov$BillDate))
                )%>%
                gt_add_divider(ProductName:sold, color = "grey", weight = px(1)) %>% 
                tab_source_note(md("**Viz**: Zaprify | **Data**: Cheers Retail")) %>% 
                tab_options(
                    table.border.bottom.color = "grey",
                    table.width = px(600)
                )
                
        }) 
    
    
    output$raw_data <-
        # render_gt(
        #   expr = gt_tbl,
        #   height = px(600),
        #   width = px(600)
        # )
        
        
        render_gt({
            gt_tbl2() %>%gt()%>%gt_theme_espn()%>%gt_color_rows(purchased:sales, palette = "ggsci::blue_material")%>%
                tab_header(
                    title = paste0("Cheers Purchase vs Sales Details (Vendor Wise) updated till ",max(sales_nov$BillDate))
                )%>%
                #gt_add_divider(Vendor:sales, color = "grey", weight = px(1)) %>% 
                tab_source_note(md("**Viz**: Zaprify | **Data**: Cheers Retail")) %>% 
                tab_options(
                    table.border.bottom.color = "grey",
                    table.width = px(600)
                )
        }) 
    
    output$acc1 <-
        renderDT({
            DT::datatable(chrs_acc%>% 
                              select(date,particulars,instrument_date,credit,cheque_in_hand_issued_on), options = list(
                                  pageLength=50, scrollY='400px'), filter = 'top')
        })
    
    output$acc2 <-
        renderDT({
            DT::datatable( cheque_cleared_dt, caption = "Cheques cleared",options = list(
                pageLength=50, scrollY='400px'), filter = 'top')
        })
  ###Changing Summary to GT table ###
    
    # output$summary <-
    #     renderDT({
    #         DT::datatable(chrs_sum)
    #     })
    
    output$summary <-
        render_gt({
            chrs_sum%>%
                gt()%>%
                tab_style(
                    style = list(
                        cell_text(weight = "bold")
                    ),   locations = cells_column_labels(everything())
                )%>%
                gt::opt_table_lines()%>%
                gt_theme_538()%>%
                #gt_theme_nytimes() %>% 
                tab_header(title = md("**Cheques Summary**"),
                           subtitle = paste0("As on ",Sys.Date()))%>%
                tab_spanner(columns = vars(cv:crpl),
                            label = md("**Cheers Market**"))%>%
                # trim gives smaller range of colors
                # so the green and purples are not as dark
                gt_color_rows(total, palette = "ggsci::blue_material")%>%
                tab_footnote(
                    footnote = md("Cheques yet to be prepared **(Total Outstanding)**"),
                    locations = cells_body(
                        columns = particulars,
                        rows = particulars == "Cheques to be Prepared"
                    ))
            
            
        })
        
        
    output$crpl <- 
        render_gt({
                gt_tbl3() %>%
                gt::gt() %>% 
                gt_plt_bullet(column =AgeDays , target = CreditDays,width = 100,colors = c("#E1341E","#1ECBE1")) %>% 
                gt_fa_column(column = type) %>% 
                #gt_theme_guardian()%>%
                gt_theme_espn()%>%
                # gt_theme_nytimes() %>% 
                fmt_symbol_first(column = Days_Delayed, suffix = "Days", decimals = 0) %>%
                cols_label(
                    CreditDays = html(
                        "<span style='color:#1ECBE1;'>ActualCreditDays</span> vs <span style='color:#E1341E;'>CurrentDay</span>"
                    )
                ) %>% 
                tab_header(
                    title = "CRPL Vendors Payment Ageing",
                    subtitle = "Taken into account vendors pending payment > 50 Days"
                ) %>% 
                #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
                gt_add_divider(type, color = "grey", weight = px(1)) %>% 
                tab_source_note(md("**Viz**: Zaprify | **Data**: Cheers Retail")) %>% 
                tab_options(
                    table.border.bottom.color = "grey",
                    table.width = px(800)
                )
        })
    
    output$cheque <-
        render_gt({
   gt_tbl4()%>%
                gt()%>%
                gt_highlight_rows(
                    #rows = 1:nrow(cheque_viz), 
                    fill = "lightgrey",
                    bold_target_only = TRUE,
                    target_col = Total
                )%>%
               # gt_color_rows(Cheque_Cleared, palette = "ggsci::blue_material")%>%
               gt_color_rows(Sales, palette = "ggsci::blue_material")%>%
                gt_plt_bar_stack(
                    column=Salary, palette = pal_salary,
                    position = 'stack', labels = c("Purchase %","Cheque_Cleared %","Sales %"),
                    width = 80,trim=FALSE,
                    fmt_fn = scales::percent_format()
                )%>%
                tab_header(
                    title = paste0("Purchase vs Sales Vs CC : Updated Till " ,max(cheque_viz$Date)),
                    subtitle = md("Showing last 7 Days - **Filter the Dates in sidebar for the relevant data**")
                )%>%
                tab_source_note(
                    source_note = md("**Data:** CRPL | **Viz:** Zaprify")
                )%>%
                # Style
                gt_theme_nytimes()%>%
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            name = "Bebas Neue"), 
                            size='xx-large',
                            color='indianred'
                        )),
                    locations = cells_title(groups = "title")
                )%>%
                # Subtitle
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            #name = "Roboto Condensed"
                            name = "Roboto"), align = "left",size='small')),
                    locations = cells_title(groups = "subtitle")
                )%>%
                # Headers
                tab_style(
                    style = list(
                        cell_text(
                            font=google_font(name = "Noto Sans Display"), 
                            align = "center",v_align='middle',
                            transform = 'capitalize',weight='bold'),
                        cell_borders(color='dimgrey',style='solid',sides=c('top'))
                    ),
                    locations = cells_column_spanners()
                ) %>%
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            name = "Roboto Condensed"
                        ), align = "center",size='small',
                        transform = 'lowercase',v_align='middle'),
                        cell_borders(color='dimgrey',style='solid',sides=c('bottom'))),
                    locations = cells_column_labels()
                ) %>%
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            name = "Fira Sans"),align = 'left'
                        )),
                    locations = cells_body(columns = c(Date))
                )%>%
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            name = "Fira Sans"),align = 'center'
                        )),
                    locations = cells_body(columns = c(Total))
                )%>%
                # Footnote
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            name = "Roboto Condensed"
                        ),style = "italic"),
                        cell_borders(color='dimgrey',style='solid',sides=c('top'))),
                    locations = cells_footnotes()
                )%>%
                # Source note
                tab_style(
                    style = list(
                        cell_text(font=google_font(
                            name = "Roboto Condensed"
                        ))),
                    locations = cells_source_notes()
                )%>%
                # Borders
                tab_options(
                    table.border.top.style = "solid",
                    table.border.top.color = "dimgrey",
                    table.border.bottom.style = "hidden"
                )%>%
                grand_summary_rows(
                    columns = Purchase:Cheque_Cleared,
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
                        columns = Purchase:Cheque_Cleared,
                        rows = 1)
                ) %>%
                # tab_style(
                #     style = cell_text(color = "e7e8d1", weight = "bold"),
                #     locations = cells_body(
                #         columns = vars(Total),
                #         rows = Total >= 200000
                #     )
                # )%>%
                tab_footnote(
                    footnote = md("Cheque Cleared are Zero on some days as they are **Weekends / Holiday**"),
                    locations = cells_column_labels(4)
                )%>%
                data_color(
                    columns = vars(Cheque_Cleared),
                    colors = scales::col_numeric(
                        palette = c(
                            "white", "orange", "red"),
                        domain = NULL)
                )
            
            
        })
}




shinyApp(ui=ui,server=server)
library(tidyverse)

# install.packages('tidymodels',dependencies = TRUE)
# install.packages('lubridate',dependencies = TRUE)
# install.packages('skimr',dependencies = TRUE)
# install.packages('janitor',dependencies = TRUE)
# install.packages('ggthemes',dependencies = TRUE)
# install.packages('magick',dependencies = TRUE)
#install.packages('plotly',dependencies = TRUE)
#devtools::install_github("ropensci/piggyback")

#install.packages("gt")
library(shiny)
library(shinythemes)
library(piggyback)
library(fullPage)
library(echarts4r)

library(ggiraph)

library(googleVis)
 library(plotly)
 library(shiny)

library(tidymodels)
library(skimr)
library(broom)
library(scales)
library(readxl)
library(janitor)
library(ggthemes)
library(scales) ## Formatting numbers and values
library(hrbrthemes)# For changing ggplot theme
library(extrafont) # More font options
library(magick)
library(patchwork)
library(png)
library(grid) # to put an image at a given location
library(gridExtra) # functions to work with pictures
library(ggplot2)
library(plotly)
library(ggridges)
library(cowplot)
library(magick)
library(lubridate)
library(dplyr)
library(forcats)
library(gt)
library(gtExtras)
library(shinyBS)
library(describer)
library(reactablefmtr)

cheers_market <- read_excel("SalesNov.xlsx",skip=5)

logo <- image_read("~/cheers.png")
logo <- readPNG("~/cheers.png", native = TRUE)



qplot(1:10, 1:10, geom="blank") +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point()
cheers_market %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  group_by(Department)%>%
  summarise(sum_total=sum(Total))%>%
  arrange(desc(sum_total))%>%
  ungroup()%>%
  mutate(Department=fct_reorder(Department,sum_total))%>%
  ggplot(aes(Department,sum_total,fill=Department))+
  geom_col()+
  labs(y="# of items sold",
       x="Categories",
       color="Department"
  ) +
  coord_flip()
  #scale_color_discrete(guide=guide_legend(reverse = FALSE))+


#Total Units Sold
cheers_market %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  group_by(Department)%>%
  summarise(sum_total=sum(Total))%>%
  arrange(desc(sum_total))%>%
  ungroup()%>%
  mutate(Department=fct_reorder(Department,sum_total))%>%
  ggplot(aes(Department,sum_total,fill=Department))+
geom_col(show.legend = FALSE,color="black")+
  geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
  scale_y_comma()+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+
  theme_classic()+
  labs(title = "Total Unit Sold breakdown by categories in Cheers Adyar",subtitle="2 Day Sales : Dates considered May 8 & 9",
       x="Categories",y= "Total Units Sold",caption="Source : Cheers Market Adyar")
ggsave("TotalUnits.png")

cheers_sales_data <- read_excel("SalesReport.xlsx")

#Total sales in adyar
sales_all %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  #filter(StoreName=="Adyar") %>%
  select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
  group_by(Department)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))%>%
  ungroup()%>%
  mutate(Department=fct_reorder(Department,sum_total))%>%
  ggplot(aes(Department,sum_total,fill=Department))+
  geom_col(show.legend = FALSE,color="black")+
  geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
  scale_y_comma()+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+
  theme_classic()+
  labs(title = "Top Selling categories ",
       subtitle="Groceries,Packed Foods & Dairy Products are the Top Sellers",x="Categories",y= "Total sales")

ggsave("TotalSales_2Days.png")


range(sales_nov$BillDate)

sales_nov%>%
  count(Department)
#top selling product oct 2021

sales_nov %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  filter(!Department=="FRUITS AND VEG")%>%
  filter(!grepl('Aavin', ProductName))%>%
  #filter(StoreName=="Adyar") %>%
  select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
  group_by(ProductName)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))%>%
  top_n(10,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  ggplot(aes(ProductName,sum_total,fill=ProductName))+
  geom_col(show.legend = FALSE,color="black")+
  geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
  scale_y_comma()+
  scale_fill_manual(values=cbPalette)+
  #scale_fill_distiller()+
  coord_flip()+
 theme_few()+
  #theme_classic()+
  labs(title = "Top Selling Products ",
       subtitle="Excluding Daily Needs (F&V , Aavin Milk) ",x="",y= "sales",
       caption = "Viz : Zaprify , Data : CRPL")

ggsave("TotalSales_2Days.png")


#####
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#39A388","#56B4E9", "#009E73", "#F0E442","#464660", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
scale_fill_manual(values=cbPalette)




# To use for line and point colors, add
scale_colour_manual(values=cbPalette)
##Rukmini##
sales_nov %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  filter(grepl('RUKMINI', ProductName))%>%
  #filter(StoreName=="Adyar") %>%
  select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
  group_by(ProductName)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))%>%
  top_n(10,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  ggplot(aes(ProductName,sum_total,fill=ProductName))+
  geom_col(show.legend = FALSE,color="black")+
  geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
  scale_y_comma()+
  scale_fill_manual(values=cbPalette)+
  #scale_fill_distiller(palette = "RdPu")+
  coord_flip()+

  #theme_tufte()+
  theme_classic()+
  labs(title = "Top Selling RUKMINI Products ",
       subtitle="GROCERIES",x="",y= "sales")
####
  #Each Categories
  
  cheers_sales_data %>%
    mutate(BillDate=as.Date(BillDate))%>%
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
    filter(StoreName=="Adyar") %>%
    #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
   #filter(BillDate=='2021-05-08')%>%
    select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
    group_by(ProductName,Department)%>%
    summarise(sum_total=sum(Amount))

    
  sales_all %>%
   # mutate(BillDate=as.Date(BillDate))%>%
    #filter(ProductName)
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
    #filter(StoreName=="Adyar") %>%
    #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
    #filter(BillDate=='2021-05-08')%>%
    select(Department,StoreName,ProductName,Amount,BillDate)%>%
    group_by(ProductName,Department)%>%
    summarise(sum_total=sum(Amount))%>%
    group_by(Department)%>%
    top_n(5,sum_total)%>%
    #ungroup()%>%
    mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
    mutate(Department=fct_reorder(Department,sum_total))%>%
    #filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
    arrange(desc(sum_total))%>%
    ggplot(aes(sum_total,ProductName,fill=Department))+
    geom_col()+
      #facet_wrap(~StoreName,scales="free_x")+
   # geom_text(aes(label=ProductName),size=2,hjust=1,color="black")+
    scale_x_comma()+
    scale_fill_brewer(palette = "Paired")+
      guides(fill = guide_legend(reverse = TRUE))+
  
    #coord_flip()+
    theme_classic()+
    labs(title = "Top 5 Sales in Each Categories",
         subtitle="2 Day Sales : Dates considered May 8 & 9",
         x="Total Sales (INR)",y= "Products")
  
  
  ggsave("Top5_2Days.png")
  


  chs_aug %>%
    mutate(BillDate=as.Date(BillDate))%>%
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
    filter(StoreName=="Cheers Annanagar") %>%
    #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
   # filter(BillDate=='2021-05-08')%>%
    select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
    group_by(ProductName,Department)%>%
    summarise(sum_total=sum(Amount))%>%
    group_by(Department)%>%
    top_n(5,sum_total)%>%
    ungroup()%>%
    mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
    filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
    arrange(desc(sum_total))%>%
    ggplot(aes(sum_total,ProductName,fill=Department))+
    geom_col(show.legend = FALSE,position = "stack")+
    # geom_text(aes(label=ProductName),size=2,hjust=1,color="black")+
    #scale_x_comma()+
    scale_fill_brewer(palette = "Paired")+
    facet_wrap(~Department,scales="free")+
    #coord_flip()+
    theme_classic()+
    expand_limits(x=c(0,1000))+
    labs(title = "Top 5 in Each Categories",subtitle="Dates considered May 8 and May 9 for Cheers Adyar",
         x="Total Sales (INR)",y= "Products")
  
  #Timeline
  cheers_sales_data %>%
    mutate(BillDate=as.Date(BillDate))%>%
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
   # filter(StoreName=="Cheers Annanagar") %>%
    #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
    #filter(BillDate=='2021-05-08')%>%
    select(StoreName,ProductName,ProductCode,Amount,BillDate)%>%
    group_by(StoreName,BillDate)%>%
    summarise(sum_total=sum(Amount))

  
  #Aug6
  
  library(hrbrthemes)
  library(lubridate)
  library(formattable)
  library(plotly)

 chr_daily <- read_csv("chr_daily.csv")

 chr_daily_fmt <- chr_daily%>%
   mutate(dt = dmy(DATE))%>%
mutate(weekday=factor(weekdays(dt,T),levels = rev(c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun"))))%>%
   mutate(year=format(dt,'%Y'))%>%
   mutate(week=as.numeric(format(dt,"%W")))

 
 

#heat map daily graph
ggplot(chr_daily_fmt, aes(x = week, y = weekday, fill = Tot_sales)) +
  labs(title = "Daily Sales Perspective",
       subtitle = "Empty Rows in-between are Lockdown Protocol Days")+
   viridis::scale_fill_viridis(name="Total Sales in INR",
                               option = 'D',
                               direction = 1,
                               na.value = "grey93") +
   geom_tile(color = 'white', size = 0.04) +
   facet_wrap('year', ncol = 1) +

   scale_x_continuous(
     expand = c(0, 0),
     breaks = seq(1, 52, length = 12),
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
   theme_ipsum(plot_title_family = 'Slabo 27px')

cheers_s%>%
  count(StoreName)
chs_aug %>%
  count(AlternateStoreCode,StoreName)

 #top five in shiny

sales_nov %>%
 # mutate(BillDate=dmy(BillDate))%>%
  #filter(SupplierName=="DOOR AND KEY AGENCY")%>%
  filter(!Department=="BABY CARE")%>%
 # filter(StoreName=="Adyar") %>%
  #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
  #filter(BillDate=='2021-05-08')%>%
  select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
  group_by(ProductName,Department)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%
  top_n(5,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  #filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
  arrange(desc(sum_total))%>%
  ggplot(aes(sum_total,ProductName,fill=Department))+
  #geom_col()+
  # geom_text(aes(label=ProductName),size=2,hjust=1,color="black")+
  #scale_x_comma()+
geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(hjust = 1))+
  #scale_fill_manual() +
  #guides(fill = guide_legend(nrow = 5, byrow=TRUE, ncol=5))+
  #scale_fill_brewer(palette = "Paired")+
  #theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Top 5 Sales in Each Categories (Cheers Adyar)",
       x="Total Sales (INR)",y= "Products")+
  coord_flip()+
  facet_wrap(~Department,scale="free")+
  geom_text(aes(label=ProductName))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cheers_tree <- sales_all %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  #filter(StoreName=="Adyar") %>%
  #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
  #filter(BillDate=='2021-05-08')%>%
  select(Department,StoreName,ProductName,Amount,BillDate)%>%
  group_by(ProductName,Department)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%
  #top_n(5,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  #filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
  arrange(desc(sum_total))


treemap(cheers_tree,index = "Department", vSize = "sum_total",type="index",
        title = "Department Relative Size on Sales", palette="Set3")


library(readr)
library(treemap)

#Sales Data Aug 23 2021
sales_nov <- read_csv("SalesNov.csv",skip=5)



sales_nov%>%
  clean_names()%>%

  filter(SupplierName == "Balaji Agencies")


sales_nov%>%
 filter(StoreName=="Adyar")%>%
  select(BillDate,BillTime,BillNumber,CustomerName,Amount,ProductName,Department)%>%
  filter(Department=="FRUITS AND VEG")

sales_nov %>%
  mutate(BillDate=dmy(BillDate))%>%
  filter(BillDate > "2021-07-04")%>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  #filter(StoreName=="Adyar") %>%
  filter(SupplierName=="INDRA AGENCIES")%>%
  #filter(grepl('', ProductName))%>%
  select(Department,StoreName,ProductName,ProductCode,Amount,BillDate,SupplierName)%>%
  group_by(ProductName,Department,SupplierName)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%summarise(tot_Sales=sum(sum_total))
  
  
  
  sales_nov %>%
    mutate(BillDate=dmy(BillDate))%>%
    #mutate(BillDate=as.Date(BillDate))%>%View()
    filter(BillDate > "2021-07-11")%>%
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
    #filter(StoreName=="Adyar") %>%
    filter(SupplierName=="SRI GOKUL STORES")%>%
    #filter(grepl('GRB', ProductName))%>%
    select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
    group_by(ProductName,Department)%>%
    summarise(sum_total=sum(Amount))%>%
    group_by(Department)%>%
  summarise(tot_Sales=sum(sum_total))
  
  
  sales_nov %>%
    mutate(BillDate=dmy(BillDate))%>%
    #mutate(BillDate=as.Date(BillDate))%>%View()
    filter(BillDate > "2022-31-01")%>%
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
    #filter(StoreName=="Adyar") %>%
    #filter(SupplierName=="SHYAM ENTERPRISES")%>%
    filter(grepl('GRB', ProductName))%>%
    select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
    group_by(ProductName,Department)%>%
    summarise(sum_total=sum(Amount))%>%
    group_by(Department)%>%
    summarise(tot_Sales=sum(sum_total))
  

#Sep 2
cheers_sales_data %>%
  #filter(grepl('J.S', SupplierName))%>%
  #select(SupplierName)%>%unique(SupplierName)
#filter(StoreName=="Adyar") 
  filter(SupplierName=="J.S AGENCIES")%>%
  mutate(BillMonth=month(ymd(BillDate)),BillYear=year(ymd(BillDate)))%>%
  select(Department,StoreName,ProductName,ProductCode,MRP,Amount,BillDate,BillTime,BillMonth,BillYear)%>%
  group_by(Department,BillYear,BillMonth)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))



cheers_sales_data %>%
  #filter(grepl('J.S', SupplierName))%>%
  #select(SupplierName)%>%unique(SupplierName)
  #filter(StoreName=="Adyar") 
  filter(SupplierName=="GL DISTRIBUTORS LLP")%>%
  mutate(BillMonth=month(ymd(BillDate)),BillYear=year(ymd(BillDate)))%>%
  select(Department,StoreName,ProductName,ProductCode,MRP,Amount,BillDate,BillTime,BillMonth,BillYear)%>%
  group_by(Department,BillYear,BillMonth)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))

range(sales_nov$BillDate)


cheers_total_sales %>%
 # filter(grepl('pamban', SupplierName))%>%
  #select(SupplierName)
  #filter(StoreName=="Adyar") 
  filter(SupplierName=="NIRMAL DISTRIBUTORS")%>%
  mutate(BillMonth=month(ymd(BillDate)),BillYear=year(ymd(BillDate)))%>%
  select(Department,StoreName,ProductName,ProductCode,MRP,Amount,BillDate,BillTime,BillMonth,BillYear)%>%
  group_by(Department,ProductName,BillDate)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))





cheers_total_sales %>%
  # filter(grepl('pamban', SupplierName))%>%
  #select(SupplierName)
  #filter(StoreName=="Adyar") 
  filter(SupplierName=="NIRMAL DISTRIBUTORS")%>%
  mutate(BillMonth=month(ymd(BillDate)),BillYear=year(ymd(BillDate)))%>%
  select(Department,StoreName,ProductName,ProductCode,MRP,Amount,BillDate,BillTime,BillMonth,BillYear)%>%
  group_by(Department,ProductName,BillDate)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))

cheers_total_sales <- read_csv("SalesOct.csv",skip = 5)

mutate(dt = dmy(DATE))

cheers_total_sales%>%
  filter(SupplierName=="V DO EVENTS")%>%
  #filter(BillMonth>4)%>%
  select(Department,RegionName,StoreName,ProductName,ProductCode,MRP,Amount,BillDate,BillTime,BillMonth,BillYear)%>%
  group_by(Department)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))


cheers_total_sales <-cheers_total_sales%>%
  mutate(BillDate = dmy(BillDate))%>%
  mutate(BillMonth=month(BillDate),BillYear=year(BillDate))

sva_pro <- cheers_total_sales%>%
  filter(StoreName=="cheers Ayanavaram")%>%
  filter(BillMonth>6)%>%
  filter(BillDate<"2021-10-20")%>%
  select(BillDate,BillMonth,Amount)%>%
  group_by(BillDate)%>%
  summarise(Total=sum(Amount))


ang_pro <- cheers_total_sales%>%
  filter(StoreName=="Cheers Annanagar")%>%
  filter(BillMonth>6)%>%
  filter(BillDate<"2021-10-20")%>%
  select(BillDate,BillMonth,Amount)%>%
  group_by(BillDate)%>%
  summarise(Total=sum(Amount))


ady_pro <- cheers_total_sales%>%
  filter(StoreName=="Adyar")%>%
  filter(BillMonth>6)%>%
  filter(BillDate<"2021-10-20")%>%
  select(BillDate,BillMonth,Amount)%>%
  group_by(BillDate)%>%
  summarise(Total=sum(Amount))

cheers_total_sales%>%
  count(StoreName)

cheers_total_sales%>%
  mutate(BillDate=as.Date(BillDate))%>%
  mutate(BillMonth=month(BillDate),BillYear=year(BillDate))%>%
  filter(StoreName=="Adyar")%>%
  #filter(grepl('DHARS*', SupplierName))%>%
  filter(SupplierName=="DHARSHAN FRUIT AND VEGETABLE")%>%
  #filter(Department=="FRUITS AND VEG")%>%
  filter(BillMonth>9)%>%
  # filter(Department=="GROCERIES")%>%
  select(Department,RegionName,SupplierName,StoreName,ProductName,ProductCode,MRP,Amount,BillDate,BillTime,BillMonth,BillYear)%>%
  group_by(Department,SupplierName)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))

range(cheers_total_sales$BillDate)


cheers_total_sales%>%
  filter(grepl('V DO*', SupplierName))%>%select(SupplierName)
  #filter(grepl('TROPICANA*', ProductName))%>%
  #select(SupplierName)%>%View()
  group_by(ProductName)%>%count()%>%
  arrange(desc(n))%>%
  filter(n>10)%>%View()
  
  
cheers_total_sales%>%
mutate(BillDate=as.Date(BillDate))%>%
  filter(BillDate>'2021-03-01')


#Testing


liquid <- data.frame(val = c(0.5, 0.4))

liquid |> 
  e_charts() |> 
  e_liquid(val) 

ch_echarts_trans<-ch_echarts%>%
  filter(StoreName%in% c("Adyar","Cheers Annanagar","cheers Ayanavaram"))%>%
  group_by(Department,StoreName)%>%
  summarise(tot=sum(Amount))



dat <-   ch_echarts %>% 
  group_by(Department,StoreName,mnths)%>%
  summarise(tot=sum(Amount))

## plot
dat %>%
  dplyr::group_by(StoreName) %>% 
  echarts4r::e_charts(unique(mnths)) %>% 
  echarts4r::e_tooltip(trigger = "item") %>% 
  echarts4r::e_x_axis(axisTick = list(interval = 0)) %>% 
  echarts4r::e_color(unique(dat$Department)) %>% 
  echarts4r::e_toolbox(bottom = 0) %>% 
  echarts4r::e_toolbox_feature(feature = "dataZoom") %>% 
  echarts4r::e_toolbox_feature(feature = "dataView")


my_colors <- tibble::tibble(
  Entity = continents,
  color = rcartocolor::carto_pal(n = 7, name = "Antique")
)

## filter selected and add oclor information
dat <-
  crops %>% 
  dplyr::filter(Crop %in% input$crop_select3) %>%
  dplyr::left_join(my_colors, by = "Entity") 


library(dplyr)

mtcars |>
  tibble::rownames_to_column("model") |> 
  mutate(total = mpg + qsec) |>
  arrange(desc(total)) |>
  e_charts(model) |>
  e_bar(mpg, stack = "grp") |>
  e_bar(qsec, stack = "grp")



library(ggiraph)
library(ggplot2)
dataset <- mtcars
dataset$carname <- row.names(dataset)
gg_scatter <- ggplot(dataset, aes(x = disp, y = qsec, tooltip = carname, data_id = carname, color= wt) ) + 
  geom_point_interactive(size=3)

girafe(ggobj = gg_scatter, 
       options = list(opts_selection(type = "single", only_shiny = TRUE)) )


#Sep 8

library("ggpubr")
library("wesanderson")
library("pdftools")
library("showtext")

chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')%>%mutate(sum=sum(gross_inv_chain))%>%
  group_by(meta_cat)%>%
  summarise(gross_inv_chain=sum(gross_inv_chain),
            percentage=gross_inv_chain*100/147687008)%>%
  ungroup()%>%
  arrange(desc(percentage))%>%
  mutate(factor=factor(percentage),
         gross_inv_chain=round(gross_inv_chain))


ggplot(chain_investment,aes(ymax=gross_inv_chain,ymin=0,xmax=4,xmin=3))+
  geom_rect(fill="red")+
  geom_rect(ymin=0,ymax=60000000,fill="red",alpha=0.1)+
  facet_wrap(~fct_rev(factor),ncol=4)+
  coord_polar(theta = "y")+
 # ylim(c(0,60000000))+
#  xlim(c(-1, 7))+
  theme_void()+ 
  geom_text(aes(x=6,y=0,label=meta_cat,
                family= "Times New Roman"),size=4)+
  geom_text(aes(x=-1,y=0,label=gross_inv_chain,
                family= "Times New Roman"),size=4,col="red")+
  #theme_void()+ 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
     plot.title=element_text(),
    plot.subtitle = element_text(),
    plot.caption = element_text(),
    legend.position = "none",
    plot.margin = unit(c(1,1,1,1),"cm")
  )+
  labs(title="U.S. Infrastructure Investments",
       subtitle = "Gross investment (chained 2021 dollars) in millions of USD",
       caption="Data from Bureau of Economic Analysis | Visualisation by Marius Grabow")


chs_stock <- read_csv("stock.csv")


glimpse(chs_stock)

chs_stock_trimmed<-chs_stock %>%
  filter(Stock!=36)%>%
  mutate(percentage= ((Stock - SaleQuantity)/Stock)*100)%>%
  select(percentage,Stock,SaleQuantity,StoreName,ProductName,Department,StoreBrand,SupplierName)



chs_stock_trimmed$percentage = replace_na(chs_stock_trimmed$percentage,0)

chs_stock_trimmed%>%
  count(StoreName)

#write to csv for shiny upload
#write_csv(sales_oct,"shinyoctsales.csv")

#write_csv(pur_oct,"shinyoctpur.csv")


cheers_total_sales<-cheers_total_sales%>%
  select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
         Amount,BillTime,BillMonth,BillYear)



chs_stock_percentplot <- chs_stock_trimmed%>%
  filter(StoreName=="Adyar")%>%
   filter(Department!="FRUITS AND VEG")%>%
  filter(Department!="Unknown")%>%
#  filter(percentage>1)%>%
  group_by(Department)%>%
  summarise(gross_inv_chain=mean(Stock),
            StockPercent=round(mean(percentage),2))%>%
  arrange(desc(StockPercent))%>%
  mutate(factor=factor(StockPercent),
         gross=rescale(gross_inv_chain))







chs_stock_percentplot%>%
  ggplot(aes(ymax=StockPercent,ymin=0,xmax=4,xmin=3))+
  geom_rect(fill="#FF0000")+
  geom_rect(ymin=0,ymax=110,fill="red",alpha=0.2)+
  coord_polar(theta = "y")+
  facet_wrap(~fct_rev(factor),ncol=4)+
  theme_void()+ 
  geom_text(aes(x=7,y=1,label=Department,
                family= "Times New Roman"),size=4)+
  geom_text(aes(x=-1,y=1,label=paste0(factor," %"),
                family= "Times New Roman"),size=3,col="red")+
  #theme_void()+ 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    plot.title=element_text(),
    plot.subtitle = element_text(),
    plot.caption = element_text(),
    legend.position = "none",
    plot.margin = unit(c(1,1,1,1),"cm")
  )+
  labs(title="Cheers Stocks Availability - (Anna Nagar)",
       subtitle = "Period : Jan - Aug 2021",
       caption="Data from CRPL | Visualisation by Zaprify")
  

chs_stock_trimmed%>%
  group_by(SupplierName)%>%
  summarise(StockPercent=mean(percentage))
  


library(echarts4r) # charts

library(prophet) # forecasting
library(nycflights13) # data


pie <- count(flights, origin) %>% 
  e_charts(x = origin) %>% 
  e_pie(n, legend = FALSE, name = "Flights") %>% 
  e_tooltip() %>% 
  e_title("Flights by origin", "This is really hard with ggplot2")
pie


chs_stock_percentplot

pie <- chs_stock_percentplot %>%  
  filter(Department %in% c('SNACKS AND BISCUITS','BEVERAGES','DAIRY AND FROZEN'))%>%
  e_charts(x = Department) %>% 
  e_pie(StockPercent, legend = FALSE,name="Stocks Avaialbe in %") %>% 
  e_tooltip() %>% 
  e_title("Flights by origin", "This is really hard with ggplot2")
pie


top_destinations <- flights %>% 
  count(dest) %>% 
  top_n(15, n) %>% 
  arrange(desc(n))

top_destinations %>%
  e_charts(x = dest) %>%
  e_bar(n, legend = FALSE, name = "Flights") %>% 
  e_labels(position = "right") %>% 
  e_tooltip(
    formatter = e_tooltip_item_formatter("percent")
  ) %>% 
  e_title("Flights by destination", "Top 15 destinations") %>% 
  e_flip_coords() %>% 
  e_y_axis(splitLine = list(show = FALSE)) %>% 
  e_x_axis(show = FALSE) %>% 
  e_toolbox_feature(
    feature = "saveAsImage",
    title = "Save as image"
  )


flights_ts <- flights %>% 
  transmute(week = as.Date(cut(time_hour, "month")), dep_delay, origin) %>% 
  group_by(origin, week) %>% # works with echarts
  summarise(dep_delay = sum(dep_delay, na.rm = TRUE))


ts_base <- flights_ts %>% 
  e_charts(x = week) %>% 
  e_datazoom(
    type = "slider", 
    toolbox = FALSE,
    bottom = -5
  ) %>% 
  e_tooltip() %>% 
  e_title("Departure delays by airport") %>% 
  e_x_axis(week, axisPointer = list(show = TRUE))

ts_base %>% e_line(dep_delay)
ts_base %>% e_area(dep_delay, stack = "grp")

cv_ps <- read_csv("cv_ps.csv")

cv_ps_tr <- cv_ps%>%
  mutate(dt = dmy(Months))%>%
  transmute(week = as.Date(cut(dt, "month")),Amount,Type)%>%
  group_by(Type, week)%>%
  summarise(Amount = sum(Amount, na.rm = TRUE))

#write_csv(cv_ps_tr,"ps_tr.csv")  


ps_cheers <- cv_ps_tr%>%
  e_charts(x = week) %>% 
  e_datazoom(
    type = "slider", 
    toolbox = TRUE,
    bottom = -5
  ) %>% 
  e_tooltip() %>% 
  e_title(text = "Cheers Market : Purchase vs Sales",
          subtext = "Period : Apr 2018 - Mar 2021") %>% 

  e_x_axis(week, axisPointer = list(show = TRUE))%>%
  echarts4r::e_toolbox(top = 0) %>% 
  echarts4r::e_toolbox_feature(feature = "dataZoom") %>% 
  echarts4r::e_toolbox_feature(feature = "dataView")%>%
  e_toolbox_feature(
    feature = "saveAsImage",
    title = "Save as image")




ps_cheers %>% e_line(Amount)
ps_cheers %>% e_area(Amount, stack = "grp")


####Sales & Purchase

ch_p_js2021%>%
  count(ReferenceStoreName)

range(ch_s_js2021$BillDate)

ch_s_js2021 <- read_csv("SalesSep.csv",skip=5)
ch_p_js2021 <- read_csv("Purchase.csv",skip=6)
#rm(ch_s_js2021)
range(ch_p_js2021$GRNDate)
unique(ch_s_js2021$SupplierName)
ch_s_js2021%>%
  mutate(BillDate=as.Date(BillDate))%>%
  mutate(BillMonth=month(BillDate),BillYear=year(BillDate))%>%
  select(SupplierName,Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
  filter(grepl('irene*',SupplierName))
  filter(SupplierName=="T.A.V.PRODUCTS PRIVATE LIMITED")%>%
  filter(BillMonth>4)
  
  
  
 # cheers_total_sales_old <-cheers_total_sales
  
  #cheers_pur_old <- cheers_pur

  
  cheers_total_sales%>%
    filter(SupplierName=="A.G.AGENCIES")%>%
    filter(BillMonth>3)%>%
    select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
           Amount,BillTime,BillMonth,BillYear)%>%
    group_by(Department,SupplierName)%>%
    summarise(sales=sum(Amount))%>%
    arrange(desc(sales))
  
  #shiny app for pur vs sales october 20 2021
  
  

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
#   
  #shiny app
  
  
  sales_oct%>%
  #  filter(grepl("DHARSHAN *",SupplierName))%>%View()
    filter(SupplierName=="DHARSHAN FRUIT AND VEGETABLE")%>%
    filter(BillMonth>9)%>%
    filter(BillDate>"2021-10-01")%>%
    select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
           Amount,BillTime,BillMonth,BillYear)%>%
    group_by(Department,SupplierName)%>%
    summarise(sales=sum(Amount))%>%
    arrange(desc(sales))
  
  
  sales_oct%>%
    #  filter(grepl("DHARSHAN *",SupplierName))%>%View()
    filter(SupplierName=="DHARSHAN FRUIT AND VEGETABLE")%>%
    filter(BillMonth>9)%>%
    filter(BillDate<"2021-10-04")%>%
    select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
           Amount,BillTime,BillMonth,BillYear)%>%
    group_by(StoreName)%>%
    summarise(sales=sum(Amount))%>%
    arrange(desc(sales))
  
  
  
  
  
  
  pur_oct%>%
    filter(SupplierName =="DHARSHAN FRUIT AND VEGETABLE")%>%
    filter(mnths>9)%>%
    filter(StoreName=="Adyar")%>%
    filter(GRNDate>"2021-10-01")%>%
    select(Department,GRNDate,StoreName,SupplierName,ProductName,
           Quantity,MRP,Amount,PurchasePrice,mnths)%>%
    group_by(StoreName,ProductName)%>%
    summarise(sales=sum(Amount))%>%
    arrange(desc(sales))
  
  
  
  sales_all %>%
    filter(!Department=="Unknown")%>%
    filter(!Department=="BABY CARE")%>%
    filter(!grepl('RUKMINI', ProductName))%>%
    filter(!grepl('Aavin', ProductName))%>%
    filter(!grepl('ONION', ProductName))%>%
    #filter(!grepl('UPF', ProductName))%>%
    #filter(StoreName=="Adyar") %>%
    select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
    group_by(ProductName)%>%
    summarise(sum_total=sum(Amount))%>%
    arrange(desc(sum_total))%>%
    top_n(20,sum_total)%>%
    ungroup()
  
  
  ##detailed analysis
  sales_oct%>%
    filter(SupplierName=="ARCHANA TRADING")%>%
    filter(BillMonth>7)%>%
    select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
           Amount,BillTime,mnths=BillMonth,BillYear)%>%
    group_by(ProductName,Department)%>%
    summarise(sales=sum(Amount))%>%
    arrange(desc(sales))%>%inner_join(pur_oct%>%
                                        filter(SupplierName =="ARCHANA TRADING")%>%
                                        filter(mnths>7)%>%
                                        select(Department,GRNDate,StoreName,SupplierName,ProductName,
                                               Quantity,MRP,Amount,PurchasePrice,mnths)%>%
                                        group_by(ProductName,Department)%>%
                                        summarise(purchased=sum(Amount))%>%
                                        arrange(desc(purchased)), by="ProductName")%>%
    select(  ProductName ,purchased,sales)%>%
    group_by( ProductName)%>%
    summarise(sold = sum(sales),purchase=sum(purchased))%>%
    # summarize(spark = list(sold,purchase), .groups = "drop")%>%
   gt()%>%gt_theme_espn()%>%gt_color_rows(purchase:sold, palette = "ggsci::blue_material")
  
  
  
  gt_sparkline_tab <- mtcars %>%
    dplyr::group_by(cyl) %>%
    # must end up with list of data for each row in the input dataframe
    dplyr::summarize(mpg_data = list(mpg), .groups = "drop") %>%
    gt() %>%
    gt_sparkline(mpg_data)
  
library(htmltools)
install.packages("htmltools")
library(bslib)  
remotes::install_github("rstudio/htmltools")


##Burnout##

bo <- read_csv("exp.csv")

bo%>%
  

#####November10

sales_nov <- read_excel("SalesNov.xlsx",skip=5)
range(sales_nov$BillDate)

sales_nov%>%
  count(StoreName)

sales_nov%>%
 # group_by(StoreName)%>%
 filter(StoreName=="Adyar")%>%
  filter(BillMonth>5)%>%
group_by(BillYear)%>%
  summarise(avgsale=round(mean(sum(Amount))/7))

sales_nov%>%
  filter(StoreName=="Cheers Nungambakkam")%>%
  filter(BillMonth>11)%>%
  group_by(BillYear)%>%
  summarise(avgsale=round(mean(sum(Amount))))

sales_nov %>%
#filter(SupplierName=="DOOR AND KEY AGENCY")%>%
  filter(!Department=="BABY CARE")%>%
  # filter(StoreName=="Adyar") %>%
  #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
  #filter(BillDate=='2021-05-08')%>%
  select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
  group_by(ProductName,Department)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%
  top_n(5,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  #filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
  arrange(desc(sum_total))%>%
  ggplot(aes(sum_total,ProductName,fill=Department))+
  #geom_col()+
  # geom_text(aes(label=ProductName),size=2,hjust=1,color="black")+
  #scale_x_comma()+
  geom_bar(stat = "identity") +
  coord_flip()+
  theme(axis.text.x = element_text(hjust = 1))+
  #scale_fill_manual() +
  #guides(fill = guide_legend(nrow = 5, byrow=TRUE, ncol=5))+
  #scale_fill_brewer(palette = "Paired")+
  #theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Top 5 Sales in Each Categories (Cheers Adyar)",
     x="Total Sales (INR)",y= "Products")+

  facet_wrap(~Department,scale="free")+
  geom_text(aes(label=ProductName))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cheers_tree <- sales_nov %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  #filter(StoreName=="Adyar") %>%
  #filter(ProductName=="Aavin Blue Magic Milk 500Ml")%>%
  #filter(BillDate=='2021-05-08')%>%
  select(Department,StoreName,ProductName,Amount,BillDate)%>%
  group_by(ProductName,Department)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%
  #top_n(5,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  #filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
  arrange(desc(sum_total))

treemap(cheers_tree,index = "Department", vSize = "sum_total",type="index",
        title = "Department Relative Size on Sales", palette="Set3")

plotly(p)
library(readr)
library(treemap)

#Sales Data Aug 23 2021
sales_nov <- read_csv("SalesNov.csv",skip=5)



sales_nov%>%
  clean_names()%>%
  
  filter(SupplierName == "Balaji Agencies")


sales_nov%>%
  filter(StoreName=="Adyar")%>%
  select(BillDate,BillTime,BillNumber,CustomerName,Amount,ProductName,Department)%>%
  filter(Department=="FRUITS AND VEG")

sales_nov %>%
  mutate(BillDate=dmy(BillDate))%>%
  filter(BillDate > "2021-07-04")%>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  #filter(StoreName=="Adyar") %>%
  filter(SupplierName=="INDRA AGENCIES")%>%
  #filter(grepl('', ProductName))%>%
  select(Department,StoreName,ProductName,ProductCode,Amount,BillDate,SupplierName)%>%
  group_by(ProductName,Department,SupplierName)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%summarise(tot_Sales=sum(sum_total))


#unique customers
sales_nov%>%
  distinct(CustomerMobile)%>%
  nrow()

sales_nov%>%
  group_by(BillNumber,CustomerName)%>%
  summarise(tot=sum(Amount))%>%
  arrange(desc(tot))%>%
  ungroup()%>%
top_n(1,wt=tot) %>%
  select(tot)

sales_nov %>%
  #filter(SupplierName=="DOOR AND KEY AGENCY")%>%
  filter(Department=="BEVERAGES")%>%
  filter(StoreName=="Cheers Annanagar") %>%
 filter(ProductName=="7UP 1.5 LTR")%>%
  filter(BillDate>'2021-10-01')%>%
  select(Department,StoreName,ProductName,ProductCode,Amount,BillDate)%>%
  group_by(ProductName,Department)%>%
  summarise(sum_total=sum(Amount))%>%
  group_by(Department)%>%
  top_n(5,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  #filter(Department%in% c('DAIRY AND FROZEN',"BEVERAGES","GROCERIES"))%>%
  arrange(desc(sum_total))%>%
  ggplot(aes(sum_total,ProductName,fill=Department))+
  #geom_col()+
  # geom_text(aes(label=ProductName),size=2,hjust=1,color="black")+
  #scale_x_comma()+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(hjust = 1))+
  #scale_fill_manual() +
  #guides(fill = guide_legend(nrow = 5, byrow=TRUE, ncol=5))+
  #scale_fill_brewer(palette = "Paired")+
  #theme_classic()+
  theme(legend.position = "none")+
  labs(title = "Top 5 Sales in Each Categories (Cheers Adyar)",
       x="Total Sales (INR)",y= "Products")+
  coord_flip()+
  facet_wrap(~Department,scale="free")+
  geom_text(aes(label=ProductName))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

sales_nov%>%
  count(StoreName)


sales_nov %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  filter(StoreName=="cheers Ayanavaram") %>%
  
  
  select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
  group_by(Department)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))%>%
  ungroup()%>%
  mutate(Department=fct_reorder(Department,sum_total))%>%
  ggplot(aes(Department,sum_total,fill=Department))+
  geom_col(show.legend = FALSE,color="black")+
  geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
  #scale_y_comma()+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+
  theme_classic()+
  #geom_text(aes(label=scales::comma(sum_total)), hjust=0, nudge_y=2000) +
  scale_y_comma(limits=c(0,600000))+
  labs(title = "Top Selling categories ",
       subtitle="Groceries,Packed Foods & Dairy Products are the Top Sellers",x="Categories",y= "Total sales")





unique(sales_nov$StoreName)
#November total sales across stores

sales_nov%>%
  group_by(StoreName)%>%
  summarise(TotalSales=round(sum(Amount)))%>%
  arrange(desc(TotalSales))%>%
  gt::gt() %>%
  #gt::opt_table_lines()%>%
  gt_plt_dot(column = TotalSales, category_column = StoreName,  max_value = 2000000,
             palette = c("#1C7947", "#90AACB", "#63d64a", "#C56824", "#4fabf7", "grey")) %>%
  #gt_theme_538()%>%
  gt_theme_nytimes() %>% 
  tab_header(title = "Total Sales across stores till Nov 9 2021")%>%
  # trim gives smaller range of colors
  # so the green and purples are not as dark
  gt_color_rows(TotalSales, palette = "ggsci::blue_material")%>%
  cols_width(StoreName ~ px(300), 2 ~ px(90))

Rukmini <- sales_nov%>%
  #filter(BillNumber=="SC13140")%>%
  group_by(BillNumber,ProductName)%>%
  summarise(tot=sum(Amount ))%>%
  filter(grepl("RUKMINI",ProductName))


Non_Rukmini <- sales_nov%>%
  #filter(BillNumber=="SC13140")%>%
  group_by(BillNumber,ProductName)%>%
  summarise(tot=sum(Amount ))%>%
  filter(!grepl("RUKMINI",ProductName))
  
#analysing rukmini percentage presence in basket

Rukmini_joined <- Rukmini%>%
  inner_join(Non_Rukmini,by="BillNumber")

Rukmini_wojoined<-Non_Rukmini%>%
  anti_join(Rukmini_joined,by="BillNumber")

Rukmini_wojoined%>%
  distinct(BillNumber)
  


Ruk_Bas<-Rukmini_joined%>%
  distinct(BillNumber)%>%
  nrow()

NonRuk_Bas<-sales_nov%>%
  distinct(BillNumber)%>%
  nrow()


tibble(Ruk_Bas,NonRuk_Bas)

sales_nov%>%
  filter(BillNumber=="SC12861")%>%
  select(ProductName)
sales_nov %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  filter(!Department=="FRUITS AND VEG")%>%
  filter(!grepl('Aavin', ProductName))%>%
  filter(StoreName!="Adyar") %>%
  select(Department,BillNumber,StoreName,ProductName,MRP,Amount,BillDate)%>%
  group_by(BillNumber)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))%>%
  top_n(10,sum_total)%>%
  ungroup()%>%
  mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
  ggplot(aes(ProductName,sum_total,fill=ProductName))+
  geom_col(show.legend = FALSE,color="black")+
  geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
  scale_y_comma()+
  scale_fill_manual(values=cbPalette)+
  #scale_fill_distiller()+
  coord_flip()+
  theme_few()+
  #theme_classic()+
  labs(title = "Top Selling Products ",
       subtitle="Excluding Daily Needs (F&V , Aavin Milk) ",x="",y= "sales",
       caption = "Viz : Zaprify , Data : CRPL")





sales_nov <- read_csv("SalDec1.csv",skip=5)
pur_nov <- read_csv("PurDec1.csv",skip=6)

#write to csv for shiny upload updated nov 15 with nov 14 data
write_csv(sales_nov,"shinynovsales.csv")

write_csv(pur_nov,"shinynovpur.csv")


  sales_nov <- sales_nov %>%
    filter(StoreName!="Teynampet")%>%
    mutate(BillDate=dmy(BillDate),
           BillMonth=month(BillDate),
           BillYear=year(BillDate))%>%
    select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,Amount,BillTime,BillMonth,BillYear)



  pur_nov <- pur_nov %>%
    mutate(GRNDate=dmy(GRNDate),
           mnths=month(GRNDate),
           BillYear=year(GRNDate))%>%
    select(Department,GRNDate,SupplierName,StoreName,ProductName,Quantity,
      MRP,Amount,PurchasePrice,mnths)
  chrs_acc <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/cheers1.csv',skip=2)


chrs_acc <- chrs_acc%>%clean_names()
colnames(chrs_acc)
chrs_acc%>%clean_names()%>%
  select(date,particulars,instrument_number,instrument_date,credit,cheque_in_hand_issued_on)%>%
  gt()%>%gt_theme_espn()%>%
  tab_header(
    title = ("Cheques in hand updated  ")
  )%>%
  #gt_add_divider(Vendor:sales, color = "grey", weight = px(1)) %>% 
  tab_source_note(md("**Viz**: Zaprify | **Data**: Cheers Retail")) %>% 
  tab_options(
    table.border.bottom.color = "grey",
    table.width = px(800)
  )




chrs_acc2 <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/cheers2.csv',skip=2)

chrs_acc2<- chrs_acc2%>%
  clean_names()%>%
  select(date,particulars,instrument_date,credit,bill_date,payment_type,cheque_in_hand_issued_on,
         cheque_debited_on)
  


chrs_sum <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/summary.csv',skip=1)

chrs_sum <- chrs_sum%>%
  clean_names()
  
##shiny pre work for cheers feb 08 2022
##shiny sales prework


sales_nov <- read_csv("Sal_jan19.csv",skip=5)
pur_novs <- read_csv("Pur_jan19.csv",skip=6)

  


sales_nov <- sales_nov %>%
 #filter(StoreName!="Teynampet")%>%
mutate(BillDate=dmy(BillDate),
BillMonth=month(BillDate),
BillYear=year(BillDate))%>%
select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,Amount,BillTime,BillMonth,BillYear,Quantity)


 
pur_nov <- pur_novs %>%
mutate(GRNDate=dmy(GRNDate),
mnths=month(GRNDate),
BillYear=year(GRNDate),InvoiceDate=dmy(InvoiceDate))%>%
select(Department,GRNDate,InvoiceDate,SupplierName,StoreName,ProductName,Quantity,
MRP,Amount,PurchasePrice,mnths)




write_csv(sales_nov,"shinydecsales.csv")

write_csv(pur_nov,"shinydecpur.csv")

###end of shiny pre work
unique(pur_nov$SupplierName)

colnames(pur_nov)

pur_novs%>%
 # filter(SupplierName== "JOYTHI ASSOCIATES")%>%
  filter(grepl('LAYS ',ProductName))%>%
  filter(StoreName=="Cheers Annanagar")%>%
  #filter(mnths>=input$rateThreshold)%>%
  select(Department,GRNDate,StoreName,SupplierName,ProductName,
         Quantity,MRP,Amount,PurchasePrice,mnths)%>%
  group_by(ProductName,Department)%>%
  summarise(purchased=sum(Amount))%>%
  arrange(desc(purchased))


range(sales_nov$BillDate)
rm(sales_violin)

###Remove##
salesremove%>%
  filter(BillNumber=="SC860")%>%
  View()

  salesremove%>%
    filter(grepl("Suresh",CustomerName))%>%
    filter(grepl("RICE",ProductName))%>%
    dplyr::select(StoreName,ProductName,Amount,BillDate,BillNumber,CustomerName)%>%
    View()
  

  
  
#####

sales_violin <- sales_nov%>%
   # filter(SupplierName=="JOYTHI ASSOCIATES")%>%
  filter(BillDate =='2021-05-02' )%>%
  #filter(StoreName=="Cheers Annanagar")%>%
  #filter(BillMonth>=input$rateThreshold)%>%
 #group_by(BillDate)%>%
  summarise(sales=sum(Amount))%>%
  arrange(desc(sales))



stock <- read_csv("stock.csv")

stock<- stock%>%
  select(StoreName,Department,ProductName,SupplierName,Stock,MRP,MRPValue,TotalPurchasePrice,LastReceiveDate)

stock<-stock%>%
  mutate(Stock=round(Stock),MRP=round(MRP),MRPValue=round(MRPValue),TotalPurchasePrice=round(TotalPurchasePrice))

stock%>%
  filter(StoreName=="Cheers Annanagar")%>%
  filter(Stock>=0 & Stock <=5)%>%
  group_by(SupplierName)%>%
  head(15)%>%
  group_by(Department)%>%
  gt()%>%
    data_color(
      columns = vars(Stock),
      colors = scales::col_numeric(
        palette = c(
          "red", "orange", "green"),
        domain = NULL)
    )

#repalce NA
  sipl1 <- read_csv("sipl1.csv")
  
  sipl1<- sipl1 %>% mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
    clean_names()
  
  
  replace(sipl1$collected,"-",0)
  
  sipl1$x31_60_days<-as.numeric(sipl1$x31_60_days)
colnames(sipl1)


sipl1<-sipl1 %>% mutate(
  across(starts_with("x"), ~as.numeric(.))
)
sipl1<-sipl1 %>% mutate(
  across(total_payable:not_applicable, ~as.numeric(.))
)



###CRM Analysis

CRM <- read_csv("CRM.csv")


CRM <- CRM %>%
  clean_names()%>%
  mutate(service_date=dmy(service_date))

CRM%>%View()

CRM %>%
  ggplot(aes(service_date,received_valve,col=type))+
  geom_line()+
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 36, length = 9),
    labels = c( "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","Jan"))

CRM %>%
  ggplot(aes(fct_reorder(source,received_valve),received_valve,fill=source))+
  geom_col(position = "stack")+
  scale_y_continuous(labels = comma)

####Source Proportion
CRM%>%
  group_by(source)%>%
  summarise(total=sum(received_valve))%>%
  #coulmn wise proportion
  mutate(per = round( 100 * total/sum(total))) %>% 
  ungroup()

#Type Proportion

CRM%>%
  mutate(service_type = case_when(
    str_detect(service_type, "Pest") ~ "Pest Control",
    str_detect(service_type, "CCTV") ~ "CCTV",
    str_detect(service_type, "Chemicals") ~ "Deep Cleaning",
    TRUE ~ service_type
  ))%>%
  group_by(service_type)%>%
  summarise(total=sum(received_valve))%>%
  #coulmn wise proportion
  mutate(per = round( 100 *total/sum(total))) %>% 
  ungroup()
 # summarise(total=sum(received_valve))%>%
  arrange(desc(total))%>%
 # mutate(across(c(source), .fns = list(prop = ~ .x /total), .names = "{.fn}_{.col}"))
  ggplot(aes(fct_reorder(source,-total),total,fill=source))+
  geom_col(position = "stack")+
  scale_y_log10()+
  scale_y_continuous(labels = comma)
  

chrsummary <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/summary.csv',skip=1)

chrsummary<-chrsummary%>%
  clean_names()

chrsummary%>%
  gt()
  
#String Remove
str_remove("Sri Vinayaga Agencies`_468",gsub("^.*_","_","Sri Vinayaga Agencies`_468"))

print(str_replace_all("ATGAS_1121", "_", ""))



tally <- read_csv("tally.csv")
wsoft <- read_csv("wsoft.csv")
wsoft<-wsoft%>%
  select("name"=Wsoft_Name,"wsoft_amnt"=Amount)%>%
  mutate(wsoft_amnt=round(wsoft_amnt,2))


tally<-tally%>%
  mutate(name=str_remove(Tally_Name,gsub("^.*_","_",Tally_Name)))%>%
  select(name,"tally_amnt"=Amount)


tally_Tf <- tally%>%
  inner_join(wsoft,by="name")

tally_Tf%>%
  mutate(diff = tally_amnt-wsoft_amnt)%>%
  gt()%>%
  grand_summary_rows(
    columns = tally_amnt:diff,
    fns = list(
      TOTAL = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 2
  )%>%
  tab_style(
    style = cell_text(color = "#F05454", weight = "bold"),
    locations = cells_body(
      columns = vars(diff),
      rows = diff < 0
    )
  ) %>% 
  tab_style(
    style = cell_text(color = "#4FBDBA", weight = "bold"),
    locations = cells_body(
      columns = vars(diff),
      rows = diff == 0
    )
  )%>%
  tab_style(
    style = cell_text(color = "#F05454", weight = "bold"),
    locations = cells_body(
      columns = vars(diff),
      rows = diff > 0
    )
  )%>%
  tab_style(
    style = list(
      cell_text(style = "italic"),
      cell_fill(color = "lightblue")
    ),
    locations = cells_grand_summary(
      columns = tally_amnt:diff,
      rows = 1)
  )



chrs_sum <- read_csv('https://raw.githubusercontent.com/SimbhuTwice/CheersAcc/main/summary.csv',skip=1)

chrs_sum <- chrs_sum%>%
  clean_names()%>%
  select(s_no:total)



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


stockaudit_1 <- sales_nov%>%
count(Department)

salr <- sales_nov%>%
  filter(Department!="FRUITS AND VEG")%>%
  group_by(Department,ProductName)%>%
  filter(BillMonth<3)%>%
  select(Department,ProductName)%>%
  distinct(ProductName)%>%
  mutate(Store.Qty=0)

salr$Department <- as.factor(salr$Department)
salr$ProductName <- as.factor(salr$ProductName )



write_csv(salr,"testsales.csv")

####Updated sales data jan 20 2022
sal_daily <- sales_nov%>%
  select(Date=BillDate,everything())%>%
  filter(Date >'2022-01-31')%>%
 # filter(BillYear>2021)%>%
  group_by(Date)%>%
  dplyr::summarise(Total = round(sum(Amount)))%>%View()

write_csv(sal_daily,"saldaily.csv")
write_csv(pur_daily,"purdaily.csv")

pur_daily <- pur_nov%>%
  select(Date=GRNDate,Amount)%>%
  filter(Date>'2021-12-01',Amount>0)%>%
  group_by(Date)%>%
  summarise(Total = round(sum(Amount)))

cheque_viz<-cheque_viz%>%
  mutate(Date=dmy(Date))%>%left_join(pur_daily,by='Date')%>%
  mutate(Pur=Total.y)

cheque_viz<- cheque_viz%>%
  mutate(Date=dmy(Date))%>%left_join(sal_daily,by='Date')%>%
  mutate(Sales=Total.y)

cheque_viz$Pur<-cheque_viz%>%
  mutate(Date=dmy(Date))%>%left_join(pur_daily,by='Date')%>%
  mutate(Pur=Total.y)%>%pull(Pur)

cheque_viz$Sales<- cheque_viz%>%mutate(Date=dmy(Date))%>%left_join(sal_daily,by='Date')%>%
  mutate(Sales=Total.y)%>%select(Sales)%>%pull(Sales)

cheque_viz<-cheque_viz%>%
  mutate(Sales=round(Sales))%>%
  mutate(CC=replace_na(CC,0),Pur=replace_na(Pur,0),Sales=replace_na(Sales,0))%>%
group_by(Date)%>%
  mutate(Total=round(sum(Pur+CC+Sales)))%>%
  mutate(Pur_Prop=round(Pur/Total,2)*100,CC_Prop=round(CC/(Total),2)*100,Sale_Prop=round(Sales/(Total),2)*100)%>%
  ungroup()


cheque_viz_join <- cheque_viz%>%
  group_by(Date)%>%
  mutate(
    X25=round(get('Pur_Prop')),
    X50=round(get('CC_Prop')),
    X100=round(get('Sale_Prop'))
  )%>%
  arrange(-X100)%>%
  summarise(Salary = list(c(X25,X50,X100)))


sales_nov%>%
  count(SupplierName)%>%View()

sales_nov %>%
  filter(!Department=="Unknown")%>%
  filter(!Department=="BABY CARE")%>%
  filter(SupplierName=="B AND M HOT BREADS PVT LTD")%>%
  #filter(StoreName=="Adyar") %>%
  select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
  group_by(Department)%>%
  summarise(sum_total=sum(Amount))%>%
  arrange(desc(sum_total))

####Jan 21 2022

cheque_cleared <- read_excel("band.xls",skip=10)

cheque_cleared_dt <- read_csv("band.csv",skip=10)

cheque_icici <- read_csv("icici.csv")

cheque_icici<-cheque_icici%>%
  clean_names()%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(Date=dmy(transaction_date))%>%
  mutate(withdrawal=as.numeric(withdrawal_amt_inr),
         balance=as.numeric(balance_inr))%>%
  select(Date,description=transaction_remarks,withdrawal,balance)%>%
  filter(grepl('CASH', description))

cheque_band<-cheque_cleared%>%
  clean_names()%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(Date=dmy(date))%>%
  filter(grepl('WDL', description)|grepl('SELVAKUMAR', description))%>%
  select(-date)

cheque_cleared%>%
  filter((grepl('WDL', Description)))%>%
           group_by(Date)%>%
  filter(Date>="2022-03-01")%>%
           summarise(sum=sum(Debit))

cheque_cleared%>%
  filter(grepl('DEPOSIT', Description) & grepl('SELVAKUMARBAALU', Description))%>%
  group_by(Date)%>%
  filter(Date>="2022-03-01")%>%
  summarise(sum=sum(Credit))%>%View()
cheque_All <- cheque_band%>%
  left_join(cheque_icici,by="Date")

cheque_icici<-cheque_icici%>%
 # filter(grepl('SelvaAxis', description))%>%
  group_by(Date)%>%
  summarise(ICICI_Withdrawal=sum(withdrawal))

cheque_band<-cheque_band%>%
 filter(grepl('WDL', description))%>%
  group_by(Date)%>%
  summarise(Band_Withdrawal=sum(debit))


cheque_cleared%>%clean_names()
cheque_cleared_dt <- cheque_cleared_dt%>%
  clean_names()%>%
  mutate(ref_cheque_no=as.numeric(ref_cheque_no))%>%
  filter(str_length(ref_cheque_no)< 4)%>%
  mutate(date=dmy(date),debit=str_replace(debit,",",""),
         credit=str_replace(credit,",",""),
         balance=str_replace(balance,",",""))%>%
  mutate(debit=as.numeric(debit),
         credit=as.numeric(credit),
         balance=as.numeric(balance))



cheque_cleared <- cheque_cleared%>%
  select(date,debit)%>%
  group_by(date)%>%
  summarise(amount=sum(debit))%>%
  select(Date=date,amount)


cheque_viz$CC <-  cheque_viz%>%
  mutate(Date=dmy(Date))%>%left_join(cheque_cleared_tf,by='Date')%>%
  pull(amount)


cheque_cleared_dt%>%
  select(date,description,ref_cheque_no,debit)%>%
  gt()%>%
  grand_summary_rows(
    columns = debit,
    fns = list(
      TOTAL = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 0
  )


###TD Receivables

TDR <- read_csv("tdr.csv")

TDR_tf <- TDR%>%
  clean_names()%>%
  mutate(name=tolower(name))%>%
  select(name,company:status)%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )
  


tdr%>%
  clean_names()%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(name=snakecase::to_upper_camel_case(name))%>%
  mutate(date=dmy(date),due_on=dmy(due_on),name=as.factor(name))%>%
  select(date,due_on,invoice_no,name,bill_value,pending_value,final_value=final_pending_value,company,
         Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days)



tdr_tf<-tdr%>%
  clean_names()%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  mutate(name=snakecase::to_upper_camel_case(name))%>%
  mutate(date=dmy(date),due_on=dmy(due_on),name=as.factor(name))%>%
  select(date,due_on,invoice_no,name,bill_value,pending_value,final_value=final_pending_value,company,
         Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days)


tdr_tf%>%
  group_by(name,company)%>%
  summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),billdate=min(date))%>%
  mutate(Actual_Due=as.numeric(due_on-billdate))


tdr_tf2<-tdr_tf%>%
  group_by(name,company)%>%
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
  select(name,company,BillValue,PendingValue,Days_Delayed,due_on)



  TDR_tf %>% 
  #tibble::rownames_to_column(var = "name") %>%
  gt(groupname_col = "status", rowname_col = "company") %>% 
  summary_rows(
    groups = TRUE,
    fns = list(Total = ~sum(.))
  )
  
  TDR_tf%>%
    mutate(name=tolower(name))%>%
    select(name)
  ##Multiple Groups
  
  TDR_tf %>% 
    group_by(status,company)%>%
    gt(rowname_col = "name") %>% 
    summary_rows(
      groups = TRUE,
      fns = list(Total = ~sum(.))
    )%>%
    tab_stubhead("TD Receivables") %>% 
    tab_style(
      style = list(
        cell_fill("#FFC600"),
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_stubhead()
    )%>%
    tab_style(
      style = list(
        cell_fill("#325288"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_row_groups()
    ) %>% 
    tab_style(
      style = cell_text(color = "#92A9BD", weight = "bold",font = google_font("Ubuntu")),
      locations = cells_stub()
    )%>%
    tab_style(
      style = list(
        cell_text(color = "black", font = google_font("Montserrat")),
        cell_fill("#FFC600")
      ),
      locations = cells_summary()
    )
  
  
  

  
  
  

  head(mtcars, 8) %>% 
    tibble::rownames_to_column(var = "name") %>%
    mutate(cyl = paste(cyl, "Cylinders")) %>% 
    group_by(cyl, gear) %>% 
    arrange(cyl) %>%
    gt(rowname_col = "name") %>% 
    summary_rows(
      groups = TRUE,
      fns = list(Average = ~mean(.))
    )
  
  tdr_tf2%>%View()
  
 reactable(tdr_tf2, resizable = TRUE, filterable = FALSE,
                   searchable = TRUE,borderless=FALSE,compact=TRUE,showPageSizeOptions = TRUE, 
                   onClick = "expand", highlight = TRUE, 
           groupBy = "name",
                   theme = reactableTheme(
                     headerStyle = list(
                       "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                       "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                       borderColor = "#555"
                     )
                   ),
                   columnGroups = list(
                     colGroup(name = "Collection Group", columns = knockout_cols2)
                   ),columns = list(name = colDef(name = "name",aggregate = "sum"), 
                                    Days_Delayed=colDef(name = "Days Delayed"),
                                    company=colDef(name = "Company"),
                                    BillValue=colDef(name = "Bill Value"),
                                    PendingValue=colDef(name = "Pending Value"),
                                    due_on=colDef(name = "Due From"),
                                    BillValue=colDef(name = "Bill Value")
                   ), 
           details = function(index) { # i
           sec_lvl = tdr_tf[tdr_tf$name == tdr_tf2$name[index], ] 
           reactable(data       = sec_lvl,
                     compact    = TRUE, 
                     filterable = TRUE,
                     bordered   = TRUE, 
                     resizable  = TRUE,
                     columns    = list(
                       Days_LT30    = colDef(name = "< 30 Days",    width = 70, align = "center"),
                       Days_30to60      = colDef(name = "30 to 60 Days"),
                       Days_60to90      = colDef(name = "60 to 90 Days", width = 90, align = "center"),
                       Days_GT90         = colDef(name = "> 90 Days",    width = 50, align = "center")
                     )
           )
           }
 )
 

  
  ####TD Receivables jan 25 2022
  
  tdr <- read_csv("TDR1.csv")
  
  tdr_tf<-tdr%>%
    clean_names()%>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    )%>%
    mutate(date=dmy(date),due_on=dmy(due_on),name=as.factor(name))%>%
    select(date,due_on,invoice_no,name,bill_value,pending_value,final_value=final_pending_value,company,
           Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days)
  
  
  tdr_tf%>%
    group_by(name,company)%>%
    summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),billdate=min(date))%>%
    mutate(Actual_Due=as.numeric(due_on-billdate))
  
  
  
  
  tdr_tf1 <-   tdr_tf%>%
    group_by(name,company)%>%
    summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),
              billdate=min(date))%>%
    mutate(Actual_Due=as.numeric(due_on-billdate))%>%
    mutate(AgeDays = case_when(
      as.numeric(Sys.Date()-billdate)<= Actual_Due ~ 0,
      TRUE ~ as.numeric(Sys.Date()-billdate-Actual_Due)
    ))%>%
    mutate(Days_Delayed = AgeDays , .after = name)%>%
    mutate(name=as.character(name))%>%
    ungroup()
  
  
  
  tdr_tf2<-tdr_tf%>%
    group_by(name,company)%>%
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
    select(name,company,BillValue,PendingValue,Days_Delayed,due_on)
    
  

##TDR Summary
  
  tdr_tf1 %>%
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
    #gt_theme_guardian()%>%
    gt_theme_espn()%>%
    #gt_theme_nytimes() %>% 
    fmt_symbol_first(column = Days_Delayed, suffix = "Days", decimals = 0) %>%
    cols_label(
      Actual_Due = html(
        "<span style='color:#1ECBE1;'>Actual Due</span> vs <span style='color:#E1341E;'>Today</span>"
      )
    ) %>% 
    tab_header(
      title = "Receivables SUmmary",
      subtitle = "Considering the actual due based in the clients"
    ) %>% 
    #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
    gt_add_divider(Days_Delayed, color = "grey", weight = px(1)) %>% 
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
  
  ####TDR Details
    
  
  make_color_pal <- function(colors, bias = 1) {
    get_color <- colorRamp(colors, bias = bias)
    function(x) rgb(get_color(x), maxColorValue = 255)
  }
  
  
  
  good_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
  red_color <- make_color_pal(c("#FFCDD2FF", "#EF9A9AFF", "#E57373FF", "#F44336FF", "#C62828FF"), bias = 2)
  
  knockout_cols2 <- c("name","BillValue","PendingValue","Days_Delayed","due_on","company")
  tdr_tf2 <- tdr_tf2[, c( knockout_cols2)]
  
  
  
 ####Workable reactable tdr data 
  sticky_style <- list(backgroundColor = "#f7f7f7")
  


  reactable(
    data       = tdr_tf2,elementId = "company",
    compact    = FALSE, # for minimum row height
    filterable = TRUE, # for individual column filters
    striped    = TRUE, # banded rows
    resizable  = TRUE, # for resizable column widths
    columns    = list(
      name   = colDef(name = "Name",  width = 150,  align = "center"),
      company  = colDef(name = "Company",width = 90, align = "center"),
      BillValue = colDef(name = "Bill Value",width = 70, align = "center"), 
      PendingValue  = colDef(name = "Pending Value",      width = 120, align = "center"),
      Days_Delayed    = colDef(name = "Days Delayed",        width = 120, align = "center"),
      due_on    = colDef(name = "Due On",        width = 120, align = "center")

    ),
    details = function(index) { 
      sec_lvl = tdr_tf[tdr_tf$name == tdr_tf2$name[index], ] %>%
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
                  Days_GT90         = colDef(name = "> 90 Days",    width = 50, align = "center")
                )
      )
    }
  )

  
  
  #####TD Purchase
  
  tdp<-read_csv("tdp.csv")
  
  tdp_tf<-  tdp%>%
    clean_names()%>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    )%>%
    mutate(name=snakecase::to_upper_camel_case(name))%>%
    mutate(date=dmy(date),due_on=dmy(due_on),name=as.factor(name),
           value=str_trim(str_replace(value,"Cr|Dr","")),
           final=str_trim(str_replace(final,"Cr|Dr","")))%>%
    select(name,date,due_on,invoice_no,name,bill_value=value,pending_value=pending,final_value=final,company,
           Days_LT30=x30_days,Days_30to60=x30_to_60_days,Days_60to90=x60_to_90_days,Days_GT90=x90_days)%>%
    mutate(bill_value=as.numeric(gsub(",","",bill_value,fixed=TRUE)))%>%
    mutate(final_value=as.numeric(gsub(",","",final_value,fixed=TRUE)))
  

  

  
  tdp_tf%>%
    group_by(name,company)%>%
    summarise(BillValue=sum(bill_value),PendingValue=sum(pending_value),due_on=min(due_on),billdate=min(date))%>%
    mutate(Actual_Due=as.numeric(due_on-billdate))
  


  tdp_tf2<-tdp_tf%>%
    group_by(name,company)%>%
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
    select(name,company,BillValue,PendingValue,Days_Delayed,due_on)
  
  
  

  tdp%>%
    clean_names()%>%
    mutate(
      across(everything(), ~replace_na(.x, 0))
    )%>%
    mutate(name=snakecase::to_upper_camel_case(name))%>%
  mutate(sal=str_trim(str_replace(value,"Cr","")))%>%
    select(value,sal)
  
  

 tdp%>%
   clean_names()%>%
   mutate(
     across(everything(), ~replace_na(.x, 0))
   )%>%
   mutate(name=snakecase::to_upper_camel_case(name))%>%
   mutate(sal=substring(value,1,nchar(value)-3))%>%
   select(value,sal)
 
 chs_sales%>%
   count(AlternateStoreCode,StoreName)
 
 
 chs_sales <- read_csv("Sal_jan19.csv",skip=5)
 
 chs_pur <- read_csv("Pur_jan19.csv",skip=6)
 
 
 ####Purchase vs Sales
 
 chs_pur_j <- chs_pur%>%head()%>%View()
  filter(StoreName=="cheers alwar")%>%
 mutate(GRNDate=dmy(GRNDate),
        mnths=month(GRNDate),
        BillYear=year(GRNDate),InvoiceDate=dmy(InvoiceDate))%>%
   select(Department,ProductCode,InvoiceDate,GRNDate,InvoiceDate,SupplierName,ProductName,Quantity,
          MRP,Amount,PurchasePrice,mnths,ReceivedQuantity)%>%
   filter(grepl("HG",ProductName))
 
 

 ##Rukmini Sales
 chs_sales_j <-chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   filter(StoreName=="cheers alwar")%>%
   filter(grepl('CHHOLE', ProductName))%>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity,BaseValue,ProductCode)%>%
   group_by(ProductName,ProductCode)%>%
   summarise(total_sold=sum(Quantity))
 
 
 chs_pur_j%>%
   inner_join(chs_sales_j,by="ProductCode")%>%
   select(Department,GRNDate,Product = ProductName.x,ReceivedQuantity,QuantitiesSold=total_sold)
 
 
 
 sales_nov %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>2)%>%
   ##filter(BillDate>'2022-03-20')%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(grepl('BAJRANG',SupplierName))%>%
   #filter(StoreName=="cheers Ayanavaram")%>%
  # filter(grepl('CHHOLE', ProductName))%>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate)%>%
 # group_by(ProductName)%>%
   summarise(total_sold=sum(Amount))
 
chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   filter(StoreName=="cheers alwar")%>%
   filter(!grepl('MC', SupplierName))%>%head()%>%View()
 
 
 write_csv(chs_sales,"shinysales.csv")
 
 chs_sales <- chs_sales%>%
  # filter(StoreName!="Teynampet")%>%
   mutate(BillDate=dmy(BillDate),
          BillMonth=month(BillDate),
          BillYear=year(BillDate))
 
 ###Pur vs sales different using suppliername
 chs_pur_j<-chs_pur%>%
   #filter(StoreName=="cheers alwar")%>%
   mutate(GRNDate=dmy(GRNDate),
          mnths=month(GRNDate),
          BillYear=year(GRNDate),InvoiceDate=dmy(InvoiceDate))%>%
   select(Department,ProductCode,InvoiceDate,GRNDate,InvoiceDate,SupplierName,ProductName,Quantity,
          MRP,Amount,PurchasePrice,mnths,ReceivedQuantity)%>%
   filter(grepl("ARCHANA",SupplierName))%>%
   group_by(ProductName,ProductCode,GRNDate)%>%
   summarise(total_received =sum(ReceivedQuantity))
 
 chs_sales_j<-chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   filter(StoreName=="cheers alwar")%>%
   filter(grepl("ARCHANA",SupplierName))%>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity,BaseValue,ProductCode)%>%
   group_by(ProductName,ProductCode)%>%
   summarise(total_sold=sum(Quantity))
 
 
 
 chs_pur_j%>%
   inner_join(chs_sales_j,by="ProductCode")%>%
   select(Product = ProductName.x,GRNDate,ReceivedQuantity=total_received,QuantitiesSold=total_sold)
 
 
 ############End of Pur Sales####
 chs_sales <- chs_sales%>%
   mutate(
     across(CustomerName, ~replace_na(.x, "Unknown"))
   )%>%
   count(CustomerName)%>%
   arrange(desc(n))
 
 chs_sales%>%
   filter(StoreName=="cheers alwar")%>%
   count(SupplierName)
 
 chs_sales%>%
   filter(StoreName=="cheers alwar")%>%
   #filter(CustomerName=="sudha ramamurthy")%>%
   group_by(BillNumber,BillDate)%>%
   summarise(total=sum(Amount))
 filter(BillNumber==unique(BillNumber))
 
 
 
 #top selling product feb 2022 cheers alwarpet
 
 cbPalette1 <- c("#999999", "#E69F00", "#39A388","#56B4E9", "#009E73",
                "#F0E442","#464660", "#0072B2", "#D55E00", "#CC79A7","#139487","#EF6D6D",
                "#00B4D8","#6A5495","#464E2E")
 
 chs_sales %>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
 #  filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   filter(StoreName=="cheers alwar")%>%
   #filter(StoreName=="Adyar") %>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity)%>%
   group_by(ProductName)%>%
   summarise(sum_total=sum(Amount),n=sum(Quantity))%>%
   arrange(desc(sum_total))%>%
   top_n(14,sum_total)%>%
   ungroup()%>%
   mutate(ProductName=fct_reorder(ProductName,sum_total))%>%
   ggplot(aes(ProductName,sum_total,fill=ProductName))+
   geom_col(show.legend = FALSE,color="black")+
   geom_text(aes(label=comma(sum_total)),size=3,hjust=1,color="white")+
   scale_y_comma()+
   scale_fill_manual(values=cbPalette1)+
   #scale_fill_distiller()+
 #scale_fill_economist()+
   coord_flip()+
   theme_few()+
   #theme_classic()+
   labs(title = "Top Selling Products (Alwarpet)",
        subtitle="Excluding Daily Needs (F&V , Aavin Milk) ",x="",y= "sales",
        caption = "Viz : Zaprify , Data : CRPL")
 
 
 chs_sales%>%
   ##filter(StoreName=="cheers alwar")%>%
  # filter(BillDate>"2022-02-15")%>%
   #filter(CustomerName=="sudha ramamurthy")%>%
   group_by(CustomerName)%>%
   summarise(total=sum(Amount))%>%
   arrange(desc(total))%>%
   ungroup()%>%
filter(BillNumber==unique(BillNumber))%>%
   mutate(
     across(CustomerName, ~replace_na(.x, "Unknown"))
   )%>%View()
 
 pur_nov%>%
   filter(grepl('TATTVA', ProductName))%>%
   filter(Quantity>0)%>%
   dplyr::select(GRNDate,InvoiceDate,ProductName,SupplierName,Quantity,MRP)%>%
   gt ::gt()%>%
   tab_header(
     title = "Organic Tattva Purchase Details",
     subtitle = "Includes PR"
   )%>%
   gt::opt_table_lines()%>%
   gt_theme_538()%>%
  # gt_theme_espn()%>%
   tab_options(
     table.border.bottom.color = "grey",
     table.width = px(410)
   )%>%
   gt_color_rows(Quantity, palette = "ggsci::blue_material")%>%
   tab_options(
     table.border.bottom.color = "grey",
     table.width = px(500)
   )%>%
   grand_summary_rows(
     columns = Quantity,
     fns = list(
       TOTAL = ~sum(.)
     ),
     formatter = fmt_number,
     decimals = 0
   )%>%
   grand_summary_rows(
     columns = MRP,
     fns = list(
       TOTAL = ~sum(.)
     ),
     formatter = fmt_number,
     decimals = 0
   )

 
 #Top selling products in cheers alwarpet
 chs_sales %>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   mutate(Productname=str_to_lower(Productname))%>%
   filter(grepl('PADMINI', ProductName))%>%
  # filter(StoreName=="cheers alwar")%>%
filter(StoreName=="Cheers Annanagar")%>%
   filter(BillYear>2021)%>%
   filter(BillMonth>0)%>%
   #filter(StoreName=="Adyar") %>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity)%>%
   group_by(ProductName,Department)%>%
   summarise(sum_total=sum(Amount),n=round(sum(Quantity)))%>%
   arrange(desc(n))%>%View()
 
 chs_sales%>%
   count(Department)
 
 
 chs_sales %>%
   filter(!Department=="Unknown")%>%
  #filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   # filter(StoreName=="cheers alwar")%>%
   filter(StoreName=="Cheers Annanagar")%>%
   filter(Department=="SNACKS AND BISCUITS")%>%
   filter(BillYear>2021)%>%
   filter(BillMonth>0)%>%
   mutate(ProductName=str_to_lower(ProductName))%>%
   filter(grepl('lays', ProductName))%>%
   #filter(StoreName=="Adyar") %>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity)%>%
   group_by(ProductName,Department)%>%
   summarise(sum_total=sum(Amount),n=round(sum(Quantity)))%>%
   arrange(desc(sum_total))%>%View()
 
 
 
 reactable( chs_sales %>%
              filter(!Department=="Unknown")%>%
              #filter(!Department=="BABY CARE")%>%
              filter(!Department=="FRUITS AND VEG")%>%
              filter(!grepl('Aavin', ProductName))%>%
              filter(StoreName=="cheers alwar")%>%
             # filter(StoreName==input$selected_branch)%>%
              filter(Department=="SNACKS AND BISCUITS")%>%
              filter(BillYear>2021)%>%
              filter(BillMonth>0)%>%
              mutate(ProductName=str_to_lower(ProductName))%>%
              filter(grepl("lays", ProductName))%>%
              #filter(StoreName=="Adyar") %>%
              select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity)%>%
              group_by(ProductName,Department)%>%
              summarise(Total_Amnt_Sold=sum(Amount),Units_Sold=sum(Quantity))%>%
              arrange(desc(Total_Amnt_Sold))%>%
              top_n(10,Total_Amnt_Sold),
            compact    = FALSE, # for minimum row height
            filterable = FALSE, # for individual column filters
            striped    = TRUE, # banded rows
            searchable = TRUE,
            resizable  = TRUE)
 
 
 ##March 4
 
 ##Cotton Cloth
 chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>2)%>%
  #filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
  filter(StoreName=="cheers alwar")%>%
 filter(grepl('COTTON', ProductName))%>%
   #filter(StoreName=="Adyar") %>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity,BaseValue,FreeMRPValue)%>%
   group_by(ProductName,Department)%>%
   summarise(sum_total=sum(Amount),n=round(sum(Quantity)))%>%
   arrange(desc(n))
 
 
 
 chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   filter(StoreName=="cheers alwar")%>%
   filter(grepl('COTTON', ProductName))%>%
   #filter(StoreName=="Adyar") %>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity,BaseValue,FreeMRPValue)%>%
   group_by(BillDate)%>%
   summarise(sum_total=sum(Amount),n=round(sum(Quantity)))%>%
   arrange(desc(n))%>%View()
 
 
 
 ##Rukmini
 chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   ##filter(StoreName=="cheers alwar")%>%
   filter(grepl('WHEAT', ProductName))%>%
 # filter(Productname=="RUKMINI WHEAT 1 KG")%>%
   #filter(StoreName=="Adyar") %>%
   select(Department,StoreName,ProductName,MRP,Amount,BillDate,Quantity,BaseValue,FreeMRPValue)%>%
   group_by(ProductName,Department)%>%
   summarise(sum_total=sum(Amount),n=round(sum(Quantity)))%>%
   arrange(desc(n))
 ##Rukmini Sales
 chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   filter(StoreName=="Cheers Annanagar")%>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
  #filter(StoreName=="cheers alwar")%>%
   filter(grepl('HG', ProductName))%>%
   count(ProductName)%>%
   arrange(desc(n))%>%View()
   filter(n<10)%>%
   mutate(ProductName=fct_reorder(ProductName,n))%>%
   ggplot(aes(ProductName,n))+
   geom_col()+
   coord_flip()
 
 
 
 chs_sales %>%
   filter(!Department=="Unknown")%>%
   filter(!Department=="BABY CARE")%>%
   #  filter(!Department=="FRUITS AND VEG")%>%
   filter(!grepl('Aavin', ProductName))%>%
   #filter(StoreName=="Adyar")%>%
   filter(StoreName=="Cheers Annanagar")%>%
   #filter(ProductCode=="42597")%>%
  #filter(Productname=="HIMALAYA MOISTURISING ALMOND SOAP 125 GM")%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(BillYear>2021)%>%
  filter(BillMonth>1)%>%
   filter(grepl('lays', ProductName))%>%
   #filter(StoreName=="Adyar") %>%
   arrange(desc(BillDate))%>%
   group_by(ProductName)%>%
   summarise(sum_total=sum(Amount),n=sum(Quantity))%>%
   arrange(desc(sum_total))%>%
   top_n(100,sum_total)
 
 
 
 chs_sales %>%
   #filter(!Department=="Unknown")%>%
   #filter(!Department=="BABY CARE")%>%
   #  filter(!Department=="FRUITS AND VEG")%>%
   #filter(!grepl('Aavin', ProductName))%>%
   #filter(StoreName=="Adyar")%>%
   filter(StoreName=="Cheers Annanagar")%>%
   
   #filter(ProductCode=="42597")%>%
   #filter(Productname=="HIMALAYA MOISTURISING ALMOND SOAP 125 GM")%>%
   #filter(StoreName=="Cheers Annanagar")%>%
   filter(BillYear>2021)%>%
   filter(BillMonth>2)%>%
   #filter(StoreName=="Adyar") %>%
   arrange(desc(BillDate))%>%
   group_by(BillDate)%>%
   summarise(sum_total=sum(Amount))
 
 
 chs_sales %>%
   filter(BillYear>2021)%>%
   filter(BillMonth>1)%>%
   filter(StoreName=="Cheers Annanagar")%>%
   filter(grepl('RUKMINI WHEAT 1 KG', ProductName))%>%View()
 
 source_tag <- "ExpectedETHReturn is the Average ETH Return based on the 18 month period"
 
 
eth <-  read_csv("eth.csv")

eth%>%
  clean_names()%>%
  mutate(Months = dmy(month))%>%
  select(Months,Investment = investment,ETH_USD_PRICE=eth_usd_price,ETH_in_INR=eth_price_inr,
         Ethereum_Owned=ethereum_owned,ETH_Return=eth_return,Mining_Revenue=mining_per_month)%>%
  gt ::gt()%>%
  gt_theme_nytimes() %>% 
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = vars(ETH_Return),
      rows = ETH_Return < 3000000
    )
  ) %>% 
  tab_style(
    style = cell_text(color = "blue", weight = "bold"),
    locations = cells_body(
      columns = vars(ETH_Return),
      rows = ETH_Return >= 3000000
    )
  )%>%
  grand_summary_rows(
    columns = ETH_Return,
    fns = list(
      ExpectedETHReturn  = ~mean(.)
    ),
    formatter = fmt_number,
    decimals = 0
  )%>%
  grand_summary_rows(
    columns = Mining_Revenue,
    fns = list(
      TotalMiningReturn  = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 0
  )%>%
  tab_header(
    title = "ETH or Mining",
    subtitle = "Considering 18 Months Assumption Taken from Jan 2022 - June 2023 based on the historical ETH Monthly Price Volatility"
  ) %>% 
  #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
  gt_add_divider(Investment, color = "grey", weight = px(1)) %>% 
  gt_color_rows(Mining_Revenue, palette = "ggsci::blue_material")%>%
  tab_footnote(
    footnote = "Ethereum is highly volatile as cryptocurrencies relatively change more often and more severly ",
    locations = cells_column_labels(
      columns = ETH_USD_PRICE
    ))%>%
  tab_footnote(
    footnote = "Ethereum Return is calculated by : Current Month Rolling ETH Price * Ethereum Owned",
    locations = cells_column_labels(
      columns = ETH_Return
    ))%>%
  tab_footnote(
    footnote = "Mining Revenue is calculated by : 0.5 ETH / Month",
    locations = cells_column_labels(
      columns = Mining_Revenue
    ))%>%
  tab_source_note(html(source_tag))%>%
  tab_source_note(md("**Mining Excluding**: Capex | **Ethereum Excluding**: Very High Volatility")) %>% 
  tab_options(
    table.border.bottom.color = "grey"
  )


#Cheuque GT Tables

#cheq_fev <- read_csv("cheq2.csv")


cheq_fev <- read_csv("cheersjun.csv")

cheq_fev%>%
  clean_names()%>%
  select(1:5)%>%
  mutate(Bill_Date=dmy(bill_date),Cheque_Date=dmy(cheque_date),Amount=amount,Type=type)%>%
  mutate(Year=year(Bill_Date),AgeinginDays=Sys.Date()-Bill_Date)%>%
  select(-bill_date)%>%
 # group_by(Cheque_Date)%>%
  gt:: gt(groupname_col = "Cheque_Date")%>%
 # gt_theme_nytimes() %>% 
  gt_theme_538()%>%
  summary_rows(
    groups = TRUE,
    columns = Amount,
    fns = list(TotalExpected = ~sum(.))
  )%>%
  grand_summary_rows(
    columns = Amount,
    fns = list(
      MonthTotal = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 0
  )%>%
  fmt_symbol_first(column = AgeinginDays, suffix = " Days", decimals = 0)%>%
  gt_highlight_rows(columns = Type, rows = Type =="ADMIN",
                    font_weight = "normal",fill = "lightgrey")%>%
  gt_highlight_rows(columns = Year, rows = Year =="2021",
                    font_weight = "normal",fill = "#EF6D6D")%>%
  tab_style(
    style = list(
      cell_fill("black"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_row_groups()
  )%>%
  tab_style(
    style = cell_text(color = "#035397"),
    locations = cells_body(
      columns = vars(Type),
     # rows = ProductRanges =="ADMIN"
     rows = grepl("ADMIN",Type)
    ))%>%
  tab_header(
    title = "Cheques Expected For June 2022" ,
    subtitle = "PS : HomeTech Labour Services cheque hits on 27th June") %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFD36E")
    ),
    locations = cells_body(
    #  columns = vendors, # not needed if coloring all columns
      rows = vendors=="HOMETECH SERVICES PRIVATE LIMITED")
  )


colors_safe <- c("#6EBF8B", "#FC4F4F")

remplotly <- safemebp%>%clean_names()%>%
  ggplot(aes(system_type,fill=status))+
  geom_bar()+
  scale_color_manual(name="",labels = c("Completed","Open"),
                     values= colors_safe)+
  scale_fill_manual(name="",values = colors_safe,
                    labels = c("Completed","Open"))+
  coord_flip()+
 theme_classic()+
theme(legen)

ggplotly(remplotly)

safemebp%>%clean_names()%>%
  ggplot(aes(status,fill=system_type))+
  geom_bar()+
  facet_wrap(~block,scales = "free_y")+
  theme_classic()


##SIcare

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

chs_sales <- chs_sales%>%
  # filter(StoreName!="Teynampet")%>%
  mutate(BillDate=dmy(BillDate),
         BillMonth=month(BillDate),
         BillYear=year(BillDate))

chs_sales %>%
  filter(BillYear>2021)%>%
  filter(BillDate =='2022-03-17')%>%
  group_by(BillDate,StoreName)%>%
  dplyr::summarise(Total = round(sum(Amount)))

sales_nov%>%
  select(Date=BillDate,BillYear,Amount,StoreName)%>%
  filter(Date =='2022-03-17')%>%
  # filter(BillYear>2021)%>%
 group_by(Date,StoreName)%>%
  dplyr::summarise(Total = round(sum(Amount)))


#Admin Bills

admin <- read_csv("admin.csv")

admin%>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )%>%
  clean_names()%>%
  mutate(bill_date=dmy(bill_date))%>%
  mutate(Ageing_Days=Sys.Date()-bill_date)%>%
  gt()%>%
  tab_row_group(
    group = "Cheers Specific",
    rows = responsible =="Cheers") %>% 
  tab_row_group(
    group = "Shared ( Can be Shared / Split across the companies)",
    rows = responsible =="Shared")%>%
 # gt_plt_bullet(column =AgeDays , target = actualtarget,colors = c("#E1341E","#1ECBE1")) %>% 
  #gt_theme_guardian()%>%
 # gt_theme_espn()%>%
 # gt_theme_nytimes() %>% 
  gt_theme_538()%>%
  tab_header(
    title = paste0("Admin Cheques Overall As on - ",Sys.Date()),
    subtitle ="All Admin Cheques are processed under Cheers , Can we share it across the group companies , If that's a possibility ahead? "
  ) %>% 
  #gt_highlight_rows(rows = distributor == "HBO", fill = "grey", alpha = 0.4) %>% 
  gt_add_divider(instrument_number, color = "grey", weight = px(1)) %>% 
  tab_source_note(md("**Data Source**: CRPL")) %>% 
  tab_options(
    table.border.bottom.color = "grey",
    table.width = px(500)
  )%>%
  gt::summary_rows(
    groups = TRUE, columns = vars(credit),
    fns = list(Total = ~sum(.)),
    formatter = fmt_number, decimals = 1
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "white", font = google_font("Fira Mono")),
      cell_fill("black")
    ),
    locations = cells_summary()
  )



cug <- read_csv("cug.csv")



cug%>%
  gt()%>%
  gt_color_rows(Yearly_Jio_Savings, palette = "ggsci::green_material")%>%
  gt_color_rows(Monthly_Jio_Savings, palette = "ggsci::yellow_material")%>%
  tab_footnote(
    footnote = "Proposed Jio Plan for the Same plan",
    locations = cells_column_labels(
      columns = Jio_Bill_Monthly
    ))%>%
  gt_theme_nytimes()%>%
  tab_source_note(md("**Intention**: This came as ANG store has serious Network issue and customer complaints of not able to pay as there is no signal , so looked for jio and came up with this solution and worked with Boo to get the current Plan and Costs"))%>%
  tab_header(
    title = "Airtel Vs Jio CUG Plans",
    subtitle ="Why dont we change CUG to Jio which has same plan good coverage even faster bandwidth"
  )%>%
  grand_summary_rows(
    columns = Yearly_Jio_Savings,
    fns = list(
      SAVINGS_PER_YEAR = ~sum(.)
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
      columns = Yearly_Jio_Savings,
      rows = 1)
  )


##Specif data cheque detail geneation march 29



cheque_viz%>%
  left_join(cheq_viz_join)%>%
  select(Date,"Purchase"=Pur,Sales,"Cheque_Cleared"=CC,"CapitalInfusion"=CI,Salary)%>%
  filter(Date >= "2022-03-01" & Date <= "2022-03-29") 


cheque_viz%>%
  left_join(cheq_viz_join)%>%
  select(Date,"Purchase"=Pur,Sales,"Cheque_Cleared"=CC,"CapitalInfusion"=CI,Salary)%>%
  filter(Date >= "2022-03-01" & Date <= "2022-03-29") %>%
  gt()%>%
  cols_hide(
    columns = c(
      CapitalInfusion
    )
  )%>%
  # gt_highlight_rows(
  #     #rows = 1:nrow(cheque_viz), 
  #     fill = "lightgrey",
  #     bold_target_only = TRUE,
  #     target_col = Total
  # )%>%
  # gt_color_rows(Cheque_Cleared, palette = "ggsci::blue_material")%>%
  gt_color_rows(Sales, palette = "ggsci::green_material")%>%
  #gt_color_rows(CapitalInfusion, palette = "ggsci::red_material")%>%
  gt_color_rows(Cheque_Cleared, palette = "ggsci::yellow_material")%>%
  gt_plt_bar_stack(
    column=Salary, palette = pal_salary,
    position = 'stack', labels = c("Purchase %","Cheque_Cleared %","Sales %"),
    width = 80,trim=FALSE,
    fmt_fn = scales::percent_format()
  )%>%
  tab_header(
    title = paste0("Purchase vs Sales Vs CC : Updated Till " ,max(cheque_viz$Date)),
    subtitle = md("Showing last 30 Days - **Filter the Dates in sidebar for the relevant data**")
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
  # tab_style(
  #     style = list(
  #         cell_text(font=google_font(
  #             name = "Fira Sans"),align = 'center'
  #         )),
  #     locations = cells_body(columns = c(Total))
  # )%>%
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
  tab_footnote(
    footnote = md("Total Capital **Infused** on a daily basis"),
    locations = cells_column_labels(5)
  )
# data_color(
#     columns = vars(Cheque_Cleared),
#     colors = scales::col_numeric(
#         palette = c(
#             "white", "orange", "red"),
#         domain = NULL)
# )

chs_sales%>%
  count(StoreName)

chs_sales_plot<-chs_sales%>%
  filter(StoreName=="cheers alwar")%>%
  filter(BillDate>="2022-03-01"&BillDate<="2022-03-10")%>%
  group_by(Department)%>%
  summarise(TotalSales=sum(Amount))%>%
  arrange(desc(TotalSales))


chs_sales_pplot<-chs_sales%>%
  filter(StoreName=="cheers alwar")%>%
  filter(BillDate>="2022-01-01"&BillDate<="2022-03-31")%>%
  group_by(Department)%>%
  summarise(TotalSales=sum(Amount))%>%
  mutate(Percentage= round(((TotalSales)/sum(TotalSales))*100,1)/100)%>%
  arrange(desc(Percentage))%>%
  select(Department,Percentage)

chs_waffle <- chs_sales%>%
  filter(BillYear>2021)%>%
 # filter(BillMonth==3)%>%
  filter(StoreName=="cheers alwar")%>%
  #filter(BillDate>="2022-01-01"&BillDate<="2022-03-31")%>%
  #filter(Department=="SNACKS AND BISCUITS")%>%
  group_by(Department)%>%
  summarise(TotalSales=sum(Amount))%>%
  mutate(Percentage= round(((TotalSales)/sum(TotalSales))*100,2))%>%
  mutate(Department=as.factor(Department))%>%
  arrange(desc(Percentage))%>%
  select(Department,Percentage)

chs_waffle%>%
 # filter(BillMonth==3)%>%
ggplot(aes(fill = fct_reorder(Department,Percentage), values = Percentage)) +
  geom_waffle(n_rows = 10, cols = 100, size = 0.2, colour = "white", flip = TRUE,
              show.legend = FALSE) +
  scale_fill_manual(name = NULL,
                    values = c("#77C3C2", "grey87"))+
  coord_fixed() +
  facet_wrap(vars(Department)) +
  theme(axis.title = element_blank())
library(glue)
library(waffle)
chs_waffle%>%
  #filter(BillMonth==3)%>%
  mutate(share_renewable_precise = Percentage,
         share_renewable = round(Percentage),
         share_other = 100 - share_renewable) %>% 
  pivot_longer(cols = c(share_renewable, share_other), names_pattern = "share_(.+)") %>% 
  mutate(name = factor(name, levels = c("renewable", "other")))%>%
  mutate(
    share_fmt = sprintf("%.1f", share_renewable_precise),
    label = glue::glue("{Department} {share_fmt} %"),
    label = fct_reorder(label, -share_renewable_precise))%>%
  ggplot(aes(fill = name, values = value)) +
  geom_waffle(n_rows = 10, cols = 20, size = 0.1, colour = "white", flip = TRUE,
              show.legend = FALSE) +
  scale_fill_manual(name = NULL,
                    values = c("#FFD93D", "#243D25")) +
  coord_fixed() +
  facet_wrap(vars(label)) +
  plot_labs +
 # theme(axis.title = element_blank())
  theme_bw()


maxval <- max(chs_sales_plot$TotalSales)

#percentage
reactable(
  chs_sales_pplot,
  pagination = FALSE,
  defaultColDef = colDef(
    cell = data_bars(chs_sales_pplot, 
                     fill_color = c("#1efffd", "#1e20ff"), 
                     fill_gradient = TRUE, 
                     background = "lightgrey", 
                     max_value = 0.09, 
                     brighten_text = FALSE,
                     text_color = "white",
                     number_fmt = scales::percent)
  )
)



reactable(
  chs_sales_plot,
  pagination = FALSE,
  defaultColDef = colDef(
    cell = data_bars(chs_sales_plot, 
                     fill_color = c("#1efffd", "#1e20ff"), 
                     fill_gradient = TRUE, 
                     background = "lightgrey",
                     max_value = maxval+1000 , 
                     brighten_text = FALSE,
                     text_color = "white",
                     number_fmt = scales::comma)
  )
)


chs_gp <- read_csv("sales.csv",skip=5)
chs_pur<- read_csv("purc.csv",skip=6)


chs_pur <- chs_pur %>%
  mutate(GRNDate=dmy(GRNDate),
         mnths=month(GRNDate),
         BillYear=year(GRNDate),InvoiceDate=dmy(InvoiceDate))
chs_pur_tf <-chs_pur%>%
  filter(BillYear>2021)%>%
  #filter(mnths==3)%>%
  mutate(Gross=MRP-PurchasePrice)%>%
  select(Department,GRNDate,InvoiceDate,SupplierName,StoreName,ProductName,Gross,MRP,PurchasePrice,Quantity,
         MRP,Amount,mnths)
#filter(Department=="FRUITS AND VEG")%>%View()



chs_gp <- chs_gp %>%
  #filter(StoreName!="Teynampet")%>%
  mutate(BillDate=dmy(BillDate),
         BillMonth=month(BillDate),
         BillYear=year(BillDate))


chs_gp_tf<-chs_gp%>%
  filter(BillYear>2021)%>%
  filter(BillMonth==3)%>%
  select(Department,BillDate,StoreName,SupplierName,ProductName,
         BaseValue,BillNumber,Amount,BillTime,BillMonth,BillYear,Quantity)%>%
 # filter(!Department=="FRUITS AND VEG")%>%
  group_by(StoreName)%>%
  summarise(TotalSales=sum(Amount))





chs_gp_tf%>%
  inner_join(chs_pur_tf,by="ProductName")%>%
  #filter(!Department.x=="Unknown")%>%
 # filter(!Department=="BABY CARE")%>%
  filter(!Department.x=="FRUITS AND VEG")%>%
  #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
  filter(ProductName=="LAYS AMERICAN STYLE CREAM & ONION 52 GMS")%>%
  group_by(BillNumber,StoreName.x,ProductName,BillMonth,Gross)%>%
  count(Quantity.x)%>%
  mutate(GP=Gross*Quantity.x)%>%
  group_by(StoreName.x)%>%
  summarise(GrossProfit=sum(GP))


###Summarise
chs_gp_tf%>%
  inner_join(chs_pur_tf,by="ProductName")%>%
  #filter(!Department.x=="Unknown")%>%
  # filter(!Department=="BABY CARE")%>%
  filter(!Department.x=="FRUITS AND VEG")%>%
  #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
  #filter(ProductName=="CADBURY DAIRY MILK SILK FRUIT & NUT 137 GMS")%>%
 # filter(StoreName.x=="cheers alwar")%>%
  #group_by(BillNumber,StoreName.x,ProductName,BillMonth,Gross)%>%
  group_by(BillNumber,ProductName,SupplierName.x,Gross,StoreName.x)%>%
  count(Quantity.x)%>%
  filter(n>1)%>%
  mutate(GP=Gross*Quantity.x)%>%
  group_by(StoreName.x)%>%
  summarise(GrossProfit=sum(GP))

  
  
  ##Department Wise ANG
  chs_gp_tf%>%
    inner_join(chs_pur_tf,by="ProductName")%>%
    #filter(!Department.x=="Unknown")%>%
    # filter(!Department=="BABY CARE")%>%
    filter(!Department.x=="FRUITS AND VEG")%>%
    #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
    #filter(ProductName=="CADBURY DAIRYMILK SILK HAZELNUT 58GMS")%>%
    filter(StoreName.x=="Cheers Annanagar")%>%
    #group_by(BillNumber,StoreName.x,ProductName,BillMonth,Gross)%>%
    group_by(BillNumber,Department.x,SupplierName.x,Gross,StoreName.x)%>%
    count(Quantity.x)%>%
    filter(n>1)%>%
  mutate(GP=Gross*Quantity.x)%>%
    group_by(Department.x)%>%
    summarise(GrossProfit=sum(GP))%>%
    arrange(desc(GrossProfit))%>%
    select(Department=Department.x,GrossProfit)%>%
    gt::gt()%>%
    #gt_theme_538()%>%
    gt_theme_espn() %>% 
    tab_header(title = "AnnaNagar Gross Profit by Department for March 2022",
               subtitle = "Excluding F&V as there are few things to be consolidated on the gms to unit quantity")%>%
    fmt_symbol_first(column = GrossProfit, suffix = " INR", decimals = 2)%>%
    # trim gives smaller range of colors
    # so the green and purples are not as dark
    gt_color_rows(GrossProfit, palette = "ggsci::blue_material")
  
  
  ###SVA
  chs_gp_tf%>%
    inner_join(chs_pur_tf,by="ProductName")%>%
    #filter(!Department.x=="Unknown")%>%
    # filter(!Department=="BABY CARE")%>%
    filter(!Department.x=="FRUITS AND VEG")%>%
    #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
    #filter(ProductName=="CADBURY DAIRYMILK SILK HAZELNUT 58GMS")%>%
    filter(StoreName.x=="cheers Ayanavaram")%>%
    #group_by(BillNumber,StoreName.x,ProductName,BillMonth,Gross)%>%
    group_by(BillNumber,Department.x,SupplierName.x,Gross,StoreName.x)%>%
    count(Quantity.x)%>%
    filter(n>1)%>%
    mutate(GP=Gross*Quantity.x)%>%
    group_by(Department.x)%>%
    summarise(GrossProfit=sum(GP))%>%
    arrange(desc(GrossProfit))%>%
    select(Department=Department.x,GrossProfit)%>%
  gt::gt()%>%
    #gt_theme_538()%>%
    gt_theme_espn() %>% 
    tab_header(title = "SVA Gross Profit by Department for March 2022",
               subtitle = "Excluding F&V as there are few things to be consolidated on the gms to unit quantity")%>%
    fmt_symbol_first(column = GrossProfit, suffix = " INR", decimals = 2)%>%
    # trim gives smaller range of colors
    # so the green and purples are not as dark
    gt_color_rows(GrossProfit, palette = "ggsci::blue_material")
  
  
  ### Department wise Alwar
  chs_gp_tf%>%
    inner_join(chs_pur_tf,by="ProductName")%>%
    #filter(!Department.x=="Unknown")%>%
    # filter(!Department=="BABY CARE")%>%
    filter(!Department.x=="FRUITS AND VEG")%>%
    #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
    #filter(ProductName=="CADBURY DAIRYMILK SILK HAZELNUT 58GMS")%>%
    filter(StoreName.x=="cheers alwar")%>%
    #group_by(BillNumber,StoreName.x,ProductName,BillMonth,Gross)%>%
    group_by(BillNumber,Department.x,SupplierName.x,Gross,StoreName.x)%>%
    count(Quantity.x)%>%
    filter(n>1)%>%
    mutate(GP=Gross*Quantity.x)%>%
    group_by(Department.x)%>%
    summarise(GrossProfit=sum(GP))%>%
    arrange(desc(GrossProfit))%>%
    select(Department=Department.x,GrossProfit)%>%
    gt::gt()%>%
    #gt_theme_538()%>%
    gt_theme_espn() %>% 
    tab_header(title = "Alwarpet Gross Profit by Department for March 2022",
               subtitle = "Excluding F&V as there are few things to be consolidated on the gms to unit quantity")%>%
    fmt_symbol_first(column = GrossProfit, suffix = " INR", decimals = 2)%>%
    # trim gives smaller range of colors
    # so the green and purples are not as dark
    gt_color_rows(GrossProfit, palette = "ggsci::blue_material")
  
  gp <- read_csv("GP.csv")
  
  
  
  gp %>%
    gt::gt()%>%
    #gt_theme_538()%>%
    gt_theme_espn() %>% 
    tab_header(title = "Gross Profit across stores for March 2022",
               subtitle = "Excluding F&V as there are few things to be consolidated on the gms to unit quantity")%>%
    fmt_symbol_first(column = GrossProfitPercent, suffix = " %", decimals = 2)%>%
    # trim gives smaller range of colors
    # so the green and purples are not as dark
    gt_color_rows(GPPerMonth, palette = "ggsci::blue_material")
  
  
######ENd of GP#####
  
  chs_gp_tf%>%
    inner_join(chs_pur_tf,by="ProductName")%>%
    #filter(!Department.x=="Unknown")%>%
    # filter(!Department=="BABY CARE")%>%
    filter(!Department.x=="FRUITS AND VEG")%>%
    #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
    filter(StoreName.x=="cheers alwar")%>%
    filter(ProductName=="CADBURY DAIRY MILK SILK FRUIT & NUT 137 GMS")%>%
    group_by(BillNumber,ProductName,SupplierName.x,Gross)%>%
    count(Quantity.x)%>%
    filter(n>1)%>%
    mutate(GP=Gross*Quantity.x)
  

chs_gp_tf%>%
  inner_join(chs_pur_tf,by="ProductName")%>%
  #filter(!Department.x=="Unknown")%>%
  # filter(!Department=="BABY CARE")%>%
  filter(!Department.x=="FRUITS AND VEG")%>%
  filter(SupplierName.x=="R V S S ENTERPRISES")%>%
  group_by(BillNumber)%>%
  count(Quantity.x)%>%
  mutate(GP=Gross*n)%>%
  group_by(StoreName.x)%>%
  summarise(GrossProfit=sum(GP))

chs_gp_tf%>%
  filter(BillMonth==3)%>%
  filter(StoreName=="cheers alwar")%>%
  filter(grepl("LAYS AMERICAN STYLE CREAM & ONION 52 GMS",ProductName))%>%
  View()



chs_gp_tf%>%
  filter(BillMonth==3)%>%
  filter(StoreName=="Cheers Annanagar")%>%
  filter(grepl("LAYS",ProductName))%>%
  count(Quantity)
  View()



chs_gp_tf%>%
  filter(BillMonth==3)%>%
filter(SupplierName=="R V S S ENTERPRISES")%>%
  filter(StoreName=="cheers alwar")%>%
  filter(ProductName=="HG POTATO CHIPS THIN AND CRISPY 100G")%>%View()


chs_gp_tf%>%
  inner_join(chs_pur_tf,by="ProductName")%>%
  #filter(!Department.x=="Unknown")%>%
  # filter(!Department=="BABY CARE")%>%
  filter(!Department.x=="FRUITS AND VEG")%>%
  filter(StoreName.x=="Cheers Annanagar")%>% 
  #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
  group_by(StoreName.x,Department.x,BillMonth,Gross)%>%
  count(Quantity.x)%>%
  mutate(GP=Gross*n)%>%
  group_by(Department.x)%>%
  summarise(GrossProfit=sum(GP))%>%
  arrange(desc(GrossProfit))

###SVA
chs_gp_tf%>%
  inner_join(chs_pur_tf,by="ProductName")%>%
  #filter(!Department.x=="Unknown")%>%
  # filter(!Department=="BABY CARE")%>%
  filter(!Department.x=="FRUITS AND VEG")%>%
  filter(StoreName.x=="cheers Ayanavaram")%>% 
  #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
  group_by(BillNumber,Department.x,SupplierName.x,Gross,StoreName.x)%>%
  count(Quantity.x)%>%
  filter(n>1)%>%
  mutate(GP=Gross*Quantity.x)%>%
  group_by(Department.x)%>%
  summarise(GrossProfit=sum(GP))%>%
  arrange(desc(GrossProfit))


###Alwar

chs_gp_tf%>%
  inner_join(chs_pur_tf,by="ProductName")%>%
  #filter(!Department.x=="Unknown")%>%
  # filter(!Department=="BABY CARE")%>%
  filter(!Department.x=="FRUITS AND VEG")%>%
  filter(StoreName.x=="cheers alwar")%>% 
  #filter(SupplierName.x=="R V S S ENTERPRISES")%>%
  group_by(StoreName.x,Department.x,BillMonth,Gross)%>%
  count(Quantity.x)%>%
  mutate(GP=Gross*n)%>%
  group_by(Department.x)%>%
  summarise(GrossProfit=sum(GP))%>%
  arrange(desc(GrossProfit))



sales_nov<-read_csv("shinydecsales.csv",show_col_types = FALSE)

pur_nov <- read_csv("shinydecpur.csv",show_col_types = FALSE)


sales_nov%>%
  filter(SupplierName=="R V S S ENTERPRISES")%>%
filter(StoreName=="cheers alwar")%>%
  filter(BillDate >= "2022-03-01" & BillDate <= "2022-04-10")%>%
  #filter(BillMonth>=input$rateThreshold)%>%
  select(Department,BillDate,StoreName,SupplierName,ProductName,MRP,
         Amount,BillTime,mnths=BillMonth,BillYear)%>%
  group_by(ProductName,Department)%>%
  summarise(sales=sum(Amount))%>%
  arrange(desc(sales))%>%inner_join(pur_nov%>%
                                      filter(SupplierName=="R V S S ENTERPRISES")%>%
                                      #filter(mnths>=input$rateThreshold)%>%
                                      filter(GRNDate >= "2022-03-01" & GRNDate <= "2022-04-10")%>%
                                      select(Department,GRNDate,StoreName,SupplierName,ProductName,
                                             Quantity,MRP,Amount,PurchasePrice,mnths)%>%
                                      group_by(ProductName,Department)%>%
                                      summarise(purchased=sum(Amount))%>%
                                      arrange(desc(purchased)), by="ProductName")%>%
  select(  ProductName ,purchased,sales)%>%
  group_by( ProductName)%>%
  summarise(purchase=round(sum(purchased)),sold = round(sum(sales)))%>%
  arrange(desc(purchase))%>% gt ::gt()



#CRM Dashboard

crm_dash <- read_csv("crm_dash.csv")

crm_dash%>%
  mutate(Date=dmy(Date))%>%
  group_by(Date)%>%
  summarise(Dialled_calls=sum(Dialled_calls),Active_Calls=sum(Active_Calls))%>%
pivot_longer(cols = Dialled_calls:Active_Calls,names_to = "Type")%>%
  mutate(Type=as.factor(Type))%>%
  ggplot(aes(Date,value,color=Type))+
  geom_line()


pur_nov%>%
  filter(grepl("NATHAN",ProductName))%>%
  filter(!grepl("SNACKS",ProductName))%>%
  filter(!grepl("CANDY",ProductName))%>%
  filter(InvoiceDate=="2022-03-01")%>%
  dplyr::select(ProductName,MRP,PurchasePrice)%>%
  mutate(margin=100-round((PurchasePrice/MRP)*100))%>%
  gt:: gt()%>%
  gt_theme_538()%>%
  tab_header(title = md("**Nathan Snacks Purchase Margin**"))%>%
  data_color(
    columns = c(PurchasePrice),
    colors = scales::col_factor(
      palette = paletteer::paletteer_d(
        n = 3, palette = "colorblindr::OkabeIto"
      ) %>% as.character(),
      domain = NULL
    )
  )



#sales per product


sales_nov%>%
  filter(grepl("SKM ",ProductName))%>%
  select(BillDate,ProductName,Amount,MRP,Quantity)%>%
  #group_by(ProductName)%>%
  arrange(-Amount)%>%
  gt :: gt()%>%
  gt_theme_espn() %>% 
  tab_header(title = "Home Grown Sales Overall")%>%
  # trim gives smaller range of colors
  # so the green and purples are not as dark
  gt_color_rows(Amount, palette = "ggsci::blue_material")%>%
  grand_summary_rows(
    columns = c('Amount','MRP'),
    fns = list(
      TOTAL = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 0
  )

  
  
  sales_cust <- read_csv("Sal_jan19.csv",skip=5)
  
  sales_cust%>%
    count(CustomerMobile)%>%
    write_csv("cheers_cust.xls")
  
  
  chs_sales<-read_csv("shinydecsales.csv",show_col_types = FALSE)
  
  
  chs_sales%>%
    count(StoreName)
  
  
  chs_sales%>%
  #  filter(Department==input$select_dept)%>%
    #  filter(!Department=="FRUITS AND VEG")%>%
    filter(!grepl('Aavin', ProductName))%>%
    filter(grepl('GOLD ', ProductName))%>%
    #filter(StoreName=="Adyar")%>%
    #filter(ProductCode=="42597")%>%
    filter(StoreName=="cheers alwar")%>%
   # filter(Productname=="HG RAW BANANA CHIPS THIN AND CRISPY 200G")%>%
    #filter(BillYear>2021)%>%
    #filter(BillMonth>1)%>%
    #filter(StoreName=="Adyar") %>%
    arrange(desc(BillDate))%>%
    group_by(ProductName)%>%
    summarise(TotalSales=sum(Amount),Units_Sold=sum(Quantity))%>%
    arrange(desc(TotalSales))
  
  pur_nov%>%
    filter(grepl('GOLD WINNER', ProductName))%>%
    count(SupplierName)
  
  
  reactable(iris[1:5, ],
            details = function(index) {
              if (index %in% c(3, 5)) {
                reactable(data.frame(x = c(1, 2, 3), y = c("a", "b", "c")), fullWidth = FALSE)
              }
            },
            columns = list(
              Petal.Length = colDef(details = function(index) {
                paste("Petal.Length: ", iris[index, "Petal.Length"])
              }),
              Sepal.Length = colDef(format = colFormat(digits = 1), details = JS("
      function(rowInfo) {
        return 'Sepal.Length: ' + rowInfo.values['Sepal.Length']
      }
    "))
            )
  )
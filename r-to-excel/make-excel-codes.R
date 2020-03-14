## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())#clear environment
wd <- "~/GitHub/Pet-projects/r-to-excel/"#set working directory
#Set up default knitr chunk options
library("knitr")
knitr::opts_chunk$set(
  echo = TRUE,
  eval = TRUE,
  message = FALSE,
  warning = FALSE, 
  cache = TRUE,
  cache.lazy = FALSE
) 


## ----load packages---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
packages <- c(
  "tidyverse",
  "openxlsx",
  "lubridate",
  "finalfit"
)
packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x)
      library(x, character.only = TRUE)
    }
  }
)
select<-dplyr::select


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#import raw data
addsToCart <- read.csv("addsToCart.csv",
                  stringsAsFactors = FALSE)
sessionCounts <- read.csv("sessionCounts.csv",
                  stringsAsFactors = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#print a summary
summary(sessionCounts)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#check missing values
missing_glimpse(sessionCounts)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Engineer the month variable
sessionCounts <-
  sessionCounts %>% mutate(dim_month = month(dim_date %>% as.Date()))#round date to month
sessionCounts$dim_month %>% summary()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#make the month device sheet
month_device <-
  sessionCounts %>% #take this data
  group_by(Month = dim_month, `Device Category` = dim_deviceCategory) %>% #group by month and category
  summarise(
    Sessions = sum(sessions),
    Transactions = sum(transactions),
    QTY = sum(QTY),
  ) %>% #compute summary statistics
  mutate(ECR = Transactions / Sessions) %>% #compute ECR
  arrange(Month, `Device Category`) # arrange by month then device

month_device


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#modify names of addtocard data
names(addsToCart)<-c("Month","Adds To Cart")

#make the month summary sheet
month_summary <-  month_device %>%
  group_by(Month) %>% #group by month
  summarize(
    Sessions = sum(Sessions),
    Transactions = sum(Transactions),
    QTY = sum(QTY)
  ) %>% #print summary stats
  mutate(ECR = Transactions / Sessions,
         `Device Category` = "Overall") %>% #compute metrics again
  full_join(addsToCart, by = "Month") %>% #join with add to cart data
  select(Month,
         `Device Category`,
         Sessions,
         Transactions,
         QTY,
         `Adds To Cart`,
         everything()) %>% #reorder variables
  arrange(Month) #arrange by month
month_summary


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#create a new wb
wb <- createWorkbook()
#create new sheets
addWorksheet(wb, "Month x Device")
addWorksheet(wb, "Month Summary")
#write data into sheet
writeData(wb, "Month x Device", month_device)
writeData(wb, "Month Summary", month_summary)
#create headline style
hs <- createStyle(wrapText = TRUE,
                  textDecoration = "BOLD", 
                  border = "bottom",
                  fgFill = "#E5E5E5")
#add hs
addStyle(wb,sheet = 1,hs,rows = 1, cols = 1:6)
addStyle(wb,sheet = 2,hs,rows = 1, cols = 1:7)
#set column width
setColWidths(wb, sheet = 1, cols = 1:6, widths = 11)
setColWidths(wb, sheet = 2, cols = 1:7, widths = 11)
#not show grid lines
showGridLines(wb, 1, showGridLines = FALSE)
showGridLines(wb, 2, showGridLines = FALSE)
#save workbook
saveWorkbook(wb, "workbook-from-r.xlsx", overwrite = TRUE)


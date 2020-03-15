## ----setup, include=FALSE--------------------------------------------------------------------
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


## ----load packages---------------------------------------------------------------------------
packages <- c(
  "tidyverse",
  "DBI",
  "RSQLite",
  "randomNames",
  "lubridate"
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


## --------------------------------------------------------------------------------------------
# create a SQLite instance and create one connection.
m <- dbDriver('SQLite')
# initialize a new database to a tempfile
tfile <- tempfile()
con <- dbConnect(m, dbname=tfile)


## --------------------------------------------------------------------------------------------
set.seed(1)
#patient data
patient_id <- c(1:1000)
Name <- randomNames(1000)
Birthdate <-
  sample(seq(as.Date('1917/01/01'), as.Date('2017/01/01'), by = "day"), 1000)%>%as.character()
patient <- data.frame(patient_id,
                         Name,
                         Birthdate)
#encounter data
encounter_id <- c(1:3000)
patient_id<- sample(c(1:1000),3000,replace = TRUE)
service_date <-
  sample(seq(as.Date('2017/01/01'), as.Date('2019/01/01'), by = "day"), 3000, replace = TRUE)%>%as.character()
service_type <- sample(c("inpatient", "outpatient", "ED"),3000,replace = TRUE)
Encounter <- data.frame(encounter_id,
                         patient_id,
                         service_date,
                        service_type)

#Diagnosis data
diagnosis_ID <- c(1:5000)
encounter_id<- sample(c(1:3000),5000,replace = TRUE)
Icd9_code <- sample(seq(290,300,0.01),5000,replace = TRUE)
Diagnosis <- data.frame(diagnosis_ID,
                         encounter_id,
                         Icd9_code)


## --------------------------------------------------------------------------------------------
#copy dataframe into sql database
dbWriteTable(con, 'patient', patient)
dbWriteTable(con, 'Encounter', Encounter)
dbWriteTable(con, 'Diagnosis', Diagnosis)

## --------------------------------------------------------------------------------------------
#R codes follow the same analytical process as to the SQL code. I use tidyverse mainly. 
#patient data with qualified ages
patient_qualified <-
  patient %>% mutate(Age = interval(ymd(Birthdate), ymd("2020-03-14")) %>%
                       time_length("year") %>% 
                       round(0)) %>% 
                       filter(Age>=18,
                              Age<=64)
#encounter data with qualified date and service type
encounter_qualified <-
  Encounter %>% filter(service_date%>%as.Date()>="2017-01-01",
                       service_date%>%as.Date()<="2017-12-31",
                       service_type%in%c("inpatient"))
#encounter-based diagnosis data with qualified diagnosis code
diagnose_qualified <-
  Diagnosis %>% 
  mutate(qualified_code=(Icd9_code>=290 & Icd9_code<=319)) %>%
  group_by(encounter_id) %>%
  summarise(Num_PositiveICD9_Diagnosis = sum(qualified_code))
#join all data
encounter_qualified_joined<- patient_qualified %>% 
  inner_join(encounter_qualified, by = "patient_id") %>%
  inner_join(diagnose_qualified, by = "encounter_id") %>%
  group_by(patient_id) %>%
  summarise(QualifiedAdmissions = n())

sum(encounter_qualified_joined$QualifiedAdmissions)/nrow(encounter_qualified_joined)
#There is minor difference between the SQL and R output (2 out of 3000 cases after manual check). If an actual project requires we can output the final dataset in SQL and R to reverse engineer the difference of codes that lead to that


## --------------------------------------------------------------------------------------------
#make flags in encounter data
encounter_flagged <- Encounter %>% 
  mutate(inpatient = service_type %in% c("inpatient"),
         outpatient = service_type %in% c("outpatient"),
         ED = service_type %in% c("ED"))
#make flags in diagnosis
diagnosis_flagged <- Diagnosis %>% 
  mutate(schizophrenia = Icd9_code %in% c(295.00, 295.01, 295.02, 295.03, 295.04, 295.05, 295.10, 295.11, 295.12, 295.13, 295.14, 295.15, 295.20, 295.21, 295.22, 295.23, 295.24, 295.25, 295.30, 295.31, 295.32, 295.33, 295.34, 295.35, 295.40, 295.41, 295.42, 295.43, 295.44, 295.45, 295.50, 295.51,295.52, 295.53, 295.54, 295.55, 295.60, 295.61, 295.62, 295.63,295.64, 295.65, 295.70, 295.71, 295.72, 295.73, 295.74, 295.75,
295.80, 295.81, 295.82, 295.83, 295.84, 295.85, 295.90, 295.91, 295.92, 295.93, 295.94, 295.95),
         bipolar = Icd9_code %in% c(296.00, 296.01, 296.02, 296.03, 296.04, 296.05, 296.06, 296.10, 296.11, 296.12, 296.13, 296.14, 296.15, 296.16, 296.40, 296.41, 296.42, 296.43, 296.44, 296.45, 296.46, 296.50, 296.51, 296.52, 296.53, 296.54, 296.55, 296.56, 296.60, 296.61, 296.62, 296.63, 296.64, 296.65, 296.66, 296.7, 296.80, 296.81, 296.82, 296.89, 296.90, 296.99)) %>%
  group_by(encounter_id) %>%
  summarise(bipolarSum = sum(bipolar),
            schizophreniaSum = sum(schizophrenia))
#merge all data
encounter_joined <- patient %>% 
  inner_join(encounter_flagged, by = "patient_id") %>%
  inner_join(diagnosis_flagged, by = "encounter_id") %>%
  group_by(patient_id) %>%
  summarise(TotalInpatient = sum(inpatient),
            TotalOutpatient = sum(outpatient),
            TotalED = sum(ED),
            FlagBipolar = (sum(bipolarSum)>0) %>% as.numeric(),
            FlagSchizophrenia = (sum(schizophreniaSum)>0) %>% as.numeric()) %>% 
  arrange(patient_id)

encounter_joined 
#the output looks identical to the SQL output. If the project requires, we can always output the data from SQL and compare with the data.frame in R


## --------------------------------------------------------------------------------------------
#clean up database
dbDisconnect(con)
file.remove(tfile)
#rmarkdown::render("simulate-analysis.Rmd")


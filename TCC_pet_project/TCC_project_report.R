## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
#Set up default knitr chunk options
library("knitr")
knitr::opts_chunk$set(
  echo = FALSE,
  eval = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 7,
  fig.width = 15,
  fig.align = "center",
  cache = TRUE,
  cache.lazy = FALSE
)
knitr::opts_knit$set(root.dir = "C:\\Users\\Tianl\\Box\\TCC_Project_Langyi")
options(htmltools.dir.version = FALSE)


## ----theme-map, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
#set up theme map
theme_simplemap <- function(base_size = 9,
                            base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      plot.background = element_blank(),
      legend.position = "none"
    )
}


## ----paths-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#fix paths and system parameters
tokens<-read.csv("Data_storage\\tokens.csv",
                 stringsAsFactors = FALSE)
#setwd("C:\\Users\\Administrator\\Box\\TCC_Project_Langyi")#Change accordingly when run at another device
Sys.setenv(plotly_username = tokens$key[tokens$tokenname=="plotly_username"],
plotly_api_key = tokens$key[tokens$tokenname=="plotly_api_key"],
AWS_ACCESS_KEY_ID = tokens$key[tokens$tokenname=="AWS_ACCESS_KEY_ID"],
AWS_SECRET_ACCESS_KEY = tokens$key[tokens$tokenname=="AWS_ACCESS_KEY_ID"],
AWS_DEFAULT_REGION = "us-east-2")


## ----load packages---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load packages.
packages <- c(
  "devtools",
    "shiny",
    "here",
    "pkgnet",
    "praise",
    "rsconnect",
  #"readxl",
  #"excel.link",
  #"tidyxl",
  #"rJava",
  #"xlsx",
  #"XLConnect",
  "tidyverse",
  "stringr",
  "purrr",
  "data.table",
  "DT",
  "knitr",
  "ggplot2",
  "plotly",
  "widgetframe",
  "gridExtra",
  "lubridate",
  "ggmap",
  "FactoMineR",
  "factoextra",
  "RColorBrewer",
  "tseries",
  "glmnet",
  "randomForest",
  "jtools",
  "ggstance",
  "ggraph",
  "igraph"
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


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dta_processed_list <-
  list.files(
    "Data_storage/dta_processed",
    pattern = "*.RData",
    full.names = TRUE
  )
for (i in dta_processed_list){
  load(i)
}


## ----current cohort, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 5, fig.width = 10, fig.align = "center"--------------------------------------------------------------------
pie_cohort <-
  dta_merged[dta_merged$terminated == FALSE, ] %>% select(name, hire_date)
pie_cohort$hire_date <-
  pie_cohort$hire_date %>% as.Date() %>% cut("year") %>% as.character() %>%
  str_extract("^[:digit:]{4}")
pie_cohort <- pie_cohort %>% group_by(hire_date) %>% summarise(freq = n())
pir_cohort_caption <- paste(
  "Current size ",
  dta_merged %>% filter(terminated == FALSE) %>% nrow(),
  ", ",
  dta_merged %>% filter(
    hire_date %>% as.Date() >= "2018-01-01",
    hire_date %>% as.Date() < "2019-01-01"
  ) %>% nrow(),
  " hired 2018, ",
  dta_merged %>% filter(hire_date %>% as.Date() >= "2019-01-01") %>%
    nrow(),
  " hired 2019",
  sep = ""
)
plot_pie_cohort <- ggplot(pie_cohort,
                          aes(x = "",
                              y = freq,
                              fill = hire_date)) +
  geom_bar(stat = "identity",
           color = "seashell4") +
  geom_text(
    aes(label = paste(freq)),
    color = "gray16",
    position = position_stack(vjust = 0.5),
    size = 5.5
  ) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(caption = pir_cohort_caption %>% paste(),
       fill = "Year Hired") +
  theme_void() +
  theme(text = element_text(size = 20))
plot_pie_cohort


## ----num_by_group, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"----------------------------------------------------------------------
tenure_by_group <-
  dta_merged[dta_merged$group %>% is.na() == FALSE &
               dta_merged$group != "Training", ]
tenure_by_group <-
  tenure_by_group %>% group_by(group, terminated) %>% summarise(
    num = n(),
    qa_mean =
      mean(qa_mean, na.rm = TRUE),
    tenure =
      mean(tenure, na.rm = TRUE)
  )
tenure_by_group$num[tenure_by_group$terminated == TRUE] <-
  (-1 * tenure_by_group$num[tenure_by_group$terminated == TRUE])
tenure_by_group$terminated[tenure_by_group$terminated == TRUE] <-
  "Terminated"
tenure_by_group$terminated[tenure_by_group$terminated == FALSE] <-
  "Active"
#Plot pyramid plot
plot_tenure_group <- ggplot(tenure_by_group,
                            aes(
                              x = group %>% reorder(num),
                              y = num,
                              fill = terminated
                            )) +
  geom_bar(data = filter(tenure_by_group, terminated == "Terminated"),
           stat = "identity") +
  geom_bar(data = filter(tenure_by_group, terminated == "Active"),
           stat = "identity") +
  scale_y_continuous(breaks = seq(-50, 20, 5),
                     labels = c(seq(50, 0,-5),
                                seq(5, 20, 5))) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  coord_flip(expand = TRUE) +
  xlab("") +
  ylab("") +
  labs(fill = "Status") +
  theme_classic() +
  theme(text = element_text(size = 20))

plot_tenure_group


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
attrition_pie <- NULL
for (i in dta_merged$group[is.na(dta_merged$group) == FALSE &
                           dta_merged$group != "Training"] %>% unique()) {
  dta <- dta_merged %>% filter(group == i)
  dta <- dta$reason_detail %>% table() %>% as.data.frame()
  top3 <- dta[order(dta$Freq, decreasing = TRUE), ][1:3, ]
  names(top3)[1] <- "reason"
  top3$reason <- top3$reason %>% as.character()
  top3 <- top3[order(top3$Freq, decreasing = TRUE), ]
  top3 <- rbind(top3, c(NA, (sum(dta$Freq) - sum(top3$Freq)), na.rm = TRUE))
  top3$Freq <- top3$Freq %>% as.numeric()
  reasons <-
    top3$reason %>% fct_reorder(top3$Freq, .fun = max) %>% as.character()
  top3 <- top3 %>% filter(top3$Freq != 0)
  
  attrition_pie[[i]] <- ggplot(top3,
                               aes(
                                 x = "",
                                 y = Freq,
                                 fill = reason %>% fct_reorder(Freq, .fun = max)
                               )) +
    geom_bar(stat = "identity",
             color = "seashell4") +
    geom_text(
      aes(label = paste(Freq)),
      color = "gray16",
      position = position_stack(vjust = 0.5),
      size = 5.5
    ) +
    coord_polar(theta = "y", start = 0) +
    scale_fill_brewer(
      palette = "YlOrRd",
      direction = 1,
      labels = c(reasons[3],
                 reasons[2],
                 reasons[1],
                 "Others")
    ) +
    labs(caption = paste(i),
         fill = "") +
    theme_void() +
    theme(text = element_text(size = 15))
}
grid.arrange(
  attrition_pie[[1]],
  attrition_pie[[2]],
  attrition_pie[[3]],
  attrition_pie[[4]],
  attrition_pie[[5]],
  attrition_pie[[6]],
  attrition_pie[[8]],
  attrition_pie[[9]],
  attrition_pie[[10]]
)



## ---- message=FALSE, echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
dta_lm_old <- dta_merged %>% filter(hire_date %>% as.Date() < "2018-01-01")
lm_old <-
  lm(tenure ~ age + gender + commute_time + qa_mean_w4 + min + gap + supe,
     data = dta_lm_old)
dta_lm_new <- dta_merged %>% filter(hire_date %>% as.Date() > "2018-01-01")
lm_new <-
  lm(tenure ~ age + gender + commute_time + qa_mean_w4 + min + gap + supe,
     data = dta_lm_new)


## ---- message=FALSE, echo=FALSE,warning=FALSE, fig.height = 4, fig.width = 15, fig.align = "center"------------------------------------------------------------------------------------
jtools::plot_summs(
  lm_old,
  lm_new,
  coefs = c(
    "Commute time (min)" = "commute_time",
    "Agent age (year)" = "age",
    "QA Average in first 4 weeks (point)" = "qa_mean_w4",
    "Tenure QA lowest score (point)" = "min",
    "Largest successive decline in QA (point)" =
      "gap"
  ),
  legend.title = "Population",
  model.names = c("Senior agents",
                  "New hires"),
  
  ci_level = 0.5
) +
  scale_x_continuous(breaks = seq(-1.5, 2, by = 0.5)) +
  xlab("Additional impact on agent tenure (month)") +
  ylab("") +
  theme_classic() +
  theme(text = element_text(size = 20))


## ---- message=FALSE, echo=FALSE,warning=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"------------------------------------------------------------------------------------
supe_names <- lm_new$xlevels$supe
var_names <- paste("supe", supe_names, sep = "")
jtools::plot_summs(
  lm_old,
  lm_new,
  coefs = c(
    "Male agents (compared with female)" = "genderM",
    "Ariel Stillman" = "supeAriel Stillman",
    "Carla Martin" = "supeCarla Martin",
    "Devon Gilliam" = "supeDevon Gilliam",
    "Donovan Tucker" = "supeDonovan Tucker",
    "Gia Weinberger" = "supeGia Weinberger",
    "Jeff Mahady" = "supeJeff Mahady",
    "Marie Jenkins" = "supeMarie Jenkins",
    "Megan Stanko" = "supeMegan Stanko",
    "Naila Butt" = "supeNaila Butt",
    "Ray Davis" = "supeRay Davis",
    "Shamala Turner" = "supeShamala Turner"
  ),
  legend.title = "Population",
  model.names = c("Senior agents",
                  "New hires"),
  
  ci_level = 0.5
) +
  scale_x_continuous(breaks = seq(-40, 20, by = 5)) +
  labs(caption = "All supervisors compared with Alison Herman") +
  xlab("Additional impact on agent tenure (month)") +
  ylab("") +
  theme_classic() +
  theme(text = element_text(size = 20))


## ----analyzing supe tenure, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"-------------------------------------------------------------
qa_supe <-
  dta_merged %>% filter(is.na(supe) == FALSE, hire_date %>% as.Date() > "2018-01-01")
qa_supe$terminated[qa_supe$terminated == TRUE] <- "Terminated"
qa_supe$terminated[qa_supe$terminated == FALSE] <- "Active"
tenures <-
  qa_supe %>% group_by(supe) %>% summarise(tenure = tenure %>% median(na.rm =
                                                                        TRUE),
                                           qa_mean = qa_mean %>% median(na.rm =
                                                                          TRUE))
tenures <- tenures[order(tenures$tenure, decreasing = FALSE), ]
labels_supe <- NULL
for (i in 1:nrow(tenures)) {
  record <-
    paste(tenures$supe[i], ": ", tenures$tenure[i] %>% round(digits = 1))
  labels_supe <- c(labels_supe, record)
}

colourCount <- length(unique(qa_supe$supe))
getPalette <- colorRampPalette(brewer.pal(9, "RdBu"))

plot_tenure_supe_box <- ggplot(
  qa_supe,
  aes(
    x = supe %>% fct_reorder(tenure, .fun = median),
    y = tenure,
    fill = supe %>% fct_reorder(tenure, .fun = median),
    color = terminated
  )
) +
  geom_boxplot(color = "black",
               outlier.shape = NA) +
  scale_fill_manual(values = getPalette(colourCount),
                    labels = labels_supe) +
  xlab("") +
  ylab("Average Tenure") +
  labs(color = "Status",
       fill = "Average Tenure",
       caption = "Agents hired after 2018-01-01") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_tenure_supe_box +
  theme(text = element_text(size = 20)) +
  geom_jitter()


## ---- message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
plot_tenure_supe_box <- plot_tenure_supe_box +
  geom_jitter(size = 1,
              aes(
                text = paste(
                  name,
                  "\n",
                  "Age: ",
                  age %>% round(digits = 0),
                  "\n",
                  "Group: ",
                  group,
                  "\n",
                  "Supervisor: ",
                  supe,
                  "(",
                  supe_time,
                  "months total)",
                  "\n",
                  "Hired at: ",
                  hire_date,
                  "\n",
                  "Status: ",
                  terminated,
                  "\n",
                  "Tenure QA:",
                  qa_mean %>% round(digits = 0),
                  "%",
                  "\n",
                  "Tenure: ",
                  tenure %>% round(digits = 1),
                  "months",
                  "\n",
                  "Reason: ",
                  reason_detail,
                  "\n",
                  "---raw codes below-------------"
                )
              )) +
  theme(legend.position = 'none')
d <- api_create(
  plot_tenure_supe_box %>% plotly::ggplotly(height = 400,
                                            width = 1100),
  filename = "plot_tenure_supe_box",
  sharing = "secret"
)
cat(paste(
  "Interactive plot at",
  paste(
    d$web_url %>% str_replace("/$", "?share_key="),
    d$share_key,
    sep = ""
  )
))


## ----commute distance, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"------------------------------------------------------------------
commute <- dta_merged
commute$old <- commute$tenure > 10
commute <-
  commute %>% filter() %>% group_by(commute_start) %>% summarise(
    num = n(),
    old =
      100 * sum(old, na.rm = TRUE) / n(),
    commute_time =
      mean(commute_time, na.rm = TRUE),
    tenure = mean(tenure, na.rm =
                    TRUE),
    mode = commute_mode[1]
  )
#write.csv(commute,"Data_storage/dta_processed/commute.csv")
#commute<-read.csv("Data_storage/dta_processed/commute.csv")
commute$commute_start <-
  commute$commute_start %>% str_replace_all("COCKYSVIL", "Cockeysville") %>%
  str_replace_all("LUTHVLE TIMON", "Lutherville-Timonium")
plot_commute <- ggplot(
  commute %>% filter(commute_time < 100,
                     tenure < 30,
                     is.na(mode) == FALSE),
  aes(
    x = commute_time,
    y = tenure,
    size = num,
    color = old,
    text = commute_start
  )
) +
  geom_point() +
  geom_text(
    data = commute %>% filter(num > 10 |
                                (commute_time < 20 & tenure < 30 & tenure > 10)),
    aes(label = commute_start),
    position = position_jitter(width = 0.1, height = 0.1),
    color = "black",
    size = 4,
    hjust = -0.2,
    vjust = -0.2
  ) +
  scale_size_continuous(range = c(1.5, 25),
                        guide = "none") +
  scale_color_gradient(low = "darkseagreen1", high = "cyan4") +
  xlab("Commute time (minutes)") +
  ylab("Tenure (months)") +
  labs(color = "% agents > 10 mo") +
  theme_classic() +
  theme(text = element_text(size = 20))

plot_commute


## ----analyzing hiring source, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"-----------------------------------------------------------
qa_hire <- dta_merged %>% filter(
  tenure > 4,
  is.na(hire_source) == FALSE,!hire_source %in% c("Aerotek",
                                                  "Kelly Temporary Services",
                                                  "Randstad")
)
qa_hire$terminated[qa_hire$terminated == TRUE] <- "Terminated"
qa_hire$terminated[qa_hire$terminated == FALSE] <- "Active"
plot_qa_hire <- ggplot(qa_hire,
                       aes(
                         x = hire_source,
                         y = qa_mean,
                         fill = hire_source,
                         color = terminated
                       )) +
  geom_boxplot(color = "black") +
  scale_fill_brewer(palette = "Accent",
                    guide = "none") +
  xlab("") +
  ylab("Average QA performance") +
  labs(caption = "Agents w/ tenure > 4 mo",
       color = "Status") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_qa_hire +
  theme(text = element_text(size = 20)) +
  geom_jitter(size = 3)


## ---- message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
plot_qa_hire <- plot_qa_hire +
  geom_jitter(size = 1,
              aes(
                text = paste(
                  name,
                  "\n",
                  "Age: ",
                  age %>% round(digits = 0),
                  "\n",
                  "Group: ",
                  group,
                  "\n",
                  "Supervisor: ",
                  supe,
                  "\n",
                  "Hired at: ",
                  hire_date,
                  "\n",
                  "Status: ",
                  terminated,
                  "\n",
                  "Tenure QA:",
                  qa_mean %>% round(digits = 0),
                  "%",
                  "\n",
                  "Tenure: ",
                  tenure %>% round(digits = 1),
                  "months",
                  "\n",
                  "Reason: ",
                  reason_detail,
                  "\n",
                  "---raw codes below-------------"
                )
              )) +
  theme(legend.position = 'none')
b <- api_create(
  plot_qa_hire %>% plotly::ggplotly(height = 400,
                                    width = 1100),
  filename = "plot_qa_hire",
  sharing = "secret"
)
cat(paste(
  "Interactive plot at",
  paste(
    b$web_url %>% str_replace("/$", "?share_key="),
    b$share_key,
    sep = ""
  )
))


## ----plot monthly trend, message = FALSE, warning = FALSE, echo = FALSE, fig.height = 7, fig.width = 15, fig.align = "center"----------------------------------------------------------
month_dta <- full_dta_merged
month_dta$month <- month_dta$week %>% cut("month")
monthly_qa_by_group <-
month_dta %>% group_by(group, month) %>% summarise(average_QA_score = mean(qa_score, na.rm = TRUE),
n())
monthly_qa_by_group$month <- monthly_qa_by_group$month %>% as.Date()
plot_qa_group <-
ggplot(
monthly_qa_by_group %>% filter(
group %in% c(
"Care",
"Banyan Hill",
"Oxford Club",
"New Market Health",
"Legacy"
)
),
aes(x = month,
y = average_QA_score,
color = group)
) +
geom_line(size = 0.8) +
geom_point(size = 1.8) +
scale_color_brewer(palette = "Set2") +
theme_classic() +
scale_x_date(name = "",
date_breaks = "2 months") +
scale_y_continuous(
name = "QA score",
breaks = seq(0, 100, by = 5),
limits = c(70, 100)
) +
labs(color = "Affiliation Group") +
theme_classic() +
theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
theme(text = element_text(size = 20))

plot_qa_group


## ----reasons, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 10, fig.align = "center"---------------------------------------------------------------------------
pie_dta <- dta_merged[is.na(dta_merged$reason) == FALSE, ]
pie_dta <- pie_dta %>% group_by(reason) %>% summarise(freq = n())
plot_pie_dta <- ggplot(pie_dta,
                       aes(
                         x = "",
                         y = freq %>% reorder(freq),
                         fill = reason
                       )) +
  geom_bar(stat = "identity",
           color = "seashell4") +
  geom_text(
    aes(label = paste(freq)),
    color = "gray16",
    position = position_stack(vjust = 0.5),
    size = 5
  ) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(fill = "Reason") +
  theme_void() +
  theme(text = element_text(size = 20))

plot_pie_dta


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qas <-
qa_supe %>% group_by(supe) %>% summarise(tenure = tenure %>% median(na.rm =
TRUE),
qa_mean = qa_mean %>% median(na.rm =
TRUE))
qas <- qas[order(qas$qa_mean, decreasing = FALSE), ]
labels_supe_2 <- NULL
for (i in 1:nrow(qas)) {
record <- paste(qas$supe[i], ": ", qas$qa_mean[i] %>% round(digits = 1))
labels_supe_2 <- c(labels_supe_2, record)
}

colourCount <- length(unique(qa_supe$supe))
getPalette <- colorRampPalette(brewer.pal(9, "RdYlGn"))

plot_qa_supe_box <- ggplot(
qa_supe,
aes(
x = supe %>% fct_reorder(qa_mean, .fun = median),
y = qa_mean,
fill = supe %>% fct_reorder(qa_mean, .fun = median),
color = terminated
)
) +
geom_boxplot(color = "black",
outlier.shape = NA) +
scale_fill_manual(values = getPalette(colourCount),
labels = labels_supe_2) +
xlab("") +
ylab("Average QA performance") +
labs(color = "Status",
fill = "Average QA Score",
caption = "Agents hired after 2018-01-01") +
theme_classic() +
theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_qa_supe_box +
theme(text = element_text(size = 20)) +
geom_jitter()


## ---- message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
plot_qa_supe_box <- plot_qa_supe_box +
  geom_jitter(size = 1,
              aes(
                text = paste(
                  name,
                  "\n",
                  "Age: ",
                  age %>% round(digits = 0),
                  "\n",
                  "Group: ",
                  group,
                  "\n",
                  "Supervisor: ",
                  supe,
                  "\n",
                  "Hired at: ",
                  hire_date,
                  "\n",
                  "Status: ",
                  terminated,
                  "\n",
                  "Tenure QA:",
                  qa_mean %>% round(digits = 0),
                  "%",
                  "\n",
                  "Tenure: ",
                  tenure %>% round(digits = 1),
                  "months",
                  "\n",
                  "Reason: ",
                  reason_detail,
                  "\n",
                  "---raw codes below-------------"
                )
              )) +
  theme(legend.position = 'none')
e <- api_create(
  plot_qa_supe_box %>% plotly::ggplotly(height = 400,
                                        width = 1100),
  filename = "plot_qa_supe_box",
  sharing = "secret"
)
cat(paste(
  "Interactive plot at",
  paste(
    e$web_url %>% str_replace("/$", "?share_key="),
    e$share_key,
    sep = ""
  )
))


## ----  message=FALSE,echo=FALSE,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
dta_ml <- dta_merged %>% select(qa_mean,
                                age,
                                commute_time,
                                gender,
                                qa_mean_w4,
                                min,
                                gap,
                                group) %>% na.omit()
dta_ml$group <- dta_ml$group %>% str_replace_all(" |./", "")
#dta_ml$supe<-dta_ml$supe%>%str_replace_all(" |./","")
y <- dta_ml$qa_mean
x <- model.matrix(qa_mean ~ ., dta_ml)[, -1]
set.seed(1)
train <- sample(1:nrow(x), nrow(x) * 1 / 2)
test <- -train
y.test <- y[test]


## ---- message=FALSE,echo=FALSE,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
final_rf <- randomForest(y ~ ., x,
                         importance = TRUE)
yhat.rf <- predict(final_rf, x[test,])
rf_MSE <- mean((yhat.rf - y.test) ^ 2, na.rm = TRUE)
rf_MPE <- mean((yhat.rf - y.test) / y.test, na.rm = TRUE)
rf_MAPE <- mean(abs((yhat.rf - y.test) / y.test), na.rm = TRUE)
final_error <- mean(abs(yhat.rf - y.test) * 100 / y.test)


## ----  message=FALSE,echo=FALSE,warning=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"------------------------------------------------------------------------------------
set.seed(2)
train_rf <- randomForest(y ~ ., x,
                         importance = TRUE,
                         maxnodes = 20)
final_model <- train_rf
tree_num <-
  which(final_model$forest$ndbigtree == min(final_model$forest$ndbigtree))[1]
# get tree by index
tree <- randomForest::getTree(final_model,
                              k = tree_num,
                              labelVar = TRUE) %>%
  tibble::rownames_to_column()
tree$prediction <- tree$prediction %>% round(digits = 0)
tree$`split point` <- tree$`split point` %>% round(digits = 0)
tree$`split point`[tree$`split point` == 0] <- "Yes/No"
tree$`split var` <- tree$`split var` %>% str_replace_all("group", "")
tree$`split var` <-
  tree$`split var` %>% str_replace_all("min", "Lowest QA")
tree$`split var` <-
  tree$`split var` %>% str_replace_all("qa_mean_w4", "QA in 1st mo")
tree$`split var` <- tree$`split var` %>% str_replace_all("age", "Age")
tree$`split var` <-
  tree$`split var` %>% str_replace_all("commute_time", "Commute time")
# prepare data frame for graph
graph_frame <- data.frame(
  from = rep(tree$rowname, 2),
  to = c(tree$`left daughter`, tree$`right daughter`)
)
# convert to graph and delete the last node that we don't want to plot
graph <- graph_from_data_frame(graph_frame) %>%
  delete_vertices("0")
# set node labels
V(graph)$node_label <-
  gsub("_", " ", as.character(tree$`split var`))
V(graph)$leaf_label <- as.character(tree$prediction)
V(graph)$split <- as.character(tree$`split point`)
# plot
plot <- ggraph(graph, 'dendrogram') +
  theme_bw() +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(
    aes(label = node_label),
    size = 5,
    vjust = -2.5,
    na.rm = TRUE,
    repel = TRUE
  ) +
  geom_node_label(aes(label = split),
                  vjust = 2.5,
                  na.rm = TRUE,
                  fill = "white") +
  geom_node_label(
    aes(label = leaf_label, fill = leaf_label),
    na.rm = TRUE,
    repel = TRUE,
    colour = "white",
    fontface = "bold",
    show.legend = FALSE
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 18)
  ) +
  labs(caption = paste(
    "Prediction accuracy of final model (%): ",
    100 - final_error %>% round(digits = 2)
  )) +
  theme(text = element_text(size = 20))
print(plot)


## ----plot_qa_tenure, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"--------------------------------------------------------------------
qa_tenure <- dta_merged
qa_tenure$hire_source[is.na(qa_tenure$hire_source) == FALSE &
                        !qa_tenure$hire_source %in% c("TCC Direct Hires",
                                                      "Priority One Staffing",
                                                      "Ultimate Staffing")] <-
  "Others"
qa_tenure$hire_source[is.na(qa_tenure$hire_source) == TRUE] <-
  "Unknown"
plot_qa_tenure <- ggplot(qa_tenure %>% filter(is.na(reason) == FALSE),
                         aes(x = qa_mean,
                             y = tenure,
                             color = reason)) +
  scale_color_brewer(palette = "Paired") +
  xlab("QA Score Over Time") +
  ylab("Tenure (months)") +
  labs(color = "Reason for leaving") +
  theme_classic()

plot_qa_tenure +
  geom_jitter(size = 3) +
  theme(text = element_text(size = 20))


## ----plot_qa_tenure online plotly, message=FALSE, warning=FALSE, echo=FALSE------------------------------------------------------------------------------------------------------------
plot_qa_tenure <- plot_qa_tenure + geom_jitter(size = 1,
                                               aes(
                                                 text = paste(
                                                   name,
                                                   "\n",
                                                   "Age: ",
                                                   age %>% round(digits = 0),
                                                   "\n",
                                                   "Group: ",
                                                   group,
                                                   "\n",
                                                   "Supervisor: ",
                                                   supe,
                                                   "\n",
                                                   "Hired at: ",
                                                   hire_date,
                                                   "\n",
                                                   "Status: ",
                                                   terminated,
                                                   "\n",
                                                   "Tenure QA:",
                                                   qa_mean %>% round(digits = 0),
                                                   "%",
                                                   "\n",
                                                   "Tenure: ",
                                                   tenure %>% round(digits = 1),
                                                   "months",
                                                   "\n",
                                                   "Reason: ",
                                                   reason_detail,
                                                   "\n",
                                                   "---raw codes below-------------"
                                                 )
                                               ))
a <- api_create(
  plot_qa_tenure %>% plotly::ggplotly(height = 400,
                                      width = 1100),
  filename = "plot_qa_tenure",
  sharing = "secret"
)
cat(paste(
  "Interactive plot at",
  paste(
    a$web_url %>% str_replace("/$", "?share_key="),
    a$share_key,
    sep = ""
  )
))


## ----print plotly, message=FALSE, warning=FALSE, echo=FALSE----------------------------------------------------------------------------------------------------------------------------
frameWidget(plot_qa_tenure %>% plotly::ggplotly(),
            width = "100%",
            height = "75%")


## ----plot supervisor by time, message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------
monthly_qa_by_supe <-
  month_dta %>% group_by(group, month, supe) %>% summarise(average_QA_score =
                                                             mean(qa_score, na.rm = TRUE),
                                                           n()) %>%
  na.omit()
monthly_qa_by_supe$month <- monthly_qa_by_supe$month %>% as.Date()
supe_sub <- team_list %>% group_by(supe, group) %>% summarise(n())

plot_qa_supe <- function(group_name = supe_sub$group,
                         supe_name = supe_sub$supe) {
  plot_qa_supe <-
    ggplot(
      monthly_qa_by_supe %>% filter(
        group %in% group_name,
        supe %in% supe_name,
        supe %in% supe_sub$supe[supe_sub$group ==
                                  group_name]
      ),
      aes(x = month,
          y = average_QA_score,
          color = supe)
    ) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Set2") +
    theme_classic() +
    scale_x_date(name = "",
                 date_breaks = "2 months") +
    scale_y_continuous(
      name = "QA score",
      breaks = seq(0, 100, by = 5),
      limits = c(70, 100)
    ) +
    labs(color = "Supervisor",
         caption = paste("Affiliation group: ", group_name)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(text = element_text(size = 20))
  return(plot_qa_supe)
}


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe(group_name = "Care")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe(group_name = "Legacy")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe(group_name = "New Market Health")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe(group_name = "Banyan Hill")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe(group_name = "Oxford Club")


## ----plot qa by group under same supervisor, message=FALSE, warning=FALSE, echo=FALSE--------------------------------------------------------------------------------------------------
plot_qa_supe_group <- function(supe_name = supe_sub$supe,
                               group_name = supe_sub$group) {
  plot_qa_supe <-
    ggplot(
      monthly_qa_by_supe %>% filter(
        group %in% group_name,
        supe %in% supe_name,
        group %in% supe_sub$group[supe_sub$supe %in%
                                    supe_name]
      ),
      aes(x = month,
          y = average_QA_score,
          color = group)
    ) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Set1") +
    theme_classic() +
    scale_x_date(name = "",
                 date_breaks = "2 months") +
    scale_y_continuous(
      name = "QA score",
      breaks = seq(0, 100, by = 5),
      limits = c(70, 100)
    ) +
    labs(color = "Affiliation group",
         caption = paste("Supervisor: ", supe_name)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(text = element_text(size = 20))
  return(plot_qa_supe)
}


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Alison Herman")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Ariel Stillman")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Carla Martin")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Chuck Shirley")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Devon Gilliam")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Donovan Tucker")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Gia Weinberger")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Jeff Mahady")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Marie Bacchus")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Megan Stanko")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Naila Butt")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Patrick Meyers")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Ray Davis")


## ---- message=FALSE, warning=FALSE, echo=FALSE, fig.height = 9, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------------------
plot_qa_supe_group("Shamala Turner")


## ---- message=FALSE, eval=FALSE,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
## #MCA based on the data
## dta_mca <- dta_merged %>% na.omit()
## dta_mca$qa_mean <- dta_mca$qa_mean %>% cut(
##   breaks = c(
##     -Inf,
##     quantile(dta_mca$qa_mean,
##              0.3,
##              na.rm = TRUE),
##     quantile(dta_mca$qa_mean,
##              0.7,
##              na.rm = TRUE),
##     Inf
##   ),
##   labels = c("low_QA",
##              "mid_QA",
##              "high_QA")
## )
## dta_mca$qa_sd <- dta_mca$qa_sd %>% cut(
##   breaks = c(
##     -Inf,
##     quantile(dta_mca$qa_sd,
##              0.3,
##              na.rm = TRUE),
##     quantile(dta_mca$qa_sd,
##              0.7,
##              na.rm = TRUE),
##     Inf
##   ),
##   labels = c("low_QA_sd",
##              "mid_QA_sd",
##              "high_QA_sd")
## )
## dta_mca$tenure <- dta_mca$tenure %>% as.character() %>% as.numeric()
## dta_mca$tenure <- dta_mca$tenure %>% cut(
##   breaks = c(-Inf,
##              4,
##              10,
##              20,
##              Inf),
##   labels = c("<4weeks",
##              "4-10weeks",
##              "10-20weeks",
##              ">20weeks")
## )
## dta_mca$age <- dta_mca$age %>% cut(
##   breaks = c(-Inf,
##              25,
##              30,
##              40,
##              Inf),
##   labels = c("age<25",
##              "age25-30",
##              "age30-40",
##              "age>40")
## )
## dta_mca$commute_time <-
##   dta_mca$commute_time %>% cut(
##     breaks = c(-Inf, 15, 30, Inf),
##     labels = c("fast_commute",
##                "mid_commute",
##                "slow_commute")
##   )
## for (i in names(dta_mca)) {
##   dta_mca[[i]] <- dta_mca[[i]] %>% as.factor()
## }


## ---- eval=FALSE,message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
## mca <- MCA(
##   dta_mca %>% select(-name),
##   ncp = 3,
##   na.method = "Average",
##   graph = FALSE
## )
## #Visualize MCA outcome
## fviz_mca_biplot(
##   mca,
##   repel = TRUE,
##   col.var = "orange2",
##   col.ind = "darkseagreen2",
##   label = "var"
## ) +
##   ggtitle("Multiple Correspondence Analysis of Performance relations",
##           subtitle = "Groups in orange, individuals in blue") +
##   labs(caption = "Source: Merged data") +
##   theme_classic()


## ---- eval=FALSE,message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
## ggplot(dta_merged[dta_merged$tenure < 12],
##        aes(x = age,
##            y = tenure)) +
##   geom_jitter() +
##   xlab("age") +
##   ylab("Tenure (months)")
## 
## 
## #a%>%group_by(age.x)%>%summarise(n(),
## #                                mean(tenure.y,na.rm = TRUE),
## #                                mean(qa_mean.y,na.rm = TRUE),
## #                                mean(commute_time.y,na.rm = TRUE))


## ---- eval=FALSE,message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
## #So what's the difference between terminated people and those who are not?
## dta_merged %>% group_by(terminated) %>% summarise(mean(qa_mean, na.rm = TRUE), mean(qa_sd, na.rm = TRUE))


## ----eval=FALSE,message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
## ggplot(dta_merged,
##        aes(x = commute_dist,
##            y = tenure)) +
##   geom_jitter()


## ---- eval=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
## dta_lm <- dta_merged %>% select(-name,-hire_date,-termination_date,-home_zip,-center,-team)
## #dta_lm$center<-dta_lm$center%>%as.character()
## #dta_lm$team<-dta_lm$team%>%as.character()
## for (i in names(dta_lm)) {
##   if (class(dta_lm[[i]]) == "integer") {
##     dta_lm[[i]] <- dta_lm[[i]] %>% as.numeric()
##   }
## }
## lm(tenure ~ ., data = dta_lm) %>% summary()


## ----headcount flow, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"--------------------------------------------------------------------
hire <- cbind(rep("hire", length(dta_merged$hire_date)),
              dta_merged$hire_date %>% as.character())
termination <-
  cbind(rep("termination", length(dta_merged$termination_date[dta_merged$terminated ==
                                                                TRUE])),
        dta_merged$termination_date[dta_merged$terminated == TRUE] %>%
          as.character())
flow_dta <- rbind(hire, termination) %>% as.data.frame()
names(flow_dta) <- c("decision", "time")
flow_dta$time <-
  flow_dta$time %>% as.Date() %>% cut("1 month") %>% as.Date()
flow_dta <- flow_dta %>% group_by(time, decision) %>% summarise(head = n())
plot_flow <- ggplot(flow_dta[flow_dta$time > "2017-12-31", ],
                    aes(x = time,
                        y = head,
                        color = decision)) +
  geom_line() +
  geom_point(size = 1) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  scale_x_date(name = "",
               date_breaks = "2 months") +
  scale_y_continuous(name = "Headcount") +
  ggtitle("Headcount flow since Jan 2018",
          subtitle = "Summary every month") +
  labs(caption = "Source: ADP Workforce Data",
       color = "Type") +
  theme_classic() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 30, hjust = 1))

plot_flow


## ----qa vs sd over time, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 10, fig.align = "center"----------------------------------------------------------------
plot_qa_sd <- ggplot(dta_merged[dta_merged$n_eval_sum >= 2, ],
                     aes(x = qa_sd,
                         y = qa_mean,
                         color = n_eval_sum)) +
  geom_jitter() +
  scale_color_gradient(low = "darkseagreen1", high = "cyan4") +
  ggtitle("High performers tend to perform also more stably",
          subtitle = "agent with <3 evaluations noted in red") +
  ylab("Quality Assurance Score over time") +
  xlab("Fluctuation (Standard Deviation) of QA score over time") +
  labs(caption = "Source: Quality Assurance Data (Jan 2018 to May 2019)",
       color = "Times evaluated") +
  theme_classic()


## ----qa_by_cohort, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 10, fig.align = "center"----------------------------------------------------------------------
qa_by_cohort <- dta_merged
qa_by_cohort$hire_date <- qa_by_cohort$hire_date %>% as.Date()
qa_by_cohort <- qa_by_cohort[qa_by_cohort$hire_date < "2019-04-14", ]
qa_by_cohort$longmonth <- NA
qa_by_cohort$longmonth[qa_by_cohort$tenure < 2] <- "0 to 2 months"
qa_by_cohort$longmonth[qa_by_cohort$tenure >= 2 &
                         qa_by_cohort$tenure <= 4] <- "2 to 4 months"
qa_by_cohort$longmonth[qa_by_cohort$tenure > 4] <-
  "more than 4 months"
plotly_qa_cohort <- ggplot(qa_by_cohort,
                           aes(
                             x = hire_date,
                             y = qa_mean,
                             color = longmonth,
                             text = paste(
                               "Agent: ",
                               name,
                               "Tenure: ",
                               tenure,
                               "Terminated: ",
                               terminated,
                               sep = "\n"
                             )
                           )) +
  geom_jitter(size = 1) +
  ggtitle("Significant decrease in short-term quit") +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(name = "Date hired/Class",
               date_breaks = "2 months") +
  ylab("Average QA Score") +
  labs(color = "Tenure") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plotly_qa_cohort


## ----age_by_cohort, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"---------------------------------------------------------------------
plotly_age_cohort <- ggplot(qa_by_cohort,
                            aes(
                              x = hire_date,
                              y = qa_mean,
                              color = longmonth,
                              text = paste(
                                "Agent: ",
                                name,
                                "Tenure: ",
                                tenure,
                                "Terminated: ",
                                terminated,
                                sep = "\n"
                              )
                            )) +
  geom_jitter(size = 1,
              width = 3,
              height = 0.6) +
  ggtitle("Significant decrease in short-term quit") +
  scale_color_brewer(palette = "Set1") +
  scale_x_date(name = "Date hired/Class",
               date_breaks = "2 months") +
  ylab("Average QA Score") +
  labs(color = "Tenure") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plotly_age_cohort


## ----analyzing qa by group, message=FALSE, warning=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"-------------------------------------------------------------
qa_group <- dta_merged %>% filter(is.na(group) == FALSE &
                                    group != "Training")
qa_group$terminated[qa_group$terminated == TRUE] <- "Terminated"
qa_group$terminated[qa_group$terminated == FALSE] <- "Active"
plot_qa_group_box <- ggplot(qa_group,
                            aes(
                              x = group,
                              y = qa_mean,
                              fill = group,
                              color = terminated
                            )) +
  geom_boxplot(color = "black") +
  scale_fill_brewer(palette = "Set3",
                    guide = "none") +
  xlab("") +
  ylab("Average QA performance") +
  labs(color = "Status",
       caption = "Agent hired after 2018-01-01") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

plot_qa_group_box +
  theme(text = element_text(size = 20)) +
  geom_jitter()


## ---- message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
plotly_qa_group_box <- plot_qa_group_box +
  geom_jitter(size = 1,
              aes(
                text = paste(
                  name,
                  "\n",
                  "Age: ",
                  age %>% round(digits = 0),
                  "\n",
                  "Group: ",
                  group,
                  "\n",
                  "Supervisor: ",
                  supe,
                  "\n",
                  "Hired at: ",
                  hire_date,
                  "\n",
                  "Status: ",
                  terminated,
                  "\n",
                  "Tenure QA:",
                  qa_mean %>% round(digits = 0),
                  "%",
                  "\n",
                  "Tenure: ",
                  tenure %>% round(digits = 1),
                  "months",
                  "\n",
                  "Reason: ",
                  reason_detail,
                  "\n",
                  "---raw codes below-------------"
                )
              )) +
  theme(legend.position = 'none')
c <- api_create(
  plotly_qa_group_box %>% plotly::ggplotly(height = 400,
                                           width = 1100),
  filename = "plotly_qa_group_box",
  sharing = "secret"
)
cat(paste(
  "Interactive plot at",
  paste(
    c$web_url %>% str_replace("/$", "?share_key="),
    c$share_key,
    sep = ""
  )
))


## ----  message=FALSE,echo=FALSE,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
grd <- 10 ^ seq(10, -2, length = 100)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
#plot(cv.out)
bestlam <- cv.out$lambda.min
out <- glmnet(x,
              y,
              nlambda = 1,
              lambda = bestlam,
              alpha = 1)
ridge.pred <- predict(cv.out, s = bestlam, newx <- x[test,])
#mean(abs(ridge.pred-y.test))


## ----  message=FALSE,echo=FALSE,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
coef(out)


## ----  message=FALSE,echo=FALSE,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------
lasso_out <- cbind(ridge.pred, y.test, x[test, ]) %>% as.data.frame()
ggplot(lasso_out) +
  geom_jitter(aes(x = qa_mean_w4,
                  y = y.test),
              color = "black") +
  geom_jitter(aes(x = qa_mean_w4,
                  y = ridge.pred),
              color = "red") +
  theme_classic()


## ---- message=FALSE, warning=FALSE, eval=FALSE, echo=FALSE, fig.height = 7, fig.width = 15, fig.align = "center"-----------------------------------------------------------------------
## month_dta <- full_dta_merged
## month_dta$month <- month_dta$week %>% cut("month")
## month_dta <-
##   month_dta %>% group_by(name, month) %>% summarise(average_QA_score = mean(qa_score, na.rm = TRUE),
##                                                     n())
## month_dta$month <- month_dta$month %>% as.Date()
## dta_merged$stafftime <- time_length(difftime(dta_merged$staff_date,
##                                              dta_merged$hire_date,
##                                              units = "days"),
##                                     unit = "months") * -1
## dta_merged <- dta_merged %>% filter(stafftime > 1,
##                                     is.na(dta_merged$stafftime) == FALSE)
## yo <- yo[order(yo$stafftime, decreasing = TRUE), ]
## yo_names <- yo$name
## 
## aya <- function(haha) {
##   hire_time <- dta_merged$hire_date[dta_merged$name == haha]
##   staff_time <- dta_merged$stafftime[dta_merged$name == haha]
##   staff_dat <- dta_merged$staff_date[dta_merged$name == haha]
## 
##   plot_qa_group <- ggplot(month_dta %>% filter(name %in% c(haha)),
##                           aes(x = month,
##                               y = average_QA_score)) +
##     geom_line(size = 1.5) +
##     geom_point(size = 3) +
##     scale_color_brewer(palette = "Set2") +
##     scale_x_date(name = "",
##                  date_breaks = "1 month") +
##     scale_y_continuous(
##       name = "QA score",
##       breaks = seq(0, 100, by = 5),
##       limits = c(70, 100)
##     ) +
##     geom_vline(
##       aes(xintercept = as.numeric(as.Date(hire_time))),
##       linetype = "dotted",
##       color = "blue",
##       size = 1.5
##     ) +
##     labs(color = "Affiliation Group") +
##     ggtitle(paste(
##       haha,
##       staff_dat,
##       staff_time %>% round(digits = 0),
##       hire_time,
##       sep = ","
##     )) +
##     theme_classic() +
##     theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
##     theme(text = element_text(size = 20))
## 
##   return(plot_qa_group)
## }


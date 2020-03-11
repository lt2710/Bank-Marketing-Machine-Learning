## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls())#clear environment
wd <- "~/GitHub/Pet-projects/vote-analysis/"#set working directory
#Set up default knitr chunk options
library("knitr")
knitr::opts_chunk$set(
  echo = FALSE,
  eval = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.height = 6,
  fig.width = 15,
  fig.align = "center",
  cache = TRUE,
  cache.lazy = FALSE
)
options(htmltools.dir.version = FALSE)


## ----theme-map, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ----load packages---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
packages <- c(
  "tidyverse",
  "tidymodels",
  "tune",
  "vip",
  "ggplot2",
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
dta <- read.csv("Salaries.csv",
                  stringsAsFactors = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#print a summary
summary(dta)
salary_plot <- ggplot(dta %>% mutate(salary = salary),
                      aes(x = salary)) +
  geom_histogram(fill = "skyblue2") +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(text = element_text(size = 15))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#print an answer to the question 1
cat((nrow(dta%>%filter(rank=="AsstProf",yrs.service<5))*100/nrow(dta))%>%round(2),
    "% of records are Assistant Professors with less than 5 years of experience.",
    sep = "")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#view gender difference in salary
dta%>%group_by(sex)%>%summarise(mean(salary))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#perform wilcoxon test
wilcox.test(salary~sex,data = dta)


## ----fig.height=3.5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ggplot2 framwork
rank_plot <- ggplot(dta,
                    aes(x = rank %>% reorder(dta$salary),
                        y = salary)) +
  geom_boxplot(fill = "skyblue2")
discipline_plot <- ggplot(dta,
                          aes(x = discipline %>% reorder(dta$salary),
                              y = salary)) +
  geom_boxplot(fill = "skyblue2")
#a function to make the plots more pretty
plot_pretty <- function(plot) {
  pretty_plot <- plot + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(text = element_text(size = 15)) 
  return(pretty_plot)
}
#integrate the two plots
grid.arrange(plot_pretty(rank_plot),
             plot_pretty(discipline_plot),
             nrow = 1)


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
discipline_B<-(dta$discipline=="B")%>%as.numeric()#dummy code it to avoid redundancy/collinearity. will use dplyr::mutate if manipulating a dataframe


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(discipline_B)#view the result


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#inspect missing values
missing_glimpse(dta)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#test correlation between years of service and years since phd
cor.test(dta$yrs.since.phd, dta$yrs.service, method="pearson")


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dta<-dta%>%mutate(gap=yrs.since.phd-yrs.service)#manually construct a variable measuring gap between year of phd graduation and year starting service


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#view summary
dta<-dta%>%select(-yrs.since.phd)
summary(dta$gap)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
#initial 8:2 split betwee train test set
dta_split <- initial_split(dta, prop = .8)
train <- training(dta_split)
test  <- testing(dta_split)
#define 5 fold cv in training set to tune hyperparameters
vfold <- vfold_cv(train, v = 5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#modeling framework
formula.reg <- as.formula(salary ~ .)
salary_rec <- recipe(formula.reg,
                     data = train) %>%
  step_log(all_outcomes(), base = 10) %>%
  step_string2factor(all_nominal(),-all_outcomes()) %>%
  step_dummy(all_nominal(),-all_outcomes()) %>%
  step_poly(yrs.service, degree = 2)  %>%
  step_normalize(all_predictors(),-all_nominal())

salary_rec


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define glmn model
glmn_model <-
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
glmn_model


## ----results=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define parameter grid
glmn_grid <- glmn_model %>% parameters() %>% grid_regular(levels = c(10,10))
#grid search
glmn_reg_search <-
  tune_grid(salary_rec,
            model = glmn_model,
            resamples = vfold,
            grid = glmn_grid)
show_best(glmn_reg_search, metric = "rmse", maximize = FALSE)


## ----fig.height=3.5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#visualize CVed performance on parameter grids
rmse_vals <- collect_metrics(glmn_reg_search) %>% filter(.metric == "rmse")
rmse_vals %>% 
  mutate(mixture = format(mixture)) %>% 
  ggplot(aes(x = penalty, y = mean, col = mixture)) + 
  ylab("RMSE") +
  geom_line() + 
  scale_x_log10() + 
  scale_color_brewer() +
  theme_classic() +
  theme(text = element_text(size = 15)) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#inspect the best coefficient
best_glmn <-
  select_best(glmn_reg_search, metric = "rmse", maximize = FALSE)
best_glmn


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#finalize the recipe
final_rec <- salary_rec %>% prep()
#finalize the best trained glmn model
final_glmn_model <-
  glmn_model %>%
  finalize_model(best_glmn) 
fit_final_glmn_model <- final_glmn_model%>%
  fit(formula.reg, data = juice(final_rec))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#plot var importance
vip(fit_final_glmn_model,
    fill = "skyblue2")+
  xlab ("Importance (coefficient magnitude)") +
  theme_classic() +
  theme(text = element_text(size = 15))


## ---- fig.height=3.5-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#preprocess the test set using the same recipe
test_baked<-bake(final_rec,test)
#plot residual
test_prediction<-cbind(.pred=predict(fit_final_glmn_model,new_data = test_baked),
                       salary=test_baked$salary)
ggplot(test_prediction, aes(x = .pred, y = salary)) + 
  geom_abline(col = "skyblue2") + 
  geom_point(alpha = .8) + 
  xlab("Predicted value") +
  ylab("Salary") +
  theme_classic() +
  theme(text = element_text(size = 15))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#calculate RMSE on test set
test_prediction%>%rmse(truth=salary,
                       estimate=.pred)


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dta <- dta %>% mutate(salary = (salary >= median(dta$salary)) %>% as.numeric()%>% as.factor())#Dummy code the salary variable


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#inspect salary indicator
table(dta$salary)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
#initial 8:2 split betwee train test set
dta_split <- initial_split(dta, prop = .8)
train <- training(dta_split)
test  <- testing(dta_split)
#define 5 fold cv in training set to tune hyperparameters
vfold <- vfold_cv(train, v = 5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#modeling framework
formula.reg <- as.formula(salary ~ .)
salary_rec <- recipe(formula.reg,
                     data = train) %>%
  step_string2factor(all_nominal(),-all_outcomes()) %>%
  step_dummy(all_nominal(),-all_outcomes()) %>%
  step_poly(yrs.service, degree = 2)  %>%
  step_normalize(all_predictors(),-all_nominal())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define glmn model
cart_model <-
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>%
  set_mode("classification")
cart_model 


## ----results=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define parameter grid
cart_grid <- cart_model %>% parameters() %>% grid_regular(levels = c(5,5))
#grid search
ctrl <- control_grid(save_pred = TRUE)
cart_reg_search <-
  tune_grid(salary_rec,
            model = cart_model,
            resamples = vfold,
            grid = cart_grid,
            control = ctrl)
show_best(cart_reg_search, metric = "accuracy", maximize = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
best_cart <-
  select_best(cart_reg_search, metric = "accuracy", maximize = FALSE)
#inspect the best coefficient
best_cart


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#finalize the recipe
final_rec <- salary_rec %>% prep()
#finalize the best trained glmn model
final_cart_model <-
  cart_model %>%
  finalize_model(best_cart) 
fit_final_cart_model <- final_cart_model%>%
  fit(formula.reg, data = juice(final_rec))


## ---- fig.height=4---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#preprocess the test set using the same recipe
test_baked <- bake(final_rec, test)
#plot residual
test_prediction <-
  cbind(
    .pred_class = predict(fit_final_cart_model, new_data = test_baked),
    salary = test_baked$salary
  ) %>% mutate(.pred_class = .pred_class %>% as.factor())
conf_mat_test<-conf_mat(test_prediction,
         truth = salary,
         estimate = .pred_class)
autoplot(conf_mat_test, type = "heatmap")+
  theme_classic() +
  theme(text = element_text(size = 15))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#calculate accuracy on test set
test_prediction %>% accuracy(truth = salary,
                             estimate = .pred_class)


## ----echo=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(pwr)
pwr.f2.test(
  u = 10,#10 independent variables (less intercept)
  f2 = 0.5/(1-0.5),#Assume the R2 is 0.5
  sig.level = 0.001,#Need 0.001 significance level 
  power = 0.8#Assuming 0.8 power
)


## ----fig.height=3.5--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
salary_plot#pull up y plot again


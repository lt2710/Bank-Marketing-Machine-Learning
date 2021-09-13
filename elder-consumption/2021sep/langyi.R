library(jtools)
library(DescTools)
library(qwraps2)
library(tidyverse)
library(VGAM)
library(gtsummary)
library(car)
library(lmtest)
############### data cleaning #######################################
CFPS_elders_cons3 <-
  CFPS_elders_cons3 %>%
  mutate(age_group2cat = cut(age, breaks = c(15, 35, 96)),
         age_quant = cut(age, breaks = quantile(CFPS_elders_cons3$age,na.rm = T)),
         fincome1_logged = log(fincome1+1),
         med_log = log(med+1),
         familysize_cap = pmin(familysize,5) %>% as.character(),
         trav_leisure_exp_logged = log(trav_leisure_exp+1),
         fincomeQ_filtered = cut(fincome1, 
                                 breaks = quantile(CFPS_elders_cons3$fincome1[CFPS_elders_cons3$trav_leisure_exp>0],na.rm = T)),
         trav_leisure_exp_quant = cut(trav_leisure_exp, 
                                      breaks = quantile(CFPS_elders_cons3$trav_leisure_exp[CFPS_elders_cons3$trav_leisure_exp>0],na.rm = T)))

############### inspect predictor correlation #####################
predictors_numeric = c("fincome1",
               "education",
               "age",
               "urban_hukou",
               "cfps_party",
               "med_log",
               "familysize")
# correlation among predictors
data_temp = 
  CFPS_elders_cons3 %>% 
  select(all_of(predictors_numeric),trav_leisure_exp) %>%
  mutate(education = as.numeric(education))
cor(data_temp, use= "complete.obs")
######################## modeling binary #######################
# inspect target
summary(CFPS_elders_cons3$trav_leisure_exp_bin)
# inspect target vs. key predictor
CFPS_elders_cons3 %>%
  tbl_cross(row = "age_quant",
            col = "trav_leisure_exp_bin",
            percent = "row",
            missing = "no")
# inspect target vs. predictor
CFPS_elders_cons3 %>% 
  group_by(trav_leisure_exp_bin) %>%
  summarise(
    n(),
    mean(fincome1, na.rm =T),
    mean(education %>% as.numeric(), na.rm =T),
    mean(age, na.rm =T),
    mean(urban_hukou, na.rm =T),
    mean(cfps_party, na.rm =T),
    mean(med, na.rm =T)
  )
# build model
predictors = c("fincomeQ",
               "education",
               # "age",
               "age_group3cat",
               # "age_group2cat",
               # "age_quant",
               "urban_hukou",
               "cfps_party",
               "med_log",
               "familysize"
               # "age_group3cat:fincome1_logged"
               )
formula1 <-
  formula(paste("trav_leisure_exp_bin ~",
                paste(predictors,collapse = "+")))
#for the binary logistic regression
model1 <-
  glm(
    formula1,
    data = CFPS_elders_cons3,
    family = "binomial"
  )
export_summs(model1)
# confusion matrix
fitted.y = ifelse(model1$fitted.values>0.5,
             1,
             0)
table(model1$y,fitted.y)

###################### model continuous ########################
# inspect target
summary(CFPS_elders_cons3$trav_leisure_exp[CFPS_elders_cons3$trav_leisure_exp>0])
summary(CFPS_elders_cons3$trav_leisure_exp_logged[CFPS_elders_cons3$trav_leisure_exp_logged>0])
# inspect outcome vs. key predictor
data_temp = 
  CFPS_elders_cons3 %>% 
  filter(trav_leisure_exp>0) 
data_temp %>%
  tbl_cross(row = "age_quant",
            col = "trav_leisure_exp_quant",
            percent = "row",
            missing = "no")
# inspect outcome vs. predictor
for (var in predictors_numeric){
  print(var)
  cor_var = cor(data_temp$trav_leisure_exp,data_temp[[var]]%>%as.numeric(),use= "complete.obs")
  print(cor_var)
}
data_temp %>% 
  mutate(med_quant = cut(med, breaks = quantile(data_temp$med, na.rm = T))) %>%
  group_by(med_quant) %>%
  summarise(n(),
            mean(trav_leisure_exp_logged, na.rm = T))
# build model
formula2 <-
  formula(paste("trav_leisure_exp_logged ~",
                paste(predictors,collapse = "+")))
## lm
model2 = lm(
  formula2,
  data = data_temp
)
export_summs(model2)
## tobit
model3 = vglm(formula2,
     tobit(Lower  = 1),
     data = CFPS_elders_cons3)
summary(model3)
# model diagnostics 
## resid
car::residualPlot(model2)
## BP test
lmtest::bptest(formula2, data = data_temp)
## LM assumption
data_for_gvlma = data_temp %>% 
  select(trav_leisure_exp_logged, predictors) %>%
  na.omit()
gvlma::gvlma(formula2, data_for_gvlma)

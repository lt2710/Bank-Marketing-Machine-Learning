#Data preparation from CFPS_elder_cons

#See other script for the details
#Whate are the idea:
#1 Recode occupation according to the scheme previously used by langyi
#2. Create variable related to travel+leisure expenditure, log of travel+leisure, and one binary
#3. Recreate different age category (2cat= born after/before the reform, and 3cat Born after the great famine, experienced great famine, born before PRC establishment,5cat ) 
#4. Create two new subset of variables. CFPS_elders_cons3 contains all the observation minus fincome<0 and fincome na, CFPS_elders_cons3 is the same but only with people having expenditure
#5. Divide Fincome into quantile because the linearity at the log need to be respected in the binary model and to get better linear model (logfincome do not work)
#6. We ends up with two very good binary logistic model (model_bin_agegroup/model_bin_3agegroup). they do not include as much variable than Langyi's previous model because otherwise, predicted prop 0 or 1 happen (overfitted)
#7.We ends up with two acceptable linear regression model but need robust regression (Breusch Pagan test not passed)

#Step1: the binary logistic models
#try to redo the model with born after/before the reforms
CFPS_elders_cons3$age_group2cat<-cut(CFPS_elders_cons3$age,breaks = c(15,35,96))
table(CFPS_elders_cons3$age_group2cat)
#for the binary logistic regression
model_bin_agegroup<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group2cat+urban_hukou+cfps_party+med, data=CFPS_elders_cons3,family="binomial")
export_summs(model_bin_agegroup)
PseudoR2(model_bin_agegroup,which="all")
confusion_matrix(model_bin_agegroup)
#74.8% of correctly predicted no, 68% of correctly predicted yes / mac fadden 0.189
#The model for three age category is good as weel
model_bin_3agegroup<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group3cat+urban_hukou+cfps_party+med, data=CFPS_elders_cons3,family="binomial")
export_summs(model_bin_3agegroup)
PseudoR2(model_bin_3agegroup,which="all")
confusion_matrix(model_bin_3agegroup)
#The two models of logistic regression are okay in term of goodness and fit and assumptions
#Conclusion:
#While medical expenditure is controlled in order to assest physical condition (which can have an impact on travel expenditure)
# While three sorts of capital, economic, cultural, political and hukous status are include
#generation still significantly determining whether people have/haven't expenditure on travel and leisure
#More precisely, people having experience of food shortage are 35% less likely at the odd to have expenditure
#People having experience of war and gret famine have 29% less likely at the odd to have expenditure
#For the model in 2 categories
# People born before the reforms have 55% less chance at the odd to have expenditure

#Step 2: Trying to predict the expenditure for those who have
#for the linear regression
CFPS_elders_cons3_ter$age_group2cat<-cut(CFPS_elders_cons3_ter$age,breaks = c(15,35,96))
model_lin_agegroup<-glm(data=CFPS_elders_cons3_ter,log_travel_leisure~fincomeQ+education+age_group2cat+urban_hukou+cfps_party+familysize+med)
export_summs(model_lin_agegroup)
model_lin_3agegroup<-glm(data=CFPS_elders_cons3_ter,log_travel_leisure~fincomeQ+education+age_group3cat+urban_hukou+cfps_party+familysize+med)
export_summs(model_lin_3agegroup,model_lin_agegroup)

#Test of assumption for model_lin_3agegroup
residualPlot(model_lin_3agegroup)
c_d = cooks.distance(model_lin_3agegroup)
summary(c_d)
#Residual plot and Cook distance (influential point) seems okay 

#Test of assumption for model_lin_agegroup
residualPlot(model_lin_agegroup)
c_d = cooks.distance(model_lin_agegroup)
summary(c_d)
#Residual plot and Cook distance (influential point) seems okay 

#plot a actual vs predicted
#need to create a new dataframe for that
lm_model1<-lm(data=CFPS_elders_cons3_ter,log_travel_leisure~fincomeQ+education+age_group2cat+urban_hukou+cfps_party+familysize+med)
plot(lmodel1)
CFPS_elders_cons3_ter$predicted<-predict(lm_model1)
df_for_estimate<-CFPS_elders_cons3_ter[,c('log_travel_leisure','fincomeQ','education','age_group3cat','urban_hukou','cfps_party','familysize','med')]
df_for_estimate<-na.omit(df_for_estimate)
lm_estimate<-lm(data=df_for_estimate,log_travel_leisure~fincomeQ+education+age_group3cat+urban_hukou+cfps_party+familysize+med)
plot(predict(lm_estimate),df_for_estimate$log_travel_leisure,xlab = "Predicted Values",ylab = "Observed Values")
abline(a=0, b=1)
bptest(lm_estimate)
gvlma(lm_model1)
#Conclusion
#While the OLs models look fine, they do not pass the breusch pagan test, therefore, we need to use robust regressions model


#robust regressions alternative
rlm_model<-rlm(data=CFPS_elders_cons3_ter,log_travel_leisure~fincomeQ+education+age_group2cat+urban_hukou+cfps_party+familysize+med)
stargazer(rlm_model,type="text")
rlm_model2<-rlm(data=CFPS_elders_cons3_ter,log_travel_leisure~fincomeQ+education+age_group3cat+urban_hukou+cfps_party+familysize+med)
stargazer(rlm_model2,type="text")
rlm_model2<-rlm(data=CFPS_elders_cons3_ter,log_travel_leisure~fincomeQ+education+age_group5cat+urban_hukou+cfps_party+familysize+med)
stargazer(rlm_model2,type="text")
#sandwitch regression alternative (the sandwich package computes robust covariance matrix estimators)
#https://data.library.virginia.edu/understanding-robust-standard-errors/
#test with HC1
coeftest(rlm_model, vcov = vcovHC(rlm_model, type="HC1"))
plot(rlm_model)
coeftest(rlm_model2, vcov = vcovHC(rlm_model2, type="HC1"))
plot(rlm_model2)
#test with HC3
coeftest(rlm_model, vcov = vcovHC(rlm_model, type="HC3"))
plot(rlm_model)
coeftest(rlm_model2, vcov = vcovHC(rlm_model2, type="HC3"))
plot(rlm_model2)



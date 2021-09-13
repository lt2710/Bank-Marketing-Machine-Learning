#binary variable for travel + consumption expenditure
CFPS_elders_cons$trav_leisure_exp_bin<-CFPS_elders_cons$trav_leisure_exp
CFPS_elders_cons$trav_leisure_exp_bin<-dplyr::recode(CFPS_elders_cons$trav_leisure_exp_bin, `0` = "no", .default = "yes")
table(CFPS_elders_cons$trav_leisure_exp_bin)
class(CFPS_elders_cons$trav_leisure_exp_bin)
CFPS_elders_cons$trav_leisure_exp_bin<-as.factor(CFPS_elders_cons$trav_leisure_exp_bin)
table(CFPS_elders_cons$urban_hukou)
summary(CFPS_elders_cons$familysize)
CFPS_elders_cons$log_familysize<-log(CFPS_elders_cons$familysize)
table(CFPS_elders_cons$job)
CFPS_elders_cons$occupation_4cat<-CFPS_elders_cons$job
class(CFPS_elders_cons$occupation_4cat)
CFPS_elders_cons$occupation_4cat<-as.factor(CFPS_elders_cons$occupation_4cat)
tabulate(CFPS_elders_cons$occupation_4cat)
CFPS_elders_cons$occupation_4cat<-dplyr::recode(CFPS_elders_cons$occupation_4cat,'1'="Agricultural laborer or unemployed",'2'="Agricultural laborer or unemployed",'3'="Routine manual non-manual and individual bus. own",'4'="Routine manual non-manual and individual bus. own",'5'="Routine manual non-manual and individual bus. own",'6'="semi skilled and skilled white colars",'7'="semi skilled and skilled white colars",'8'="ruling position",'9'="ruling position",'10'="ruling position")
tabulate(CFPS_elders_cons$occupation_4cat)
CFPS_elders_cons$agegroup<-as.factor(CFPS_elders_cons$agegroup)
summary(CFPS_elders_cons$med)
CFPS_elders_cons$education<-as.factor(CFPS_elders_cons$education)
#created dailypce
CFPS_elders_cons$dailypce<-CFPS_elders_cons$daily/CFPS_elders_cons$expense

#First model
first_model<-glm(trav_leisure_exp_bin~fincome1+education+agegroup+urban_hukou+dailypce+home_value, data=CFPS_elders_cons,family="binomial")
confusion_matrix(first_model)
#Good prediction with 80% of the no correctly predicted and 61.4% for yes
PseudoR2(first_model,which="all")
#okay with 0.17 in Mac Fadden
#AIC 6211 / BIC 6257

#Second model (family size added)
second_model<-glm(trav_leisure_exp_bin~fincome1+education+agegroup+urban_hukou+dailypce+home_value+familysize, data=CFPS_elders_cons,family="binomial")
confusion_matrix(second_model)
#no improvement from confusion matrice
PseudoR2(second_model,which="all")
#AIC 6213 / BIC 6265
#worst in AIC and BIC

# third model (log familysize added)
third_model<-glm(trav_leisure_exp_bin~fincome1+education+agegroup+urban_hukou+dailypce+home_value+log_familysize, data=CFPS_elders_cons,family="binomial")
confusion_matrix(third_model)
#no improvement from confusion matrice
PseudoR2(third_model,which="all")
#AIC 6213 / BIC 6265
#worst in AIC and BIC

#fourth model (from the first including occupation)
fourth_model<-glm(trav_leisure_exp_bin~fincome1+education+agegroup+urban_hukou+dailypce+home_value+occupation_4cat, data=CFPS_elders_cons,family="binomial")
confusion_matrix(fourth_model)
#no improvement from the first model in no prediction 79.3%, but improvment in yes 67.6%
PseudoR2(fourth_model,which="all")
#Good improvment of AIC and BIC 4076 / 4138
#MAc Fadden 0.22 (pass the 0.2 for very strong)
export_summs(fourth_model)

#Fifth model (based on fourth plus medical expenditure)
fifth_model<-glm(trav_leisure_exp_bin~fincome1+education+agegroup+urban_hukou+dailypce+home_value+occupation_4cat+med, data=CFPS_elders_cons,family="binomial")
confusion_matrix(fifth_model)
#Very little improvment on yes prediction (7), no is less
PseudoR2(fifth_model,which="all")
#AIC and BIC indicate less reliable model 4045/4120
#Mac Fadden improved a bit
export_summs(fifth_model)

#The fourth model looks like a good candidate but many things needs to be verified

# issue with potential non linearity at the logg odd for continuous variable

#Remove the na in fincome1 in quantile
CFPS_elders_cons2<-CFPS_elders_cons[!is.na(CFPS_elders_cons$fincome1),]
CFPS_elders_cons2$fincomeQ<-quantcut(CFPS_elders_cons2$fincome1,q=5)
table(CFPS_elders_cons2$fincomeQ)
fourth_model_bis<-glm(trav_leisure_exp_bin~fincomeQ+education+agegroup+urban_hukou+dailypce+home_value+occupation_4cat, data=CFPS_elders_cons2,family="binomial")
export_summs(fourth_model_bis)
# beta coeff substantially different for different quantile
PseudoR2(fourth_model_bis,which="all")
# higher Psuedo R 2 (0.235)  and different coef when fincome1 is in quantile
#Improvement in AIC and BIC 4015/4115
confusion_matrix(fourth_model_bis)
#78.3% of no correctly predicted, 69.4% of yes correctly predicted
vif(fourth_model_bis)
#no colinearity issue
dx(fourth_model_bis)
dx(fourth_model_bis,byCov = FALSE)
#still one last influential case

#test colinearity at the log ood for dailypce
describe(CFPS_elders_cons2$dailypce)
is.numeric(CFPS_elders_cons2$dailypce)
CFPS_elders_cons3<-CFPS_elders_cons2[!is.na(CFPS_elders_cons2$dailypce),]
CFPS_elders_cons3$dailypceQ<-quantcut(CFPS_elders_cons3$dailypce,q=5)
table(CFPS_elders_cons3$dailypceQ)
fourth_model_ter<-glm(trav_leisure_exp_bin~fincomeQ+education+agegroup+urban_hukou+dailypceQ+home_value+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(fourth_model_ter)
#improvmemnt in AIc and BIC 3959 /4077
vif(fourth_model_ter)
#no collinearity
confusion_matrix(fourth_model_ter)
#78.3% of no correctly predicted, 70.3% of yes correctly predicted
dx(fourth_model_ter,byCov = FALSE)

#Try with a probit
fourth_model_quat<-glm(trav_leisure_exp_bin~fincomeQ+education+agegroup+urban_hukou+dailypceQ+home_value+occupation_4cat, data=CFPS_elders_cons3,family="binomial"(link="probit"))
export_summs(fourth_model_quat)
# model with a probit is not as good in AIC and BIC



# verify alignement of agregroup with the other regression model
# agregroupe 1 2014-38=1976 born after 1976
# agegroup 2  2014-48=1966 born during the revo cul
#agegroup 3 born before the revo cul
describe(CFPS_elders_cons3$age)
CFPS_elders_cons3$age_group3cat<-cut(CFPS_elders_cons3$age,breaks = c(15,53,65,96))
table(CFPS_elders_cons3$age_group3cat)
sixth_model_ter<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group3cat+urban_hukou+dailypceQ+home_value+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(sixth_model_ter)
#decrease in in AIc and BIC 4009 /4127
vif(sixth_model_ter)
#no collinearity
confusion_matrix(sixth_model_ter)
#76.9% of no correctly predicted, 70.4% of yes correctly predicted
dx(sixth_model_ter,byCov = FALSE)
#no influential case
PseudoR2(sixth_model_ter,which="all")
#Mac Fadden still very high with 0.237
#problem the coeff for the last age group is not significant

# model in 5 cat of age groupe
CFPS_elders_cons3$age_group5cat<-cut(CFPS_elders_cons3$age,breaks = c(15,53,60,65,70,96))
table(CFPS_elders_cons3$age_group5cat)
seventh_model<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group5cat+urban_hukou+dailypceQ+home_value+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(seventh_model)
#Big issue with this model coeef are not signifcant with the new categories
vif(seventh_model)
#no collinearity issue
# try with including hosuehold size
height_model<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group5cat+urban_hukou+dailypceQ+home_value+occupation_4cat+familysize, data=CFPS_elders_cons3,family="binomial")
export_summs(height_model)
#same issue about the coeff

# fincome1 linearity at the log odd
#need to verify through graphical representation
#fail at reproducing my former code
lineraity.assumption<-CFPS_elders_cons[!is.na(lineraity.assumption$fincome1),!is.na(lineraity.assumption$education),!is.na(lineraity.assumption$agergoup),!is.na(lineraity.assumption$dailypce),!is.na(lineraity.assumption$occupation_4cat),!is.na(lineraity.assumption$home_value)]
linearity.assumption <- linearity.assumption %>%dplyr::select_if(is.numeric)
linearity.assumption<-linearity.assumption[c(374)]
predict4<-predict(fourth_model,type="response")
predictors <- colnames(linearity.assumption)
linearity.assumption <- linearity.assumption %>%mutate(logit = log(predict4/(1-predict4))) %>%gather(key = "predictors", value = "predictor.value", -logit)
ggplot(linearity.assumption, aes(logit, predictor.value))+geom_point(size = 0.5, alpha = 0.5)+geom_smooth(method = "loess")+facet_wrap(~predictors, scales = "free_y")


#fincome1 and house value, possible collinearity
#collinearity
vif(fourth_model)
#no issue with collinearity
#influential covariate
dx(fourth_model)
#two covariate are influential with dBhat up to 1
plot(fourth_model)


#trying to fix the new models
ninth_model<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group5cat+urban_hukou+dailypceQ+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(ninth_model)
tenth_model<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group3cat+urban_hukou+dailypceQ+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(tenth_model)
eleventh_model<-glm(trav_leisure_exp_bin~fincome1+education+age_group5cat+urban_hukou+dailypceQ+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(eleventh_model)
twelve_model<-glm(trav_leisure_exp_bin~fincome1+education+age_group3cat+urban_hukou+dailypce+occupation_4cat, data=CFPS_elders_cons3,family="binomial")
export_summs(twelve_model)
table1<-table(CFPS_elders_cons3$trav_leisure_exp_bin,CFPS_elders_cons3$age_group5cat)
pem(table1)
#the pem shows the correlation we wants, but because of many controls parameters, agegroup might not be significant anymore depending on where the cut is done
# this might explain why we have the message Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred# 

#To be further explored / interesting model
thirteenth_model_<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group5cat+urban_hukou+cfps_party, data=CFPS_elders_cons3,family="binomial")
export_summs(thirteenth_model_)
PseudoR2(thirteenth_model_,which="all")
#Mac Fadden okay with 0.1835, AIC/BIC 6127/6219
confusion_matrix(thirteenth_model_)
#still acceptable 78% of no well predicted 63.8% of yes well predicted
dx(thirteenth_model_)
#no influential case
vif(thirteenth_model_)
#no colinearity at all

#one of the best model
fourteenth_model_<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group3cat+urban_hukou+cfps_party, data=CFPS_elders_cons3,family="binomial")
export_summs(fourteenth_model_)
PseudoR2(fourteenth_model_,which="all")
#Mac Fadden okay with 0.1823, AIC/BIC 6132/6211
confusion_matrix(fourteenth_model_)
#still acceptable 77.1% of no well predicted 64.3% of yes well predicted
dx(fourteenth_model_)
#no influential case
vif(fourteenth_model_)
#no colinearity at all

#Create a subset with fincome1>1
CFPS_elders_cons4<-subset(CFPS_elders_cons3,fincome1>0)
CFPS_elders_cons4$fincomeQ<-quantcut(CFPS_elders_cons4$fincome1,q=5)
#model with log of fincome
CFPS_elders_cons4$fincome1_log<-log(CFPS_elders_cons4$fincome1)
fifthteen_model_<-glm(trav_leisure_exp_bin~fincome1_log+education+age_group3cat+urban_hukou+cfps_party, data=CFPS_elders_cons4,family="binomial")
export_summs(fifthteen_model_)
PseudoR2(fifthteen_model_,which="all")
confusion_matrix(fifthteen_model_)
#model with FincomeQ recode in the new dataset
CFPS_elders_cons4$fincome1_log<-log(CFPS_elders_cons4$fincome1)
fifthteen_model_<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group3cat+urban_hukou+cfps_party, data=CFPS_elders_cons4,family="binomial")
export_summs(fifthteen_model_)
PseudoR2(fifthteen_model_,which="all")
confusion_matrix(fifthteen_model_)
#Predictive stregth decline with fincome_log instead of fincomeQ
#Predictive strength decline while the negative value of fincome are deleted

# Sort out a linear regression model
#Subset from CFPS_Elders_cons_3 with only the value of travel + lesiure up to 0
CFPS_elders_cons3_bis<-subset(CFPS_elders_cons3,trav_leisure_exp>0)
linear_model_1<-glm(data=CFPS_elders_cons3_bis,trav_leisure_exp~fincomeQ+education+age_group3cat+urban_hukou+cfps_party)
export_summs(linear_model_1)
#R2 is extremly low
#That is normal need to create trav_leisure_pce
CFPS_elders_cons3_bis$trav_leisure_pce<-CFPS_elders_cons3_bis$trav_leisure_exp/CFPS_elders_cons3_bis$expense
describe(CFPS_elders_cons3_bis$trav_leisure_pce)
ggplot(CFPS_elders_cons3_bis,aes(x=trav_leisure_pce, fill=age_group3cat))+geom_density(colour="black",alpha=0.5)
#distribution extremly skwede suggetsing to try log transformation
CFPS_elders_cons3_bis$log_trav_leisure_pce<-log(CFPS_elders_cons3_bis$trav_leisure_pce)
ggplot(CFPS_elders_cons3_bis,aes(x=log_trav_leisure_pce, fill=age_group3cat))+geom_density(colour="black",alpha=0.5)
#very nice once we get a log transfromation. Seems very workable
linear_model_2<-glm(data=CFPS_elders_cons3_bis,log_trav_leisure_pce~fincomeQ+education+age_group3cat+urban_hukou+cfps_party)
export_summs(linear_model_2)
#R2 still extremly low
#reintroduce the variable found by Langyi
linear_model_3<-glm(data=CFPS_elders_cons3_bis,log_trav_leisure_pce~fincomeQ+education+age_group3cat+urban_hukou+cfps_party+med+home_value+familysize+dailypce)
export_summs(linear_model_3)
#model might be also better with log fincome
describe(CFPS_elders_cons3_bis$fincome1)
CFPS_elders_cons3_ter<-subset(CFPS_elders_cons3_bis,fincome1>0)
CFPS_elders_cons3_ter$log_fincome1<-log(CFPS_elders_cons3_ter$fincome1)
linear_model_4<-glm(data=CFPS_elders_cons3_ter,log_trav_leisure_pce~log_fincome1+education+age_group3cat+urban_hukou+cfps_party+med+home_value+familysize+dailypce+female_size)
export_summs(linear_model_4)
ggplot(CFPS_elders_cons3_ter,aes(x=log_fincome1, fill=age_group3cat))+geom_density(colour="black",alpha=0.5)
linear_model_5<-glm(data=CFPS_elders_cons3_ter,log_trav_leisure_pce~log_fincome1+education+age_group5cat+urban_hukou+cfps_party+med+home_value+familysize+dailypce+female_size)
export_summs(linear_model_5)

#try to predict log travel + leisure instead of log travel+exp pce
CFPS_elders_cons3_ter$log_travel_leisure<-log(CFPS_elders_cons3_ter$trav_leisure_exp)
ggplot(CFPS_elders_cons3_ter,aes(x=log_travel_leisure, fill=age_group3cat))+geom_density(colour="black",alpha=0.5)
linear_model_6<-glm(data=CFPS_elders_cons3_ter,log_travel_leisure~log_fincome1+education+age_group5cat+urban_hukou+cfps_party+med+home_value+familysize+dailypce+female_size)
export_summs(linear_model_6)
#much more better with R square to 15%
#let's check the goodness and fit and assumptions

#try to redo the model with born after/before the reforms
CFPS_elders_cons3$age_group2cat<-cut(CFPS_elders_cons3$age,breaks = c(15,35,96))
table(CFPS_elders_cons3$age_group2cat)
#for the binary logistic regression
model_bin_agegroup<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group2cat+urban_hukou+cfps_party+med, data=CFPS_elders_cons3,family="binomial")
export_summs(model_bin_agegroup)
PseudoR2(model_bin_agegroup,which="all")
confusion_matrix(model_bin_agegroup)
#74.8% of correctly predicted no, 68% of correctly predicted yes / mac fadden 0.189
model_bin_3agegroup<-glm(trav_leisure_exp_bin~fincomeQ+education+age_group3cat+urban_hukou+cfps_party+med, data=CFPS_elders_cons3,family="binomial")
export_summs(model_bin_3agegroup)
PseudoR2(model_bin_3agegroup,which="all")
confusion_matrix(model_bin_3agegroup)
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
bptest(model_lin_3agegroup)
#Heteroscedasticity issue from breush pagan, but residual plot and Cook distance (influential point) seems okay 


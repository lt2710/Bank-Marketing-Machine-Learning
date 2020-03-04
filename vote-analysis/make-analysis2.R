# Grace Hopper 2020 SMS analysis

# ----------
# Setup
# ----------

## Check and install package dependencies
packages <- c("tidyverse",
              "finalfit",
              "ggplot2")
packages <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x)
      library(x, character.only = TRUE)
    }
  }
)

rm(list = ls())#clear environment
wd <- "~/GitHub/Pet-projects/vote-analysis/"#set working directory

# Read in data
rand_data <- read.csv(file.path(wd, "data", "randomization.csv"))
sms_data <- read.csv(file.path(wd, "data", "text_message_data.csv"))
survey_data <- read.csv(file.path(wd, "data", "survey_data.csv"))
turnout_data <- read.csv(file.path(wd, "data", "turnout_data.csv"))

# Combine turnout data
df <- full_join(rand_data, turnout_data, by = "ai_id")

# Add survey data
## update: replace merge with dplyr::full_join to avoid dropped cases due to no phone follow-up
survey_data$phone_number <- survey_data$phone_number %>% as.factor()
df <- full_join(df, survey_data, by = "phone_number")


# ----------
# SMS Data
# ----------

# We need to make sure everyone was sent the right texts
## I'm confused because there are multiple texts for each person in the dataset. Can you check this?
## update: I filtered out respondents who didnot receive text as planned in experiment. for details see below???

# Codes below as a whole check if individual text message history correspond correctly to his/her treatment/placebo assignment
sms_data_outbound <- sms_data %>% filter(message_direction == "outbound")#keep only outbound text relevant to the experiment
df_sms <- full_join(sms_data_outbound, rand_data %>% select(phone_number, sms_treat))#merge the RAND data in for assignment information.
## Because the SMS data is not unit-based, we want to reshape it so we can compare what texts are sent to everyone
df_sms_unique <-  df_sms %>% spread(key = message_text, value = sms_treat)#reshape the data to be individual-based; generate binary indicator of receiving certain text
names(df_sms_unique)[3:6] <- c("ptext", "ttext1", "ttext2", "notext")#rename text history indicators. e.g. "ttext2" means "Since the year 2000..."
sms_match <- df_sms_unique %>% select(-phone_number,-message_direction) %>% table(useNA = "always") %>% as.data.frame() %>% filter(Freq > 0)#summarize frequency of text types
sms_match[order(sms_match$Freq, decreasing = TRUE),]#visualize through a table
## e.g. reading the first row: 12137 people received placebo text under Placebo assignment, and didn't receive any other text
## e.g. readin the third row: 374 people received treatment text1 (push to vote) under Treatment assignment, but didn't received the second message
## We can see around 1000 people did not receive text message correcntly associating with their assignment
## I filter only those receiving correct texts according to their treatment

# filtering out observations with problems in experiment implementation
sms_placebo <- df_sms_unique %>% filter(ptext == "Placebo",
                                        is.na(ttext1) == TRUE,
                                        is.na(ttext2) == TRUE,
                                        is.na(notext) == TRUE)# filter correctly placeboed people: received placebo text, and no any other texts
sms_treatment <- df_sms_unique %>% filter(is.na(ptext) == TRUE,
                                          ttext1 == "Treatment",
                                          ttext1 == "Treatment",
                                          is.na(notext) == TRUE)# filter correctly treated people: received both treatment text, and no any other texts
df <-
  df %>% filter(
    phone_number %in% sms_placebo$phone_number |
      phone_number %in% sms_treatment$phone_number)# in final dataset, only include those belonging to either of the correct treatment/control group

# ----------
# Clean data
# ----------

# Delete duplicates
## Delete total duplicates
print(duplicated(df) %>% sum())#can see around 200 duplicates
df <- distinct(df)#delete them
## Check duplicate seperately in id/phone (to avoid cases where someone has two phone numbers, vice versa)
df$phone_number %>% duplicated() %>% sum()#no duplicate in phone number
df$ai_id %>% duplicated() %>% sum()#no duplicate in id

# Missing value check
df[df == "" |
     df == "unknown" |
     df == "Unknown" |
     df == "refused"] <- NA # first, replace all manual notations of missingness to NA
ff_glimpse(df) # use a handy function to inspect missing values
## Most variables are complete. There's minor missingness around 3% in some variables. We'll further inspect them
df %>% select(race, gender, marital_status) %>% missing_pattern() 
## The missingness looks like by random. 
## For simplicity, I assume it's the case and delete them listwise (multiple imputation can be also good)
df <- df %>% filter(
  is.na(race) == FALSE,
  is.na(gender) == FALSE,
  is.na(marital_status) == FALSE,
  is.na(age) == FALSE
)

# Recoding factors in demographic informaiton
## Print a quick overview of factors 
str(df)#exclude factors we are not gonna use in the summary
## binning race
df$race[df$race %in% c("caucasian", "native_american", "other")] <- "other" #i regard these labels as other (instead of existing groups)
## Clean up marital status variable
df$marital_status[df$marital_status == "separated"] <- "unmarried" #regard separated people as unmarried
## Bin age variable
## I don't really know the best way to do this
## update: I recode them roughly by quantile value to make sure there are enough people in each category so that anlaysis makes sense
df$age <-
  cut(
    df$age,
    breaks = c(0, 25, 40, 55, Inf),
    labels = c("Under 25", "25-39", "40-55", "55+"),
    include.lowest = TRUE
  )#breaking the data into 4 equal portions according to quantiles roughly

# recode support for Hopper outocme
## We want to predict against it, but probably not in an ordinal model with 5 levels as it is
## Rather, recoding it to numeric then a OLS would be good
## here I transform the survey response to a 0-10 scale, hoping it will make the understanding of models easier later
df$support_hopper <- df$support_hopper %>% as.character()
df$support_hopper_cat <- df$support_hopper
df$support_hopper_cat[is.na(df$support_hopper_cat) == TRUE] <- "unknown"
df$support_hopper[df$support_hopper == "strongly oppose"] <- 0
df$support_hopper[df$support_hopper == "somewhat oppose"] <- 2.5
df$support_hopper[df$support_hopper == "neither support nor oppose"] <- 5
df$support_hopper[df$support_hopper == "somewhat support"] <- 7.5
df$support_hopper[df$support_hopper == "strong support"] <- 10
table(df$support_hopper) #inspect result


# ----------
# Balance
# ----------

# Check for balance across assignment
## We need to double-check treatment assignment balance by age, race, and marital status
## update: why don't we include gender? I added it in the equation as well as ALL models below.
bal <- glm(sms_treat ~ race + age + marital_status + gender,
           data = df,
           family = "binomial")
summary(bal)


# ----------
# Results
# ----------

# Both result estimations use multivariate models. This can be debatable (in many cases univariate testing would be enough), but I'll stick to the original method for now.

# Estimate Turnout

## Control for race, marital status, age (binned), gender, and support for hopper. 
turnout_model <-
  glm(
    turnout ~ sms_treat + race + marital_status + gender + age + support_hopper_cat,
    data = df,
    family = "binomial"
  )
summary(turnout_model) 

# Estimate Persuasion

## Control for race, marital status, age (binned), gender, and whether they ended up voting in the election later

persuasion_model <-
  lm(
    support_hopper ~ sms_treat + race + marital_status + gender + age + turnout,
    data = df
  )
summary(persuasion_model)


# Subgroup Effects
## For subgroup analysis, I use the most popular One-by-One Interaction Testing. 
## Here I keep it simple with only OLS model
## Referece: https://www.pcori.org/sites/default/files/PCORI-Methodology-Standards-Curriculum-Treatment-Effects-3.pdf
# A function to return bar plot of subgroup differential effects given data, treatment, outcome, and the subgroup name
subgroup_effects <- function(data = df,
                             subgroup = "race",
                             treatment = "sms_treat",
                             outcome = "turnout",
                             ...) {
  # construct a formula for data input. default will be: turnout ~ sms_treat + race + sms_treat:race
  formula_subgroup <-
    as.formula(paste(
      ifelse(outcome == "turnout", "turnout", "support_hopper"),
      "~",
      paste(
        "sms_treat",
        "+",
        subgroup,
        "+",
        "sms_treat:",
        subgroup,
        ifelse(outcome == "turnout", "", "+turnout")
      ),
      sep = ""
    ))
  
  # choose releveant model as source according to desired outcome, or generate an error message
  if (outcome == "turnout") {
    model <- glm(
      formula_subgroup,
      data = df,
      family = "binomial"
    )
  } else if (outcome == "persuasion") {
    model <- lm(
      formula_subgroup,
      data = df
    )
  } else {
    print("please specify turnout or persuasion in outcome parameter")
  }
  
  # extract table of coefficients from the model object
  ## dont have time to extract coefficient significance, but if have chance will add them as fill in bar plot
  effect_table <-
    data.frame(
      variable = model$coefficients %>% names()%>%as.character(),
      effect = model$coefficients %>% as.numeric(),
      stringsAsFactors=FALSE
    )
 
  # modify variable labels e.g. from "sms_treatTreatmentracewhite" to "white" for presentation
  effect_subgroup <-
    effect_table %>% filter(str_detect(variable, "sms_treatTreatment") == TRUE)#only keep treatment and interactions
  effect_subgroup<-effect_subgroup%>% mutate(variable = variable %>% str_sub(start = (str_count(subgroup) + 20)))#get rid of first 20 cahracters
  effect_subgroup$variable[1]<-"base"#correct the label of base group ATE
  
  # calculate ATE differential for subgroups by universal ATE+interaction coefficient
  effect_subgroup$effect[-1]<-effect_subgroup$effect[1]+effect_subgroup$effect[-1] 
  
  # generate bar plot and relevant notations. pretty much self-explained
  race_plot <- ggplot(data = effect_subgroup,
                      aes(x = variable %>% reorder(effect),
                          y = effect)) +
    geom_bar(stat = "identity",
             fill = "skyblue2") +
    xlab("") +
    ylab("") +
    ggtitle("Differential treatment effect in subgroups") +
    labs(
      caption = ifelse(
        outcome == "turnout",
        "Coefficients from logistics model",
        "Coefficients from OLS model"
      )
    ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(text = element_text(size = 15))
  
  return(race_plot)
}

# Then let's check subgroup effects on turnout by gender & race
subgroup_effects(subgroup = "gender")

subgroup_effects(subgroup = "race")

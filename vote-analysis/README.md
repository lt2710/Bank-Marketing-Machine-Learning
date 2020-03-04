# Grace Hopper 2020 SMS analysis

## Data preparation technical note
Different from the original analysis, about half respondents not receiving the follow-up phone interview attempt in the SMS data were also merged, because they can be included for the analsyis on the binary voting behavior (1/0). 
169 duplicated observations were removed after merging. 
In the experiment, 1169 respondents (4.6%) did not receive text message correcntly corresponding with their assignment (e.g. 355 people in the placebo group did not receive any text). I filtered out these observations to maintain a good treatment/control distinction. 
There are around 3% missing values (both hand-coded and NA) in some of RAND's demographic data. The missing pattern is largely random and the portion is small, so I deleted missing observations list-wise before analysis. 
I recoded race, matiral status and age to brief categories to make them more interpretable in the regression models. 
Because we want to predict against support for Hopper. Compared with ordinal models, OLS would be a good alternative since it has 5 levels. I transform the survey response to a 0-10 scale, hoping it will make the understanding of models easier later. 
After recoding, a data consists of complete demograhic information has 21974 observations, with 4293 valid response on Hopper support and 21770 on voting behavior. From a logistic regression on the final data, there's only small difference with no significance in distribution of confounding variables between treatment and placebo group, therefore it's safe to regard it as a result from randomized experiment. 

## High level analytical findings
Overall, the SMS program showed a weak significance in increasing the likelihood of people to participate voting: receiving the treatment will increase the odds of voting by 0.05, with a about 0.05 p-value indicating moderate level of confidence. However, by simply looking at ATE for subgroups in gender and race, such effect turns out to be very heterogenious. The treatment effect for women adds 0.24 to the odds of voting, yet there's barely any signs of encouragement for men. Another subgroup analysis that the program can even have a negative treatment effect on latino race voters, which means the program discourages them from voting. 

Within those we were able to interview, we see a significant positive impact of the SMS program on the support for Hopper. In a scale of 10 (0=strongly oppose, 10=strongly support), the treatment would increase around 0.4 support. After checking regression tables for subgroup analysis, overall we didn't find significant differences in ATE across subgroups. 


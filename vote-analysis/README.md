# Grace Hopper 2020 SMS analysis

## Data preparation technical note
Different from the original analysis, about half respondents not receiving the follow-up phone interview attempt in the SMS data were also merged, because they can be included for the analsyis on the binary voting behavior (1/0). 
169 duplicated observations were removed after merging. 
In the experiment, 1169 respondents (4.6%) did not receive text message correcntly corresponding with their assignment (e.g. 355 people in the placebo group did not receive any text). I filtered out these observations to maintain a good treatment/control distinction. 
There are around 3% missing values (both hand-coded and NA) in some of RAND's demographic data. The missing pattern is largely random and the portion is small, so I deleted missing observations list-wise before analysis. 
I recoded race, matiral status and age to brief categories to make them more interpretable in the regression models. 
Because we want to predict against support for Hopper. Compared with ordinal models, OLS would be a good alternative since it has 5 levels. I transform the survey response to a 0-10 scale, hoping it will make the understanding of models easier later. 
After recoding, a data consists of complete demograhic information has 21974 observations, with 4293 valid response on Hopper support and 21770 on voting behavior. From a logistic regression on the final data, there's no significant different in distribution of confounding variables between treatment and placebo group, therefore it's safe to regard it as a result from randomized experiment. 
## High level analytical findings
The SMS program showed a weak amount of significance in increasing the likelihood of people to participate voting. Because the p-value is around 0.5, we can only assume a moderate confidence for this to be an universal impact. Interestingly, we see that for opponents or strong opponennts of Hopper, receiving the message add a lot of likelihood for them to participate voting, which probably would not be favoring. On the other hand, no significant effect can be seen from her supporters. In other words, the program might not be effective in motivating supporters, but might lead to the action of opponents. 
Within those we are able to interview, we see a significant leveraging impact. The SMS program turns out to be effective in increasing support for Hopper. In a scale of 10 (0=strongly oppose, 10=strongly support), the treatment would increase around 0.4 support. However, such effect is lowered down to 0.2 for those who participated to vote. A possible explanation could be, the increased support might not immediately converts to actual votes. 


# Grace Hopper 2020 SMS analysis

## Data preparation technical note
Different from the original analysis, about half respondents not receiving the follow-up phone interview attempt in the SMS data were also merged, because they can be included potentilly for the analsyis on the binary voting behavior (1/0). 
169 duplicated observations were removed after merging. 
In the experiment, 1169 respondents (4.6%) did not receive text message correcntly corresponding with their assignment (e.g. 355 people in the placebo group did not receive any text). I filtered out these observations to maintain a good treatment/control distinction. 
There are around 3% missing values (both hand-coded and NA) in some of RAND's demographic data. The missing pattern is largely random and the portion is small, so I deleted missing observations list-wise before analysis. 
I recoded race, matiral status and age to brief categories to make them more interpretable in the regression models. After recoding, a data consists of complete demograhic information has 21974 observations, with 4293 valid response on Hopper support and 21770 on voting behavior. 
Because we want to predict against support for Hopper. Compared with ordinal models, OLS would be a good alternative since it has 5 levels. I transform the survey response to a 0-10 scale, hoping it will make the understanding of models easier later.

## High level analytical findings

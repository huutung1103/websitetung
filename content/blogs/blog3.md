---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: ""
draft: false
image: pic08.jpg
keywords: ""
slug: blog3
title: General Social Survey
---

The [General Social Survey (GSS)](http://www.gss.norc.org/) gathers data on American society in order to monitor and explain trends in attitudes, behaviours, and attributes. Many trends have been tracked for decades, so one can see the evolution of attitudes, etc in American Society.


In this assignment we analyze data from the **2016 GSS sample data**, using it to estimate values of *population parameters* of interest about US adults. The GSS sample data file has 2867 observations of 935 variables, but we are only interested in very few of these variables and you are using a smaller file.


```{r, read_gss_data, cache=TRUE}
gss <- read_csv(here::here("data", "smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))
```

You will also notice that many responses should not be taken into consideration, like "No Answer", "Don't Know", "Not applicable", "Refused to Answer".

We will be creating 95% confidence intervals for population parameters. The variables we have are the following:

- hours and minutes spent on email weekly. The responses to these questions are recorded in the `emailhr` and `emailmin` variables. For example, if the response is 2.50 hours, this would be recorded as emailhr = 2 and emailmin = 30.
- `snapchat`, `instagrm`, `twitter`: whether respondents used these social media in 2016
- `sex`: Female - Male
- `degree`: highest education level attained


## Instagram and Snapchat, by sex

Can we estimate the *population* proportion of Snapchat or Instagram users in 2016?

1. Create a  new variable, `snap_insta` that is *Yes* if the respondent reported using any of Snapchat (`snapchat`) or Instagram (`instagrm`), and *No* if not. If the recorded value was NA for both of these questions, the value in your new variable should also be NA.
1. Calculate the proportion of Yes’s for `snap_insta` among those who answered the question, i.e. excluding NAs.
1. Using the CI formula for proportions, please construct 95% CIs for men and women who used either Snapchat or Instagram
```{r, read_gss_data2, cache=TRUE}

#calculate proportion of Yes's for snap_insta
proportion_snap_insta <- gss %>%
  filter(snapchat %in% c("Yes","No","NA"),
         instagrm %in% c("Yes","No","NA")) %>%
  mutate(snap_insta=case_when(
    snapchat=="Yes" | instagrm=="Yes" ~ "Yes",
    snapchat=="NA" & instagrm=="NA" ~ "NA",
    TRUE ~ "No")) %>% filter(snap_insta!="NA") %>% 
  specify(response=snap_insta,success="Yes") %>% 
  generate(reps=1000,type="bootstrap") %>%
  calculate(stat="prop")

#calculate snap_insta ci
snap_insta_ci <- proportion_snap_insta %>% 
   get_confidence_interval(level = 0.95,type="percentile")

#visualize snap_insta ci
proportion_snap_insta %>% visualize(bins = 15) + 
  shade_confidence_interval(endpoints = snap_insta_ci)

snap_insta_ci

```
## Twitter, by education level

Can we estimate the *population* proportion of Twitter users by education level in 2016?. 

There are 5 education levels in variable `degree` which, in ascneding order of years of education, are Lt high school, High School, Junior college, Bachelor, Graduate. 

1. Turn `degree` from a character variable into a factor variable. Make sure the order is the correct one and that levels are not sorted alphabetically which is what R by default does. 
1. Create a  new variable, `bachelor_graduate` that is *Yes* if the respondent has either a `Bachelor` or `Graduate` degree. As before, if the recorded value for either was NA, the value in your new variable should also be NA.
1. Calculate the proportion of `bachelor_graduate` who do (Yes) and who don't (No) use twitter. 
1. Using the CI formula for proportions, please construct two 95% CIs for `bachelor_graduate` vs whether they use (Yes) and don't (No) use twitter. 
1. Do these two Confidence Intervals overlap?

## Proportion of bachelor graduate:
```{r, twitter_proportion}

# Split degree into factor variables
twitter_gss <- gss %>% 
  mutate(degree=factor(degree,
                       levels = c(
                         "Lt high school",
                         "High School",
                         "Junior college",
                         "Bachelor",
                         "Graduate"),
                       ordered=TRUE))

# create bachelor_degree var
twitter_gss <- gss %>% 
  mutate(bachelor_degree=case_when(
    degree=="Bachelor" |degree=="Graduate" ~ "Yes",
    is.na(degree) ~ "NA",
    TRUE ~ "No"
  ))

#create sample proportion
twitter_prop <- twitter_gss %>%
  filter(bachelor_degree=="Yes",twitter %in% c("Yes","No")) %>% 
  summarise(twitter_yes=mean(twitter=="Yes"),
            twitter_no=mean(twitter=="No"))
twitter_prop
```
## 95% Confidence interval for bachelor graduate who use Twitter:

```{r, twitter_yes}
# create bachelor_degree var
twitter_gss <- gss %>% 
  mutate(bachelor_degree=case_when(
    degree=="Bachelor" |degree=="Graduate" ~ "Yes",
    is.na(degree) ~ "NA",
    TRUE ~ "No"
  ))

twitter_user_yes <- twitter_gss %>% 
  filter(bachelor_degree=="Yes",twitter %in% c("Yes","No")) %>%
  specify(response=twitter,success="Yes") %>% 
  generate(reps=1000,type="bootstrap") %>%
  calculate(stat="prop")

#create confidence interval
twitter_user_yes_ci <- twitter_user_yes %>%
  get_confidence_interval(level = 0.95,type="percentile")

#visualize twitter_user_yes ci
twitter_user_yes %>% visualize(bins = 15) + 
  shade_confidence_interval(endpoints = twitter_user_yes_ci)

twitter_user_yes_ci
```
### 95% Confidence interval for bachelor graduate who do not use Twitter

```{r, twitter_no}
# create bachelor_degree var
twitter_gss <- gss %>% 
  mutate(bachelor_degree=case_when(
    degree=="Bachelor" |degree=="Graduate" ~ "Yes",
    is.na(degree) ~ "NA",
    TRUE ~ "No"
  ))

twitter_user_no <- twitter_gss %>% 
  filter(bachelor_degree=="Yes",twitter %in% c("Yes","No")) %>%
  specify(response=twitter,success="No") %>% 
  generate(reps=1000,type="bootstrap") %>%
  calculate(stat="prop")

#create confidence interval
twitter_user_no_ci <- twitter_user_no %>%
  get_confidence_interval(level = 0.95,type="percentile")

#visualize twitter_user_no ci
twitter_user_no %>% visualize(bins = 15) + 
  shade_confidence_interval(endpoints = twitter_user_no_ci)
twitter_user_no_ci
```

According to the graphs above, among individuals who have a 'bachelor' degree or higher, the proportion that use Twitter overlaps with the confidence interval for people who do not use Twitter. This occurs at the higher end of the two confidence intervals because people who do not use Twitter fall into the interval 73%-81%, and people who do use Twitter fall into the interval 19%-27%.

## Email usage

Can we estimate the *population* parameter on time spent on email weekly?

1. Create a new variable called `email` that combines `emailhr` and `emailmin` to reports the number of minutes the respondents spend on email weekly.
1. Visualise the distribution of this new variable. Find the mean and the median number of minutes respondents spend on email weekly. Is the mean or the median a better measure of the typical amoung of time Americans spend on email weekly? Why?
1. Using the `infer` package, calculate a 95% bootstrap confidence interval for the mean amount of time Americans spend on email weekly. Interpret this interval in context of the data, reporting its endpoints in “humanized” units (e.g. instead of 108 minutes, report 1 hr and 8 minutes). If you get a result that seems a bit odd, discuss why you think this might be the case.
1. Would you expect a 99% confidence interval to be wider or narrower than the interval you calculated above? Explain your reasoning.

```{r, email_usage_skim}
email <- gss %>% 
  mutate(emailhr=as.numeric(emailhr),emailmin=as.numeric(emailmin))

email <- email %>%
  mutate(emailmin_total=emailhr*60+emailmin)
skim(email %>% select(emailmin_total))

```
The mean is 417 while the median is 120. Because of the data set and the number of outliers, we can assume that the median is the more accurate measure.


```{r, email_usage_ci}
email <- email %>% 
  specify(response=emailmin_total) %>% 
  generate(reps=1000,type="bootstrap") %>%
  calculate(stat="mean")

email_ci <- email %>%
  get_confidence_interval(level = 0.95,type="percentile") %>% 
  mutate(lower_ci_hrs=lower_ci%/%60,lower_ci_mins=lower_ci%%60,
         upper_ci_hrs=upper_ci%/%60,upper_ci_mins=upper_ci%%60) 

# visualize email_ci
email_ci

```

The lower confidence interval is 386.77 minutes (6 hours, 26.77 minutes), and the upper confidence interval is 446.87 minutes (7 hours,26.87 minutes). If we increase the confidence interval to 99%, we would project the range to be larger to make sure that the result would be in the range. As a result, the lower confidence interval would be smaller than 386 minutes and the higher confidence interval would be bigger than 446 minutes.

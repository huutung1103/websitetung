---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic02.jpg
keywords: ""
slug: blog1
title: Movies Analysis
---
We will look at a subset sample of movies, taken from the [Kaggle IMDB 5000 movie dataset](https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset)
```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
library(tidyquant)
library(kableExtra)
```

  
```{r,load_movies, warning=FALSE, message=FALSE, eval=FALSE}

movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)

```

Besides the obvious variables of `title`, `genre`, `director`, `year`, and `duration`, the rest of the variables are as follows:

- `gross` : The gross earnings in the US box office, not adjusted for inflation
- `budget`: The movie's budget 
- `cast_facebook_likes`: the number of facebook likes cast memebrs received
- `votes`: the number of people who voted for (or rated) the movie in IMDB 
- `reviews`: the number of reviews for that movie
- `rating`: IMDB average rating 

## Use your data import, inspection, and cleaning skills to answer the following:

- Are there any missing values (NAs)? Are all entries distinct or are there duplicate entries?
- Produce a table with the count of movies by genre, ranked in descending order
- Produce a table with the average gross earning and budget (`gross` and `budget`) by genre. Calculate a variable `return_on_budget` which shows how many $ did a movie make at the box office for each $ of its budget. Ranked genres by this `return_on_budget` in descending order
- Produce a table that shows the top 15 directors who have created the highest gross revenue in the box office. Don't just show the total gross amount, but also the mean, median, and standard deviation per director.
- Finally, ratings. Produce a table that describes how ratings are distributed by genre. We don't want just the mean, but also, min, max, median, SD and some kind of a histogram or density graph that visually shows how ratings are distributed. 
```{r}
movies %>% 
               group_by(genre) %>% 
               summarise(mean_rating = mean(rating), 
                         med_rating = median(rating), 
                         min_rating = min(rating), 
                         max_rating = max(rating), 
                         sd_rating = sd(rating)) %>% view()

movies %>% ggplot(aes(x=rating))+
               geom_histogram()+
               facet_wrap(~genre, scales = "free_y")


```

## Use `ggplot` to answer the following

  - Examine the relationship between `gross` and `cast_facebook_likes`. Produce a scatterplot and write one sentence discussing whether the number of facebook likes that the cast has received is likely to be a good predictor of how much money a movie will make at the box office. What variable are you going to map to the Y- and X- axes?
  
  
```{r, gross_on_fblikes}
movies%>%
  ggplot(aes(x=cast_facebook_likes, y=gross)) +
  geom_point(alpha = 0.5) + xlim(0,100000) + geom_smooth(method="lm") + labs(title ="Relationship between facebook likes and box office revenue",x="Facebook likes", y="Box office revenue")
  
#correlation between cast facebook likes and movies box offfice:
cor( movies$gross,movies$cast_facebook_likes)

```
While there is a **positive relationship** between cast facebook likes and box office revenue, **the correlation of 0.213 is too small for cast facebook likes to be a good predictor of box office revenue**.

  - Examine the relationship between `gross` and `budget`. Produce a scatterplot and write one sentence discussing whether budget is likely to be a good predictor of how much money a movie will make at the box office.

```{r, gross_on_budget}
movies%>%
  ggplot(aes(x=budget, y=gross)) +
  geom_point(alpha = 0.5) + xlim(0,20000000)+ ylim(0,10000000)+ geom_smooth(method="lm")

#find correlation
cor(movies$budget, movies$gross)

```
There is a **positive correlation** between movie budget and box office revenue. Since the correlation is 0.641, **movie budget is not a strong predictor for box office revenue but is better than cast facebook likes**. 
  
  - Examine the relationship between `gross` and `rating`. Produce a scatterplot, faceted by `genre` and discuss whether IMDB ratings are likely to be a good predictor of how much money a movie will make at the box office. Is there anything strange in this dataset?

```{r, gross_on_rating}
library(plyr)
correlation <- ddply(movies, .(genre), 
              summarise, 
              cor = round(cor(rating, gross), 2))

movies%>%
  ggplot(aes(x=rating, y=gross)) +
  geom_point(alpha = 0.3) + facet_wrap(~genre, scales = "free_y") +
  geom_smooth(method ="lm") + geom_text(data = correlation, aes(label = paste("r=", cor, sep="")), x = 1, y = -0.25)

cor(movies$rating,movies$gross)
```
Overall, while there is a **positive relationship** between rating and box office revenue, **the correlation of 0.269 is too small for rating to be a good predictor of box office revenue**. 

When we examine the relationship between rating and box office revenue per genre, **there is not any strong correlation**. The strange thing is for **Documentaries and Sci-fi**, there is **a negative relationship** between rating and genre. Thus, we need to examine the reason behind such negative relationship.

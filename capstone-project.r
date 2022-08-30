---
title: "Disney Movies"
author: "Sarah Pearl"
date: '2022-04-01'
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Introduction

I am new to Data Analytics and am currently taking the Google Data Analytics Professional Certificate Course. For my capstone project to receive the certificate, I am completing a case study for a fictional movie studio to give them recommendations for what types of movies they should be making. I have been tasked with specifically analyzing movies from Walt Disney Studios using [this](https://www.kaggle.com/datasets/prateekmaj21/disney-movies) dataset. It starts with the first movie put out by Disney and goes through 2016. It includes information about each movie's release date, genre, MPAA rating, amount grossed, and the gross amount adjusted for inflation. 

Prior to uploading the dataset to RStudio, I cleaned up the data a little bit in Google Sheets. There were a few movies that didn't have information for their genre or MPAA rating, so I used [Rotten Tomatoes](https://www.rottentomatoes.com/) to fill in that missing information. I was unable to find MPAA rating information for *Bon Voyage* and *America's Heart*. I was also unable to find MPAA ratings and gross profit information for *The Many Adventures of Winnie the Pooh*, *Amy*, *Condorman*, and *Frank McKlusky*. Since these six movies accounted for only 1.04% of the total movies listed, a negligible amount, I decided to purge them from the data.

Now it's time to switch over to working in RStudio, starting with loading in the necessary packages.

```{r}
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
```


And loading in the dataset we'll be working with. 


```{r}
movies <- read_csv("disney_movies.csv")
```


Now I want to check it out to see how it looks.

```{r}
head(movies)
```


And double check the column names I'm working with. 


```{r}
colnames(movies)
```


I will be working with the profits and would like the numbers to be easier to read, so I decided to add a column to represent the amount grossed in millions of dollars.

```{r}
movies_mill <- mutate(movies, inflation_gross_in_millions = inflation_adjusted_gross/1000000)
```


Just checking that I ran that code correctly. 

```{r}
colnames(movies_mill)
```


Taking another look at the data.

```{r}
head(movies_mill)
```


Let's see if there are any noticeable trends in gross profits over time.

```{r}
ggplot(data=movies_mill) + geom_line(mapping=aes(x=release_date, y=inflation_gross_in_millions)) 

```

Ok, that's a bit jarring to look at! Let's see if this is any better.

```{r}
ggplot(data=movies_mill) + geom_point(mapping=aes(x=release_date, y=inflation_gross_in_millions)) + geom_smooth(mapping=aes(x=release_date, y=inflation_gross_in_millions)) + labs(title="Amount Grossed Over Time", x="Release Date", y="Amount Grossed in Millions of Dollars")   

```

It looks like Disney's first few movies were big hits! Then there was quite a large decline in profits, with a bit of an upward trend over the last decade.

I wonder what the top grossing movies are!

```{r}
movies_mill %>% 
  arrange(desc(inflation_gross_in_millions)) %>% 
  top_n(10)
```

There are a few unexpected movies near the top! Now I'm curious to find out what the most successful genres are.

```{r}
movies_genre <- movies_mill %>%
  group_by(genre) %>%
  summarize(mean_gross = mean(inflation_gross_in_millions))

ggplot(data=movies_genre) + geom_col(mapping=aes(x=genre, y=mean_gross, fill=genre)) + theme(axis.text.x = element_text(angle=90), legend.position = "none") + labs(title="Average Amount Grossed by Genre", x="Genre", y="Gross in Millions of Dollars") + scale_x_discrete(labels=c("Concert/Performance"="Concert", "Romantic Comedy"="Romance", "Thriller/Suspense"="Thriller"))   

```

On average, musicals tend to bring in much more money than any other genre. However, documentaries and horror movies don't seem to do too well. This was an interesting finding, now let's see how MPAA ratings affect the revenue.

```{r}
movies_rating <- movies_mill %>%
  group_by(mpaa_rating) %>%
  summarize(mean_gross = mean(inflation_gross_in_millions))

ggplot(data=movies_rating) + geom_col(mapping=aes(x=mpaa_rating, y=mean_gross, fill=mpaa_rating)) + labs(title="Average Amount Grossed by Rating", x="MPAA Rating", y="Gross in Millions of Dollars") + theme(legend.position = "none")      

```


Since Disney is known for making movies for children and families, I'm not too surprised that G rated movies make the most money. Going off of that finding, we would expect to see that G rated movies are also the ones that Disney has made the most of. If they bring in the most money, it makes sense that more of them would be made.

```{r}
ggplot(data=movies) + geom_bar(mapping=aes(x=mpaa_rating, fill=mpaa_rating)) + labs(title="Count of Movies by MPAA Rating", x="MPAA Rating", y="Number of Movies") + theme(legend.position = "none")

```


Despite the fact that movies with a PG rating were the second least grossing movies, they are movies that Disney has made the most of. I wonder if PG movies have become more profitable over time, causing more of them to be made recently. To test this hypothesis, I am going to check the last 20 years of data.

```{r}
recent_movies <- movies_mill %>%
  filter(release_date >= "1996-01-01") %>%
  group_by(mpaa_rating) %>%
  summarize(mean_gross=mean(inflation_gross_in_millions))

head(recent_movies)
```

It looks like we are still getting the same result in regards to PG movies. Just out of curiosity, I wonder what the results would be if I narrowed it down to checking the last 10 years of data.

```{r}
most_recent_movies <- movies_mill %>%
  filter(release_date >= "2006-01-01") %>%
  group_by(mpaa_rating) %>%
  summarize(mean_gross=mean(inflation_gross_in_millions))

head(most_recent_movies)
```

PG movies have finally surpassed G movies in profits, however PG-13 movies are now holding the number one spot. This still doesn't support my hypothesis. I think I'll need more data if I want to get a better understanding of why so many PG movies are being made, despite the fact that they aren't the most profitable.

Now that we've found this rise in popularity for PG-13 movies, let's find out exactly what types of these movies tend to bring in the most money.

```{r}
movies_pg13 <- movies_mill %>%
  filter(release_date >= "2006-01-01", mpaa_rating == "PG-13") %>%
  group_by(genre) %>%
  summarize(mean_gross=mean(inflation_gross_in_millions))

ggplot(data=movies_pg13) + geom_col(mapping=aes(x=genre, y=mean_gross, fill=genre)) + labs(title="Average Amount Grossed by Genre", subtitle="For PG-13 Movies", x="Genre", y="Gross in Millions of Dollars") + theme(legend.position = "none") + scale_x_discrete(labels=c("Romantic Comedy"="Romance", "Thriller/Suspense"="Thriller"))    

```

Among the PG-13 crowd, adventure movies are by far the most popular, followed by action movies.



### My Findings

It is clear that Disney knows what they're doing! Musicals are by far their most profitable movie genre, while movies with a G rating have been the most profitable for them overall. Despite historical data, movies with a PG-13 rating have become the most profitable over the last decade. My recommendation is to focus on making movies that fall into two distinct categories- movies geared toward children and families, particularly musicals, and movies geared toward adults and young adults that fall into the action and adventure genres. 









# Challenge 1
library(curl)
library(readr)
library(tidyverse)
library(dplyr)
f <- "https://raw.githubusercontent.com/difiore/ada-2021-datasets/main/IMDB-movies.csv"
movies <- read_csv(f, col_names = TRUE)
head(movies)
movies_filtered <- filter(movies, startYear %in% 1920:1979, runtimeMinutes %in% 60:180)%>%
  mutate(decade = floor(startYear/10)*10) #this creates a new variable, 'decade'
movies_filtered
library(ggplot2)
movie_plots <- ggplot(movies_filtered, aes(runtimeMinutes)) +
  geom_histogram(na.rm = TRUE, color = "black", fill = "purple") +
  facet_wrap(~decade)
movie_plots #this creates a series of histograms for movie runtimes grouped by decade
library(mosaic)
library(radiant)
results <- movies_filtered %>% 
  group_by(decade) %>%
  summarize(
    pop_mean = mean(runtimeMinutes, na.rm = TRUE),
    pop_SD = sdpop(runtimeMinutes, na.rm = TRUE),
  ) 
results #this creates a tibble displaying the mean and SD grouped by decade
n = 100
movies_by_decade <- group_by(movies_filtered, decade) %>% sample_n(size = n, replcae = FALSE) %>%
  summarise(mbd_sampsize = n(), mbd_mean = mean(runtimeMinutes), mbd_sd = sd(runtimeMinutes), mbd_se = sd(runtimeMinutes)/sqrt(n))
movies_by_decade
comparison <- merge(results, movies_by_decade)
comparison #this is a merged data frame of the population and sample summary statistics; overall, they are similar. 
mbd_means_vctrs <- rep(NA, 1000)
for (i in 1:1000){
  group_by(movies_filtered, decade)
  mbds <- sample(movies_filtered$runtimeMinutes, size = 100)
  mbd_means_vctrs[i] <- mean(mbds)
}
mbd_means_vctrs #means of samples of movie runtimes
mean(mbd_means_vctrs) #mean of the sampling distribution
mbd_sd_vctrs <- rep(NA, 1000)
for (i in 1:1000){
  group_by(movies_filtered, decade)
  mbds <- sample(movies_filtered$runtimeMinutes, size = 100)
  mbd_sd_vctrs[i] <- mean(mbds)
}
mbd_sd_vctrs #standard deviations of samples of movie runtimes
sd(mbd_sd_vctrs)/sqrt(100) #standard deviation of the sampling distribution
mbd_sampdist <- merge(mbd_means_vctrs, mbd_sd_vctrs)
movie_sampdist_plot <- histogram(mbd_means_vctrs)
movie_sampdist_plot #this histogram has a normal shape with a slightly right skew

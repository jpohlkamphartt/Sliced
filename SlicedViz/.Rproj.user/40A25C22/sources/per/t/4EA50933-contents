library(tidyverse)
library(tidymodels)
library(recipes)
library(themis)
library(vip)
library(ggplot2)
library(furrr)
library(here)
library(lubridate)
library(mgcv)
library(parallel)
library(workflowsets)
library(tidyposterior)
library(discrim)
library(hablar)
library(doParallel)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)
# 
# all_cores <- parallel::detectCores()
# registerDoParallel(cores=all_cores-2)

options(scipen=999)
commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}
source("SlicedTheme.R")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}

modelData<-as_tibble(read.csv("Season1/s00e03/Copy of sliced_data.csv",header=T,stringsAsFactors=F,sep=","))

# Plan
# data cleanup
# model if volitile
# feature engineering: temporal, game title similarity, ?
# model direction
# viz

skimr::skim(modelData)


modelData<-modelData%>%
  dplyr::select(# remove duplicate variables
    -month,
    -month_num,
    -year
  )%>%
  mutate(# fix dates/character columns
    avg_peak_perc = as.numeric(stringr::str_replace_all(avg_peak_perc,"%",""))/100,
    date = ymd(yearmonth))%>%
  dplyr::select(-yearmonth)

corMat<-cor(modelData%>%select_if(is.numeric), use = "pairwise.complete.obs")%>%
  as.data.frame()%>%rownames_to_column()



modelData<-modelData%>%group_by(gamename)%>%
  mutate(gain_1 = lag(gain,1),
         # gain_2 = lag(gain,2),
         # gain_3 = lag(gain,3),
         gain_4 = lag(gain,4),
         # gain_5 = lag(gain,5),
         gain_6 = lag(gain,6),
         # gain_7 = lag(gain,7),
         # gain_8 = lag(gain,8),
         # gain_9 = lag(gain,9),
         # gain_10 = lag(gain,10),
         # gain_11 = lag(gain,11),
         gain_12 = lag(gain,12),
         avg_peak_perc_1 = lag(avg_peak_perc,1),
         # avg_peak_perc_2 = lag(avg_peak_perc,2),
         # avg_peak_perc_3 = lag(avg_peak_perc,3),
         avg_peak_perc_4 = lag(avg_peak_perc,4),
         # avg_peak_perc_5 = lag(avg_peak_perc,5),
         avg_peak_perc_6 = lag(avg_peak_perc,6),
         # avg_peak_perc_7 = lag(avg_peak_perc,7),
         # avg_peak_perc_8 = lag(avg_peak_perc,8),
         # avg_peak_perc_9 = lag(avg_peak_perc,9),
         # avg_peak_perc_10 = lag(avg_peak_perc,10),
         # avg_peak_perc_11 = lag(avg_peak_perc,11),
         avg_peak_perc_12 = lag(avg_peak_perc,12)
  )%>%ungroup()%>%
  arrange(desc(avg))
         
topwords<-names(sort(table(unlist(strsplit(head(unique(modelData$gamename),20)," "))),decreasing = T))
library(corpus)
corpus <- as_corpus_frame(unique(modelData$gamename)) # Project Gutenberg #55, _The Wizard of Oz_
text_filter(corpus)$drop_punct <- TRUE # ignore punctuation
nameterms<-term_stats(corpus, ngrams = 2:3)
  
acf(ts(modelData%>%group_by(date)%>%summarise(gain = mean(gain, na.rm = T))%>%ungroup()%>%arrange(date)%>%select(gain)))
pacf(ts(modelData%>%group_by(date)%>%summarise(gain = mean(gain, na.rm = T))%>%ungroup()%>%arrange(date)%>%select(gain)))
# we are seeing some correlations with lags of 1, 4, 6, 12 depending on the game, 
# overall there is a moving average of about 12 months and lag 12 auto-regression

modelData%>%
  filter(topTitle)%>%arrange(date)%>%
  ggplot(aes(x=date, y = avg, color = gamename))+
  geom_point()+
  geom_smooth()+
  labs(main = "progression of top titles")

  
  
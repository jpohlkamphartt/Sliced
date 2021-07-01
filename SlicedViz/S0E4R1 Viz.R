library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
library(parallel)
library(patchwork)
library(extrafont)
library(skimr)
library(beepr)
here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e04/train.csv",header=T,stringsAsFactors=F,sep=","))
modelData<-modelData%>%
  mutate(date = ymd(date))
modelData<-modelData%>%
  arrange(date)%>%
  group_by(location)%>%
  mutate(rain_today_182 = lag(rain_today, 182),
         rain_today_181 = lag(rain_today, 181),
         rain_today_183 = lag(rain_today, 183),
         rain_today_365 = lag(rain_today, 365),
         rain_today_364 = lag(rain_today, 364),
         rain_today_366 = lag(rain_today, 366),
         rain_today_1 = lag(rain_today, 1),
         rain_today_2 = lag(rain_today, 2)
  )%>%
  mutate(rain_half = mean(c(rain_today_182,rain_today_181,rain_today_183),na.rm=T),
         rain_whole = mean(c(rain_today_365,rain_today_364,rain_today_366),na.rm=T))%>%
  ungroup()

modelData<-modelData%>%
  mutate(month = month(date))%>%
  dplyr::select(
    -date,-rain_today_182,-rain_today_183,-rain_today_181,
    -rain_today_365,-rain_today_364,-rain_today_366,-id
  )%>%
  mutate_at(
    vars(rain_today_1,rain_today_2,rain_today,rain_tomorrow), as.factor)%>%
  mutate_if(is.character, as.factor)

modelData_ts<-modelData%>%
  mutate(date = ymd(date))%>%
  filter(location == "Sydney")%>%
  group_by(date)%>%
  summarise(rain_today = mean(rain_today))%>%
  ungroup()%>%
  complete(date = seq.Date(min(date),max(date), by = "day"),
           fill = list(rain_today = 0))
modelData%>%
  mutate(date = ymd(date))%>%
  filter(location == "Sydney")%>%
  group_by(date)%>%
  summarise(rain_today = sum(rainfall))%>%
  ungroup()%>%
  complete(date = seq.Date(min(date),max(date), by = "day"),
           fill = list(rain_today = 0))%>%
  ggplot(aes(x=date, y=rain_today))+
  geom_line(alpha = .2)+
  geom_smooth()

TS<-ts(
  modelData_ts$rain_today,
  start = c(2008,2,5),
  frequency = 365)

plot(decompose(TS)) # definite yearly seasonality
acf(TS, 400)
pcz<-pacf(TS, 400)
which(abs(pcz$acf)>.04) # looks like yearly and 1/2 year are big so lag of them would be good


skim(modelData)

## correlations
corMat<-cor(modelData%>%
            mutate(rain_tomorrow= as.numeric(rain_tomorrow))%>%
            select_if(is.numeric),
            use ="pairwise.complete.obs")%>%as.data.frame()%>%
  rownames_to_column()%>%arrange(desc(rain_tomorrow))

modelData%>%
  sample_frac(.5)%>%
  GGally::ggscatmat(columns = which(colnames(modelData)%in%corMat$rowname[2:10]),
                    color = "rain_tomorrow", alpha = .1, corMethod="spearman")+
  scale_color_sliced()

## densities
numericz<-colnames(modelData)[unlist(lapply(modelData, is.numeric))]
tranz<-rep("identity",length(numericz))
tranz[3]<-"log10"
gList<-lapply(1:length(numericz),function(i){
  
  modelData%>%
    ggplot(aes_string(x=numericz[i],fill = "rain_tomorrow"))+
    geom_density(alpha=.5)+
    scale_x_continuous(trans = tranz[i])
  
})
gList[[1]]+gList[[2]]+gList[[3]]+gList[[4]]+
  gList[[5]]+gList[[6]]+gList[[7]]+gList[[8]]+gList[[9]]

gList[[10]]+gList[[11]]+gList[[12]]+gList[[13]]+
  gList[[14]]+gList[[15]]+gList[[16]]+gList[[17]]

## bars
factz<-colnames(modelData)[unlist(lapply(modelData, is.factor))]
fList<-lapply(1:length(factz),function(i){
  
  modelData%>%
    mutate_at(vars(factz[i]),fct_lump,n=20)%>%
    group_by_at(vars(factz[i]))%>%
    summarise(prop = sum(rain_tomorrow==1)/n(),
              n = n())%>%
    ggplot(aes_string(
      x=factz[i],y = "prop",fill = "n"
    ))+
    geom_bar(stat = "identity")+
    scale_fill_sliced("Divergent",discrete = F)+
    theme(legend.position = "top",axis.text.x = element_text(size = 10,angle = 45))
  
  
})
fList[[1]]+fList[[2]]+fList[[3]]+fList[[4]]+
  fList[[5]]+fList[[6]]+fList[[7]]+fList[[8]]

# min temp not much, max a little more
# rainfall big time
# wind gust but not overall speed
# humidity bingo
# pressure = rain
# clouds = rain, especially late
# temp is unimportant at 9am, 
# rain half and whole year are interesting only on the edges
# middle month there is an obvious trend
## can probably remove wind, temp from models


## steal my sunshine, sunshine vs rain tomorrow
modelData%>%
  ggplot(aes(fill=sunshine,x = rainfall+.001))+
  geom_density(alpha=.5)+
  scale_x_continuous(trans = "log10")+
  scale_fill_sliced()+
  labs(
   title = "Ain't No Sunshine When It Rainnnnns",
   subtitle = "Impact of Rainfall on Presence of Sunshine",
   y = "Density of Occurance",
   x = "Rainfall (Log10 Scale)",
   fill = "Sunshine Present",
   caption = "Interesting Coding of NA, likely when no rain occurs."
  )
  
## regions by rainfall and temperature
model_cum<-modelData%>%
  mutate(
    monthYear=my(paste(month(date),year(date))))%>%
  complete(monthYear = seq.Date(min(monthYear),max(monthYear),by="month"),
           fill=list(rainfall=0))%>%
  group_by(monthYear,location)%>%
    summarise(rainfall = cumsum(rainfall))%>%
  summarise(rainfall = max(rainfall))%>%
  ungroup()%>%
  group_by(location)%>%
  mutate(totalRain = cumsum(rainfall))%>%
  ungroup()%>%
  group_by(monthYear)%>%
  mutate(Rank = order(totalRain, decreasing = T))%>%
  ungroup()

nosunCities<-model_cum%>%
  filter(monthYear== max(monthYear),
         Rank %in% 1:5)%>%
  dplyr::select(location)%>%unlist()
      
model_cum%>%
  filter(monthYear>ymd("2015-01-1"))%>%
  filter(location %in% nosunCities)%>%
  ggplot(aes(x = monthYear, y = Rank, color = as.factor(location)))+
  geom_point(size =5)+
  geom_bump(size = 2, smooth = 8,alpha = .95)+
  scale_color_sliced(palette = "Sequential")+
  scale_y_reverse()+
  labs(
    title = "Race to Rain!",
    subtitle = "Temporal Rankings of Current Top 5 Rained On Locations",
    y = "Ranking",
    x = "Time",
    color = "Location",
    caption = "Sale has just stayed wet for a couple years, 
    like that one gym towel.."
  )




## scatterplot numerical

## 

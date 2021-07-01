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
library(patchwork)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

# all_cores <- parallel::detectCores()
# registerDoParallel(cores=all_cores-2)

options(scipen=999)
commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}
source("SlicedTheme.R")

modelData<-as.data.frame(read.csv("Season1/s00e02/Copy of sliced_data.csv",header = T,stringsAsFactors = F,sep=","))
modelData_brewz<-as.data.frame(read.csv("Season1/s00e02/Copy of brewery_and_beer_info.csv",header = T,stringsAsFactors = F,sep=","))
modelData_peepz<-as.data.frame(read.csv("Season1/s00e02/Copy of reviewer_info.csv",header = T,stringsAsFactors = F,sep=","))

modelData<-modelData%>%
  left_join(modelData_brewz,by=c("brewery_id","beer_category"))#%>%
# left_join(modelData_peepz,by=c("review_profilename","beer_category"))

modelData<-modelData%>%
  mutate(review_time = as_datetime(review_time))
modelData<-modelData%>%group_by(review_profilename)%>%
mutate(dateSinceFirst = review_time-min(review_time))

modelData<-modelData%>%group_by(review_profilename)%>%
  mutate(dateSinceFirst = review_time-min(review_time))

modelData%>%group_by(review_profilename)%>%
  mutate(review_overall = scale(review_overall),
         n=n())%>%
  filter(n>100)%>%
ggplot(aes(x=dateSinceFirst,y=review_overall))+
  geom_point()+
  geom_smooth()

# time series year over year plot
modelData%>%
  # group_by(review_profilename)%>%
  # mutate(
  #   review_overall = scale(review_overall),
  #        n=n())%>%
  # filter(n>100)%>%
  ungroup()%>%
  group_by(month(review_time),year(review_time))%>%
  summarize(review_overall = mean(review_overall,na.rm=T))%>%
ungroup()%>%
  mutate(review_time=months(`month(review_time)`)+years(`year(review_time)`))%>%
  filter(year(review_time)>2001)%>%
  ggplot(aes(x = month(review_time), y = review_overall, group = as.factor(year(review_time)))) +
  geom_line(aes(color =as.factor(year(review_time)))) +
  labs(title = "Rating Score Over The Year", x = "Month of Year", y = "Review Overall Value",
       subtitle = "") +
  scale_y_continuous()+
  scale_x_continuous(breaks=1:12)


modelData%>%
  # group_by(review_profilename)%>%
  # mutate(
  #   review_overall = scale(review_overall),
  #        n=n())%>%
  # filter(n>100)%>%
  ungroup()%>%
  group_by(month(review_time),year(review_time))%>%
  summarize(review_overall = quantile(review_overall,.25,na.rm=T))%>%
  ungroup()%>%
  mutate(review_time=months(`month(review_time)`)+years(`year(review_time)`))%>%
  filter(year(review_time)>2001)%>%
  ggplot(aes(x = month(review_time), y = review_overall, group = year(review_time))) +
  geom_area(aes(fill =year(review_time))) +
  labs(title = "Rating Score Over The Year", x = "Month of Year", y = "Review Overall Value",
       subtitle = "") +
  scale_y_continuous()+
  scale_x_continuous(breaks=1:12)

modelData_ts <- modelData %>%
  ungroup()%>%
  select(review_time, review_overall) %>%
  mutate(review_time =date(review_time))%>%
  set_names(c("date", "value")) 

# TS diagnostics
acf(ts, lag.max = 12) # last sig correlation is p for MA(p)
pacf(ts, lag.max = 12, ylab = "PACF") # last sig correlation is q for AR(q)


#### toss out all variables missing from hold out
modelData<-modelData%>%
  dplyr::select(
    -review_aroma,
    -review_appearance,
    -review_palate,
    -review_taste,
  )%>%
  dplyr::select(
    -contains("time")
  )

#### clean up formats
modelData_prep<-modelData%>%
  dplyr::select(
    -brewery_id,
    -beer_beerid,
    -beer_name
  )%>%
  as_tibble()%>%
  # mutate_if(is.character,naFixer)%>%
  mutate_if(is.character,as.factor)

modelData_split<-initial_split(modelData_prep)
modelData_train<-training(modelData_split)
modelData_test<-testing(modelData_split)

modelData_folds<-vfold_cv(modelData_train, repeats = 1)

bsRec<-recipe(review_overall~.,data=modelData_train)%>%
  step_corr(all_numeric_predictors())%>%
  step_other(all_nominal_predictors(),threshold = 0.05)%>%
  step_novel(all_nominal_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_zv(all_numeric_predictors())%>%
  step_normalize(all_numeric_predictors())%>%
  # step_YeoJohnson(all_numeric_predictors())%>%
  step_impute_median(all_numeric_predictors())%>%
  step_sample(size=.5)

modelData_Juiced<-bsRec%>%prep()%>%juice()%>%
  mutate(rev50 = review_overall>median(review_overall))

corMat<-cor(modelData_Juiced%>%dplyr::select_if(is.numeric),use="pairwise.complete.obs")%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  arrange(desc(review_overall))
modelData_Juiced%>%
  GGally::ggscatmat(columns = which(colnames(modelData_Juiced)%in%corMat$rowname[c(1:10)]),color="rev50",alpha=.05,corMethod = "spearman")+
  tayloRswift::scale_color_taylor("lover") # taylor swift all day!
# obs: the data is very correlated for the main predictors, don't know how complicated this will be as a model but we need penalization to avoid overfitting.

# look for non-linear relationships in main predictors
col2check<-corMat$rowname[c(2:21)]
plotList<-lapply(1:20,function(i){
ggplot(modelData_Juiced,aes_string(y="review_overall",x=col2check[i]))+
  geom_point()+
  geom_smooth()
})

for(i in 1:5){
print(plotList[[(i-1)*4 +1]]+plotList[[(i-1)*4 +2]]+plotList[[(i-1)*4 +3]]+plotList[[(i-1)*4 +4]])
}
brewECDF<-ecdf(modelData_Juiced$brewery_review_overall_mean)
ggplot(modelData_Juiced,
       aes(y=review_overall,
                  x=brewery_beer_category_review_overall_median,
                  color = as.factor(round(brewECDF(brewery_review_overall_mean)*5,0)))
       )+
  geom_jitter(alpha = .1)+
  geom_smooth(method = "lm")+
  labs(color = "Brewery Class\n(5 = Best)",
       title = "Good Breweries Don't Make Any Styles Terribly,\nBut Meh Breweries Still Can Brew Some Stuff Well")


ggplot(modelData_prep%>%filter(beer_abv<20,beer_category%in%c("Berliner Weissbier","Fruit / Vegetable Beer","Gose",
                                                              "Gueuze","Lambic - Fruit","Lambic - Unblended",
                                                              "sour")),
       aes(y=review_overall,
           x=beer_abv,
           color = beer_category
       )
)+
         geom_jitter(alpha = .1)+
         geom_smooth(
           method = "lm",
           formula = y ~ splines::bs(x, df = 2, degree = 1, knots = 6)) +
         labs(color = "Stout",
              title = "Heavy Stouts Are Not As Impressive As Other Heavies")
     
betterCategories<-function(beer_category){
  
  ifelse(
    beer_category%in%c("stout","porter"),"Stout",
    ifelse(beer_category==c("ipa","pale"),"ipa",
           ifelse(grepl("dark",beer_category),"dark",
                  ifelse(grepl("Lager",beer_category),"lager",
                         ifelse(beer_category%in%c("pilsner","Czech Pilsener","German Pilsener","Kölsch"),"pilsner",
                                ifelse(beer_category%in%c("English Barleywine","Wheatwine","strong"),"strong",
                                       ifelse(beer_category%in%c("Berliner Weissbier","Fruit / Vegetable Beer","Gose",
                                                                 "Gueuze","Lambic - Fruit","Lambic - Unblended",
                                                                 "sour"),"fruit/sour",
                                              ifelse(beer_category%in%c("Tripel","Quadrupel (Quad)","Dubbel"),"belgian",
                                                     ifelse(beer_category%in%c("wheat","Saison / Farmhouse Ale"),"belgian",
                                                            
                                                     "other")))))))))
  
}

betterCategories(modelData_prep$beer_category)


ggplot(modelData_prep%>%
         mutate(beer_category=betterCategories(beer_category)),
       aes(x = review_overall, y = beer_category, fill = beer_category)) + 
  geom_density_ridges(scale = 1.5) + 
  scale_fill_cyclical(values = Sliced_Qualitative_Palette[1:2])

ggplot(modelData_prep%>%
         mutate(beer_category=betterCategories(beer_category),
                strong = beer_abv>6)%>%
         group_by(beer_category,strong)%>%
         summarise(
           review_overall = mean(review_overall)
                   )%>%
         arrange(desc(review_overall)),
       aes(y=review_overall,
           x=beer_category,
           fill = strong
       )
)+
  geom_bar(stat="identity", position=position_dodge())+
  labs(fill = "ABV>6",
       title = "Who wants a Heavy Lager/Pilsner?")
modelData%>%
  filter(brewery_review_overall_mean<quantile(brewery_review_overall_mean,.05))%>%
  # mutate(beer_category=betterCategories(beer_category))%>%
  group_by(beer_name)%>%
  summarise(
    review_overall = mean(review_overall),
    reviews = n())%>%
  filter(reviews>5)%>%
  arrange((review_overall))

modelData%>%
  filter(brewery_name == "Anheuser-Busch")%>%
  group_by(beer_name)%>%
  summarise(
    review_overall = mean(review_overall),
    reviews = n())%>%
  filter(reviews>5)%>%
  ggplot(aes(x=reviews,y = review_overall))+
  geom_point()+
  geom_text(aes(label = beer_name))+
  labs(title = "AB Makes A lot of bad beer + Michelob Seasonals")

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
library(textrecipes)

options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)

modelData<-as_tibble(read.csv("Season 2/sliced-s01e01/train.csv",header=T,stringsAsFactors=F,sep=","))

skimr::skim(modelData)

head(modelData$mechanic)
mechanics<-unique((modelData%>%separate_rows(mechanic,sep = ', '))$mechanic)
modelData[,mechanics]<-lapply(mechanics,function(mc){
  grepl(mc,modelData$mechanic)
})%>%do.call(cbind,.)

designers<-unique((modelData%>%separate_rows(designer,sep = ', '))$designer)
modelData[,designers]<-lapply(designers,function(mc){
  grepl(mc,modelData$designer)
})%>%do.call(cbind,.)
modelData_prep<-modelData%>%
  dplyr::select(
    -mechanic,
    -designer,
    -game_id,
    -names
  )
    
p1<-ggplot(modelData,aes(x=geek_rating,fill=fct_lump_n(category1,7)))+
  geom_density(alpha=.5)+
  theme(legend.position="top")+
  scale_fill_manual(values =  Sliced_Qualitative_Palette)
p2<-ggplot(modelData,aes(x=log(TotalViews),fill=as.factor(Name=="earth and nature")))+
  geom_histogram(position = "stack")+
  theme(legend.position="top")
p3<-ggplot(modelData,aes(x=log(TotalViews),fill=as.factor(Name=="arts and entertainment")))+
  geom_histogram(position = "stack")+
  theme(legend.position="top")
p4<-ggplot(modelData,aes(x=log(TotalViews),fill=as.factor(Name=="computer science")))+
  geom_histogram(position = "stack")+
  theme(legend.position="top")
p5<-ggplot(modelData,aes(x=log(TotalViews),fill=as.factor(Name=="health")))+
  geom_histogram(position = "stack")+
  theme(legend.position="top")
p6<-ggplot(modelData,aes(x=log(TotalViews),fill=as.factor(Name=="education")))+
  geom_histogram(position = "stack")+
  theme(legend.position="top")
p1+p2+p3+p4+p5+p6

modelData%>%separate_rows(designer,sep = ', ')%>%
  mutate(designer =fct_lump_n(designer,10))%>%
  group_by(designer)%>%
  summarise(geek_rating=mean(geek_rating,na.rm = T),
          max_players=mean(max_players,na.rm = T),
          warGamePerc = sum(category1=="Wargame")/n())%>%arrange(desc(geek_rating))%>%
  ggplot(aes(x=designer,y=geek_rating,fill=warGamePerc))+
           geom_col()+
  theme(axis.text.x = element_text(angle = 45))

modelData%>%separate_rows(mechanic,sep = ', ')%>%
  mutate(mechanic =fct_lump_n(mechanic,10))%>%
  group_by(mechanic)%>%
  summarise(geek_rating=mean(geek_rating,na.rm = T),
            max_players=mean(max_players,na.rm = T),
            warGamePerc = sum(category1=="Wargame")/n())%>%arrange(desc(geek_rating))%>%
  ggplot(aes(x=mechanic,y=geek_rating,fill=warGamePerc))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 45))

bsRec<-recipe(geek_rating~.,data=modelData_prep)%>%
  # step_tokenize(mechanic,token = "ngrams")%>%
  # step_stem(mechanic)%>%
  # step_stopwords(mechanic,custom_stopword_source = c(0:20000,letters))%>%
  # step_tokenfilter(mechanic, max_tokens = 50) %>%
  # step_tfidf(mechanic) %>%
  step_corr(all_numeric_predictors(),threshold=.95)%>%
  step_other(all_nominal_predictors())%>%
  step_novel(all_nominal_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_YeoJohnson(all_numeric_predictors())
class(bsRec)<-c("base",class(bsRec))

glmSpec<-linear_reg(mixture = tune(),penalty = tune())%>%
  set_engine("glmnet")
xgSpec<-boost_tree(
  trees = tune(), 
  min_n = tune(),
  tree_depth = tune()
)%>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

preprocesses<-list(bsRec)

models<-list(
  glm=glmSpec,
  xg=xgSpec
)

modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFolds<-vfold_cv(modelTrain,v=5,repeats = 1)

wflowz<-workflow_set(preprocesses,models,cross = T)%>%
  workflow_map(
    resamples = modelFolds,
    grid = 10,
    metrics = metric_set(rmse,rsq),
    seed = 1, # cause I am ...
    verbose = T)
rank_results(wokeset)
autoplot(wokeset)


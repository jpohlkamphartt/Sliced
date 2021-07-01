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

modelData<-as_tibble(read.csv("Season1/sliced-s00e04/s00e04-sliced_data.csv",header=T,stringsAsFactors=F,sep=","))

skimr::skim(modelData)

p1<-ggplot(modelData,aes(x=log(TotalVotes)))+
  geom_histogram()
p2<-ggplot(modelData,aes(x=log(TotalKernels)))+
  geom_histogram()
p3<-ggplot(modelData,aes(x=log(TotalKernels),y=log(TotalVotes)))+
  geom_point()+
  geom_smooth()
p4<-ggplot(modelData,aes(y=log(TotalCompressedBytes),x=log(TotalUncompressedBytes)))+
  geom_point()+
  geom_smooth()
p1+p2+p3+p4

p1<-ggplot(modelData,aes(x=log(TotalDownloads)))+
  geom_histogram()
p2<-ggplot(modelData,aes(x=DaysSinceCreation,y=DaysSinceLastUpdate))+
  geom_point()+
  geom_smooth()
p3<-ggplot(modelData%>%filter(Name == "arts and entertainment"),aes(x=DaysSinceLastUpdate,y=log(TotalViews)))+
  geom_point()+
  geom_smooth()
p4<-ggplot(modelData,aes(x=DaysSinceLastUpdate ,y=log(TotalViews),color=fct_lump_n(Name,8)))+
  geom_point(alpha=.1)+
  geom_smooth()
p1+p2+p3+p4

modelData%>%filter(Name == "arts and entertainment", DaysSinceLastUpdate>750,log(TotalViews)<5)%>%View()

p1<-ggplot(modelData,aes(x=log(TotalViews),fill=fct_lump_n(Name,8)))+
  geom_density(alpha=.5)+
  theme(legend.position="top")
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

modelData_prep<-modelData%>%
  dplyr::select(
   -Subtitle, -Id,-VersionUpdate
  )%>%mutate(
    CreationDate = ymd_hms(CreationDate),
    yearCreate = year(CreationDate),
    monthCreate = month(CreationDate)
  )%>%
  dplyr::select(-CreationDate)%>%
  mutate_if(is.integer,as.numeric)

corMat<-cor(modelData_prep%>%select_if(is.numeric),use = "pairwise.complete.obs")%>%as.data.frame()%>%rownames_to_column()
modelData_prep%>%
  mutate(topView = TotalViews>median(TotalViews),
         TotalDownloads = log(TotalDownloads),
         TotalVotes = log(TotalVotes),
         TotalKernels = log(TotalKernels),
         TotalViews = log(TotalViews))%>%
  IDPmisc::NaRV.omit()%>%
  GGally::ggscatmat(columns = which(colnames(modelData_prep)%in%corMat$rowname[1:7]),color = "topView",alpha = .2)+
  tayloRswift::scale_color_taylor("lover") # lover = best album cover

modelSplit<-initial_split(modelData_prep)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1)

bsRec<-recipe(TotalViews~.,data=modelData_prep)%>%
  step_tokenize(Title)%>%
  step_stem(Title)%>%
  step_stopwords(Title,custom_stopword_source = c(0:20000,letters))%>%
  step_tokenfilter(Title, max_tokens = 250) %>%
  step_tfidf(Title) %>%
  # step_corr(all_numeric_predictors(),threshold=.95)%>%
  # step_other(all_nominal_predictors())%>%
  # step_novel(all_nominal_predictors())%>%
  # step_dummy(all_nominal_predictors())%>%
  # step_YeoJohnson(all_numeric_predictors())
  prep()%>%
  juice()
class(bsRec)<-c("base",class(bsRec))
preprocess<-list(
  base =bsRec
)

glmSpec<-poisson_reg(penalty = tune(),mixture = tune())%>%
  set_engine("glmnet")

xgbSpec<-boost_tree(
  trees = tune(), 
  min_n = tune(),
  tree_depth = tune(),
  # mtry = tune() 
) %>% 
  set_engine("xgboost", objective='count:poisson') %>% 
  set_mode("regression")
models<-list(
  glm = glmSpec,
  xg = xgbSpec
  )

wokeset<-workflow_set(preprocess,models,cross = T)%>%
  workflow_map(
    resamples = modelFold,
    grid = 10,
    metrics = metric_set(rsq,rsq_trad,rmse),
    seed = 1, # cause I am ...
    verbose = T)
rank_results(wokeset)
autoplot(wokeset)

modelV1_best<-select_best(pull_workflow_set_result(wokeset,"base_xg"),metric = "rsq")
modelV1_mod<-finalize_workflow(pull_workflow(wokeset,"base_xg"),modelV1_best)
modelV1<-fit(modelV1_mod,modelTrain)

# hold out calc 1
holdData<-as_tibble(read.csv("Season1/sliced-s00e04/s00e04-holdout-data.csv",header=T,stringsAsFactors=F,sep=","))
holdData_prep<-holdData%>%
  dplyr::select(
    -Title, -Subtitle,-VersionUpdate
  )%>%mutate(
    CreationDate = ymd_hms(CreationDate),
    yearCreate = year(CreationDate),
    monthCreate = month(CreationDate)
  )%>%
  dplyr::select(-CreationDate)%>%
  mutate_if(is.integer,as.numeric)
holdData_prep$pred<-predict(modelV1,new_data = holdData_prep)$.pred

exportData<-holdData_prep%>%
  rename(TotalViews = pred)%>%
  dplyr::select(Id, TotalViews)
write_csv(exportData,"Season1/sliced-s00e04/holdoutPredz_s00e04_JPH.csv")
  
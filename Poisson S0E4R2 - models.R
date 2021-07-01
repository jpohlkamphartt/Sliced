library(lightgbm)
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
# library(workflowsets)
# library(tidyposterior)
library(discrim)
library(hablar)
library(doParallel)
library(patchwork)
library(textrecipes)
library(extrafont)
library(fastshap)
library(skimr)
library(beepr)
library(tayloRswift)
library(treesnip)
library(stacks)
library(tictoc)
library(poissonreg)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season1/sliced-s00e04/s00e04-sliced_data.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season1/sliced-s00e04/s00e04-holdout-data.csv",header=T,stringsAsFactors=F,sep=","))

skim(modelData)

modelData<-modelData%>%
  dplyr::select(
    -Id,-Subtitle
  )%>%
  mutate_at(vars(CreationDate, VersionUpdate),ymd_hms)%>%
  mutate(Name = as.factor(Name),
         createMonth = month(CreationDate),
         createYear = year(CreationDate),
         updateMonth = month(VersionUpdate),
         updateYear = month(VersionUpdate),
         )%>%
  dplyr::select(-CreationDate, -VersionUpdate)
  
modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats =1)

FRMLA<-formula(TotalViews~.)

rec_glm<-recipe(FRMLA, data = modelTrain)%>%
  step_tokenize(Title)%>%
  step_stem(Title)%>%
  step_stopwords(Title, custom_stopword_source = c(0:200000,letters))%>%
  step_tokenfilter(Title, max_tokens = 200)%>%
  step_tfidf(Title)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(),threshold = .01)%>%
  step_unknown(all_nominal_predictors())%>%
  step_YeoJohnson(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  # step_nzv(all_predictors(),freq_cut = 500/1)%>%
  step_dummy(all_nominal_predictors())

rec_knn<-recipe(FRMLA, data = modelTrain)%>%
  # step_tokenize(Title)%>%
  # step_stem(Title)%>%
  # step_stopwords(Title, custom_stopword_source = c(0:200000,letters))%>%
  # step_tokenfilter(Title, max_tokens = 500)%>%
  # step_tfidf(Title)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(),threshold = .01)%>%
  step_unknown(all_nominal_predictors())%>%
  step_YeoJohnson(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  # step_nzv(all_predictors(),freq_cut = 500/1)%>%
  step_dummy(all_nominal_predictors())

rec_cat<-recipe(FRMLA, data = modelTrain)%>%
  step_tokenize(Title)%>%
  step_stem(Title)%>%
  step_stopwords(Title, custom_stopword_source = c(0:200000,letters))%>%
  step_tokenfilter(Title, max_tokens = 200)%>%
  step_tfidf(Title)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(),threshold = .01)%>%
  step_unknown(all_nominal_predictors())%>%
  step_YeoJohnson(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_nzv(all_predictors(),freq_cut = 500/1)%>%
  step_rm(
    tfidf_Title_and ,
       tfidf_Title_data , tfidf_Title_dataset ,
        tfidf_Title_for  ,
       tfidf_Title_in , tfidf_Title_model  ,
       tfidf_Title_of , 
        tfidf_Title_the , tfidf_Title_train 
  )%>%
  step_dummy(all_nominal_predictors())

ctrl_grid<-control_stack_grid()

glmSpec<-poisson_reg(mixture = tune(), penalty = tune())%>%
  set_engine("glmnet")

glmGrid<-grid_max_entropy(
  mixture(),
  penalty(),
  size = 10,
  variogram_range = .75
)

glmWflow<-workflow()%>%
  add_recipe(rec_glm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)

collect_metrics(glmRes)

modelV1_best<-select_best(glmRes,metric = "rsq")
modelV1_mod<-finalize_workflow(glmWflow,modelV1_best)
modelV1<-fit(modelV1_mod,modelTrain)

collect_metrics(knnRes)

catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                     sample_size = tune(),learn_rate = tune(),mtry = tune())%>%
  # set_engine("catboost")%>%
  # set_engine("catboost", objective='Poisson') %>%
  set_engine("xgboost", objective='count:poisson') %>%
  # set_engine("catboost", objective='Multiclass') %>% 
  # set_engine("xgboost", objective = "multi:softprob") %>% 
  # set_mode("classification")
  set_mode("regression")


catGrid<-grid_max_entropy(
  trees(range = c(100L, 1200L)),
  tree_depth(range = c(1L, 10L)),
  min_n(range = c(2L, 25L)),
  sample_size = sample_prop(range = c(2/10, 1)),
  learn_rate(range = c(-5, -1)),
  finalize(mtry(range = c(2L, unknown())), modelTrain),
  size = 20,
  variogram_range = .75
)

catWflow<-workflow()%>%
  add_recipe(rec_cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  # metrics = metric_set(mn_log_loss,roc_auc),
  metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)

bind_rows(
  collect_metrics(catRes)%>%
    dplyr::select(.metric,mean)%>%mutate(type = "cat")
    ,collect_metrics(glmRes)%>%
    dplyr::select(.metric,mean)%>%mutate(type = "glm")
  )%>%filter(.metric =="rmse") %>% arrange((mean))
modelV1_best<-select_best(catRes,metric = "rsq")
modelV1_mod<-finalize_workflow(catWflow,modelV1_best)
modelV1<-fit(modelV1_mod,modelTrain)


predz<-predict(modelV1,
               new_data = modelData,
               type = "numeric"
               
)

modelData%>%
  mutate(predz = predz$.pred)%>%
  ggplot(aes(x= TotalViews, y = predz))+
  geom_point(alpha = .5)+
  geom_smooth()

holdData<-holdData%>%
  dplyr::select(
    -Subtitle
  )%>%
  mutate_at(vars(CreationDate, VersionUpdate),ymd_hms)%>%
  mutate(Name = as.factor(Name),
         createMonth = month(CreationDate),
         createYear = year(CreationDate),
         updateMonth = month(VersionUpdate),
         updateYear = month(VersionUpdate),
  )%>%
  dplyr::select(-CreationDate, -VersionUpdate)

predz<-predict(model_st,
               new_data = holdData,
               type = "numeric"
               
)

holdData$TotalViews<-predz$.pred

ggplot(holdData,aes(x=TotalViews))+
  geom_density()+
scale_x_continuous(trans = "log10")  

write_csv(holdData%>%dplyr::select(Id,TotalViews),"S0E4R2submission1JPH.csv")


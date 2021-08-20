#### Libraries ####
# library(lightgbm)
library(tidyverse)
library(tidymodels)
library(recipes)
library(themis)
library(vip)
library(ggplot2)
# library(furrr)
library(here)
library(lubridate)
# library(mgcv)
library(parallel)
# library(workflowsets)
# library(tidyposterior)
library(discrim)
library(hablar)
library(doParallel)
library(patchwork)
library(textrecipes)
# library(extrafont)
# library(fastshap)
library(skimr)
library(beepr)
# library(tayloRswift)
library(treesnip)
library(stacks)
# library(tictoc)
#### ####
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1987) # Mid Thirties Never Felt So Young

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e07/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e07/test.csv",header=T,stringsAsFactors=F,sep=","))

skim(modelData)
modelData$attrition_flag<-as.factor(modelData$attrition_flag)
# data is imbalanced will SMOTE
# low # of cat fields so will include in catboost
# make linear model also with all variables

modelSplit<-initial_split(modelData, strata = education_level)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=6,repeats=1)

rec_gb<-recipe(
  attrition_flag ~ ., 
  data = modelTrain)%>%
  step_rm(id)%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_smote(attrition_flag)

ctrl_grid<-control_stack_grid() ## we stacking

catSpec<-boost_tree(trees = tune(), tree_depth = tune(),
                    learn_rate = tune(), mtry = tune())%>%
  set_engine("catboost")%>%
  set_mode("classification")

catGrid<-grid_max_entropy(
  trees(range = c(750L, 1500L)),
  tree_depth(range = c(2L, 12L)),
  learn_rate(range = c(-3, -.05)),
  finalize(mtry(range = c(4L, unknown())), modelTrain),
  size = 20,
  variogram_range = .75
)

catWflow<-workflow()%>%
  add_recipe(rec_gb)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(mn_log_loss),
  control = ctrl_grid
)

catWflow%>%finalize_workflow(parameters = select_best(catRes, "mn_log_loss"))%>%fit(modelTrain)%>%pull_workflow_fit() %>%vip()

collect_metrics(catRes)%>%arrange(mean,std_err)
autoplot(catRes)


rec_lm<-recipe(   attrition_flag ~ ., 
                  data = modelTrain)%>%
  step_rm(id)%>%
  step_zv(all_predictors())%>%
  step_BoxCox(all_numeric_predictors() )%>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(), threshold = tune())%>%
  step_smote(attrition_flag)

glmSpec<-logistic_reg(mixture = tune(), penalty = tune())%>%
  set_engine("glmnet")

glmGrid<-grid_max_entropy(
  mixture(range = c(10^-15,10^-2)),
  penalty(),
  threshold(range = c(.75,.99)),
  size = 20,
  variogram_range = .7
)

glmWflow<-workflow()%>%
  add_recipe(rec_lm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss),
  control = ctrl_grid
)
beepr::beep("fanfare")

glmWflow%>%finalize_workflow(parameters = select_best(glmRes, "mn_log_loss"))%>%fit(modelTrain)%>%pull_workflow_fit() %>%vip()

collect_metrics(glmRes)%>%filter(.metric =="mn_log_loss")%>%arrange(mean,std_err)
autoplot(glmRes)

rec_knn<-recipe(  attrition_flag ~ ., 
                  data = modelTrain)%>%
  step_rm(id)%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_normalize(all_numeric_predictors() )%>%
  step_smote(attrition_flag)

knnSpec<-nearest_neighbor(neighbors = tune(), weight_func = tune())%>%
  set_engine("kknn")%>%
  set_mode("classification")

knnGrid<-grid_max_entropy(
  neighbors(range= c(25L,75L)),
  weight_func(c("optimal","rectangular", "gaussian")),
  size = 10,
  variogram_range = .7
)

knnWflow<-workflow()%>%
  add_recipe(rec_knn)%>%
  add_model(knnSpec)

knnRes<-tune_grid(
  knnWflow,
  resamples = modelFoldz,
  grid = knnGrid,
  metrics = metric_set(mn_log_loss),
  control = ctrl_grid
)

model_st<-
  stacks()%>%
  add_candidates(catRes)%>%
  add_candidates(glmRes)%>%
  add_candidates(knnRes)%>%
  blend_predictions(
    metric = metric_set(mn_log_loss),
    penalty = 10^(-6:-1), mixture = (0:5)/5)%>%
  fit_members()
autoplot(model_st, type = "weights")
autoplot(model_st)
autoplot(model_st, type = "members")

beepr::beep("fanfare")

collect_metrics(knnRes)%>%filter(.metric =="mn_log_loss")%>%arrange(mean,std_err)
autoplot(knnRes)


# holdout
holdData<-as_tibble(read.csv("Season 2/sliced-s01e07/test.csv",header=T,stringsAsFactors=F,sep=","))

predz<-predict(model_st,
               new_data = holdData,
               # type = "class"
               type = "prob"
               # type = "numeric"
               
)

holdData$attrition_flag<-predz$.pred_1

ggplot(holdData,aes(x=attrition_flag))+
  geom_histogram()

write_csv(holdData%>%dplyr::select(id,attrition_flag),"S1R7submission1JPH.csv")

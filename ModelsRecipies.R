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
library(extrafont)
# library(fastshap)
library(skimr)
library(beepr)
# library(tayloRswift)
library(treesnip)
library(stacks)
library(tictoc)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e04/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e04/test.csv",header=T,stringsAsFactors=F,sep=","))

modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1)

FRMLA<-formula(TotalViews~.)

#### Recipes for different prediction types ####
step_tokenize(Title)%>%
  step_stem(Title)%>%
  step_stopwords(Title,custom_stopword_source = c(0:20000,letters))%>%
  step_tokenfilter(Title, max_tokens = tune()) %>%
  step_tfidf(Title) %>%

# location specific impution
  step_impute_bag(vars, impute_with = factor)
  
# binary, multiclass, ordinal
Rec_Cat<-
  recipe(modelFormula_glm ,data=modelTrain)%>%
  # step_ordinalscore(aircraft_mass,species_quantity)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  # step_BoxCox(all_numeric_predictors())%>% 
  # step_YeoJohnson(all_numeric_predictors())%>% 
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  # step_dummy(all_nominal_predictors(),one_hot=T)%>%
  # step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(damaged)

Rec_glm<-
  recipe(modelFormula_glm ,data=modelTrain)%>%
  # step_num2factor(incident_month,levels = as.character(sort(unique(modelData$incident_month))))%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(damaged)

# Rec_knn<-
#   recipe(modelFormula_knn ,data=modelTrain)%>%
#   step_novel(all_nominal_predictors()) %>%
#   step_other(all_nominal_predictors(), threshold = 0.01) %>%
#   step_unknown(all_nominal_predictors()) %>%
#   step_impute_knn(all_numeric_predictors())%>%
#   step_zv(all_predictors())%>%
#   step_dummy(all_nominal_predictors(),one_hot=T)%>%
#   step_corr(all_numeric_predictors(),threshold = .9)%>%
#   step_smote(damaged)


#### Models for different prediction types

ctrl_grid <- control_stack_grid()

glmSpec<-logistic_reg(mixture = tune(),penalty = tune())%>% 
  #multinom_reg(mixture = tune(),penalty = tune())%>%
  #linear_reg(mixture = tune(),penalty = tune())%>%
  #poisson_reg(mixture = tune(),penalty = tune())%>%  #library(poissonreg)
  set_engine("glmnet")%>%
  set_mode("classification")
  # set_mode("regression")


glmGrid<-grid_max_entropy(
  mixture(),
  penalty(),
  size = 10,
  variogram_range = .75
)


glmWflow<-workflow()%>%
  add_recipe(Rec_glm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss,roc_auc,accuracy),
  # metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)

# ### no KNN for count data
# knnSpec <- nearest_neighbor(
#   neighbors = tune(), weight_func = tune()
# ) %>% set_engine("kknn") %>%
#   set_mode("classification")
# # set_mode("regression")
# 
# 
# knnGrid<-grid_max_entropy(
#   neighbors(range = c(4L,8L)),
#   weight_func(c("optimal","rectangular")),
#   size = 6,
#   variogram_range = .75
# )
# 
# knnWflow<-workflow()%>%
#   add_recipe(Rec_knn)%>%
#   add_model(knnSpec)
# 
# knnRes<-tune_grid(
#   knnWflow,
#   resamples = modelFoldz,
#   grid = knnGrid,
#   metrics = metric_set(mn_log_loss,roc_auc),
#   # metrics = metric_set(rmse,rsq,mae),
#   control = ctrl_grid
# )
catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                    sample_size = tune(),learn_rate = tune(),mtry = tune())%>%
  set_engine("catboost")%>%
  # set_engine("catboost", objective='Poisson') %>% 
  # set_engine("xgboost", objective='count:poisson'# objective = "multi:softprob") %>% 
  # set_engine("catboost", objective='Multiclass') %>% 
  # set_engine("xgboost", objective = "multi:softprob") %>% 
  set_mode("classification")
  # set_mode("regression")


catGrid<-grid_max_entropy(
  trees(range = c(100L, 1200L)),
  tree_depth(range = c(1L, 10L)),
  min_n(range = c(2L, 25L)),
  sample_size = sample_prop(range = c(2/10, 1)),
  learn_rate(range = c(-5, -1)),
  finalize(mtry(range = c(2L, unknown())), modelTrain),
  size = 8,
  variogram_range = .75
)


catWflow<-workflow()%>%
  add_recipe(Rec_Cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  # metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)


### no stack if poisson
bind_rows(
  collect_metrics(catRes)%>%
    dplyr::select(.metric,mean)%>%mutate(type = "cat")
  ,collect_metrics(glmRes)%>%
    dplyr::select(.metric,mean)%>%mutate(type = "glm")
)%>%filter(.metric =="rsq") %>% arrange(mean)
modelV1_best<-select_best(catRes,metric = "rsq")
modelV1_mod<-finalize_workflow(catWflow,modelV1_best)
modelV1<-fit(modelV1_mod,modelTrain)
modelV1 %>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features =25)

### stack for others
model_st <- 
  stacks() %>%
  add_candidates(glmRes) %>%
  add_candidates(knnRes) %>%
  add_candidates(catRes) %>%
  # determine how to combine their predictions
  blend_predictions(
    metric =metric_set(mn_log_loss),# roc_auc rmse rsq mae 
    penalty = 10^(-6:-1),mixture = (0:5)/5) %>%
  # fit the candidates with nonzero stacking coefficients
  fit_members()
model_st

# model insights
autoplot(model_st)
autoplot(model_st, type = "members")
autoplot(model_st, type = "weights")

# roc
modelData%>%
  mutate(predz = predz$.pred)%>%
  roc_curve(var, predz) %>% 
autoplot()

modelData%>%
  mutate(predz = predz$.pred)%>%
  ggplot(aes(x= TotalViews-predz, y = predz))+
  geom_point(alpha = .5)+
  geom_smooth()

modelData%>%
  mutate(predz = predz$.pred)%>%
  ggplot(aes(sample = predz)) + stat_qq() + stat_qq_line()


predz<-predict(model_st,
               new_data = holdData,
               type = "class"
               # type = "prob"
               # type = "numeric"
               
               )

holdData$profit<-predz

ggplot(holdData,aes(x=match))+
  geom_histogram(stat="count")

write_csv(holdData%>%dplyr::select(id,match),"S0E1R2submission1JPH.csv")
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

modelData<-modelData%>%
  dplyr::select(
    -id
  )%>%
  mutate(date = ymd(date))

# lag on location by day, if missing fill with median of day
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

skim(modelData)
table(modelData$rain_tomorrow)

FRMLA<-formula(rain_tomorrow~.)

modelData<-modelData%>%
  mutate(month = month(date),
         year = year(date))%>%
  dplyr::select(
    -date,-rain_today_182,-rain_today_183,-rain_today_181,
    -rain_today_365,-rain_today_364,-rain_today_366,
  )%>%
  mutate_at(
    vars(rain_today_1,rain_today_2,rain_today,rain_tomorrow), as.factor)%>%
  mutate_if(is.character, as.factor)

modelSplit<-initial_split(modelData,strata = location)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1)
tic()
rec<-recipe(FRMLA, data = modelData)%>%
  step_rm(evaporation,sunshine)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_YeoJohnson(rainfall,wind_gust_speed,wind_speed3pm,humidity9am)%>%
  step_impute_knn(all_numeric_predictors())%>%
  step_ns(month, deg_free = 4)%>%
  step_ns(year, deg_free = 2)%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(rain_tomorrow)

rec_knn<-recipe(FRMLA, data = modelData)%>%
  step_rm(evaporation,sunshine,temp9am,rain_today_1,rain_today_2)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_YeoJohnson(rainfall,wind_gust_speed,wind_speed3pm,humidity9am)%>%
  step_impute_median(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_downsample(rain_tomorrow,under_ratio=2)%>%
  step_smote(rain_tomorrow)

rec_cat<-recipe(FRMLA, data = modelData)%>%
  step_rm(evaporation,sunshine,temp9am,rain_today_1,rain_today_2,wind_dir9am,wind_gust_dir)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.02) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_YeoJohnson(rainfall,wind_gust_speed,wind_speed3pm,humidity9am)%>%
  step_impute_median(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_downsample(rain_tomorrow,under_ratio=1.5)%>%
  step_smote(rain_tomorrow)

ctrl_grid <- control_stack_grid()

glmSpec<-logistic_reg(mixture = tune(),penalty = tune())%>% 
  #multinom_reg(mixture = tune(),penalty = tune())%>%
  #linear_reg(mixture = tune(),penalty = tune())%>%
  #poisson_reg(mixture = tune(),penalty = tune())%>%
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
  add_recipe(rec)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss,roc_auc,accuracy),
  control = ctrl_grid
)

knnSpec <- nearest_neighbor(
  neighbors = tune(), weight_func = tune()
) %>% set_engine("kknn") %>%
  set_mode("classification")


knnGrid<-grid_max_entropy(
  neighbors(range = c(4L,8L)),
  weight_func(c("optimal","rectangular")),
  size = 6,
  variogram_range = .75
)

knnWflow<-workflow()%>%
  add_recipe(rec_knn)%>%
  add_model(knnSpec)

knnRes<-tune_grid(
  knnWflow,
  resamples = modelFoldz,
  grid = knnGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  # metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)
catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                     sample_size = tune(),learn_rate = tune(),mtry = tune())%>%
  set_engine("catboost")%>%
  # set_engine("catboost", objective='Poisson') %>% 
  # set_engine("xgboost", objective='count:poisson') %>% 
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
  add_recipe(rec_cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  # metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)


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


autoplot(model_st)
autoplot(model_st, type = "members")
autoplot(model_st, type = "weights")
beepr::beep("fanfare")
toc()

holdData<-as_tibble(read.csv("Season 2/sliced-s01e04/test.csv",header=T,stringsAsFactors=F,sep=","))
fillData<-as_tibble(read.csv("Season 2/sliced-s01e04/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData$hold<-T
fillData$hold<-F
holdData<-bind_rows(holdData,fillData%>%dplyr::select(-rain_tomorrow))

holdData<-holdData%>%
  mutate(date = ymd(date))

# lag on location by day, if missing fill with median of day
holdData<-holdData%>%
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

holdData<-holdData%>%
  mutate(month = month(date),
         year = year(date))%>%
  dplyr::select(
    -date,-rain_today_182,-rain_today_183,-rain_today_181,
    -rain_today_365,-rain_today_364,-rain_today_366,
  )%>%
  mutate_at(
    vars(rain_today_1,rain_today_2,rain_today), as.factor)%>%
  mutate_if(is.character, as.factor)

holdData<-holdData%>%filter(hold)

predz<-predict(model_st,
               new_data = holdData,
               # type = "class"
               type = "prob"
               # type = "numeric"
               
)

holdData$rain_tomorrow<-predz$.pred_1


ggplot(holdData,aes(x=rain_tomorrow))+
  geom_density()

write_csv(holdData%>%dplyr::select(id,rain_tomorrow),"S0E4R1submission2JPH.csv")

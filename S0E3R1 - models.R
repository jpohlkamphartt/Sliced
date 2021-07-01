library(lightgbm)
library(treesnip)
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
library(extrafont)
library(fastshap)
library(skimr)
library(beepr)
library(tayloRswift)
library(stacks)
library(zipcodeR)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

# set_dependency("boost_tree", eng = "catboost", "catboost")
# set_dependency("boost_tree", eng = "catboost", "treesnip")

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e03/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e03/test.csv",header=T,stringsAsFactors=F,sep=","))

modelData<-modelData%>%
  dplyr::select(
    -country,
    -city,
    -state,
    # -postal_code,
    -id
  )

modelData<-modelData%>%
  left_join(zip_code_db%>%
              mutate(zipcode = as.numeric(zipcode))%>%
              dplyr::select(
                zipcode,population_density,median_home_value,median_household_income
              ),
            by = c("postal_code"= "zipcode")
  )
holdData<-holdData%>%
  left_join(zip_code_db%>%
              mutate(zipcode = as.numeric(zipcode))%>%
              dplyr::select(
                zipcode,population_density,median_home_value,median_household_income
              ),
            by = c("postal_code"= "zipcode")
  )


modFormula<-formula(
  profit~ship_mode+segment+region+category+
    sub_category+sales+quantity+discount+
    population_density+median_home_value+median_household_income
)
  
modelSplit<-initial_split(modelData,strata = region)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1) # no need to be wasting time on repeats..

Rec_Cat<-
  recipe(modFormula ,data=modelTrain)%>%
  step_novel(all_nominal_predictors()) %>%
  # step_other(all_nominal_predictors(), threshold = 0.01) %>%
  # step_unknown(all_nominal_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors())%>%
  # step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)#%>%
# step_corr(all_numeric_predictors(),threshold = .9)

Rec_glm<-
  recipe(modFormula ,data=modelTrain)%>%
  step_novel(all_nominal_predictors()) %>%
  # step_other(all_nominal_predictors(), threshold = 0.01) %>%
  # step_unknown(all_nominal_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors())%>%
  # step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)#%>%
  # step_corr(all_numeric_predictors(),threshold = .9)

Rec_knn<-
  recipe(modFormula ,data=modelTrain)%>%
  step_novel(all_nominal_predictors()) %>%
  # step_other(all_nominal_predictors(), threshold = 0.01) %>%
  # step_unknown(all_nominal_predictors()) %>%
  step_YeoJohnson(all_numeric_predictors())%>%
  # step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  # step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_dummy(all_nominal_predictors(),one_hot=T)

ctrl_grid <- control_stack_grid()

glmSpec<-linear_reg(mixture = tune(),penalty = tune())%>%
  set_engine("glmnet")%>%
  set_mode("regression")

glmGrid<-grid_latin_hypercube(
  mixture(),
  penalty(),
  size = 10
)

glmWflow<-workflow()%>%
  add_recipe(Rec_glm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(rmse, rsq),
  control = ctrl_grid
)

knnSpec <- nearest_neighbor(
  neighbors = tune(), weight_func = tune()
) %>% set_engine("kknn") %>%
  set_mode("regression")

knnGrid<-grid_latin_hypercube(
  neighbors(),
  weight_func(),
  size = 10
)

knnWflow<-workflow()%>%
  add_recipe(Rec_knn)%>%
  add_model(knnSpec)

knnRes<-tune_grid(
  knnWflow,
  resamples = modelFoldz,
  grid = knnGrid,
  metrics = metric_set(rmse, rsq),
  control = ctrl_grid
)

catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                     sample_size = tune(),
                     learn_rate = tune(),mtry = tune()
                     )%>%
  set_engine("catboost")%>%
  set_mode("regression")

catGrid<-grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  sample_size = sample_prop(),
  learn_rate(),
  finalize(mtry(), modelTrain),
  size = 100
)

catWflow<-workflow()%>%
  add_recipe(Rec_Cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(rmse, rsq),
  control = ctrl_grid
)
beepr::beep("fanfare")


model_st <- 
  stacks() %>%
  add_candidates(glmRes) %>%
  add_candidates(knnRes) %>%
  add_candidates(catRes) %>%
  # determine how to combine their predictions
  blend_predictions(
    metric =metric_set(rmse),# roc_auc rmse rsq mae 
    penalty = 10^(-6:-1),mixture = (0:5)/5) %>%
  # fit the candidates with nonzero stacking coefficients
  fit_members()
model_st

autoplot(model_st)
autoplot(model_st, type = "members")
autoplot(model_st, type = "weights")


modelData$pred<-predict(model_st,
        new_data = modelData,
        type = "numeric")$.pred

ggplot(modelData,aes(x = profit,y = pred))+
  geom_point()+
  geom_smooth()

predz<-predict(model_st,
                        new_data = holdData,
                        type = "numeric")

holdData$profit<-predz$.pred



ggplot(holdData,aes(x=profit))+
  geom_histogram()

write_csv(holdData%>%dplyr::select(id,profit),"S0E1R3submission5JPH.csv")

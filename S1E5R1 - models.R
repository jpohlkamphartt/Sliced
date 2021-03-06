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
library(extrafont)
# library(fastshap)
library(skimr)
library(beepr)
# library(tayloRswift)
library(treesnip)
library(stacks)
library(tictoc)
#### ####
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1) # I am number one - Nelly

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e05/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e05/test.csv",header=T,stringsAsFactors=F,sep=","))


rmsle_vec<-function (truth, estimate, na_rm = TRUE, ...) 
{
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth+1) - log(estimate+1))^2))
  }
  metric_vec_template(metric_impl = rmsle_impl, truth = truth, 
                      estimate = estimate, na_rm = na_rm, cls = "numeric")
}

rmsle<-function(data,...){
  UseMethod("rmsle")
}

rmsle<-new_numeric_metric(rmsle,direction= "minimize")

rmsle.data.frame<-function (data, truth, estimate, na_rm = TRUE, ...) 
{
  metric_summarizer(metric_nm = "rmsle", metric_fn = rmsle_vec, 
                    data = data, truth = !!enquo(truth), estimate = !!enquo(estimate), 
                    na_rm = na_rm)
}


rmsle(data.frame(x=c(2,1), y=c(1,1)), x,y)


skim(modelData)
modelData<-modelData%>%
  dplyr::select(
    -id,
    -host_name,
  )%>%
  mutate(
    last_review= ymd(last_review),
    room_type = as.factor(room_type),
    host_id = as.factor(host_id),
    neighbourhood_group = as.factor(neighbourhood_group),
    neighbourhood = as.factor(neighbourhood),
    reviews_per_month = min(reviews_per_month, 30),
    lastMonth = month(last_review),
    lastYear = year(last_review)
  )%>%
  dplyr::select(-last_review)

hostGender<-gender((modelData$host_name))

modelData<-left_join(modelData,hostGender%>%dplyr::select(name,proportion_male))

modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain, v = 5, repeats =1)

rec<-recipe(price ~., data = modelTrain)%>%
  step_tokenize(name)%>%
  step_stem(name)%>%
  step_stopwords(name)%>%
  step_tokenfilter(name, max_tokens = 20)%>%
  step_tfidf(name)%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(), threshold = .02)%>%
  step_unknown(all_nominal_predictors())%>%
  step_BoxCox(number_of_reviews,reviews_per_month, calculated_host_listings_count )%>%
  step_impute_median(reviews_per_month,lastMonth,lastYear,proportion_male)%>%
  step_ns(lastMonth, deg_free = 4)%>%
  step_zv(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(), threshold = .9)
  
ctrl_grid<-control_stack_grid() ## we stacking

catSpec<-boost_tree(trees = tune(), min_n = tune(),
                    tree_depth = tune(), sample_size = tune(),
                    learn_rate = tune(), mtry = tune())%>%
  set_engine("catboost")%>%
  set_mode("regression")

catGrid<-grid_max_entropy(
 trees(c(100L,1200L)),
 tree_depth(range = c(1L,10L)),
 min_n(range = c(2L, 25L)),
 sample_size = sample_prop(range = c(2/10,1)),
 learn_rate(range = c(-5,-1)),
 finalize(mtry(range = c(2L, unknown())),modelTrain),
  size = 80,
  variogram_range = .7
)

catWflow<-workflow()%>%
  add_recipe(rec)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(rmsle,rmse,rsq),
  control = ctrl_grid
)

modelV1<-select_best(catRes, "rmsle")
model_mod<-finalize_workflow(catWflow,modelV1)
model1<-fit(model_mod,modelTrain)

collect_metrics(catRes)

glmSpec<-linear_reg(mixture = tune(), penalty = tune())%>%
  set_engine("glmnet")%>%
  set_mode("regression")

glmGrid<-grid_max_entropy(
  mixture(),
  penalty(),
  size = 8,
  variogram_range = .7
)

glmWflow<-workflow()%>%
  add_recipe(rec)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(rmsle,rmse,rsq),
  control = ctrl_grid
)

knnSpec<-nearest_neighbor(neighbors = tune(), weight_func = tune())%>%
  set_engine("kknn")%>%
  set_mode("regression")

knnGrid<-grid_max_entropy(
  neighbors(range= c(4L,8L)),
  weight_func(c("optimal","rectangular")),
  size = 6,
  variogram_range = .7
)

knnWflow<-workflow()%>%
  add_recipe(rec)%>%
  add_model(knnSpec)

knnRes<-tune_grid(
  knnWflow,
  resamples = modelFoldz,
  grid = knnGrid,
  metrics = metric_set(rmsle,rmse,rsq),
  control = ctrl_grid
)
collect_metrics(glmRes)

model_st<-
  stacks()%>%
  add_candidates(glmRes)%>%
  add_candidates(catRes)%>%
  add_candidates(knnRes)%>%
  blend_predictions(
    metric = metric_set(rmsle),
    penalty = 10^(6:-1), mixture = (0:5)/5)%>%
  fit_members()
autoplot(model_st, type = "weights")
autoplot(model_st)
autoplot(model_st, type = "members")


    
beep("fanfare")
### holdout
holdData<-as_tibble(read.csv("Season 2/sliced-s01e05/test.csv",header=T,stringsAsFactors=F,sep=","))

holdData<-holdData%>%
  mutate(
    last_review= ymd(last_review),
    room_type = as.factor(room_type),
    host_id = as.factor(host_id),
    neighbourhood_group = as.factor(neighbourhood_group),
    neighbourhood = as.factor(neighbourhood),
    reviews_per_month = min(reviews_per_month, 30),
    lastMonth = month(last_review),
    lastYear = year(last_review)
  )%>%
  dplyr::select(-last_review)

hostGender<-gender((holdData$host_name))

holdData<-left_join(holdData,hostGender%>%dplyr::select(name,proportion_male))

table(table(holdData$id))

holdData<-distinct(holdData)

predz<-predict(model_st,
               new_data = holdData,
               type = "numeric"
)
holdData$price<-predz$.pred

hist(holdData$price)

write_csv(holdData%>%dplyr::select(id,price), "sub6.csv")

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
set.seed(1) # I am number one - Nelly

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e05/train.csv",header=T,stringsAsFactors=F,sep=","))

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

skim(modelData)

modelData<-modelData%>%
  dplyr::select(
    -id,
  )%>%
  mutate(
    last_review= coalesce(as.numeric(ymd(Sys.Date())- ymd(last_review)),0),
    room_type = as.factor(room_type),
    host_id = as.factor(host_id),
    neighbourhood_group = as.factor(neighbourhood_group),
    neighbourhood = as.factor(neighbourhood),
    reviews_per_month = coalesce(ifelse(reviews_per_month>30, 30,reviews_per_month),0)
  )

modelData$name<-trimws(tolower(modelData$name))

modelData$BR1<-grepl("1b/r",modelData$name)|grepl("1br",modelData$name)|grepl("1 br",modelData$name)|grepl("1 bed",modelData$name)|grepl("one bed",modelData$name)
modelData$BR2<-grepl("2b/r",modelData$name)|grepl("2br",modelData$name)|grepl("2 br",modelData$name)|grepl("2 bed",modelData$name)|grepl("two bed",modelData$name)
modelData$BR3<-grepl("3b/r",modelData$name)|grepl("3br",modelData$name)|grepl("3 br",modelData$name)|grepl("3 bed",modelData$name)|grepl("three bed",modelData$name)
modelData$BR4<-grepl("4b/r",modelData$name)|grepl("4br",modelData$name)|grepl("4 br",modelData$name)|grepl("4 bed",modelData$name)|grepl("four bed",modelData$name)
modelData$BR5<-grepl("[565789]b/r",modelData$name)|grepl("[565789]br",modelData$name)|grepl("[565789] br",modelData$name)|grepl("[565789] bed",modelData$name)|grepl("five bed",modelData$name)|grepl("six bedroom",modelData$name)|grepl("seven bedroom",modelData$name)
modelData$Studio<-grepl("studio",modelData$name)
hostGender<-
  bind_rows(gender::gender(unique(modelData$host_name),method = "ssa"),
            gender::gender(unique(modelData$host_name),method = "ipums"),
            gender::gender(unique(modelData$host_name),method = "napp"),
            gender::gender(unique(modelData$host_name),method = "kantrowitz")
  )%>%group_by(name)%>%
  summarise(
    proportion_male = mean(proportion_male,na.rm = T)
  )%>%
  ungroup()%>%
  mutate(proportion_male= coalesce(proportion_male,.5))

modelData<-left_join(modelData,hostGender%>%rename(host_name = name))

modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v = 5)

rec_gb<-recipe(
  price ~ BR1+BR2+BR3+BR4+BR5+Studio+
    neighbourhood_group+room_type+latitude+longitude+minimum_nights+number_of_reviews+last_review+reviews_per_month+calculated_host_listings_count+availability_365+proportion_male, data = modelTrain)%>%
  step_novel(all_nominal_predictors())

ctrl_grid<-control_stack_grid() ## we stacking

catSpec<-boost_tree(trees = tune(), 
                    learn_rate = tune(), mtry = tune())%>%
  set_engine("catboost")%>%
  set_mode("regression")

catGrid<-crossing(
  trees=seq(200,1500,50),
  learn_rate = c(.0075,.01),
  mtry = c(4,5,6)
)

catWflow<-workflow()%>%
  add_recipe(rec_gb)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(rmsle),
  control = ctrl_grid
)

catV1<-filter_parameters(catRes,parameters = select_best(catRes, "rmsle"))


collect_metrics(catRes)%>%arrange(mean,std_err)
autoplot(catRes)

rec_lm<-recipe(price ~., data = modelTrain)%>%
  step_tokenize(name)%>%
  step_stem(name)%>%
  step_stopwords(name)%>%
  step_tokenfilter(name, max_tokens = tune())%>%
  step_tfidf(name)%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(), threshold = tune())%>%
  step_unknown(all_nominal_predictors())%>%
  step_BoxCox(number_of_reviews,reviews_per_month, calculated_host_listings_count )%>%
  step_zv(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(), threshold = .9)

glmSpec<-linear_reg(mixture = tune(), penalty = tune())%>%
  set_engine("glmnet")%>%
  set_mode("regression")

glmGrid<-grid_max_entropy(
  mixture(),
  penalty(),
  max_tokens(range = c(100,300)),
  threshold(range = c(.01,.1)),
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
  metrics = metric_set(rmsle,rmse),
  control = ctrl_grid
)
beepr::beep("fanfare")

collect_metrics(glmRes)%>%filter(.metric =="rmse")%>%arrange(mean,std_err)
autoplot(glmRes)

glmV1<-filter_parameters(glmRes,parameters = select_best(glmRes, "rmse"))

rec_knn<-recipe(price ~., data = modelTrain)%>%
  step_normalize(all_numeric_predictors())%>%
  step_tokenize(name)%>%
  step_stem(name)%>%
  step_stopwords(name)%>%
  step_tokenfilter(name, max_tokens = tune())%>%
  step_tfidf(name)%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(), threshold = tune())%>%
  step_unknown(all_nominal_predictors())%>%
  # step_BoxCox(number_of_reviews,reviews_per_month, calculated_host_listings_count )%>%
  step_zv(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())

knnSpec<-nearest_neighbor(neighbors = tune(), weight_func = tune())%>%
  set_engine("kknn")%>%
  set_mode("regression")

knnGrid<-grid_max_entropy(
  neighbors(range= c(4L,8L)),
  weight_func(c("optimal","rectangular")),
  max_tokens(range = c(10,30)),
  threshold(range = c(.01,.1)),
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
  metrics = metric_set(rmsle,rmse),
  control = ctrl_grid
)

beepr::beep("fanfare")

collect_metrics(knnRes)%>%filter(.metric =="rmsle")%>%arrange(mean,std_err)
autoplot(knnRes)

knnV1<-filter_parameters(knnRes,parameters = select_best(knnRes, "rmsle"))

model_st<-
  stacks()%>%
  add_candidates(catRes)%>%
  add_candidates(glmRes)%>%
  add_candidates(knnRes)%>%
  blend_predictions(
    metric = metric_set(rmsle),
    penalty = 10^(-6:-1), mixture = (0:5)/5)%>%
  fit_members()
autoplot(model_st, type = "weights")
autoplot(model_st)
autoplot(model_st, type = "members")

#### hold out
holdData<-as_tibble(read.csv("Season 2/sliced-s01e05/test.csv",header=T,stringsAsFactors=F,sep=","))

holdData<-holdData%>%
  mutate(
    last_review= coalesce(as.numeric(ymd(Sys.Date())- ymd(last_review)),0),
    room_type = as.factor(room_type),
    host_id = as.factor(host_id),
    neighbourhood_group = as.factor(neighbourhood_group),
    neighbourhood = as.factor(neighbourhood),
    reviews_per_month = coalesce(ifelse(reviews_per_month>30, 30,reviews_per_month),0)
  )

holdData$name<-trimws(tolower(holdData$name))

holdData$BR1<-grepl("1b/r",holdData$name)|grepl("1br",holdData$name)|grepl("1 br",holdData$name)|grepl("1 bed",holdData$name)|grepl("one bed",holdData$name)
holdData$BR2<-grepl("2b/r",holdData$name)|grepl("2br",holdData$name)|grepl("2 br",holdData$name)|grepl("2 bed",holdData$name)|grepl("two bed",holdData$name)
holdData$BR3<-grepl("3b/r",holdData$name)|grepl("3br",holdData$name)|grepl("3 br",holdData$name)|grepl("3 bed",holdData$name)|grepl("three bed",holdData$name)
holdData$BR4<-grepl("4b/r",holdData$name)|grepl("4br",holdData$name)|grepl("4 br",holdData$name)|grepl("4 bed",holdData$name)|grepl("four bed",holdData$name)
holdData$BR5<-grepl("[565789]b/r",holdData$name)|grepl("[565789]br",holdData$name)|grepl("[565789] br",holdData$name)|grepl("[565789] bed",holdData$name)|grepl("five bed",holdData$name)|grepl("six bedroom",holdData$name)|grepl("seven bedroom",holdData$name)
holdData$Studio<-grepl("studio",holdData$name)
hostGender<-
  bind_rows(gender::gender(unique(holdData$host_name),method = "ssa"),
            gender::gender(unique(holdData$host_name),method = "ipums"),
            gender::gender(unique(holdData$host_name),method = "napp"),
            gender::gender(unique(holdData$host_name),method = "kantrowitz")
  )%>%group_by(name)%>%
  summarise(
    proportion_male = mean(proportion_male,na.rm = T)
  )%>%
  ungroup()%>%
  mutate(proportion_male= coalesce(proportion_male,.5))

holdData<-left_join(holdData,hostGender%>%rename(host_name = name))

holdData<-distinct(holdData)

predz<-predict(model_st,
               new_data = holdData,
               type = "numeric"
)
holdData$price<-predz$.pred

hist(holdData$price)

write_csv(holdData%>%dplyr::select(id,price), "sub8.csv")



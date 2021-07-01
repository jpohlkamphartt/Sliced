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
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)

options(scipen=999)
commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}
source("SlicedTheme.R")

#### Merge all data sets ####
modelData<-as.data.frame(read.csv("Season1/s00e02/Copy of sliced_data.csv",header = T,stringsAsFactors = F,sep=","))
modelData_brewz<-as.data.frame(read.csv("Season1/s00e02/Copy of brewery_and_beer_info.csv",header = T,stringsAsFactors = F,sep=","))
modelData_peepz<-as.data.frame(read.csv("Season1/s00e02/Copy of reviewer_info.csv",header = T,stringsAsFactors = F,sep=","))

modelData<-modelData%>%
  left_join(modelData_brewz,by=c("brewery_id","beer_category"))#%>%
  # left_join(modelData_peepz,by=c("review_profilename","beer_category"))

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
skimr::skim(modelData)

modelData_prep<-modelData%>%
  dplyr::select(
    -brewery_id,
    -beer_beerid,
    -beer_name
  )%>%
  as_tibble()%>%
  # mutate_if(is.character,naFixer)%>%
  mutate_if(is.character,as.factor)

skimr::skim(modelData_prep)

#### run multi-model test
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
  step_impute_median(all_numeric_predictors())#%>%
  # step_sample(size=.5)
class(bsRec)<-c("base",class(bsRec))

bsRec%>%prep()%>%juice()

yjRec<-recipe(review_overall~.,data=modelData_train)%>%
  step_corr(all_numeric_predictors())%>%
  step_other(all_nominal_predictors(),threshold = 0.05)%>%
  step_novel(all_nominal_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_zv(all_numeric_predictors())%>%
  # step_normalize(all_numeric_predictors())%>%
  step_YeoJohnson(all_numeric_predictors())%>%
  step_impute_median(all_numeric_predictors())#%>%
  # step_sample(size=.5)
class(yjRec)<-c("YeoJo",class(yjRec))

bsRec_max<-recipe(review_overall~.,data=modelData_train)%>%
  step_corr(all_numeric_predictors(),threshold = .75)%>%
  step_other(all_nominal_predictors(),threshold = 0.05)%>%
  step_novel(all_nominal_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_zv(all_numeric_predictors())%>%
  step_normalize(all_numeric_predictors())%>%
  # step_YeoJohnson(all_numeric_predictors())%>%
  step_impute_median(all_numeric_predictors())%>%
  step_interact(terms = ~ brewery_beer_category_review_overall_25p:starts_with("brewery_beer_category_review"))%>%
  step_interact(terms = ~ brewery_review_overall_mean:brewery_beer_category_review_overall_25p)
  # step_sample(size=.5)
class(bsRec_max)<-c("Max",class(bsRec_max))

pcaRec<-bsRec%>%
  step_pca(all_predictors(),num_comp = tune())
class(pcaRec)<-c("pca",class(pcaRec))

# plsRec<-bsRec%>%
#   step_pls(all_predictors(),outcome = vars(review_overall),num_comp = tune())

preprocessors<-list(
  base=bsRec,
  yj=yjRec,
  pca=pcaRec,
  # pls=plsRec
  max=bsRec_max
  )

glm_spec<-linear_reg(penalty = tune(),mixture = tune())%>%set_engine("glmnet")
knn_spec<-nearest_neighbor(neighbors = tune(),weight_func = tune())%>%set_engine("kknn")%>%set_mode("regression")
xg_spec<-boost_tree(min_n = tune(),tree_depth = tune(),learn_rate = tune())%>%set_engine("xgboost" )%>%set_mode("regression")
prior.dist = rstanarm::student_t(df = 1)
bayes_spec = linear_reg() %>%
  set_engine(engine = "stan", 
             prior_intercept = prior.dist, 
             prior = prior.dist) %>%
  set_mode(mode = "regression")
mars_spec <- mars() %>% set_engine("earth") %>% set_mode("regression")

models<-list(
 glm=glm_spec,
 # knn=knn_spec,
 # xgb=xg_spec,
 # bayes_spec,
 mars_spec
)

wflow_set<-workflow_set(preprocessors,models, cross=T)%>%
  workflow_map(
    resamples = modelData_folds,
    grid = 20,
    metrics = metric_set(rmse,rsq),
    seed = 1,
    verbose=T
  )
    

wflow_set<-wflow_set[lapply(wflow_set$result,class)!="try-error",] # remove the runs with errors, probably should just fix your errors...
# wflow_set<-wflow_set[sapply(1:nrow(wflow_set),function(i){sum(unlist(lapply(wflow_set$result[[i]]$.notes,nrow)))==0}),]

# which is best?
rank_results(wflow_set) %>%
  # select(-.metric, -std_err, -n) %>% 
  relocate(rank, mean)

# plot them to see which looks best on the 2 metrics
autoplot(wflow_set)

# Baysian test for real differences in models through resampling
roc_mod <-
  perf_mod(
    wflow_set,
    # Model different variability per workflow:
    metric = "rmse", # alt: "rsq", binary regression: "roc_auc" "mn_log_loss"
    hetero_var = TRUE,
    # Options to `rstanarm::stan_glm()`
    seed = 1,
    iter = 1000,
    refresh = 0,
    chains = 5,
    adapt_delta = 0.8, # make this bigger to avoid divergent simulations, will be slower, default is .8
    cores = all_cores-2
  )

# plot histograms of the model scores to see if they really are different
roc_mod%>% tidy(1)%>%
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50, col = "white", fill = "blue", alpha = 0.4) + 
  facet_wrap(~ model, ncol = 1) + 
  labs(x = expression(paste("Posterior for mean ", RMSE)))

# first selected model
finalRec<-bsRec
final_spec<-glm_spec

modelGrid<-grid_latin_hypercube(
 penalty(),mixture(),size=50 
)

finalModel_wf<-workflow()%>%add_recipe(finalRec)%>%add_model(final_spec)

# clean environment before running
finalModel_res<-tune_grid(
  finalModel_wf,
  resamples = modelData_folds,
  grid = modelGrid,
  metrics = metric_set(rmse, rsq)
  )
  
finalModel_best<-select_best(finalModel_res,metric = "rmse")
finalModel_mod<-finalize_workflow(finalModel_wf,finalModel_best)

finalModel_fit<-fit(finalModel_mod,modelData_train)



#### write holdout code
holdoutData<-as.data.frame(read.csv("Season1/s00e02/Copy of sliced_holdout_data.csv",header = T,stringsAsFactors = F,sep=","))
holdoutData_brewz<-as.data.frame(read.csv("Season1/s00e02/Copy of brewery_and_beer_info.csv",header = T,stringsAsFactors = F,sep=","))
holdoutData_peepz<-as.data.frame(read.csv("Season1/s00e02/Copy of reviewer_info.csv",header = T,stringsAsFactors = F,sep=","))

holdoutData<-holdoutData%>%
  left_join(holdoutData_brewz,by=c("brewery_id","beer_category"))%>%
  left_join(holdoutData_peepz,by=c("review_profilename","beer_category"))

#### toss out all variables missing from hold out
holdoutData<-holdoutData%>%
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
holdoutData_prep<-holdoutData%>%
  dplyr::select(
    -brewery_id,
    -beer_beerid,
    -beer_name
  )%>%
  as_tibble()%>%
  # mutate_if(is.character,naFixer)%>%
  mutate_if(is.character,as.factor)

holdoutData_bake<-finalRec%>%
  prep()%>%
  bake(new_data=holdoutData_prep)
holdoutData_bake$predictedRating<-predict(finalModel_fit,new_data =holdoutData)$.pred

#### data viz for EDA/feature eng

#### select best model type, add in new features, get first predictions

#### run quick model of best type

#### examine model for trimming variables, get 2nd predictions

#### run max grid search model

#### data viz some peripheral shit

#### get final predictions
holdoutData<-as.data.frame(read.csv("Season1/s00e02/Copy of sliced_holdout_data.csv",header = T,stringsAsFactors = F,sep=","))
holdoutData_brewz<-as.data.frame(read.csv("Season1/s00e02/Copy of brewery_and_beer_info.csv",header = T,stringsAsFactors = F,sep=","))
holdoutData_peepz<-as.data.frame(read.csv("Season1/s00e02/Copy of reviewer_info.csv",header = T,stringsAsFactors = F,sep=","))

holdoutData<-holdoutData%>%
  left_join(holdoutData_brewz,by=c("brewery_id","beer_category"))%>%
  left_join(holdoutData_peepz,by=c("review_profilename","beer_category"))

#### toss out all variables missing from hold out
holdoutData<-holdoutData%>%
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
holdoutData_prep<-holdoutData%>%
  dplyr::select(
    -brewery_id,
    -beer_beerid,
    -beer_name
  )%>%
  as_tibble()%>%
  # mutate_if(is.character,naFixer)%>%
  mutate_if(is.character,as.factor)

holdoutData_bake<-finalRec%>%
  prep()%>%
  bake(new_data=holdoutData_prep)
holdoutData_bake
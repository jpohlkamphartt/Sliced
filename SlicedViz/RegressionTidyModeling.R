#### Libraries 
library(tidyverse) #baseline tidy functions
library(tidymodels) #gives us the tidymodels framework but not the underlying modeling packages
library(recipes) #probably don't need this since we have the tidymodels above
library(themis) #used for unbalanced categorical data, SMOTE is the main algorithm we use
library(vip) #variable importance plots from random forests and boosted trees
library(ggplot2) #plot stuff
library(furrr) #parellelization of code, new package for tidy style mapping functions but has some issues of performance TBH
library(here) #directory manager, this is useful for reproducible code
library(lubridate) #all things dates can be fixed up by this, look up their cheatsheet for the details
library(mgcv) #to run GAM or BAM models, I use these when I want splines/tensors/mixed effects models
library(parallel) #parellel programing utility package
library(workflowsets) #tidymodel package that gives us the ability to test multiple models/preprocessing recipies
library(tidyposterior) #uses baysian posterior estimation to evaluate our models
library(discrim) #discriminant modeling
library(hablar) #has some nice utilities like auto-retypying retype() and na_if functions.
library(doParallel) #more parellel
library(skimr) #EDA utility package, use skim()

#### Setup
here() #what directory are we in
usethis::edit_r_environ() #I hack my environment to boost my RAM on a Mac, just comment out
set.seed(1) #set seed for reproducibility. #1 all day, nothing but the best.
# set up cluster for parellel code
all_cores <- parallel::detectCores()
cl <- parallel::makeCluster(all_cores-1, setup_strategy = "sequential")
registerDoParallel(cl)
options(scipen=999) # fix significant figures to display reasonably
# custom mode function because R don't have one
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(fitEvents(v, uniqv)))]
}
# clean up data with these functions
commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}

#### EDA
modelData<-as.data.frame(read.csv("GamesDataV2.csv",header=T,stringsAsFactors=F,sep=","))
modelData_hldsplit <- initial_split(modelData,.95)
modelData <- training(modelData_hldsplit)

skimr::skim(modelData) # look at the distribution of the variables

# can add some ggplots of histograms or other density distributions for more detailed info

# quick data cleanup
modelData_prep<-modelData%>%
  dplyr::select( # remove columns that is not going to be helpful
    -X,-X.1,-season,-slug,-game_uid,-game_date,-date,-team_slug
  )%>%
  # mutate_if(is.character,commaRemove)%>% # if you have issues with commas in character columns of numerical data, this is useful
  # mutate_if(is.character,naFixer)%>% # if you have issues with blank cells "" in character columns of numerical data, this is useful
  mutate_at(vars(position,role,HAW),as.factor)%>% # turn your character columns that should be factors into factors
  # retype()%>% # fix the rest of your data columns (might break the above factoring)
group_by(position,role)%>% #group by similar players
  mutate(fitEvents = as.numeric(scale(scoringChanceGeneratingPlays)+scale(LPRs)+scale(PDPs)+scale(defensivePlays)))%>% # create outcome to model
ungroup()%>% #ungroup data
  dplyr::select( # remove other event data
    -toi,-goals,-assists,-points,-iXG,-iXA,-iXP,-XPGF,-XPGA,-XPGPercentage,-LPRs,-contestedLPRs,-PDPs,-scoringChanceGeneratingPlays,-defensivePlays,-possessionTime,-controlledEntries,-controlledExits,-possMin,-pos.TOI
  )%>%mutate(fitEvents50 = fitEvents>median(fitEvents)) #create a binary response to see if there is any relatition with higher events in correllation plots coloring below
# correlation mapping, look for potential trends
corMat<-cor(modelData_prep%>%mutate(fitEvents=as.numeric(fitEvents))%>%dplyr::select_if(is.numeric),use = "pairwise.complete.obs")%>%as.data.frame()%>%rownames_to_column()%>%arrange(desc(fitEvents))
modelData_prep%>%
  GGally::ggscatmat(columns = which(colnames(modelData_prep)%in%corMat$rowname[c(1,3:9)]),color= "fitEvents50", alpha=.1, corMethod = "spearman")+
  tayloRswift::scale_color_taylor(palette = "lover") # TSwift palettes all day

# 
modelData_prep<-modelData_prep%>%
  dplyr::select(-fitEvents50)

modelData_split <- initial_split(modelData_prep)
modelData_train <- training(modelData_split)
modelData_test <- testing(modelData_split)

modelData_folds <- vfold_cv(modelData_train, repeats = 2)

bsRec<-recipe(fitEvents~., data =modelData_train)%>%
  step_normalize(all_numeric(),-all_outcomes())%>% # scales all variables
  step_YeoJohnson(all_numeric())%>% # transforms all numerical variables to fit a normal distribution
  step_impute_median(all_numeric())%>% # imputes the data with a median value
  step_novel(all_nominal(),-all_outcomes())%>% # creates a level for factors when new levels exist in non-trained data
  step_dummy(all_nominal(),-all_outcomes())%>% #turns all factors to columns of 0/1
  step_zv(all_predictors(),-all_outcomes()) # removes variables with no variance
# step_smote(fitEvents) # imputes the data for class imbalance

# performs a PCA on the input data
pcaRec<-bsRec%>% 
  step_pca(all_predictors(), num_comp  = 2)

# performs a partial least squares transformation on the input data
plsRec <- 
  bsRec %>% 
  step_pls(all_predictors(), outcome = vars(fitEvents), num_comp = 2)

# penalized linear model spec
glm_spec <- linear_reg(penalty = tune(),mixture = tune()) %>% set_engine("glmnet")
# nearest neighbor model spec
knn_spec <- nearest_neighbor(
  neighbors = tune(), weight_func = tune()
) %>% set_engine("kknn") %>% set_mode("regression")
# decision tree spec
cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")
# random forest spec
rf_spec <- rand_forest(
  # mtry = tune(), 
  trees = tune(), min_n = tune()
) %>% set_engine("ranger") %>% set_mode("regression")
# boosted tree spec
xg_spec <-  boost_tree(
  trees = tune(), 
  min_n = tune(),
  tree_depth = tune(),
  # mtry = tune() 
) %>% 
  set_engine("xgboost",# objective = "multi:softprob",
             lambda=0, alpha=1,
             #num_class=length(levels(ModelData$Outcome)),
             verbose=1) %>% 
  set_mode("regression")
# select our preprocessing
preprocessors <-
  list(
    base = bsRec,
    pca = pcaRec,
    pls = plsRec
    
  )
# select our models
models <- 
  list(
    glm = glm_spec,
    knn = knn_spec,
    xg = xg_spec#,
    # cart = cart_spec,
    # rf = rf_spec
  )
# put them together
wflow_set <- workflow_set(preprocessors, models, cross = TRUE)

# build all the models
wflow_set <-
  wflow_set %>%
  workflow_map(
    # Options to `tune_grid()`
    resamples =modelData_folds,
    grid =10, # number of different values for tune() to try - small is fast, big will be more representitive of the final model you build
    metrics = metric_set(rmse, rsq), # metrics to use binary classification use: mn_log_loss,roc_auc
    # Options to `workflow_map()`
    seed = 1,
    verbose = TRUE
  )

wflow_set<-wflow_set[lapply(wflow_set$result,class)!="try-error",] # remove the runs with errors, probably should just fix your errors...
wflow_set<-wflow_set[sapply(1:nrow(wflow_set),function(i){sum(unlist(lapply(wflow_set$result[[i]]$.notes,nrow)))==0}),]

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
    iter = 2500,
    refresh = 0,
    chains = 5,
    adapt_delta = 0.99 # make this bigger to avoid divergent simulations, will be slower, default is .8
  )

# plot histograms of the model scores to see if they really are different
roc_mod%>% tidy(1)%>%
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50, col = "white", fill = "blue", alpha = 0.4) + 
  facet_wrap(~ model, ncol = 1) + 
  labs(x = expression(paste("Posterior for mean ", RMSE)))

# are they actually different?
autoplot(roc_mod)

# test to know if there is a real difference when we only care about a meaningful change in the predicted values over the size. good to check what the range is of the predicted variable to determine a value for size
autoplot(roc_mod, size = .1, type = "ROPE")

#### final model 
# choose preprocessing
modelData_train<-bsRec%>%
  prep()%>%
  bake(new_data = NULL)

#### Model Training 
finalModel_split <- initial_split(modelData_train)
finalModel_train <- training(finalModel_split )
finalModel_test <- testing(finalModel_split )

#### if xgboost is best
# finalModel_grid <- grid_latin_hypercube(
#   trees(),
#   tree_depth(),
#   min_n(),
#   loss_reduction(),
#   sample_size = sample_prop(),
#   finalize(mtry(), finalModel_train),
#   learn_rate(),
#   size = 50
# )
# #### Model specifications
# mod_spec <- boost_tree(
#   trees = tune(), 
#   tree_depth = tune(), min_n = tune(), 
#   loss_reduction = tune(),                     ## first three: model complexity
#   sample_size = tune(), mtry = tune(),         ## randomness
#   learn_rate = tune(),                         ## step size
# ) %>% 
#   set_engine("xgboost",
#              lambda=0, alpha=1,verbose=1) %>% 
#   set_mode("regression" )

#### If glmnet is best
finalModel_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 50 # number of models to try
)
mod_spec <- linear_reg(penalty = tune(),mixture = tune()) %>% set_engine("glmnet")

# set up the tuning
finalModel_mod_wf <- workflow() %>%
  add_formula(fitEvents~.) %>%
  add_model(mod_spec)
finalModel_folds <- vfold_cv(finalModel_train,v=5) # can stratify by a column (like position) with: strata = position

# tune a bunch of models
finalModel_mod_res <- tune_grid(
  finalModel_mod_wf,
  resamples = finalModel_folds,
  grid = finalModel_grid,
  metrics =  metric_set(rmse, rsq), #mn_log_loss,roc_auc
  control = control_grid(save_pred = TRUE)
)

# pick best of all tuned models
finalModel_best_metric  <- select_best(finalModel_mod_res, metric ="rmse")

finalModel_final_mod <- finalize_workflow(
  finalModel_mod_wf,
  finalModel_best_metric
)

finalModel_final<-fit(finalModel_final_mod, finalModel_train)

# check variable importance
finalModel_final %>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features =25)

# check the model performance
finalModel_final_res <- last_fit(finalModel_final_mod, finalModel_split,metrics = metric_set(rmse, rsq))
collect_metrics(finalModel_final_res)

# # roc curve
# finalModel_final_res %>% 
#   collect_predictions(parameters =finalModel_best_metric) %>% 
#   roc_curve(fitEvents, .pred_0) %>% 
#   autoplot()

# check prediction link with response
finalModel_final_res %>%
  collect_predictions(parameters =finalModel_best_metric) %>%
  ggplot(aes(x= fitEvents,y = .pred))+
  geom_point()+
  geom_smooth(method ="lm")


# check prediction link with residual
finalModel_final_res %>%
  collect_predictions(parameters =finalModel_best_metric) %>%
  ggplot(aes(x= fitEvents- .pred,y = .pred))+
  geom_point()+
  geom_smooth(method ="lm")

finalModel_final_res %>%
  collect_predictions(parameters =finalModel_best_metric)%>%
  ggplot(aes(sample=fitEvents- .pred))+stat_qq()
# # Shap (for xgboost)
# res<-predict(xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit),newdata =     finalModel_train%>%dplyr::select((xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit))$feature_names)%>%as.matrix(), predcontrib = TRUE)
# res <- tibble::as_tibble(res)
# attr(res, which = "baseline") <- res[["BIAS"]]
# res[["BIAS"]] <- NULL
# class(res) <- c(class(res), "explain")
# 
# autoplot(res)
# 
# autoplot(res, 
#          type = "dependence", 
#          feature = "like", 
#          X = finalModel_train%>%dplyr::select((xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit))$feature_names),
#          smooth = TRUE, 
#          color_by = "like_o")
# 
# autoplot(res, 
#          type = "dependence", 
#          feature = "attr", 
#          X = finalModel_train%>%dplyr::select((xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit))$feature_names),
#          smooth = TRUE, 
#          color_by = "attr_o")


### hold out ###
# holdData<-as.data.frame(read.csv("holdout.csv",header=T,stringsAsFactors=F,sep=","))
holdData <- testing(modelData_hldsplit)

holdData_prep<-holdData%>%
  dplyr::select( # remove columns that is not going to be helpful
    -X,-X.1,-season,-slug,-game_uid,-game_date,-date,-team_slug
  )%>%
  # mutate_if(is.character,commaRemove)%>% # if you have issues with commas in character columns of numerical data, this is useful
  # mutate_if(is.character,naFixer)%>% # if you have issues with blank cells "" in character columns of numerical data, this is useful
  mutate_at(vars(position,role,HAW),as.factor)%>% # turn your character columns that should be factors into factors
  # retype()%>% # fix the rest of your data columns (might break the above factoring)
  group_by(position,role)%>% #group by similar players
  mutate(fitEvents = as.numeric(scale(scoringChanceGeneratingPlays)+scale(LPRs)+scale(PDPs)+scale(defensivePlays)))%>% # create outcome to model
  ungroup()%>% #ungroup data
  dplyr::select( # remove other event data
    -toi,-goals,-assists,-points,-iXG,-iXA,-iXP,-XPGF,-XPGA,-XPGPercentage,-LPRs,-contestedLPRs,-PDPs,-scoringChanceGeneratingPlays,-defensivePlays,-possessionTime,-controlledEntries,-controlledExits,-possMin,-pos.TOI
  )

holdData_mod<-bsRec%>%
  prep()%>%
  bake(new_data = holdData_prep)

holdPredz<-predict(finalModel_final,new_data = holdData_mod)

holdPredz%>%
  ggplot(aes(x=.pred))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.75) +
  ggtitle("Distibution of Scaled Predicted Event Totals") 

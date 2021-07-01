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

here()
usethis::edit_r_environ()
set.seed(1)
all_cores <- parallel::detectCores()
cl <- parallel::makeCluster(all_cores-1, setup_strategy = "sequential")
registerDoParallel(cl)
options(scipen=999)

source("SlicedTheme.R")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}

modelData<-as.data.frame(read.csv("Season1/s00e01/Copy of sliced-s00e01-data.csv",header=T,stringsAsFactors=F,sep=","))

skimr::skim(modelData)

modelData_prep<-modelData%>%
  dplyr::select(
    -X,-id,-idg,-dec,-dec_o,-partner,-field,-from,-career,-undergra
  )%>%
  mutate_if(is.character,commaRemove)%>%
  # mutate_if(is.character,naFixer)%>%
  mutate_at(vars(contains("_cd"),goal,date,race,career_c,match),as.factor)%>%
  retype()

corMat<-cor(modelData_prep%>%mutate(match=as.numeric(match)),use = "pairwise.complete.obs")%>%as.data.frame()%>%rownames_to_column()%>%arrange(desc(match))


modelData_prep%>%
  GGally::ggscatmat(columns = which(colnames(modelData_prep)%in%corMat$rowname[2:10]),color="match", alpha=.1, corMethod = "spearman")+
  tayloRswift::scale_color_taylor(palette = "lover")


table(modelData_prep$zipcode)

modelData_prep<-modelData_prep%>%
  mutate(match = as.factor(match))
  
modelData_split <- initial_split(modelData_prep)
modelData_train <- training(modelData_split)
modelData_test <- testing(modelData_split)

modelData_folds <- vfold_cv(modelData_train, repeats = 2)
  
bsRec<-recipe(match~., data =modelData_train)%>%
  step_zv(all_predictors(),-all_outcomes())%>%
  step_normalize(all_numeric(),-all_outcomes())%>%
  step_YeoJohnson(all_numeric(),-iid, -pid,-age,-age_o,)%>%
  step_impute_median(all_numeric())%>%
  step_novel(all_nominal(),-all_outcomes())%>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  step_smote(match)

pcaRec<-bsRec%>% 
  step_pca(all_predictors(), num_comp = tune())

plsRec <- 
  bsRec %>% 
  step_pls(all_predictors(), outcome = vars(match), num_comp = tune())

glm_spec <- logistic_reg() %>% set_engine("glm")
knn_spec <- nearest_neighbor(
  neighbors = tune(), weight_func = tune()
) %>% set_engine("kknn") %>% set_mode("classification")
cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

mars_disc_spec <- 
  discrim_flexible(prod_degree = tune()) %>% 
  set_engine("earth")

reg_disc_spec <- 
  discrim_regularized(frac_common_cov = tune(), frac_identity = tune()) %>% 
  set_engine("klaR")

rf_spec <- rand_forest(
  # mtry = tune(), 
  trees = tune(), min_n = tune()
) %>% set_engine("ranger") %>% set_mode("classification")

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
  set_mode("classification")

preprocessors <-
  list(
    base = bsRec,
    pca = pcaRec,
    pls = plsRec
    
  )

models <- 
  list(
    glm = glm_spec,
    # knn = knn_spec,
    xg = xg_spec,
    # cart = cart_spec,
    mars = mars_disc_spec
    # reg  = reg_disc_spec,
    # rf = rf_spec
    )

wflow_set <- workflow_set(preprocessors, models, cross = TRUE)

tic()
wflow_set <-
  wflow_set %>%
  workflow_map(
    # Options to `tune_grid()`
    resamples =modelData_folds,
    grid =5,
    metrics = metric_set(mn_log_loss,roc_auc),
    # Options to `workflow_map()`
    seed = 1,
    verbose = TRUE
  )
toc()
wflow_set<-wflow_set[lapply(wflow_set$result,class)!="try-error",]
rank_results(wflow_set) %>%
  # select(-.metric, -std_err, -n) %>% 
  relocate(rank, mean)

autoplot(wflow_set)


roc_mod <-
  perf_mod(
    wflow_set,
    # Model different variability per workflow:
    metric = "roc_auc", # "mn_log_loss"
    hetero_var = TRUE,
    # Options to `rstanarm::stan_glm()`
    seed = 1,
    iter = 5000,
    refresh = 0,
    chains = 5,
    adapt_delta = 0.99
  )

roc_mod%>% tidy(1)%>%
  mutate(model = forcats::fct_inorder(model)) %>%
  ggplot(aes(x = posterior)) + 
  geom_histogram(bins = 50, col = "white", fill = "blue", alpha = 0.4) + 
  facet_wrap(~ model, ncol = 1) + 
  labs(x = expression(paste("Posterior for mean ", ROC)))

autoplot(roc_mod)

autoplot(roc_mod, size = 0.01, type = "ROPE")

####
modelData_train_xgb<-bsRec%>%
  prep()%>%
  bake(new_data = NULL)

#### Model specifications
xgb_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost",
             lambda=0, alpha=1,verbose=1) %>% 
  set_mode("classification" )

#### Model Training 
finalModel_split <- initial_split(modelData_train_xgb)
finalModel_train <- training(finalModel_split )
finalModel_test <- testing(finalModel_split )
finalModel_xgb_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), finalModel_train),
  learn_rate(),
  size = 50
)
finalModel_xgb_wf <- workflow() %>%
  add_formula(match~.) %>%
  add_model(xgb_spec)
finalModel_folds <- vfold_cv(finalModel_train,v=5)#, strata = league.slug)

tic()
finalModel_xgb_res <- tune_grid(
  finalModel_xgb_wf,
  resamples = finalModel_folds,
  grid = finalModel_xgb_grid,
  metrics =  metric_set(mn_log_loss,roc_auc),
  control = control_grid(save_pred = TRUE)
)
toc()

finalModel_best_metric  <- select_best(finalModel_xgb_res, metric ="roc_auc")

finalModel_final_xgb <- finalize_workflow(
  finalModel_xgb_wf,
  finalModel_best_metric
)

finalModel_final<-fit(finalModel_final_xgb, finalModel_train)

finalModel_final %>%
  pull_workflow_fit() %>%
  vip(geom = "point",num_features =25)

finalModel_final_res <- last_fit(finalModel_final_xgb, finalModel_split,metrics = metric_set(mn_log_loss,roc_auc))
collect_metrics(finalModel_final_res)

finalModel_final_res %>% 
  collect_predictions(parameters =finalModel_best_metric) %>% 
  roc_curve(match, .pred_0) %>% 
  autoplot()

# Shap
res<-predict(xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit),newdata =     finalModel_train%>%dplyr::select((xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit))$feature_names)%>%as.matrix(), predcontrib = TRUE)
res <- tibble::as_tibble(res)
attr(res, which = "baseline") <- res[["BIAS"]]
res[["BIAS"]] <- NULL
class(res) <- c(class(res), "explain")

autoplot(res)

autoplot(res, 
         type = "dependence", 
         feature = "like", 
         X = finalModel_train%>%dplyr::select((xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit))$feature_names),
         smooth = TRUE, 
         color_by = "like_o")

autoplot(res, 
         type = "dependence", 
         feature = "attr", 
         X = finalModel_train%>%dplyr::select((xgb.Booster.complete(pull_workflow_fit(finalModel_final)$fit))$feature_names),
         smooth = TRUE, 
         color_by = "attr_o")
### hold out ###
holdData<-as.data.frame(read.csv("Season1/s00e01/Copy of sliced-s00e01-holdout.csv",header=T,stringsAsFactors=F,sep=","))

holdData_prep<-holdData%>%
  dplyr::select(
    -X,-id,-idg,-partner,-field,-from,-career,-undergra
  )%>%
  mutate_if(is.character,commaRemove)%>%
  mutate_at(vars(contains("_cd"),goal,date,race,career_c),as.factor)%>%
  retype()%>%
  mutate(mn_sat = ifelse(mn_sat=="",NA, mn_sat),
         tuition = ifelse(tuition=="",NA, tuition))%>%
  retype()

holdData_xgb<-bsRec%>%
  prep()%>%
  bake(new_data = holdData_prep)

holdPredz<-predict(finalModel_final,new_data = holdData_xgb)

table(holdPredz)

holdPredz%>%group_by(.pred_class)%>%
  summarise(prop = 100*round(n()/nrow(holdPredz),3),
            )%>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)%>%
ggplot(aes(x = "", y = prop, fill = .pred_class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = Sliced_Qualitative_Palette) +
  theme_void()+
  ggtitle("Proportion of Predicted Matches")

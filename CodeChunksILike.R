#### Code Chunks I like
# setup
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
here()
usethis::edit_r_environ()
set.seed(82)
doParallel::registerDoParallel()
no_cores <- detectCores() - 1
options(scipen=999)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


source("SlicedTheme.R")

# Check engines
library(tidymodels)
show_engines("linear_reg") # glmnet
show_engines("logistic_reg") # glmnet
show_engines("mars") # earth
show_engines("multinom_reg") # glmnet or nnet
show_engines("nearest_neighbor") # kknn
show_engines("poisson_reg") # glmnet
show_engines("rand_forest") # ranger

show_engines("surv_reg") # flexsurv or survival
show_engines("boost_tree") # xgboost
show_engines("arima_reg") # arima or auto_arima
show_engines("arima_boost") # arima_xgboost or auto_arima_xgboos

#sexy correlation plot
crtr.wide %>%
  select(-salinity)%>%
  mutate(season = lubridate::month(date) %>% as.integer(),
         season = replace(season,season %in% c(1:4,11:12), "NE"),
         season = replace(season,season %in% c(5:10), "SE"))%>%
  GGally::ggscatmat(columns = 4:10,color="season", alpha=1, corMethod = "spearman")+
  ggsci::scale_color_jco()+
  ggpubr::theme_pubclean()+
  theme(strip.background = element_blank(), 
        legend.position = "right",
        legend.key = element_blank())

# pairwise trend plots
require(wesanderson)

wesa = wes_palettes %>% names()

crtr.wide %>%
  select(-salinity)%>%
  filter(nitrate < 1 & phosphate < 1.2 & chl < 1) %>% 
  pivot_longer(cols = 5:10, names_to = "predictor", values_to = "data") %>%
  # filter(sites == "Bawe")%>%
  ggplot(aes(x = data, y = chl))+
  scale_y_continuous(trans = scales::sqrt_trans(), labels = function(x) round(x,2))+
  # scale_x_continuous(trans = scales::sqrt_trans(), labels = function(x) round(x,2))+
  geom_jitter()+
  geom_smooth(se = FALSE, method = "lm", formula = "y ~ poly(x,2)", aes(color = "Quadratic"))+
  geom_smooth(se = FALSE, method = "lm", formula = "y ~ x", aes(color = "Linear"))+
  ggsci::scale_color_jco()+
  facet_wrap(~predictor, scales = "free_x")+
  ggpubr::theme_pubclean()+
  theme(strip.background.x = element_blank(), legend.key = element_blank(), 
        legend.position = "right", panel.background = element_rect(colour = "black"))


# time series year over year plot
bike_sales_monthly %>%
  ggplot(aes(x = month, y = total.qty, group = year)) +
  geom_area(aes(fill = year), position = "stack") +
  labs(title = "Quantity Sold: Month Plot", x = "", y = "Sales",
       subtitle = "March through July tend to be most active") +
  scale_y_continuous() +
  theme_tq()

# TS diagnostics
acf(ts, lag.max = 12) # last sig correlation is p for MA(p)
pacf(ts, lag.max = 12, ylab = "PACF") # last sig correlation is q for AR(q)

# Prophet model
library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)

bike_transactions_tbl <- bike_sharing_daily %>%
  select(dteday, cnt) %>%
  set_names(c("date", "value")) 

bike_transactions_tbl

bike_transactions_tbl %>%
  plot_time_series(date, value, .interactive = FALSE)

splits <- bike_transactions_tbl %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

model_table <- modeltime_table(
  # model_fit_arima, 
  # model_fit_prophet,
  workflow_fit_glmnet,
  # workflow_fit_rf,
  workflow_fit_prophet_boost
) 

model_table

calibration_table <- calibration_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

calibration_table %>%
  modeltime_forecast(actual_data = bike_transactions_tbl) %>%
  plot_modeltime_forecast(.interactive = FALSE)

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

#
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
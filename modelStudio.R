#modelStudio tutorial

#Libraries ----
library(modelStudio)
library(DALEX)
library(tidyverse)
library(tidymodels)

#DATA -----
data_tbl <- mpg %>%
  select(hwy, manufacturer:drv, fl, class)

#Model ----
fit_xgboost <- boost_tree(learn_rate = 0.3) %>%
  set_mode("regression") %>%
  set_engine("xgboost") %>%
  fit(hwy ~., data= data_tbl)

fit_xgboost

#Explainer ----
explainer <- DALEX:: explain(
  model = fit_xgboost,
  data  = data_tbl,
  y     = data_tbl$hwy,
  label = "XGBoost"
)

#ModelStudio ----
modelStudio::modelStudio(explainer)



#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
#| output: false

library(tidymodels)
library(tidyverse)
library(dissertationData)
library(here)

# Load and prepare the YRBS 2023 dataset

#
#
#
#
#
#
#| label: load-data

analysis_data <- readRDS(here("models", "data", "analysis_data.rds"))
analysis_train <- readRDS(here("models", "data", "analysis_train.rds"))
analysis_test <- readRDS(here("models", "data", "analysis_test.rds"))
analysis_folds <- readRDS(here("models", "data", "analysis_folds.rds"))

```
#
#
#
#| label: model-rec

mental_health_recipe <- 
  recipe(formula = NotGoodMentalHealth ~ ., data = analysis_train) |>
  step_zv(all_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.7) |> 
  step_dummy(all_nominal_predictors()) 

mental_health_recipe


#
#
#
#
#
#| label: model-spec

mental_health_spec <- 
  logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm") 

mental_health_spec

#
#
#
#
#
#| label: model-workflow

mental_health_workflow <- workflow() %>%
  add_recipe(mental_health_recipe) %>%
  add_model(mental_health_spec)


mental_health_workflow

#
#
#
#
#| label: model-fit
mod_1 <- 
  fit(mental_health_workflow, data = analysis_train) 

mod_1

#
#
#
#| label: tidy-model

tidy_model <- 
  mod_1 |>
  tidy(exponentiate = TRUE,
       conf.int = TRUE, 
       conf.level = .95) |>
  mutate(p.value = scales::pvalue(p.value))

tidy_model

#
#
#
#
#
#
#
#
#
#
#
#| label: model-evaluation

# Evaluate the model
# [Add your model evaluation code here]
#
#
#
#
#
#
#
#
#
#| label: visualizations

# Add your visualizations here
#
#
#
#
#
#
#
#
#
#
#

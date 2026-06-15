source("R/00_librariers.R")
source("R/01_utils.R")
source("R/02_meta_params.R")


# Feature/Outcome data -----------------------------------------------------------

feature_data <- read_csv("data/turingAI_feature_data.csv")
outcome_data <- read_csv("data/turingAI_outcome_data.csv")

# pivot outcome to long format

feature_data %>%
  select(-source) %>%
  mutate(variable_type = "feature") %>%
  bind_rows(
outcome_data %>%
  select(date, estimated_avoidable_deaths = model_estimated_avoiable_deaths) %>%
  rename(dt = date) %>%
  pivot_longer(cols = -dt, names_to = "metric_name") %>%
  mutate(coverage = "NHS Bristol, North Somerset, South Gloucestershire Integrated Care Board",
         coverage_label = "BNSSG",
        variable_type = "outcome")
  ) %>%
  write_csv("data/turingAI_forecasting_challenge_dataset.csv")








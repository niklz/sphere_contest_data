source("R/00_librariers.R")
source("R/01_utils.R")
source("R/02_meta_params.R")


# Feature/Outcome data -----------------------------------------------------------

feature_data <- read_csv("data/turingAI_feature_data_validation.csv")

# outcome_data <- read_csv("data/turingAI_outcome_data_validation.csv")
# Take outcome from old extract pre-fix of NCTR scaling bug for continuity
outcome_data <- unzip("data/turingAI_forecasting_challenge_validation_dataset_first.zip") %>%
  read_csv() %>%
  filter(variable_type == "outcome")

# pivot outcome to long format

feature_data %>%
  select(-source) %>%
  mutate(variable_type = "feature") %>%
  bind_rows(
outcome_data 
  ) %>%
  write_csv("data/turingAI_forecasting_challenge_validation_dataset.csv")


data <- feature_data %>%
  mutate(variable_type = "feature") %>%
  bind_rows(
outcome_data %>%
  select(date, estimated_avoidable_deaths = model_estimated_avoiable_deaths) %>%
    rename(dt = date) %>%
      pivot_longer(cols = -dt, names_to = "metric_name") %>%
        mutate(coverage = "NHS Bristol, North Somerset, South Gloucestershire Integrated Care Board",
      coverage_label = "BNSSG",
      variable_type = "outcome") %>%
  mutate(source = "outcome")
  )


plot_list <- data %>%
  mutate(id = cur_group_id(), .by = metric_name) %>%
  mutate(grp = id %/% 9) %>%
  nest(.by = grp) %>%
  mutate(plot = map(data, \(d) {
    ggplot(d, aes(x = dt, y = value, color = coverage)) +
      geom_line() +
      facet_wrap(vars(metric_name), scales = "free") +
      labs(x = "date") 
  })) %>%
  pull(plot)

Cairo::CairoPDF("materials/final_time_series.pdf", height = 14, width = 21, pointsize = 8)
walk(plot_list, print)
dev.off()








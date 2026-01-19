source("R/00_librariers.R")
source("R/01_utils.R")


con <- DBI::dbConnect(odbc::odbc(), "xsw")

frontier_tbl <- tbl(con,
               in_catalog(catalog = "modelling_sql_area",
                          schema = r"(BNSSG\Sebastian.Fox)",
                          table = "dbo.frontier")
)

tol <- lubridate::dweeks(1)
# training_begin <- ymd("2025-09-30")
training_end <- ymd("2025/09/30")

frontier_df <- frontier_tbl %>%
  rename(dt = date, 
  metric_id = metrics_id,
  coverage_code = metrics_org,
  coverage = org_name) %>%
  collect() %>%
  mutate(max_dt = max(dt, na.rm = TRUE))  %>%
  mutate(coverage_label = case_when(
coverage == "Bristol Royal Infirmary" ~ "BRI",
coverage == "Bristol Royal Hospital for Children" ~ "BRI (Children's)",
coverage == "North Bristol NHS Trust" ~ "NBT",
coverage == "Weston" ~ "WGH",
str_detect(coverage, "NHS Bristol, North Somerset, South Gloucestershire Integrated Care Board") ~ "BNSSG",
str_detect(coverage, "Sirona") ~ "Sirona",
str_detect(coverage, "One Care") ~ "One Care",
str_detect(coverage, "SevernSide") ~ "SevernSide",
coverage == "Avon and Wiltshire Mental Health Partnership NHS Trust" ~ "AWP",
.default = coverage
  )) %>%
  # filter out any variables which don't have data until the last week of coverage
  filter(max(dt, na.rm = TRUE) >= max_dt - tol, .by = c(metric_name, metric_id, coverage)) %>%
  select(-c(metric_id, coverage_code, -max_dt)) 


frontier_df %>%
  summarise(min_date = min(dt, na.rm = TRUE), max_date = max(dt, na.rm = TRUE))

frontier_df %>%
  summarise(rows = n(), metrics = n_distinct(metric_name)) 

frontier_df <- frontier_df %>%
  mutate(metric_name = str_remove_all(metric_name, r"(^[1-9]\. )")) 

frontier_train <- frontier_df %>%
  filter(dt <= training_end)

frontier_test <- frontier_df %>%
  filter(dt > training_end)

frontier_train %>%
   readr::write_csv("data/turingAI_feature_data.csv")
  
frontier_test %>%
     readr::write_csv("data/turingAI_feature_data_TEST.csv")

frontier_df %>%
  summarise(n = n(), .by = metric_name) %>%
  pull(metric_name) %>%
  unique()

frontier_df %>%
  pull(metric_name) %>%
  unique() %>%
  length()

# frontier_tbl %>%
#   select(date, metric_name, org_name, value) %>%
#   head(100) %>% 
#   collect() %>%
#   View()


mode <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA) # Handle empty groups
  
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  
  # Find all modes
  modes <- ux[tab == max(tab)]
  
  # Return only the first one to ensure a single value for summarise()
  return(modes[1])
}

metric_sum <- frontier_df %>%
  filter(!is.na(dt))  %>%
  mutate(period = factor(if_else(dt > training_end, "Testing period", "Training period"), levels = c("Training period", "Testing period"))) %>%
  arrange(dt) %>%
  mutate(diff = interval(lag(dt), dt)/dminutes(1),
         hour = hour(dt),
         day = wday(dt, label = TRUE),
         .by = c(metric_name, coverage)) %>% 
  filter(!is.na(diff), diff != 0) %>%
  summarise(
    min = min(dt, na.rm = TRUE),
    max = max(dt, na.rm = TRUE),
    frequency = mode(diff),
    hour = mode(hour),
    day = mode(day),
    .by = c(period, metric_name, coverage_label)
  )  %>%
  mutate(freq_label = case_when(
    frequency == 15 ~ "Every 15 minutes",
    frequency == 30 ~ "Every 30 minutes",
    frequency == 300 ~ "Every 5 hours",
    frequency == 1425 ~ str_c("Every day at ", hour, "h00"),
    frequency == 1440 ~ str_c("Every day at ", hour, "h00"),
    frequency == 2880 ~ str_c("Every 2 days at ", hour, "h00"),
    frequency == 4320 ~ str_c("Every 3 days at ", hour, "h00"),
    # frequency == 10080   ~ str_c("Every week at ", hour, "00 hours"),
  .default = "Not fixed"
  )) %>%
mutate(metric_label = str_c(metric_name, coverage_label, sep = " - "))

readr::write_csv(metric_sum, file = "data/metric_metadata.csv")

ggplot(metric_sum, aes(x = min, xend = max, y = metric_label, yend = metric_label, col = freq_label)) +
  geom_segment() +
  labs(x = "Date", y = "Feature", col = "") +
  # scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  scale_colour_manual(values = c(colorRampPalette(hrbrthemes::ipsum_pal()(9))(12), "grey30")) +
  theme(legend.position = "bottom") +
  facet_grid(~period, scale = "free_x", space = "free_x")


ggsave(filename = "materials/metric_timerange.png",
bg = "white",
height = 40,
width = 20,
scale = 0.8)

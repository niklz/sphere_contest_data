source("R/00_librariers.R")
source("R/01_utils.R")
source("R/02_meta_params.R")

# load training set metrics
training_metrics <- readr::read_csv("data/turingAI_forecasting_challenge_dataset.csv") %>%
  distinct(metric_name) %>%
  pull(metric_name)

con <- DBI::dbConnect(odbc::odbc(), "xsw")


# FRONTIER DATA

frontier_tbl <- tbl(con,
               in_catalog(catalog = "modelling_sql_area",
                          schema = r"(BNSSG\Sebastian.Fox)",
                          table = "dbo.frontier")
)

org_lkup_tbl <-
    tbl(
      con,
      in_catalog(
        catalog = "Analyst_SQL_Area",
        schema = "CTCC",
        table = "organisation_codes"
      )
    )

# Missing metrics
# latest timestamp for each metric
max_stamps <- frontier_tbl %>%
  summarise(max_date = max(date), .by = c(metrics_id, metric_name, metrics_org, org_name)) %>%
  collect()

# frontier_bkup_tbl <-
#   tbl(
#     con,
#     in_catalog(
#       catalog = "Analyst_SQL_Area",
#       schema = "CTCC",
#       table = "api_output_submission_history"
#     )
#   )


frontier_bkup_tbl <-
  tbl(
    con,
    in_catalog(
      catalog = "Analyst_SQL_Area",
      schema = "dbo",
      table = "ctcc_api_submission_history_extract"
    )
  )


frontier_bkup_tbl_2 <-
  tbl(
    con,
    in_catalog(
      catalog = "Analyst_SQL_Area",
      schema = "dbo",
      table = "ctcc_api_submission_history_extract_2"
    )
  )

metric_lkup <- frontier_tbl %>%
  distinct(metrics_id, metric_name) %>% 
  collect()
  

  frontier_recalculated_df <- bind_rows(
  frontier_bkup_tbl %>%
  filter(between(datetime, as.Date("2026-01-01"), as.Date("2026-05-01"))) %>%
  select(date = datetime, metrics_id = metric_code, metrics_org = organisation_code, value) %>%
  mutate(src = "backup_1") %>%
  collect(),

frontier_bkup_tbl_2 %>%
  filter(between(datetime, as.Date("2026-01-01"), as.Date("2026-05-01"))) %>%
  select(date = datetime, metrics_id = metric_code, metrics_org = organisation_code, value) %>%
  mutate(src = "backup_2") %>%
  collect()) |>
  filter(value == max(value, na.rm = TRUE), .by = c(date, metrics_id, metrics_org, src)) %>%
  pivot_wider(id_cols = c(date, metrics_id, metrics_org), names_from = src, values_from = value) %>%
  mutate(value = case_when(
    is.na(backup_1) ~ backup_2, 
    is.na(backup_2) ~ backup_1, 
    .default = backup_1
  )
  ) %>%
  left_join(org_lkup_tbl, by = join_by(metrics_org == organisation_code), copy = TRUE) %>%
  left_join(metric_lkup, by = join_by(metrics_id == metrics_id), copy = TRUE) %>%
  select(date, metrics_id, metric_name, metrics_org, value, org_name = organisation_name) %>%
  mutate(org_name = str_trim(org_name)) %>%
  arrange(date)



  
# frontier_missing_tbl <- frontier_bkup_tbl %>%
#   left_join(org_lkup_tbl) %>%
#   # filter(metric_code %in% frontier_missing_metrics) %>%
#   select(date = datetime, metrics_id = metric_code, metric_name = metrics, metrics_org = organisation_code, org_name = organisation_name, value = value) %>%
#   left_join(max_stamps, by = join_by(metrics_id == metrics_id, metric_name == metric_name, metrics_org == metrics_org, org_name == org_name), copy = TRUE) %>%
#   filter(date > max_date) %>%
#   select(-max_date) %>%
#   distinct() %>%
#   mutate(value = as.numeric(value)) 

missing_lookup <- frontier_recalculated_df %>%
  distinct(metrics_id, metrics_org) 

min_recalc_dat <- frontier_recalculated_df$date %>% min()

frontier_df <- frontier_tbl %>% 
  filter(date > training_end) %>%
  #remove metrics from affected period
  anti_join(
    filter(., date > min_recalc_dat) %>% inner_join(missing_lookup, by = c("metrics_id", "metrics_org"), copy = TRUE),
    by = c("date", "metrics_id", "metrics_org", "org_name") 
  ) %>%
  union_all(frontier_recalculated_df, copy = TRUE) %>%
  rename(dt = date, 
  metric_id = metrics_id,
  coverage_code = metrics_org,
  coverage = org_name) %>%
 collect() %>%
  mutate(metric_name = recode(
    metric_name,
    !!!c(
        "(Majors)" = "Majors patient count",
        "(Minors)" = "Minors patient count"
    )
  )) %>%
  distinct(dt, metric_id, coverage, .keep_all = TRUE) %>%
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
  # remove some metircs where data is sparse
  filter(!metric_name %in% c(
    "Handover to Clear 60mins (Last Hour)",
    "Average Call Handler Handling Time in Last 15 mins",
    "Average Clinical Advisor Handling Time in Last 15 mins",
    "Average Call Handler Handling Time Since Midnight that Received",
    "Average Clinical Advisor Handling Time Since Midnight that Received"
  )) %>%
  select(-c(metric_id, coverage_code)) %>%
  arrange(dt) %>%
  mutate(source = "frontier") 
  



# UEC DAILY DATA

uec_daily_tbl <- tbl(con,
               in_catalog(catalog = "Analyst_SQL_Area",
                          schema = "dbo",
                          table = "tbl_BNSSG_Datasets_UrgentCare_Daily")
)

selected_metric_ids <- c(100324, 100330, 100331, 100328, 100327, 100329, 100326, 100332, 
260172, 259750, 260171, 267998, 346211, 347363, 347359, 353996, 
347361, 346125, 259453, 320629, 320628, 320626, 320630, 320635, 
320636, 320637, 320638, 320900, 258835, 354066, 100054, 100342, 
100353, 100367, 100368, 100370, 100366, 100369, 100354, 100356, 
100154, 100337, 100341, 100338, 100043, 100042, 100212, 100189, 
100152, 100196, 100044, 100063, 100041, 100062, 100190, 100141, 
100048, 100355, 100351, 100146, 100136, 100135, 100134, 100137, 
100155, 100365, 100211, 100209, 100191, 100192, 100200, 100217, 
100068, 100069, 100216, 100158, 100159, 100066, 100364, 100350, 
100336, 100065, 100038, 100145, 100052, 100195, 100208, 100215, 
100140, 100157, 100059, 100151, 100067, 100047, 100361, 100362, 
100347, 100333, 100348, 267989, 259460, 260164, 259454, 260165, 
260167, 260168, 295464, 295465, 347334, 347348, 346199, 346203, 
346200, 346183, 346186, 347351, 347331, 346098, 346085, 346114, 
346117, 347318, 347357, 346170, 346209)



uec_daily_df <- 
  uec_daily_tbl %>%
  filter(Report_Date > training_end) %>%
  # filter(METRIC_ID %in% selected_metric_ids) %>%
  select(dt = Report_Date, metric_id = METRIC_ID, metric_name = METRIC_NAME, coverage = Provider, value = Value) %>%
  distinct(dt, metric_id, coverage, .keep_all = TRUE) %>%
  collect() %>%
  select(-metric_id) %>%
  mutate(metric_name = recode(
    metric_name,
    !!!c(
      # "Number of Discharges",
      "General & Acute Beds - Total G&A escalation beds open" = "Escalation beds open",
      "Of total G&A beds open, number occupied" = "G&A Bed occupancy",
      "Total G&A escalation beds open" = "Escalation beds open",
      # "Number of Admissions",
      "Beds occupied by Long-stay patients 21+ days? beds occupied by patients with a length of stay of 21 or more days" = "Beds occupied by long-stay patients (21+ days)",
      "Beds occupied by Long-stay patients 21+ daysâ€“ beds occupied by patients with a length of stay of 21 or more days" = "Beds occupied by long-stay patients (21+ days)",
      "Beds occupied by Long-stay patients 21+ days– beds occupied by patients with a length of stay of 21 or more days" = "Beds occupied by long-stay patients (21+ days)",
      "Beds occupied by Long-Stay Patients 21+ days" = "Beds occupied by long-stay patients (21+ days)",
      "A&E Performance - Number of A&E Attendances - Type 1 - Paediatrics"  = "A&E attends - paediatrics",
      "Number of A&E Attendances - Type 1 - Paediatrics" = "A&E attends - paediatrics",
      "General & Acute Beds - Of total G&A beds open, number occupied" = "G&A Bed occupancy",
      "Of total G&A beds open number occupied" = "G&A Bed occupancy",
      "Total G & A core bed stock open"="G&A beds, core stock open", 
      "Total G&A core bed stock open"="G&A beds, core stock open", 
      "General & Acute Beds - Total G&A core bed stock open"="G&A beds, core stock open"
    )
  )) %>%
  # make sure that we have data for all metrics until start of validation
  # filter(max(dt, na.rm = TRUE) >= validation_start - tol, .by = c(metric_name, coverage)) %>%
  mutate(dt = as.POSIXct(paste(ymd(dt), "12:00:00"))) %>%
  arrange(dt) %>%
  mutate(coverage = recode(coverage, "Severnside" = "SevernSide")) %>%
  mutate(coverage_label = coverage) %>%
  mutate(location_tag = str_extract(metric_name, "Bristol|NS|SG")) %>% 
  mutate(coverage = if_else(!is.na(location_tag), 
                            paste(coverage, location_tag, sep = " - "), 
                            coverage)) %>%
  mutate(metric_name = str_remove_all(metric_name, regex(r"(^\((Sirona|UHB|SevernSide)\)( Bristol - | NS - | SG - )?)"))) %>%
  mutate(metric_name = str_remove_all(metric_name, regex(r"(\(Bris[dD]oc\) :? ?)"))) %>%
  mutate(metric_name = str_trim(metric_name)) %>%
  select(-location_tag) %>%
  mutate(source = "uec_daily")


# COMBINED DATA

metric_data <- 
  bind_rows(frontier_df, uec_daily_df) %>%
  # cleaning up names
  mutate(metric_name = str_replace_all(metric_name, "^Av\\.", "Average")) %>%
  mutate(metric_name = str_replace_all(metric_name, "Resus ", "Resuscitation "))  %>%
  mutate(metric_name = str_replace_all(metric_name, "rehab ", "rehabilitation "))  %>%
  mutate(metric_name = str_replace_all(metric_name, "Cat ", "Category "))  %>%
  mutate(metric_name = str_replace_all(metric_name, "s136", "Section 136"))  %>%
  mutate(metric_name = str_remove_all(metric_name, r"(^[1-9]\. )")) %>%
  filter(metric_name %in% training_metrics)


  # PLOT METRIC TIME SERIES

metric_recodes <- metric_data$metric_name %>%
  unique() %>%
  set_names(., .) %>%
  str_wrap(40)

max_pts <- 5000

pal <- c("uec_daily" = "lightblue", "frontier" = "orange")

Cairo::CairoPDF("materials/feature_time_series_validation.pdf", height = 14, width = 21, pointsize = 8)
metric_data %>%
  filter(!is.na(dt)) %>%
  filter(n() <= max_pts | row_number() %% (n() %/% max_pts) == 0, .by = metric_name) %>%
  mutate(metric_org = str_c(metric_name, "\n", coverage)) %>%
  mutate(id = cur_group_id(), .by = metric_org) %>%
  mutate(grp = id %/% 9) %>%
  # mutate(metric_name = recode(metric_name, !!!metric_recodes)) %>%
  nest(.by = grp) %>%
  mutate(plot = map(data, \(d) d %>%
    arrange(dt) %>%
    ggplot(aes(x = dt, y = value)) +
  #   geom_bin2d(bins = 100) + 
  # scale_fill_viridis_c() +
    geom_path(aes(col = source)) +
    scale_colour_manual(values = pal) +
    # ggrastr::rasterise(geom_path(), dpi = 300) + 
    facet_wrap(vars(metric_org), scales = "free") +
    labs(x = "date") 
  )) %>%
  pull(plot) 
dev.off()


# metric_data <- metric_data %>%
#   mutate(metric_name = str_remove_all(metric_name, r"(^[1-9]\. )")) 

metric_data_validation <- metric_data %>%
  filter(as.Date(dt) > training_end, as.Date(dt) <= validation_end)


metric_data_validation %>%
  summarise(min_date = min(dt, na.rm = TRUE), max_date = max(dt, na.rm = TRUE))

metric_data_validation %>%
  summarise(rows = n(), metrics = n_distinct(metric_name)) 

# all variables included where in training?
metric_data_validation %>% 
  distinct(metric_name) %>%
  pull(metric_name) %>%
  `%in%`(training_metrics) %>%
  all()

# variables not present in training (should be outcome at this stage)
setdiff(training_metrics,
   metric_data_validation %>% 
  distinct(metric_name) %>%
  pull(metric_name)
)


metric_data_validation %>%
   readr::write_csv("data/turingAI_feature_data_validation.csv")
  

metric_data %>%
  summarise(n = n(), .by = metric_name) %>%
  pull(metric_name) %>%
  unique()

metric_data %>%
  pull(metric_name) %>%
  unique() %>%
  length()


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

metric_sum <- metric_data %>%
  filter(!is.na(dt)) %>%
  mutate(period = factor(if_else(dt > training_end, "Testing period", "Training period"), 
                         levels = c("Training period", "Testing period"))) %>%
  filter(period == "Training period") %>%
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
    .by = c(metric_name, coverage_label, source)
  ) %>%
  mutate(freq_label = case_when(
    frequency == 15 ~ "Every 15 minutes",
    frequency == 30 ~ "Every 30 minutes",
    frequency == 300 ~ "Every 5 hours",
    frequency %in% c(1425, 1440) ~ str_c("Every day at ", hour, "h00"),
    frequency == 2880 ~ str_c("Every 2 days at ", hour, "h00"),
    frequency == 4320 ~ str_c("Every 3 days at ", hour, "h00"),
    .default = "Not fixed"
  )) %>%
  select(-c(min, max, hour, day)) %>%
  mutate(metric_label = str_c(metric_name, coverage_label, sep = " - ")) %>%
  distinct()

readr::write_csv(metric_sum, file = "data/metric_metadata.csv")

metric_sum %>%
    filter(period == "Training period") %>%
ggplot(aes(x = min, xend = max, y = metric_label, yend = metric_label, col = freq_label)) +
  geom_segment() +
  labs(x = "Date", y = "Feature", col = "") +
  # scale_color_brewer(palette = "Set3") +
  theme_minimal() +
  scale_colour_manual(values = c(colorRampPalette(hrbrthemes::ipsum_pal()(9))(12), "grey30")) +
  theme(legend.position = "bottom") #+
  # facet_grid(~ period, scales = "free", space = "free_x")


ggsave(last_plot(),
filename = "materials/metric_timerange.png",
bg = "white",
height = 35*sqrt(2),
width = 35,
scale = 0.6)
  
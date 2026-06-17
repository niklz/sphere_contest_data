# META PARAMETERS

# TRAIN/TEST windows
tol <- lubridate::dweeks(1)
training_begin <- ymd("2023-03-16")
training_end <- ymd("2025/09/30")
training_end <- ymd("2025/09/30")
validation_start <- ymd("2026-01-01")
# validation_end <- ymd("2026-03-31")
validation_end <- ymd("2026-02-17")

# OUTCOME THRESHOLDS/PARAMS
ed_threshold <- 8*60 # 8 hours for harm
ambulance_threshold <- 30 # 30 minutes for harm
sustain_window <- 7 # 3 days no harm pre  & all harm post for sustained SAH

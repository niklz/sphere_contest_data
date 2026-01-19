# META PARAMETERS

# TRAIN/TEST windows
tol <- lubridate::dweeks(1)
training_begin <- ymd("2023-03-16")
training_end <- ymd("2025/09/30")
validation_start <- ymd("2026-01-01")
# OUTCOME THRESHOLDS
ed_threshold<-8*60
ambulance_threshold<-30

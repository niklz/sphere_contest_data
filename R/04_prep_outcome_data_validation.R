source("R/00_librariers.R")
source("R/01_utils.R")
source("R/02_meta_params.R")

# 17/06/2026 !! NOTE !! we no longer use this code to generate outcome due to 
# missing nctr site scaling bug which was present during training data generation 
# the bug is fixed here for completeness but we use an older extract to produce assessment data


con <- DBI::dbConnect(odbc::odbc(), "xsw")

# A&E data ---------------------------------------------------------------

ae_dat <- local({
  mort_mod <- readRDS("data/mort_effects_output_multilvl.RDS") %>% `[[`("mort_mod_raw")
  vw_impute_rec <- readRDS("data/vw_impute_rec.RDS")
  site_core_stock <- tibble(site = c("BRISTOL ROYAL INFIRMARY",
                                   "WESTON GENERAL HOSPITAL",
                                   "SOUTHMEAD HOSPITAL"),
                         core_stock = c(24, 20, 13)
                         # core_stock = c(692, 271, 878)
                         )

  nctr_tbl <- tbl(
    con,
    in_catalog(
      "Analyst_SQL_Area",
      "dbo",
      "vw_NCTR_Status_Report_Daily_JI"
    )
  )

  nctr_df <- nctr_tbl %>%
    filter(Census_Date == training_end) %>%
    # head(1, .by = CDS_Unique_Identifier) %>%
    filter(Criteria_To_Reside == "N", Bed_Type == "Medicine") %>%
    mutate(Site_Name = stringr::str_to_upper(Site_Name)) %>%
    summarise(nctr = n(), .by = Site_Name) %>%
    collect() %>%
    left_join(site_core_stock, by = join_by(Site_Name == site)) %>%
    mutate(nctr = nctr/core_stock) %>%
    select(-core_stock)

  ae_tbl <- tbl(
    con,
    in_catalog(
      catalog = "Analyst_SQL_Area",
      schema = "dbo",
      table = "tbl_BNSSG_ECDS"
    )
  )

  aedat <- ae_tbl %>%
    filter(
      Site %in% c('RA701', 'RVJ01', 'RA7C2'),
      `Treatment_Function_Code` %in% c(300, 326, 430),
      !is.na(Decision_To_Admit_Date)
    ) %>%
    mutate(Destination_Desc = coalesce(Destination_Desc, 'Missing')) %>%
    filter(
      !Destination_Desc %in%
        c(
          'Emergency department discharge to emergency department short stay ward',
          'Emergency department discharge to same day emergency care service',
          'Emergency department discharge to ambulatory emergency care service',
          'Emergency department discharge to neonatal intensive care unit',
          'Emergency department discharge to special care baby unit',
          'Admission to the mortuary',
          'Discharge to home',
          'Discharge to hospital at home service',
          'Discharge to nursing home',
          'Discharge to residential home',
          'Discharge to police custody',
          'Patient discharge, to legal custody',
          'Urgent admission to hospice'
        )
    ) %>%
    select(
      Site_Name,
      # spec = `Treatment_Function_Code`,
      imd = `Index_Of_Multiple_Deprivation`,
      age = HES_Age_At_Arrival,
      sex = Stated_Gender,
      ed_los_minutes = Departure_Time_Since_Arrival,
      arrival_date = Arrival_Date,
      arrival_time = Arrival_Time,
      dta_date = Decision_To_Admit_Date,
      dta_time = Decision_To_Admit_Time,
      departure_date = Departure_Date,
      departure_time = Departure_Time,
    ) %>%
    collect() %>%
    mutate(
      age = cut(
        age,
        breaks = c(0, seq(30, 100, 10), Inf),
        right = FALSE,
        labels = c(
          "29 years old or under",
          "30-39 years old",
          "40-49 years old",
          "50-59 years old",
          "60-69 years old",
          "70-79 years old",
          "80-89 years old",
          "90-99 years old",
          "100 years old or over"
        )
      )
    ) %>%
    filter(sex %in% 1:2) %>%
    mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
    mutate(
      imd = factor(
        imd,
        levels = 1:10,
        labels = c(
          "1 (most deprived)",
          "2",
          "3",
          "4",
          "5",
          "6",
          "7",
          "8",
          "9",
          "10 (least deprived)"
        )
      )
    ) %>%
    left_join(nctr_df) %>%
    mutate(
      arrival_dt = paste0(arrival_date, " ", substr(arrival_time, 1, 8))
    ) %>%
    mutate(
      arrival_dt = as.POSIXct(
        ifelse(grepl("NA", arrival_dt), NA, arrival_dt),
        format = "%Y-%m-%d %H:%M:%S"
      )
    ) %>%
    mutate(dta_dt = paste0(dta_date, " ", substr(dta_time, 1, 8))) %>%
    mutate(
      dta_dt = as.POSIXct(
        ifelse(grepl("NA", dta_dt), NA, dta_dt),
        format = "%Y-%m-%d %H:%M:%S"
      )
    ) %>%
    mutate(
      departure_dt = as.POSIXct(
        paste0(departure_date, " ", substr(departure_time, 1, 8)),
        format = "%Y-%m-%d %H:%M:%S"
      )
    ) %>%
    mutate(dta_time = interval(dta_dt, departure_dt) / dminutes(1)) %>%
    mutate(time_to_dta = interval(arrival_dt, dta_dt) / dminutes(1)) %>%
    mutate(
      wkend = factor(ifelse(wday(arrival_dt, week_start = 1) < 6, "No", "Yes"))
    ) %>%
    mutate(
      arrival_time = factor(hms::as_hms(floor_date(arrival_dt, "6 hour")))
    ) %>%
    mutate(
      arrival_time = forcats::fct_recode(
        arrival_time,
        "00:00-05:59" = "00:00:00",
        "06:00-11:59" = "06:00:00",
        "12:00-17:59" = "12:00:00",
        "18:00-23:59" = "18:00:00"
      )
    ) %>%
    mutate(arrival_month = factor(month.name[month(arrival_dt)], levels = month.name)) %>%
    mutate(n_ed = 0) # vastly the case in the actual data


  # Produce model estimated mortality probabilities

  aedat2 <- recipes::bake(vw_impute_rec, new_data = mutate(aedat, vw_score = NA_real_))

  model_estimated_deaths_full <- aedat2 %>%
    mutate(across(c(dta_time, time_to_dta), \(x) x / 60)) %>%
    lme4:::predict.merMod(mort_mod, newdata = ., type = "response", re.form = NA)

  model_estimated_deaths_nodta <- aedat2 %>%
    mutate(across(c(dta_time, time_to_dta), \(x) x / 60)) %>%
    mutate(dta_time = 0) %>%
    lme4:::predict.merMod(mort_mod, newdata = ., type = "response", re.form = NA)

  model_estimated_avoiable_deaths <- model_estimated_deaths_full - model_estimated_deaths_nodta
  

  aedat2 <- aedat %>%
    mutate(model_estimated_avoiable_deaths = model_estimated_avoiable_deaths) %>%
    drop_na() %>%
    select(-dta_dt, dta_date, departure_date) %>%
    mutate(date = departure_date) %>%
    mutate(date = as.Date(substr(date, 1, 10))) %>%
    filter(date > training_end) %>%
    filter(date <= validation_end) %>%
    mutate(
      nnk_estimated_avoidable_deaths = case_when(
        dta_time >= 4 * 60 ~ 1,
        .default = 0
      )
    ) %>%
    summarise(
      nnk_estimated_avoidable_deaths = sum(nnk_estimated_avoidable_deaths) / 69,
      model_estimated_avoiable_deaths = sum(model_estimated_avoiable_deaths),
      .by = c(date)
    ) %>%
    arrange(date)

  # for interest's sake, plot it
  aedat2 %>%
    tsibble::as_tsibble(index = date) %>%
    fabletools::model(feasts::STL(model_estimated_avoiable_deaths ~ season("year") + season("week"))) %>%
    fabletools::components() %>%
    autoplot()

  aedat2
})



# Write output -----------------------------------------------------------


write.csv(ae_dat,file="data/turingAI_outcome_data_validation.csv",row.names=FALSE)

plot <-ae_dat %>%
  ggplot(aes(x = date, y = model_estimated_avoiable_deaths)) +
  geom_path() +
  labs(y = "Estimated avoidable deaths", x = "Date");plot

png("materials/outcomes_plot.png",height=5,width=8,units="in",res=300)
print(plot)
dev.off()









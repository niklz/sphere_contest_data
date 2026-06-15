source("R/00_librariers.R")
source("R/01_utils.R")

## Lookups for metric/org
metric_lookup <- read.csv('data/metrics.csv') |>
  dplyr::select(code, metric_name = name)
org_lookup <- read.csv('data/organisations.csv')  |>
  dplyr::select(code, org_name = name)

## chunk up the api request:

# have data from 1st Jan 2026
start_date  <- lubridate::ymd("2026-02-01")
end_date  <- lubridate::ymd("2026-03-31")
chunk_size  <- 3


start_dates <- seq.Date(
  from = start_date,
  to = end_date - lubridate::ddays(chunk_size - 1),
  by = glue::glue("{chunk_size} days")
)

end_dates <- seq.Date(
  from = start_date + lubridate::ddays(chunk_size - 1),
  to = end_date,
  by = glue::glue("{chunk_size} days")
)

# force the final date to be the end_date
end_dates <- replace(
  x = end_dates,
  list = end_dates == max(end_dates),
  end_date
)


start_time <- Sys.time()
# get the data from the API
df_list <- purrr::map2(
  start_dates,
  end_dates,
  ~ get_api_data(
    .x,
    .y,
    metric_lookup = metric_lookup,
    org_lookup = org_lookup
  )
)

df <- df_list |>
  purrr::list_rbind() |>
  filter(
    metric_name != "Comment"
  ) |>
  mutate(
    value = as.numeric(value)
  )

print(Sys.time() - start_time)


# upload the data to SQL --------------------------------------------------

# Old Function need to update
# create_append_table(
#   table_name = "dbo.frontier",
#   data = df
# )




con <- DBI::dbConnect(odbc::odbc(), "xsw")

frontier_tbl <- tbl(con,
               in_catalog(catalog = "modelling_sql_area",
                          schema = r"(BNSSG\Sebastian.Fox)",
                          table = "dbo.frontier")
)




max_stamps <- frontier_tbl %>%
  summarise(max_date = max(date), .by = c(metrics_id, metric_name, metrics_org, org_name)) %>%
  collect()


# Join to new data cut and filter data that is prior to existing
frontier_new <- df_list %>%
  bind_rows() %>%
  left_join(max_stamps, by = join_by(metrics_id == metrics_id, metric_name == metric_name, metrics_org == metrics_org, org_name == org_name)) %>%
  filter(date > max_date) %>%
  select(-max_date) %>%
  mutate(value = as.numeric(value))

dbWithTransaction(con, {
  message("Starting append via DBI...")
  
  dbWriteTable(
    conn = con,
    name = Id(
      catalog = "modelling_sql_area", 
      schema  = "BNSSG\\Sebastian.Fox", 
      table   = "dbo.frontier"
    ),
    value  = frontier_new,
    append = TRUE
  )
  
  message("Append successful!")
})

# remove_table("dbo.frontier")

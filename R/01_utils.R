## Get the URL for the scrape get request
make_api_url <- function(date_from, date_to, metrics = "") {
  request <- httr2::request(
    Sys.getenv("API_ROOT_URL")
  ) |>
    httr2::req_url_path_append(date_from) |>
    httr2::req_url_path_append(date_to)

  if (metrics != "") {
    request <- request |>
      httr2::req_url_query(
        `metric_code` = metrics,
        .multi = "explode"
      )
  }

  request <- request |>
    httr2::req_headers(
      'UserAPI-Key' = Sys.getenv("API_AUTH")
    ) |>
    httr2::req_user_agent("Nick Howlett BNSSG - nick.howlet5@nhs.net") |> # tells the API who is querying it (polite)
    httr2::req_throttle(10 / 60) # throttle the calls to 10 requests every 60 seconds

  return(request)
}

record_error_body <- function(resp) {
  resp |>
    httr2::resp_body_string()
}

# Get and format data
get_api_data <- function(date_from, date_to, metrics = "", metric_lookup, org_lookup) {

  request <- make_api_url(date_from, date_to, metrics = metrics)
  api_data <- request |>
    httr2::req_error(
      is_error = \(resp) FALSE,
      body = record_error_body
    ) |>
    httr2::req_perform()

  # browser()
  if (httr2::resp_is_error(api_data)) {
    api_data <- data.frame(
      date = .POSIXct(double()),
      metrics_id = integer(),
      metric_name = character(),
      metrics_org = character(),
      org_name = character(),
      value = character()
    )
  } else if (httr2::resp_status(api_data) == 204) {
    api_data <- data.frame(
      date = .POSIXct(double()),
      metrics_id = integer(),
      metric_name = character(),
      metrics_org = character(),
      org_name = character(),
      value = character()
    )
  } else {
    api_data <- api_data |>
      httr2::resp_body_json(
        simplifyVector = TRUE
      ) |>
      data.frame() |>
      janitor::clean_names() |>
      dplyr::right_join(
        metric_lookup,
        by = dplyr::join_by('metrics_id' == 'code')
      ) |>
      dplyr::right_join(
        org_lookup,
        by = dplyr::join_by('metrics_org' == 'code')
      ) |>
      dplyr::mutate(
        date = stringr::str_replace(metrics_dt, 'T', ' '),
        date = as.POSIXct(date, format = '%Y-%m-%d %H:%M:%S')
      ) |>
      dplyr::mutate(
        date = lubridate::floor_date(date, unit = "15 mins")
      ) |>
      dplyr::rename(value = metrics_val) |>
      dplyr::select(
        date, metrics_id, metric_name, metrics_org, org_name, value
      )

    print(max(api_data$date, na.rm = TRUE))
  }



  Sys.sleep(5)

  return(api_data)
}



# database functions ------------------------------------------------------

database_connection <- function(db) {

  con <- dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "Xsw-00-ash01",
    # Database = "modelling_sql_area",
    Database = db,
    Trusted_Connection = "True",
    timeout = 120
  )
}

connect_and_find_table <- function(db, tbl_pattern) {
  con <- database_connection(db)

  on.exit(
    dbDisconnect(con)
  )

  tbls <- dbGetQuery(
    con,
    "SELECT table_schema, table_name FROM information_schema.tables"
  ) |>
    filter(
      grepl(tbl_pattern, table_name, ignore.case = TRUE)
    ) |>
    mutate(
      tbls = paste(
        db,
        table_schema,
        table_name,
        sep = "."
      )
    ) |>
    pull(tbls)

  # tbls <- dbListTables(con)
  # # tbls <- dbListObjects(con)
  #
  # tbls <- tbls[grepl(tbl_pattern, tbls, ignore.case = TRUE)]
  #
  # tbls <- paste(
  #   db,
  #   "dbo",
  #   tbls,
  #   sep = "."
  # )

  return(tbls)
}

find_db_tables <- function(tbl_pattern) {
  dbs <- c(
    "ABI",
    "Analyst_SQL_Area",
    "MODELLING_SQL_AREA"
  )

  tbls <- dbs |>
    purrr::map(
      ~ connect_and_find_table(
        .x,
        tbl_pattern
      )
    ) |>
    unlist()

  return(tbls)
}

create_append_table <- function(table_name, data) {

  con <- database_connection(
    db = "modelling_sql_area"
  )

  on.exit(
    dbDisconnect(con)
  )

  table_exist <- DBI::dbExistsTable(
    conn = con,
    name = table_name
  )

  if (!isTRUE(table_exist)) {
    field_types <- c(
      "date" = "datetime",
      "metrics_id" = "integer",
      "metric_name" = "varchar(255)",
      "metrics_org" = "varchar(255)",
      "org_name" = "varchar(255)",
      "value" = "decimal"
    )

    DBI::dbCreateTable(
      conn = con,
      name = table_name,
      fields = field_types
    )
  }


  DBI::dbAppendTable(
    conn = con,
    name = table_name,
    value = data
  )

  return(invisible(TRUE))
}

remove_table <- function(table_name) {
  con <- database_connection(
    db = "modelling_sql_area"
  )

  on.exit(
    dbDisconnect(con)
  )

  DBI::dbRemoveTable(
    conn = con,
    name = table_name
  )

  return(invisible(TRUE))
}

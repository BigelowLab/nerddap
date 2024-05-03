#
ctd_query_uri = function(buoyid = "$BUOYID", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_sbe37_all.csv?station%2Ctime%2Ctemperature%2Csalinity%2Csigma_t%2Cdepth&time%3E=2023-04-10T00%3A00%3A00Z&time%3C=2023-04-17T14%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_sbe37_all.csv", buoyid)
  query = c("station", "time", 
            "temperature",
            "temperature_qc",
            "salinity",
            "salinity_qc",
            "sigma_t", 
            "sigma_t_qc",
            "depth")
  constraints = c("time>=1980-01-01T00:00:00Z",
                  format(time_to_utc(x = Sys.time()), "time<=%Y-%m-%dT%H:%M:%SZ"))
  if (escape){
    uri =  paste0(file.path(base_uri,  name), 
                  "?", 
                  xml2::url_escape(paste(query, collapse = ",")), 
                  "&", 
                  paste(xml2::url_escape(constraints), collapse = "&"))
  } else {
    uri =  paste0(file.path(base_uri,  name), "?", 
                  paste(query, collapse = ","), "&", 
                  paste(constraints, collapse = "&"))
  }
  
  uri
}

#' Aggregate columns by interval
#' 
#' @param x tibble of data
#' @param by char the interval overwich to aggregate
#' @return summary tibble
ctd_aggregate = function(x, by = c("month", "year")[1]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  
  where_compute = function(x){
    is.numeric(x) 
  }
  dplyr::mutate(x, date = format(.data$time, fmt)) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
}


#' Read raw ctd data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
ctd_read_raw <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., NaN)))
}

#' Fetch met data for a given buoy - saves monthly and yearly aggregates
#' 
#' @param buoy char, the buoy id
#' @return tibble of data 
fetch_buoy_ctd = function(buoy = "B01"){
  
  tmpfile = tempfile()
  uri = ctd_query_uri(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  ctd_read_raw(tmpfile) |>
    mask_qc() |>
    drop_suffix()
}

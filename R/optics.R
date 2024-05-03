query_uri_optics = function(buoyid = "$BUOYID", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_optics_all.csv?station%2Ctime%2Cwater_depth%2Csolar_zenith_angle%2CEd_PAR%2Cchlorophyll&time%3E=2023-04-10T00%3A00%3A00Z&time%3C=2023-04-17T14%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_optics_hist.csv", buoyid)
  query = c("station", "time", "water_depth",
            "solar_zenith_angle",
            "solar_zenith_angle_qc",
            "Ed_PAR", 
            "Ed_PAR_qc", 
            "chlorophyll",
            "chlorophyll_qc")
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

#' Aggregate optics by the user specifed interval
#' @param x tibble of data
#' @param by cahr the interval over which to aggregate
#' @return aggregated pixel
aggregate_optics = function(x, by = c("month", "year")[1]){
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  
  dplyr::mutate(x, date = format(.data$time, fmt)) |>
    dplyr::group_by(date, .data$water_depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
}


#' Read raw optics data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
read_raw_optics <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., NaN)))
}

#' Fetch met data for a given buoy (aggregates by year and month)
#' 
#' @param buoy char, the buoy id
#' @return tibble of data (monthly)
fetch_buoy_optics= function(buoy = "B01"){
  
  
  tmpfile = tempfile()
  
  uri = query_uri_optics(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  read_raw_optics(tmpfile) |>
    mask_qc() |>
    drop_suffix()
}

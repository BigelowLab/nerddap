query_uri_adcp = function(buoyid = "$BUOYID", escape = TRUE){
  
  # http://www.neracoos.org/erddap/tabledap/B01_doppler_rt.csv?station%2Cwater_depth%2Ctime%2Cdepth%2Coffset_time%2Ccurrent_u%2Ccurrent_u_qc%2Ccurrent_v%2Ccurrent_v_qc&time%3E=2023-04-11T00%3A00%3A00Z&time%3C=2023-04-18T11%3A00%3A00Z
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_doppler_rt.csv", buoyid)
  query = c("station", "water_depth", "time", "depth",
            "current_u",
            "current_u_qc",
            "current_v",
            "current_v_qc")
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

#' Aggregate columns by by the interval specified by user
#'  
#' @param x tibble of data
#' @param by char the interval over which to aggregate
#' @return aggregated tibble
aggregate_adcp = function(x, by = c("month", "year")[1]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")  
  
  dplyr::mutate(x, date = format(.data$time, fmt)) |>
    dplyr::group_by(date, .data$water_depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
}




#' Read raw adcp data for a given buoy
#' 
#' @param filename char, the path to the data to read
#' @return tibble of data
read_raw_adcp <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., NaN)))
}

#' Fetch met data for a given buoy
#' 
#' @param buoy char, the buoy id
#' @return tibble of data
fetch_buoy_adcp= function(buoy = "B01"){
  
 
  tmpfile = tempfile()
  
  uri = query_uri_adcp(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok != 0) return(NULL)
  
  read_raw_adcp(tmpfile) |>
    mask_qc() |>
    drop_suffix()
}

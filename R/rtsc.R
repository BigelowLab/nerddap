query_uri_rtsc = function(buoyid = "$BUOYID", escape = TRUE){
  
  # http://www.neracoos.org/erddap/tabledap/B01_aanderaa_all.csv?station%2Ctime%2Ccurrent_speed%2Ccurrent_direction%2Ctemperature%2Cdepth&time%3E=2023-04-11T00%3A00%3A00Z&time%3C=2023-04-18T11%3A00%3A00Z
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_aanderaa_all.csv", buoyid)
  query = c("station", "time", 
            "current_speed",
            "current_speed_qc",
            "current_direction",
            "current_direction_qc",
            "temperature", 
            "temperature_qc",
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

#' Aggregate columns by by the interval specified by user
#'  
#' @export
#' @param x tibble of data
#' @param by char the interval over which to aggregate
#' @return aggregated tibble
aggregate_rtsc = function(x, by = c('year', 'month')[2]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  
  dplyr::mutate(x, date = format(.data$time, fmt)) |>
    dplyr::group_by(date, .data$depth) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~mean(., na.rm = TRUE)),
                     .groups = "keep")
}


#' Read raw rtsc data for a given buoy
#' 
#' @export
#' @param filename char, the path to the data to read
#' @return tibble of data
read_raw_rtsc <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., NaN))) |>
    decompose_direction("current_direction") 
}

#' Fetch met data for a given buoy
#' 
#' @export
#' @param buoy char, the buoy id
#' @return tibble of data
fetch_buoy_rtsc = function(buoy = "B01"){
  
  
  
  tmpfile = tempfile()
  uri = query_uri_rtsc(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  read_raw_rtsc(tmpfile) |>
    mask_qc() |>
    drop_suffix() 
}

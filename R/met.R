met_query_uri = function(buoyid = "B01", escape = TRUE){
  
  "http://www.neracoos.org/erddap/tabledap/B01_met_all.csv?station%2Ctime%2Cair_temperature%2Cbarometric_pressure%2Cwind_gust%2Cwind_speed%2Cwind_direction%2Cvisibility&time%3E=1980-01-01T00%3A00%3A00Z&time%3C=2023-04-13T13%3A30%3A00Z"
  base_uri = "http://www.neracoos.org/erddap/tabledap"
  name = sprintf("%s_met_all.csv", buoyid)
  query = c("station", "time", 
            "air_temperature",
            "air_temperature_qc",
            "barometric_pressure",
            "barometric_pressure_qc",
            "wind_gust", 
            "wind_gust_qc",
            "wind_speed", 
            "wind_speed_qc",
            "wind_direction", 
            "wind_direction_qc", 
            "visibility",
            "visibility_qc")
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
#' @export
#' @param x tibble of data
#' @param by char the interval overwich to aggregate
#' @return summary tibble
met_aggregate = function(x, by = c("month", "year")[1]){
  
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  # first two columns should be station and time
  ix <- sapply(x, is.numeric)
  PARAMS <- colnames(x)[ix]
  dplyr::mutate(x, date = format(.data$time, fmt)) |>
    dplyr::group_by(.data$station, .data$date) |>
    dplyr::group_map(
      function(tbl, key, params = NULL){
        v = lapply(params,
                   function(p){
                     v = sixnum(tbl |> dplyr::pull(dplyr::all_of(p))) |>
                       as.list() |>
                       dplyr::as_tibble()
                     names(v) <- paste(p, names(v), sep = ".")
                     v
                   }) #|>
        dplyr::bind_cols()
        # now add date and station
        tbl |> 
          dplyr::slice(1) |>
          dplyr::select(.data$station, .data$date) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE, params = PARAMS) |>
    dplyr::bind_rows()
  
}

#' Read raw met data for a given buoy
#' 
#' @export
#' @param filename char, the path to the data to read
#' @return tibble of data
met_read_raw <- function(filename){
  col_names <- strsplit(readLines(filename, n = 1), ",", fixed = TRUE)[[1]]
  readr::read_csv(filename,  col_names = col_names, skip = 2, show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::na_if(., NaN))) |>
    decompose_direction() 
}

#' Fetch met data for a given buoy
#' 
#' @export
#' @param buoy char, the buoy id
#' @return tibble of data
fetch_buoy_met = function(buoy = "B01"){
  
  
  tmpfile = tempfile()
  uri = met_query_uri(buoy)
  ok <- fetch_errdap_csv(uri, tmpfile)
  if (ok > 0) return(NULL)
  
  met_read_raw(tmpfile) |>
    mask_qc() |>
    drop_suffix()
}
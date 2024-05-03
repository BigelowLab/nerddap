# Accessing NERACOOS buoy data

#' Clip a table so that only complete intervals are present (for subsequent
#'  aggregation).  We count unique days in the interval, even though data may
#'  ne recorded much more frequently
#'
#' @param x tibble of buoy data
#' @param by character, one of "year" (default) or "month"
#' @param min_count numeric defaults to 364 or 365, but adjust to 28 or 29 for month
#' @return tibble clipped to include only complete intervals
complete_intervals_buoys = function(x, 
                                    by = c("year", "month")[1], 
                                    min_count = c("year" = 364, "month" = 28)[[by]]){
  
  fmt = switch(tolower(by[1]),
               "year" = "%Y-01-01",
               "month" = "%Y-%m-01")
  dplyr::mutate(x, interval_ = format(.data$time, fmt)) |>
    dplyr::group_by(.data$station, .data$interval_) |>
    dplyr::group_map(
      function(tbl, key){
        n_u = length(unique(as.Date(tbl$time)))
        if (n_u <= min_count){
          return(NULL)
        } else {
          return(tbl)
        }
      }, .keep = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::select(-dplyr::any_of("interval_"))
}


#' Returns a table of buoy metadata
#' 
#' @param filename the name of the file to read
#' @param form char, one of 'table' or 'sf'
#' @return tibble
buoy_lut = function(filename = system.file("buoy/buoy_listing.csv", package = "nerddap"),
                    form = c("table", "sf")[1]){
  x = readr::read_csv(filename, show_col_types = FALSE)
  if (tolower(form[1]) == 'sf'){
    x = sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  }
  x
}

#' Modify a timezone attribute on a POSIX object
#' 
#' @param x POSIX time (default current time)
#' @return input with timezone attribute modified
time_to_utc <- function(x = Sys.time()){
  attr(x, "tzone") <- "UTC"
  x
}

#' Decompose a direction to u,v vectors
#'
#' @param x tibble of data
#' @param varname char the name of the varoable to decompose
#' @return tibble with add varname_u and varname_v columns added
decompose_direction = function(x, varname = 'wind_direction'){
  
  d = pi*x[[varname]]/180
  u = sin(d)
  v = cos(d)
  newnames = paste(varname, c("u", "v"), sep = "_")
  dplyr::mutate(x,
                !!newnames[1] := u,
                !!newnames[2] := v)
}

#' Given a raw data table, for each 'name_qc' mask with a relacement 
#' wherever qc fails threshold
#'  
#' @param x tibble of raw data
#' @param threshold numeric records at or below this value are retained
#' @param suffix character the pattern to identify columns to be dropped
#' @param replacement numeric or NA, the value to substitute in where qc is bad
#' @return the input table masked
mask_qc <- function(x, threshold = 0, suffix = "_qc", replacement = NA_real_){
  nm = colnames(x)
  ix = grepl("_qc", nm, fixed = TRUE)
  if (!any(ix)) return(x)
  nameqc = nm[ix]
  name = gsub(suffix[1], "", nameqc, fixed = TRUE) 
  if (length(nameqc) > 0){
    for (i in seq_along(nameqc)){
      bad = x[[ nameqc[i] ]] > threshold
      x[[ name[i] ]][bad] <- replacement
    }
  }
  x
}

#' Fetch ERRDAP CSV file
#' 
#' @param x char a url for download
#' @param filename char, the destination for the file
#' @param ... other argument for \code{download.file}
#' @return 0 for success, non-zero for problems
fetch_errdap_csv <- function(x, filename = tempfile(), ...){
  
  r = httr::HEAD(x)
  if (httr::http_error(r)){
    warning("http error for ", x)
    return(1)
  }
  ok <- try(utils::download.file(x, filename, ...))
  if (inherits(ok, 'try-error')){
    print(ok)
    ok = 1
  }
  ok
}

#' Given a raw data table, drop 'name_qc' columns
#' 
#' @param x tibble of raw data
#' @param suffix character the pattern to identify columns to be dropped
#' @return the input table with selected columns dropped
drop_suffix <- function(x,  suffix = "_qc"){
  dplyr::select(x,-dplyr::any_of(dplyr::ends_with(suffix[1])))
}



#' Aggregate columns by interval
#' 
#' @param x tibble of data
#' @param by char the interval over which to aggregate
#' @return summary tibble 
aggregate_buoy = function(x, by = c("month", "year")[1]) {
  if (nrow(x) == 0) return(x)
  
  fmt = switch(tolower(by[1]),
               "month" = "%Y-%m-01",
               "year" = "%Y-01-01")
  # first two columns should be station and time
  cnames = colnames(x)
  has_depth = "depth" %in% cnames
  ix <- sapply(x, is.numeric) & !(cnames %in% c("depth", "water_depth"))
  groupies = c("station", "date", "depth", "water_depth")
  
  PARAMS <- colnames(x)[ix]
  dplyr::mutate(x, date = format(.data$time, fmt)) |>
    dplyr::group_by(dplyr::across(dplyr::any_of(groupies))) |>
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
          dplyr::select(dplyr::any_of(groupies)) |>
          dplyr::bind_cols(v)
      }, .keep = TRUE, params = PARAMS) |>
    dplyr::bind_rows()
}


#' Fetch buoy data for one or more datasets
#' 
#' @param buoy char, one or more buoy id (B01, etc)
#' @param what char, one or more datasets names
#' @return NULL
fetch_buoys <- function(buoy = buoy_lut()$id,
                        what = c("met", "ctd", "rtsc", "optics", "adcp")){
  
  if ("all" %in% what) what = c("met", "ctd", "rtsc", "optics", "adcp")
  cat("updating buoys:", paste(buoy, collapse = ", "), "\n") 
  for (wh in what){
    cat("updating", wh, "\n")
    switch(wh,
           "met" = lapply(buoy, fetch_buoy_met),
           'ctd' = lapply(buoy, fetch_buoy_ctd),
           "rtsc" = lapply(buoy, fetch_buoy_rtsc),
           "optics" = lapply(buoy, fetch_buoy_optics),
           "adcp" = lapply(buoy, fetch_buoy_adcp),
           stop("dataset not known:", wh) )
  }
  invisible(NULL)
}

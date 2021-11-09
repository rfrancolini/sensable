#' retrieve example file names
#'
#' @export
#' @return vector
example_filenames <- function(){
  x <- system.file(c("exampledata/Cape_liz_hobo.csv",
                     "exampledata/IAH-5.20.21_hobo.csv",
                     "exampledata/little_drisko_hobo.csv",
                     "exampledata/Marshall_hobo.csv"), package="sensable")
}


#' read sensor data files
#'
#' @export
#' @param filenames vector, the name of the files to be read in
#' @param sensor character, type of sensor to be analyzed, "temp", "PAR", "waves", "current"
#' @param raw logical, FALSE if passing QAQC'd data (default)
#' @param ... further arguments passed to \code{\link[hobotemp]{theme}}
#' @return tibble
read_sensor <- function(filenames = example_filenames(),
                          sensor = c("temp", "PAR", "waves", "current")[1],
                          raw = FALSE,
                          ...){

  if (raw == FALSE) {
    x <-filenames %>%
      lapply(function(i){readr::read_csv(i)}) %>%
      dplyr::bind_rows()
    }
  else {
  x <- switch(tolower(sensor[1]),
             "temp" = filenames %>%
                      lapply(function(i){hobotemp::read_hobotemp(i)}) %>%
                      dplyr::bind_rows(),
             "PAR" = filenames %>%
                    lapply(function(i){parXtreem::read_parXtreem(i)}) %>%
                    dplyr::bind_rows(),
             "waves" = "rene needs to code this too",
             "current" = "rene also needs to code this",
             stop("options for sensor are temp, PAR, waves, or current. what is ", sensor, "?"))
             }


  return(x)

}


#' graph sensor data files
#'
#' @export
#' @param x tibble of single data type
#' @param sensor character, type of sensor to be analyzed, "temp", "PAR", "waves", "current"
#' @param ... further arguments passed to \code{\link[ggplot2]{theme}}
#' @return ggplot
graph_sensor <- function(x = read_sensor(),
                        sensor = c("temp", "par", "waves", "current")[1],
                        ...){

  gg <- switch(tolower(sensor[1]),
              "temp" = hobotemp::draw_plot(x, facet = "Site"),
              "par" = parXtreem::draw_plot(x, facet = "Site"),
              "waves" = "rene needs to code this too",
              "current" = "rene also needs to code this",
              stop("options for sensor are temp, PAR, waves, or current. what is ", sensor, "?")
  )

  return(gg)

}

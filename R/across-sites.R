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
#' @param ... further arguments passed to \code{\link[hobotemp]{theme}}
#' @return tibble
read_sensor <- function(filenames = example_filenames(),
                          sensor = c("temp", "PAR", "waves", "current")[1],
                          ...){

  x <-filenames %>%
      lapply(function(i){hobotemp::read_hobotemp(i)}) %>%
      dplyr::bind_rows()

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
                        sensor = c("temp", "PAR", "waves", "current")[1],
                        ...){

  gg <- switch(tolower(sensor[1]),
              "temp" = hobotemp::draw_plot(x, facet = x$Site),
              "PAR" = "rene needs to code this",
              "waves" = "rene needs to code this too",
              "current" = "rene also needs to code this",
              stop("options for sensor are temp, PAR, waves, or current. what is ", sensor, "?")
  )

  return(gg)

}

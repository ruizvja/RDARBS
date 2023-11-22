#' Data from an experiment with continuous registration of displacement in an extended experimental chamber
#'
#' Data set exemplifying the organization of data for the last session of an experiment with water deliveries each 30 seconds in one of four dispensers. In this case, always in Dispenser 2, located in the center of left panel of chamber. 
#' Session lasted 36 minutes, with 72 deliveries. X-Y coordinates of rat's position was registered in frames of 0.20 seconds.
#'
#' @format A \code{data.frame} of 10800 rows and 3 columns.
#' \describe{
#'  \item{x}{Numeric values giving rat's position in X axis in frames of 0.2 seconds.}
#'  \item{y}{Numeric values giving rat's position in Y axis in frames of 0.2 seconds.}
#'  \item{cycle}{Numeric values indicating the corresponding cycle for each frame. In this example, 150 consecutives frames for each one of 72 cycles.}
#'  
#' }
#' @source Data obtained in an experiment. Results are published in a paper, which title is omitted for a blind review.
"xy_test1"
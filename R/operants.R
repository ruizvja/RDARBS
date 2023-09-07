#' Data from a session of a rat under a fixed interval 30 s
#'
#' The database contains two columns, one with the time of registration of experimental events and responses and another with a numeric code to identify the type of event registered over time. 
#'
#' @format A \code{data.frame} of 2006 rows and 2 columns.
#' \describe{
#'  \item{time}{Numeric values giving the moment of registration for each type of event.}
#'  \item{events}{Numeric values giving a numeric code to identify the type of registered event.}
#'  
#' }
#' @source Data from an experiment reported in <https://doi.org/10.1016/j.beproc.2022.104733>.
"operants"
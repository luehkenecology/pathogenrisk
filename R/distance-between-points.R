#' distance function
#'
#' @param points
#' @param p
#'
#' @return
#' @export
#'
#' @examples
minDist <- function(points, p){
  which.min(colSums((t(points) - p)^2))
}

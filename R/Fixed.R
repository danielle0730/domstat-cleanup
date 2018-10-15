#' @export
Fixed <- function(num, digits = 1) {
  return(format(round(num, digits), trim = TRUE, nsmall = digits))
}
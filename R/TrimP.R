#' @export
TrimP <- function(p) {
  if (!is.nan(p) & !is.na(p)) {
    if (p < 0.001) p.value <- "<0.001"
    else if (p > 0.999) p.value <- "1"
    else p.value <- Fixed(p, 3)
  } else {
    p.value <- ""
  }
  return(p.value)
}
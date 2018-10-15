#' @export
# returns % complete 
TextTable <- function(variable) {
  missing <- sum(is.na(variable))
  completed <- sum(!is.na(variable))
  n.records <- length(variable)
  completed <- paste(completed, " (",
                     Fixed(100 * (completed / n.records)), "%)",
                     sep = "")
  output <- data.frame(c("Completed", "Missing"), c(completed, missing))
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}
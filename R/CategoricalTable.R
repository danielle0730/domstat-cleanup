#' @export
# summary of categorical variable
# returns counts and frequency and missing info 
CategoricalTable <- function(variable) {
  missing <- sum(is.na(variable))
  counts <- table(variable)
  if (missing < length(variable)) {
    percentages <- Fixed(100 * prop.table(counts))
  } else {
    percentages <- rep(" ", length(counts))
  }
  frequencies <- paste(counts, " (", percentages, "%)", sep = "")
  levels <- names(counts)
  output <- data.frame(levels, frequencies)
  output <- rbind(output, c("Missing", missing))
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}
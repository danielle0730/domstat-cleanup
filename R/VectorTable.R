#' @export
# table of checked entires in vector variable ("check all that apply")
# returns count and frequencies per option 
VectorTable <- function(variables) {
  n.records <- dim(variables)[1]
  n.options <- dim(variables)[2]
  output <- data.frame(rep("", n.options), rep("", n.options))
  for (i in 1:n.options) {
    output[i, 1] <- unlist(strsplit(colnames(variables)[i], " \\| "))[2]
    count <- sum(variables[, i] == "CHECKED")
    percentage <- Fixed(100 * (count / n.records))
    output[i, 2] <- paste(count, " (", percentage, "%)", sep = "")
  }
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}
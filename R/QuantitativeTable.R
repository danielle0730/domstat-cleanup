#' Quantitative Table
#' 
#' Takes in a quantitative variable and returns summary statistics and information. 
#' 
#' @param x Quantitative variable
#' 
#' @export
# summary of quantitative variable
# returns 5 num summary and missing info 
QuantitativeTable <- function(variable) {
  missing <- sum(is.na(variable))
  variable <- variable[which(!is.na(variable))]
  if (length(variable) == 0) {
    output <- data.frame(c("Mean (SD)", "Median (Q1-Q3)", "Min-Max", "Missing"),
                         c(rep(" ", 3), missing))
  } else {
    mean <- Fixed(mean(variable))
    sd <- Fixed(sd(variable))
    if (length(variable) == 1) {
      sd <- " "
    }
    mean.sd <- paste(mean, " (", sd, ")", sep = "")
    median <- Fixed(median(variable))
    q1 <- Fixed(quantile(variable, 0.25))
    q3 <- Fixed(quantile(variable, 0.75))
    median.iqr <- paste(median, " (", q1, "-", q3, ")", sep = "")
    min <- Fixed(min(variable))
    max <- Fixed(max(variable))
    min.max <- paste(min, "-", max, sep = "")
    output <- data.frame(c("Mean (SD)", "Median (Q1-Q3)", "Min-Max", "Missing"),
                         c(mean.sd, median.iqr, min.max, missing))
  }
  colnames(output) <- c("COL.2", "COL.3")
  rownames(output) <- NULL
  return(output)
}
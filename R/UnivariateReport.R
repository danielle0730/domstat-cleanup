#' @export
# report calling on summary functions from above 
UnivariateReport <- function(data, dictionary) {
  output <- data.frame("", "", paste("(N=", dim(data)[1], ")", sep = ""))
  colnames(output) <- c("COL.1", "COL.2", "COL.3")
  for (i in 1:dim(dictionary)[1]) {
    if (dictionary[i, "type"] == "Vector") {
      index <- which(substr(colnames(data), 1,
                            nchar(dictionary[i, "label"]) + 3) ==
                       paste(dictionary[i, "label"], "| "))
      tbl <- VectorTable(data[, index])
    } else if (dictionary[i, "type"] == "Text") {
      tbl <- TextTable(data[, dictionary[i, "label"]])
    } else if (dictionary[i, "type"] == "Categorical") {
      tbl <- CategoricalTable(data[, dictionary[i, "label"]])
      remove <- NA
      for (j in 1:dim(tbl)[1]) {
        if (tbl[j, "COL.2"] == "[REMOVE]") {
          remove <- append(remove, j)
        }
      }
      remove <- remove[-1]
      if (length(remove) > 0) {
        tbl <- tbl[-remove, ]
      }
    } else if (dictionary[i, "type"] == "Quantitative") {
      tbl <- QuantitativeTable(data[, dictionary[i, "label"]])
    }
    label <- c(dictionary[i, "label"], rep("", 2))
    tbl <- data.frame(COL.1 = rep("", dim(tbl)[1]), tbl)
    tbl <- rbind(label, tbl)
    output <- rbind(output, tbl)
  }
  rownames(output) <- NULL
  return(output)
}
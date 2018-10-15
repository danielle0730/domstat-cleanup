#' Format a data frame according to data dictionary
#' 
#' This function takes in a data frame and formatted data dictionary. Formats
#' all entries and returns formatted data frame. 
#' 
#' @param x A data frame
#' @param y A data dictionary
#' 
#' @export
# formats data according to data dictionary, variable clean up according to each type 
FormatData <- function(data, dictionary) {
  # searching row wise per variable for clean up 
  for (i in 1:dim(dictionary)[1]) {
    variable <- dictionary[i, "variable"]
    type <- dictionary[i, "type"]
    label <- dictionary[i, "label"]
    values <- dictionary[i, "values"]
    
    if (type == "Text") {
      data[, variable] <- as.character(data[, variable])
      if ("" %in% data[, variable]) {
        data[which(data[, variable] == ""), variable] <- NA
      }
      colnames(data)[which(colnames(data) == variable)] <- label
      
    } else if (type == "Quantitative") {
      data[, variable] <- as.numeric(data[, variable])
      colnames(data)[which(colnames(data) == variable)] <- label
      
      # categorical/vector first identifying levels 
    } else if (type == "Categorical" | type == "Vector") {
      levels <- unlist(strsplit(values, " \\| "))
      pairs <- rep(NA, 2)
      for (j in 1:length(levels)) {
        level <- levels[j]
        first.comma <- regexpr(",", level)[1]
        code <- substr(level, 1, first.comma - 1)
        interpretation <- substr(level, first.comma + 2, nchar(level))
        pairs <- rbind(pairs, c(code, interpretation))
      }
      
      pairs <- pairs[-1, ]
      # categorical clean up 
      if (type == "Categorical") {
        replace <- factor(rep(NA, dim(data)[1]), levels = pairs[, 2],
                          exclude = NULL)
        for (j in 1:length(replace)) {
          if (data[j, variable] %in% pairs[, 1]) {
            index <- which(pairs[, 1] == data[j, variable])
            replace[j] <- pairs[index, 2]
          }
        }
        data[, variable] <- replace
        colnames(data)[which(colnames(data) == variable)] <- label
        
      } else if (type == "Vector") {
        # vector - identifying levels checked and unchecked 
        columns <- which(substr(colnames(data), 1, nchar(variable)) == variable)
        for (j in 1:length(columns)) {
          replace <- factor(rep(NA, dim(data)[1]),
                            levels = c("CHECKED", "UNCHECKED"), exclude = NULL)
          for (k in 1:length(replace)) {
            if (data[k, columns[j]] == 1) {
              replace[k] <- "CHECKED"
            } else if (data[k, columns[j]] == 0) {
              replace[k] <- "UNCHECKED"
            }
          }
          
          data[, columns[j]] <- replace
          this.name <- colnames(data)[columns[j]]
          code <- substr(this.name, nchar(variable) + 4, nchar(this.name))
          index <- which(pairs[, 1] == code)
          colnames(data)[columns[j]] <- paste(label, " | ",
                                              pairs[index, 2], sep = "")
        }
      }
    }
  }
  rownames(data) <- NULL
  return(data)
}
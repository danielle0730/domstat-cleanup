#' @export
# summary with stratifying variable 
# perform comparison tests 
BivariateReport <- function(data, dictionary, variable) {
  stratifier <- data[, variable]
  reduced.dictionary <- dictionary[-which(dictionary[, "label"] == variable), ]
  split.data <- data[which(stratifier == levels(stratifier)[1]), ]
  output <- UnivariateReport(split.data, reduced.dictionary)
  for (i in 2:length(levels(stratifier))) {
    split.data <- data[which(stratifier == levels(stratifier)[i]), ]
    split.report <- UnivariateReport(split.data, reduced.dictionary)
    output <- data.frame(output, split.report[, "COL.3"])
  }
  p <- rep("", dim(output)[1])
  for (i in 1:dim(output)[1]) {
    p.value <- NA
    if (!is.na(output[i, "COL.1"]) & output[i, "COL.1"] != "") {
      type <- dictionary[which(dictionary[, "label"] == output[i, "COL.1"]),
                         "type"]
      
    subtype <- dictionary[which(dictionary[, "label"] == output[i, "COL.1"]),
                          "subtype"]
      if (type == "Quantitative" & subtype == "Ordinal") {
        main.variable <- data[, output[i, "COL.1"]]
        test <- tryCatch(kruskal.test(main.variable ~ stratifier),
                         error = function(error) "N/A",
                         warning = function(warning) "N/A")
        if (!("N/A" %in% test)) {
          p.value <- kruskal.test(main.variable ~ stratifier)[["p.value"]]
          p[i] <- TrimP(p.value)
        }
      } else if(type == "Quantitative" & subtype == "Ratio"){
        main.variable <- data[, output[i, "COL.1"]] + 1 
        test <- tryCatch(anova(lm(log(main.variable) ~ stratifier)),
                         error = function(error) "N/A",
                         warning = function(warning) "N/A")
        if (!("N/A" %in% test)) {
          p.value <- anova(lm(main.variable ~ stratifier))[1, "Pr(>F)"]
          p[i] <- TrimP(p.value)
        }
      } else if (type == "Quantitative" & subtype == "Interval"){
        main.variable <- data[, output[i, "COL.1"]]
        test <- tryCatch(anova(lm(main.variable ~ stratifier)),
                         error = function(error) "N/A",
                         warning = function(warning) "N/A")
        if (!("N/A" %in% test)) {
          p.value <- anova(lm(main.variable ~ stratifier))[1, "Pr(>F)"]
          p[i] <- TrimP(p.value)
        }
      } else if (type %in% c("Categorical", "Vector")) {
        columns <- which(substr(colnames(data), 1, nchar(output[i, "COL.1"])) ==
                         output[i, "COL.1"])
        for (j in 1:length(columns)) {
          main.variable <- data[, columns[j]]
          tbl <- table(main.variable, stratifier)
          expected <- (rowSums(tbl) %*% t(colSums(tbl))) / sum(tbl)
          if (sum(expected < 5, na.rm = TRUE) == 0) {
            test <- tryCatch(chisq.test(tbl),
                             error = function(error) "N/A",
                             warning = function(warning) "N/A")
            if (!("N/A" %in% test)) {
              p.value <- chisq.test(tbl)[["p.value"]]
            }
          } else {
            test <- tryCatch(fisher.test(tbl, simulate.p.value = TRUE,
                                         B = 100000),
                             error = function(error) "N/A",
                             warning = function(warning) "N/A")
            if (!("N/A" %in% test)) {
              p.value <- fisher.test(tbl, simulate.p.value = TRUE,
                                     B = 100000)[["p.value"]]
            }
          }
          if (type == "Categorical") {
            p[i] <- TrimP(p.value) 
          } else if (type == "Vector") {
            p[i + j] <- TrimP(p.value)
          }       
        }
      }
    }
  }
  output <- data.frame(output, p)
  output <- rbind(c(rep("", 2), levels(stratifier), "P-value"), output)
  output <- rbind(c(rep("", 2), variable,
                    rep("", length(levels(stratifier)))), output)
  colnames(output) <- paste("COL.", 1:(length(levels(stratifier)) + 3),
                            sep = "")
  rownames(output) <- NULL
  return(output)
}

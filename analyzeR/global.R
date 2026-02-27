app_version <- "1.1.0"

# Load necessary packages
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(tools)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(ggdark)

# ML packages (install if missing):
#   install.packages(c("rpart", "randomForest", "e1071", "class"))
library(rpart)
library(randomForest)
library(e1071)
library(class)


# Custom summary function
custom_summary <- function(df) {
  summary_df <- data.frame(
    Min = sapply(df, function(x) if(is.numeric(x)) min(x, na.rm = TRUE) else NA),
    `1st Qu.` = sapply(df, function(x) if(is.numeric(x)) quantile(x, 0.25, na.rm = TRUE) else NA),
    Median = sapply(df, function(x) if(is.numeric(x)) median(x, na.rm = TRUE) else NA),
    Mean = sapply(df, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
    `3rd Qu.` = sapply(df, function(x) if(is.numeric(x)) quantile(x, 0.75, na.rm = TRUE) else NA),
    Max = sapply(df, function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else NA),
    `Null Count` = sapply(df, function(x) sum(is.na(x)))
  )
  rownames(summary_df) <- names(df)
  return(summary_df)
}

# Source module files
source("modules/loadR.R")
source("modules/readR.R")
source("modules/cleanR.R")
source("modules/analyzeR.R")
source("modules/plotR.R")
source("modules/removeR.R")
source("modules/learnR.R")

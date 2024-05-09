packages <- c("quantmod","TTR","jsonlite","tidyverse","fPortfolio",
              "corrplot","arules","rfm","caret","knitr",
              "tidyquant","timetk","tibbletime","tidymodels")
update.packages(packages)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

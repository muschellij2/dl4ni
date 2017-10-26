suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(dl4ni))

write.table(test_check("dl4ni"), "test_results.csv")

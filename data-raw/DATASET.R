## code to prepare `DATASET` dataset goes here

example_stem <- read.csv('data-raw/sampleTree.csv')
usethis::use_data(DATASET, overwrite = TRUE)

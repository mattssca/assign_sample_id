## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(AssignSampleIDs)

## ----test_data----------------------------------------------------------------
test_data = AssignSampleIDs::test_data
head(test_data)

## ----assign_sample_ids--------------------------------------------------------
result <- assign_sample_id(
  this_data = test_data,
  start_id = 100,
  lab_id_col = "lab_id",
  personal_id_col = "personal_id",
  date_col = "date_of_sample",
  verbose = TRUE
)

head(result)


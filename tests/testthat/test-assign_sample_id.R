library(testthat)
library(dplyr)

test_that("assign_sample_id assigns unique sample IDs", {
  test_data <- tibble(
    lab_id = c("45KF98987", "13KF85140", "82KF36719"),
    personal_id = c("19037644-5806", "19053341-4128", "19053341-4128"),
    date_of_sample = c("2021-08-20", "2018-09-15", "2024-03-12")
  )

  result <- assign_sample_id(
    this_data = test_data,
    start_id = 100
  )

  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$sample_id)))
  expect_equal(length(unique(result$sample_id)), 3)
})

test_that("assign_sample_id handles duplicate samples correctly", {
  test_data <- tibble(
    lab_id = c("45KF98987", "13KF85140", "13KF85140"),
    personal_id = c("19037644-5806", "19053341-4128", "19053341-4128"),
    date_of_sample = c("2021-08-20", "2018-09-15", "2018-09-15")
  )

  result <- assign_sample_id(
    this_data = test_data,
    start_id = 100
  )

  expect_equal(nrow(result), 3)
  expect_true(any(grepl("_1", result$sample_id)))
})

test_that("assign_sample_id assigns tumor annotations correctly", {
  test_data <- tibble(
    lab_id = c("45KF98987", "13KF85140", "82KF36719"),
    personal_id = c("19037644-5806", "19053341-4128", "19053341-4128"),
    date_of_sample = c("2021-08-20", "2018-09-15", "2024-03-12")
  )

  result <- assign_sample_id(
    this_data = test_data,
    start_id = 100
  )

  expect_true(any(grepl("_B", result$sample_id)))
})

test_that("assign_sample_id validates input columns", {
  test_data <- tibble(
    lab_id = c("45KF98987", "13KF85140", "82KF36719"),
    personal_id = c("19037644-5806", "19053341-4128", "19053341-4128")
    # Missing date_of_sample column
  )

  expect_error(
    assign_sample_id(
      this_data = test_data,
      start_id = 100
    ),
    "One or more specified columns do not exist in the incoming data."
  )
})

test_that("assign_sample_id handles missing start_id", {
  test_data <- tibble(
    lab_id = c("45KF98987", "13KF85140", "82KF36719"),
    personal_id = c("19037644-5806", "19053341-4128", "19053341-4128"),
    date_of_sample = c("2021-08-20", "2018-09-15", "2024-03-12")
  )

  expect_error(
    assign_sample_id(
      this_data = test_data
    ),
    "No starting sample_id provided"
  )
})

test_that("assign_sample_id returns the correct columns", {
  test_data <- tibble(
    lab_id = c("45KF98987", "13KF85140", "82KF36719"),
    personal_id = c("19037644-5806", "19053341-4128", "19053341-4128"),
    date_of_sample = c("2021-08-20", "2018-09-15", "2024-03-12")
  )

  result <- assign_sample_id(
    this_data = test_data,
    start_id = 100
  )

  expect_true(all(c("lab_id", "personal_id", "date_of_sample", "sample_id") %in% colnames(result)))
})


library(testthat)

test_that("generate_test_data creates the correct number of samples", {
  num_samples <- 100
  result <- generate_test_data(
    num_samples = num_samples,
    num_unique_personal_ids = 10,
    num_unique_dates = 5,
    samples_per_combination = 2
  )

  expect_equal(nrow(result), num_samples)
})

test_that("generate_test_data returns a data frame", {
  result <- generate_test_data(
    num_samples = 50,
    num_unique_personal_ids = 5,
    num_unique_dates = 3,
    samples_per_combination = 2
  )

  expect_s3_class(result, "data.frame")
})

test_that("generate_test_data includes required columns", {
  result <- generate_test_data(
    num_samples = 50,
    num_unique_personal_ids = 5,
    num_unique_dates = 3,
    samples_per_combination = 2
  )

  expect_true(all(c("lab_id", "personal_id", "date_of_sample") %in% colnames(result)))
})

test_that("generate_test_data handles more samples than combinations", {
  result <- generate_test_data(
    num_samples = 200,
    num_unique_personal_ids = 5,
    num_unique_dates = 3,
    samples_per_combination = 2
  )

  expect_equal(nrow(result), 200)
})

test_that("generate_test_data handles fewer samples than combinations", {
  result <- generate_test_data(
    num_samples = 10,
    num_unique_personal_ids = 5,
    num_unique_dates = 3,
    samples_per_combination = 2
  )

  expect_equal(nrow(result), 10)
})

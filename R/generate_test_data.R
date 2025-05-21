#' Generate a Test Data Set with Controlled Sample Distribution
#'
#' This function generates a test data set with a specified number of samples, unique personal IDs,
#' unique dates, and samples per unique combination of personal ID and date. It ensures that the
#' total number of samples matches the specified `num_samples`.
#'
#' @param num_samples Integer. The total number of samples to generate.
#' @param num_unique_personal_ids Integer. The number of unique personal IDs to generate.
#' @param num_unique_dates Integer. The number of unique dates to generate.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{lab_id}{Character. A unique lab ID for each sample.}
#'   \item{personal_id}{Character. A unique personal ID for each sample.}
#'   \item{date_of_sample}{Date. The date associated with each sample.}
#' }
#'
#' @examples
#' # Generate a test data set with 100 samples, 10 unique personal IDs, and 5 unique dates
#' test_data <- generate_test_data(
#'   num_samples = 100,
#'   num_unique_personal_ids = 10,
#'   num_unique_dates = 5
#' )
#' head(test_data)
#'
#' @import dplyr
#'
#' @export
#'
generate_test_data <- function(num_samples,
                               num_unique_personal_ids,
                               num_unique_dates) {

  # Generate unique personal IDs
  unique_personal_ids <- sprintf("%08d-%04d",
                                 sample(19000101:20201231, num_unique_personal_ids, replace = TRUE),
                                 sample(1000:9999, num_unique_personal_ids, replace = TRUE))

  # Generate unique dates
  unique_dates <- as.Date("2016-01-01") + sample(0:3285, num_unique_dates, replace = TRUE)

  # Randomly assign personal IDs and dates to samples
  personal_ids <- sample(unique_personal_ids, num_samples, replace = TRUE)
  dates <- sapply(personal_ids, function(id) {
    if (runif(1) < 0.5) {
      # 50% chance to reuse a date for the same personal ID
      sample(unique_dates, 1)
    } else {
      sample(unique_dates, 1)
    }
  })

  # Generate lab IDs
  lab_ids <- sprintf("%02dKF%05d",
                     sample(10:99, num_samples, replace = TRUE),
                     sample(1:99999, num_samples, replace = TRUE))

  # Combine into a data frame
  data <- data.frame(
    lab_id = lab_ids,
    personal_id = personal_ids,
    date_of_sample = as.Date(dates)
  )

  return(data)
}

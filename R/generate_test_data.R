#' Generate a Test Data Set with Controlled Sample Distribution
#'
#' This function generates a test data set with a specified number of samples, unique personal IDs, 
#' unique dates, and samples per unique combination of personal ID and date. It ensures that the 
#' total number of samples matches the specified `num_samples`.
#'
#' @param num_samples Integer. The total number of samples to generate.
#' @param num_unique_personal_ids Integer. The number of unique personal IDs to generate.
#' @param num_unique_dates Integer. The number of unique dates to generate.
#' @param samples_per_combination Integer. The number of samples to generate for each unique 
#' combination of personal ID and date.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{lab_id}{Character. A unique lab ID for each sample.}
#'   \item{personal_id}{Character. A unique personal ID for each sample.}
#'   \item{date_of_sample}{Date. The date associated with each sample.}
#' }
#'
#' @details The function generates a pool of unique personal IDs and dates, creates all possible 
#' combinations of these, and repeats each combination the specified number of times. If the total 
#' number of samples exceeds or falls short of `num_samples`, the function adjusts the data set 
#' accordingly by truncating or adding additional rows.
#'
#' @examples
#' # Generate a test data set with 100 samples, 10 unique personal IDs, 5 unique dates,
#' # and 2 samples per combination
#' test_data <- generate_test_data(
#'   num_samples = 100,
#'   num_unique_personal_ids = 10,
#'   num_unique_dates = 5,
#'   samples_per_combination = 2
#' )
#' head(test_data)
#'
#' @export
#' 
generate_test_data <- function(num_samples, 
                               num_unique_personal_ids, 
                               num_unique_dates, 
                               samples_per_combination){
  
  #generate a smaller pool of unique personal IDs and dates
  unique_personal_ids <- sprintf("%08d-%04d", sample(19000101:20201231, num_unique_personal_ids, replace = TRUE), sample(1000:9999, num_unique_personal_ids, replace = TRUE))
  unique_dates <- as.Date("2016-01-01") + sample(0:3285, num_unique_dates, replace = TRUE)
  
  #create all possible combinations of personal IDs and dates
  combinations <- expand.grid(personal_id = unique_personal_ids, date_of_sample = unique_dates)
  
  #repeat each combination the specified number of times
  repeated_combinations <- combinations[rep(1:nrow(combinations), each = samples_per_combination), ]
  
  #ensure the total number of samples matches the specified num_samples
  if (nrow(repeated_combinations) > num_samples) {
    repeated_combinations <- repeated_combinations[1:num_samples, ]
  } else if (nrow(repeated_combinations) < num_samples) {
    additional_rows <- num_samples - nrow(repeated_combinations)
    repeated_combinations <- rbind(
      repeated_combinations,
      repeated_combinations[sample(1:nrow(repeated_combinations), additional_rows, replace = TRUE), ]
    )
  }
  
  #generate lab IDs
  lab_ids <- sprintf("%02dKF%05d", sample(10:99, num_samples, replace = TRUE), sample(1:99999, num_samples, replace = TRUE))
  
  #combine into a data frame
  data <- data.frame(
    lab_id = lab_ids,
    personal_id = repeated_combinations$personal_id,
    date_of_sample = repeated_combinations$date_of_sample
  )
  
  return(data)
}

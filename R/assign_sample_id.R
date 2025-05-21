#' Assign Sample IDs and Annotate Tumor Information
#'
#' This function assigns unique sample IDs, annotates duplicates, and tracks tumor occurrences for each patient.
#' It also provides an option to print summary statistics about the data.
#'
#' @param this_data A data frame containing the input data.
#' @param start_id An integer specifying the starting number for the sample IDs. This is required.
#' @param lab_id_col A string specifying the column name for lab IDs. Default is `"lab_id"`.
#' @param personal_id_col A string specifying the column name for personal IDs. Default is `"personal_id"`.
#' @param date_col A string specifying the column name for the date of sample collection. Default is `"date_of_sample"`.
#' @param verbose A boolean indicating whether to print summary statistics about the data. Default is `TRUE`.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{lab_id}{The original lab ID column.}
#'   \item{personal_id}{The original personal ID column.}
#'   \item{date_of_sample}{The original date of sample collection column.}
#'   \item{sample_id}{The formatted sample ID column, which includes unique IDs, replicate numbers, and tumor annotations.}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Validates that the specified column names exist in the input data.
#'   \item Assigns unique sample IDs starting from the specified `start_id`.
#'   \item Annotates duplicate samples with a `sample_rep` column.
#'   \item Tracks tumor occurrences with a `tumor_n` column, assigning unique letters for distinct tumors.
#'   \item Replaces "A" in the `tumor_n` column with `NA` and adjusts the `sample_rep` column to start from 0.
#'   \item Creates a formatted sample ID by concatenating `sample_id`, `sample_rep`, and `tumor_n`.
#'   \item Optionally prints summary statistics, including the number of unique samples, replicates, and tumors.
#' }
#'
#' @examples
#' #load pacakges
#' library(tibble, dplyr)
#' 
#' # Example data
#' test_data <- tibble(
#'   lab_id = c("45KF98987", "13KF85140", "13KF85140", "82KF36719"),
#'   personal_id = c("19037644-5806", "19053341-4128", "19053341-4128", "19053341-4128"),
#'   date_of_sample = c("2021-08-20", "2018-09-15", "2018-09-15", "2024-03-12")
#' )
#'
#' # Run the function
#' result <- assign_sample_id(
#'   this_data = test_data,
#'   start_id = 100,
#'   lab_id_col = "lab_id",
#'   personal_id_col = "personal_id",
#'   date_col = "date_of_sample",
#'   verbose = TRUE
#' )
#'
#' @import dplyr tibble
#'
#' @export
#'
assign_sample_id = function(this_data = NULL,
                            start_id = NULL,
                            lab_id_col = "lab_id",
                            personal_id_col = "personal_id",
                            date_col = "date_of_sample",
                            verbose = TRUE){

  #check if the specified columns exist in the incoming data
  required_cols <- c(lab_id_col, personal_id_col, date_col)
  if(!all(required_cols %in% colnames(this_data))){
    stop("One or more specified columns do not exist in the incoming data.")
  }

  #checks
  if(is.null(start_id)){
    stop("No starting sample_id provided, the funciton does not know the sequence of sample IDs to adhere to...")
  }

  #create sample_id for each unique personal_id
  this_data <- this_data %>%
    mutate(sample_id = paste0("USQ_", sprintf("%05d", as.integer(factor(personal_id)) + start_id - 1)))

  #add a new column annotating duplicates
  this_data <- this_data %>%
    group_by(personal_id, date_of_sample) %>%
    mutate(sample_rep = row_number()) %>%
    ungroup()

  #add a new column tumor_n
  this_data <- this_data %>%
    group_by(personal_id) %>%
    arrange(date_of_sample) %>%
    mutate(tumor_n = LETTERS[row_number()]) %>%
    ungroup()

  #update tumor_n to ensure the same value for the same personal_id and date_of_sample
  this_data <- this_data %>%
    group_by(personal_id, date_of_sample) %>%
    mutate(tumor_n = first(tumor_n)) %>%
    ungroup()

  #update tumor_n to assign unique letters for distinct tumors and keep duplicates consistent
  this_data <- this_data %>%
    group_by(personal_id) %>%
    arrange(date_of_sample) %>%
    mutate(tumor_n = LETTERS[match(date_of_sample, unique(date_of_sample))]) %>%
    ungroup()

  #subtract 1 from all values in the sample_rep column
  this_data <- this_data %>%
    mutate(sample_rep = sample_rep - 1)

  #replace all "A" in tumor_n with NA
  this_data <- this_data %>%
    mutate(tumor_n = ifelse(tumor_n == "A", NA, tumor_n)) %>%
    mutate(sample_rep  = ifelse(sample_rep  == 0, NA, sample_rep ))

  #create the formatted_sample_id column
  this_data <- this_data %>%
    mutate(
      formatted_sample_id = paste(
        sample_id,
        ifelse(is.na(sample_rep), "", sample_rep),
        ifelse(is.na(tumor_n), "", tumor_n),
        sep = "_"
      ) %>%
        gsub("_$", "", .)
    )

  #clean up the formatted_sample_id column
  this_data <- this_data %>%
    mutate(
      formatted_sample_id = gsub("_$", "", formatted_sample_id),
      formatted_sample_id = gsub("_+", "_", formatted_sample_id)
    )

  if(verbose){
    num_unique_samples <- this_data %>%
      summarise(num_unique = n_distinct(sample_id)) %>%
      pull(num_unique)

    num_non_na_sample_rep <- this_data %>%
      filter(!is.na(sample_rep)) %>%
      nrow()

    num_sample_tumors <- this_data %>%
      summarise(tumor_unique = n_distinct(tumor_n)) %>%
      pull(tumor_unique)

    cat("Number of unique samples:", num_unique_samples, "\n")
    cat("Number of patients with replicates:", num_non_na_sample_rep, "\n")
    cat("Number of samples with multiple tumors:", num_sample_tumors, "\n")
  }

  #format the return
  this_data = this_data %>%
    select(lab_id, personal_id, date_of_sample, formatted_sample_id) %>%
    rename(sample_id = formatted_sample_id) %>%
    arrange(sample_id)

  return(this_data)
}

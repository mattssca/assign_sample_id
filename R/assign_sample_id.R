#' Assign Sample IDs and Annotate Tumor Information (Batch-aware, Robust Replicate and Tumor Tracking)
#'
#' This function assigns unique sample IDs, annotates replicates, and tracks tumor occurrences for each patient.
#' It supports batch processing by accepting a previous batch's results to ensure consistent sample IDs and tumor tracking across batches.
#'
#' @param this_data A data frame containing the input data.
#' @param start_id An integer specifying the starting number for the sample IDs. This is required.
#' @param lab_id_col A string specifying the column name for lab IDs.
#' @param personal_id_col A string specifying the column name for personal IDs.
#' @param date_col A string specifying the column name for the date of sample collection.
#' @param previous_batch A data frame from a previous batch (output of this function), or NULL.
#' @param verbose A boolean indicating whether to print summary statistics about the data. Default is TRUE.
#' @param return_full A boolean indicating if the return should have all columns, or only the concatenated sample ID column.
#'
#' @return A data frame with sample IDs and annotations.
#' @export
#'
assign_sample_id <- function(this_data = NULL,
                             start_id = NULL,
                             lab_id_col = NULL,
                             personal_id_col = NULL,
                             date_col = NULL,
                             previous_batch = NULL,
                             verbose = TRUE,
                             return_full = FALSE){

  #remove rows with NA or unexpected values in required columns
  invalid_rows <- this_data %>%
    dplyr::filter(
      is.na(.data[[lab_id_col]]) | is.na(.data[[personal_id_col]]) | is.na(.data[[date_col]]) |
        !grepl("^\\d{8}-\\d{4}$", as.character(.data[[personal_id_col]])) |
        !grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(.data[[date_col]])) |
        is.na(as.Date(as.character(.data[[date_col]]), format = "%Y-%m-%d"))
    )

  if(nrow(invalid_rows) > 0){
    message("The following samples were disregarded due to NA or unexpected values in required columns:")
    print(invalid_rows)
  }

  #keep only valid rows
  this_data <- this_data %>%
    dplyr::filter(
      !is.na(.data[[lab_id_col]]),
      !is.na(.data[[personal_id_col]]),
      !is.na(.data[[date_col]]),
      grepl("^\\d{8}-\\d{4}$", as.character(.data[[personal_id_col]])),
      grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(.data[[date_col]])),
      !is.na(as.Date(as.character(.data[[date_col]]), format = "%Y-%m-%d"))
    )

  #notify user if the function is aware of a previous batch
  if(!is.null(previous_batch)){
    message("WARNING! previous_batch is provided, the assignment of sample IDs will be in respect to
            the provided object in previous_batch....")
  }

  #input checks
  if(is.null(lab_id_col)) stop("User must provide a column name annotating lab IDs...")
  if(is.null(personal_id_col)) stop("User must provide a column name annotating personal IDs...")
  if(is.null(date_col)) stop("User must provide a column name annotating dates...")
  required_cols <- c(lab_id_col, personal_id_col, date_col)
  if(!all(required_cols %in% colnames(this_data))){
    stop("One or more specified columns do not exist in the incoming data.")
  }
  date_vals <- as.character(this_data[[date_col]])
  if(!all(grepl("^\\d{4}-\\d{2}-\\d{2}$", date_vals))){
    stop("Date column must be in YYYY-MM-DD format, e.g., 2025-06-02.")
  }
  if(any(is.na(as.Date(date_vals, format = "%Y-%m-%d")))){
    stop("Date column contains invalid dates that do not match YYYY-MM-DD format.")
  }
  if(is.null(start_id)){
    stop("No starting sample_id provided, the function does not know the sequence of sample IDs to adhere to...")
  }

  #order by date
  this_data <- this_data %>% dplyr::arrange(.data[[date_col]])

  #build mapping from previous batch if provided
  if(!is.null(previous_batch)){
    mapping <- previous_batch %>%
      dplyr::select(personal_id, sample_num, sample_id) %>%
      dplyr::distinct()
    max_id <- max(mapping$sample_num, na.rm = TRUE)
    this_data <- this_data %>%
      dplyr::left_join(mapping, by = setNames("personal_id", personal_id_col))
  }else{
    mapping <- this_data %>%
      dplyr::group_by(.data[[personal_id_col]]) %>%
      dplyr::summarise(first_date = min(.data[[date_col]]), .groups = "drop") %>%
      dplyr::arrange(first_date) %>%
      dplyr::mutate(sample_num = dplyr::row_number() + start_id - 1) %>%
      dplyr::mutate(sample_id = paste0("USQ_", sprintf("%05d", sample_num))) %>%
      dplyr::select(personal_id = !!personal_id_col, sample_num, sample_id) %>%
      dplyr::rename(!!personal_id_col := personal_id)
    max_id <- max(mapping$sample_num, na.rm = TRUE)
    this_data <- this_data %>%
      dplyr::left_join(mapping, by = personal_id_col)
  }

  #find new personal_ids in this batch not in mapping
  if(!is.null(previous_batch)){
    new_ids <- setdiff(unique(this_data[[personal_id_col]][is.na(this_data$sample_num)]), mapping$personal_id)
  }else{
    new_ids <- setdiff(unique(this_data[[personal_id_col]][is.na(this_data$sample_num)]), mapping[[personal_id_col]])
  }
  if(length(new_ids) > 0){
    new_mapping <- tibble::tibble(
      personal_id = new_ids,
      sample_num = seq(max_id + 1, by = 1, length.out = length(new_ids))
    ) %>%
      dplyr::mutate(sample_id = paste0("USQ_", sprintf("%05d", sample_num)))
    mapping <- dplyr::bind_rows(mapping, new_mapping)
    this_data <- this_data %>%
      dplyr::select(-sample_num, -sample_id) %>%
      dplyr::left_join(mapping, by = setNames("personal_id", personal_id_col))
  }

  #assign sample_rep (replicates: 0, 1, 2, ...)
  this_data <- this_data %>%
    dplyr::group_by(.data[[personal_id_col]], .data[[date_col]]) %>%
    dplyr::mutate(sample_rep = dplyr::row_number() - 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sample_rep = ifelse(sample_rep == 0, NA, sample_rep))

  #assign tumor_n across batches (dynamic column names, one per unique date)
  if(!is.null(previous_batch)){
    prev_for_tumor <- previous_batch %>%
      dplyr::select(
        personal_id,
        date_of_sample
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(batch_flag = "prev")
    curr_for_tumor <- this_data %>%
      dplyr::select(
        personal_id = !!personal_id_col,
        date_of_sample = !!date_col
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(batch_flag = "curr")
    tumor_df <- dplyr::bind_rows(prev_for_tumor, curr_for_tumor) %>%
      dplyr::arrange(personal_id, date_of_sample) %>%
      dplyr::group_by(personal_id) %>%
      dplyr::mutate(tumor_n = LETTERS[dplyr::row_number()]) %>%
      dplyr::ungroup() %>%
      dplyr::filter(batch_flag == "curr") %>%
      dplyr::select(personal_id, date_of_sample, tumor_n)
    this_data <- this_data %>%
      dplyr::left_join(
        tumor_df,
        by = c(
          setNames("personal_id", personal_id_col),
          setNames("date_of_sample", date_col)
        )
      )
  }else{
    #for first batch, assign tumor_n per unique date per patient
    tumor_df <- this_data %>%
      dplyr::distinct(.data[[personal_id_col]], .data[[date_col]]) %>%
      dplyr::group_by(.data[[personal_id_col]]) %>%
      dplyr::arrange(.data[[date_col]], .by_group = TRUE) %>%
      dplyr::mutate(tumor_n = LETTERS[dplyr::row_number()]) %>%
      dplyr::ungroup()
    this_data <- tumor_df %>%
      dplyr::right_join(this_data, by = c(personal_id_col, date_col))
  }

  #set tumor_n to NA for the first tumor (A)
  this_data <- this_data %>%
    dplyr::mutate(tumor_n = ifelse(tumor_n == "A", NA, tumor_n))

  #format sample_id
  this_data <- this_data %>%
    dplyr::mutate(
      formatted_sample_id = paste(
        sample_id,
        ifelse(is.na(sample_rep), "", sample_rep),
        ifelse(is.na(tumor_n), "", tumor_n),
        sep = "_"
      ) %>% gsub("_$", "", .)
    ) %>%
    dplyr::mutate(
      formatted_sample_id = gsub("_$", "", formatted_sample_id),
      formatted_sample_id = gsub("_+", "_", formatted_sample_id)
    )

  if(verbose){
    num_unique_samples <- this_data %>%
      dplyr::summarise(num_unique = dplyr::n_distinct(sample_id)) %>%
      dplyr::pull(num_unique)
    num_non_na_sample_rep <- this_data %>%
      dplyr::filter(!is.na(sample_rep)) %>%
      nrow()
    num_sample_tumors <- this_data %>%
      dplyr::summarise(tumor_unique = dplyr::n_distinct(tumor_n)) %>%
      dplyr::pull(tumor_unique)
    cat("Number of unique samples:", num_unique_samples, "\n")
    cat("Number of patients with replicates:", num_non_na_sample_rep, "\n")
    cat("Number of samples with multiple tumors:", num_sample_tumors, "\n")
  }

  #remove existing columns that would conflict with the new names
  conflict_cols <- c("lab_id", "personal_id", "date_of_sample")
  conflict_cols <- conflict_cols[conflict_cols %in% names(this_data) & !(conflict_cols %in% c(lab_id_col, personal_id_col, date_col))]
  if(length(conflict_cols) > 0){
    this_data <- this_data %>% dplyr::select(-dplyr::all_of(conflict_cols))
  }

  #format the return
  if(return_full){
    this_data <- this_data %>%
      dplyr::rename(
        lab_id = !!lab_id_col,
        personal_id = !!personal_id_col,
        date_of_sample = !!date_col
      ) %>%
      dplyr::select(lab_id, personal_id, date_of_sample, sample_num, sample_id, formatted_sample_id, tumor_n, sample_rep) %>%
      dplyr::arrange(date_of_sample)
  }else{
    this_data <- this_data %>%
      dplyr::rename(
        lab_id = !!lab_id_col,
        personal_id = !!personal_id_col,
        date_of_sample = !!date_col
      ) %>%
      dplyr::select(lab_id, personal_id, date_of_sample, sample_num, formatted_sample_id) %>%
      dplyr::rename(sample_id = formatted_sample_id) %>%
      dplyr::arrange(date_of_sample)
  }

  attr(this_data, "invalid_rows") <- invalid_rows
  return(this_data)
}

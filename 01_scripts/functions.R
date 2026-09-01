# functions.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(rlang)
})


#### Standardize and validate user-defined columns ####
prepare_diet_data <- function(df,
                              metadata_cols,
                              diet_cols,
                              ignore_cols = NULL,
                              id_col = NULL,
                              clean_names = TRUE) {
  
  df <- as.data.frame(df)
  
  if (clean_names) {
    names(df) <- names(df) |>
      trimws() |>
      tolower() |>
      gsub("[^a-z0-9]+", "_", x = _) |>
      gsub("_+", "_", x = _) |>
      gsub("^_|_$", "", x = _)
  }
  
  metadata_cols <- intersect(metadata_cols, names(df))
  diet_cols     <- intersect(diet_cols, names(df))
  ignore_cols   <- intersect(ignore_cols %||% character(0), names(df))
  
  if (length(diet_cols) == 0) {
    stop("No diet columns were found in the dataset.")
  }
  
  if (length(metadata_cols) == 0) {
    warning("No metadata columns were supplied or found.")
  }
  
  selected_cols <- unique(c(metadata_cols, diet_cols, ignore_cols))
  out <- df[, selected_cols, drop = FALSE]
  
  if (!is.null(id_col)) {
    id_col <- tolower(gsub("[^a-z0-9]+", "_", id_col))
    if (!id_col %in% names(out)) {
      stop("id_col was not found in the data.")
    }
  } else {
    out$.stomach_id <- seq_len(nrow(out))
    id_col <- ".stomach_id"
  }
  
  diet_df <- out |>
    mutate(across(all_of(diet_cols), ~ suppressWarnings(as.numeric(.x))))
  
  attr(diet_df, "metadata_cols") <- metadata_cols
  attr(diet_df, "diet_cols") <- diet_cols
  attr(diet_df, "ignore_cols") <- ignore_cols
  attr(diet_df, "id_col") <- id_col
  
  return(diet_df)
}

#### Convert wide prey columns to long format ####
pivot_diet_long <- function(df,
                            diet_cols = attr(df, "diet_cols"),
                            id_col   = attr(df, "id_col"),
                            metadata_cols = attr(df, "metadata_cols"),
                            prey_col_name = "prey_item",
                            value_col_name = "value",
                            remove_zero = TRUE,
                            remove_na = TRUE) {
  
  if (is.null(diet_cols) || length(diet_cols) == 0) {
    stop("diet_cols must be supplied or stored in the object attributes.")
  }
  
  keep_cols <- unique(c(id_col, metadata_cols, diet_cols))
  keep_cols <- keep_cols[keep_cols %in% names(df)]
  
  long_df <- df |>
    select(all_of(keep_cols)) |>
    pivot_longer(
      cols = all_of(diet_cols),
      names_to = prey_col_name,
      values_to = value_col_name
    )
  
  if (remove_na) {
    long_df <- long_df |> filter(!is.na(.data[[value_col_name]]))
  }
  
  if (remove_zero) {
    long_df <- long_df |> filter(.data[[value_col_name]] > 0)
  }
  
  return(long_df)
}


#### Percentage of volume (V%) ####
calc_v_percent <- function(df,
                           group_cols = NULL,
                           diet_cols = attr(df, "diet_cols"),
                           id_col = attr(df, "id_col")) {
  
  long_df <- pivot_diet_long(df, diet_cols = diet_cols, id_col = id_col)
  
  if (is.null(group_cols)) group_cols <- character(0)
  
  result <- long_df |>
    group_by(across(all_of(c(group_cols, "prey_item")))) |>
    summarise(total_volume = sum(value, na.rm = TRUE), .groups = "drop_last") |>
    mutate(group_total_volume = sum(total_volume, na.rm = TRUE),
           v_percent = ifelse(group_total_volume > 0,
                              100 * total_volume / group_total_volume,
                              NA_real_)) |>
    ungroup()
  
  return(result)
}

#### Frequency of occurrence (FO%) ####
calc_fo_percent <- function(df,
                            group_cols = NULL,
                            diet_cols = attr(df, "diet_cols"),
                            id_col = attr(df, "id_col")) {
  
  if (is.null(group_cols)) group_cols <- character(0)
  
  long_df <- pivot_diet_long(df, diet_cols = diet_cols, id_col = id_col)
  
  total_stomachs <- df |>
    group_by(across(all_of(group_cols))) |>
    summarise(n_stomachs = n_distinct(.data[[id_col]]), .groups = "drop")
  
  prey_occ <- long_df |>
    group_by(across(all_of(c(group_cols, "prey_item")))) |>
    summarise(occurrence = n_distinct(.data[[id_col]]), .groups = "drop")
  
  result <- prey_occ |>
    left_join(total_stomachs, by = group_cols) |>
    mutate(fo_percent = ifelse(n_stomachs > 0,
                               100 * occurrence / n_stomachs,
                               NA_real_))
  
  return(result)
}

##### IAi = (FO% * V%) / sum(FO% * V%) * 100 ####
calc_iai <- function(df,
                     group_cols = NULL,
                     diet_cols = attr(df, "diet_cols"),
                     id_col = attr(df, "id_col")) {
  
  if (is.null(group_cols)) group_cols <- character(0)
  
  v_tbl  <- calc_v_percent(df, group_cols = group_cols, diet_cols = diet_cols, id_col = id_col)
  fo_tbl <- calc_fo_percent(df, group_cols = group_cols, diet_cols = diet_cols, id_col = id_col)
  
  result <- v_tbl |>
    select(all_of(group_cols), prey_item, v_percent) |>
    left_join(
      fo_tbl |> select(all_of(group_cols), prey_item, fo_percent),
      by = c(group_cols, "prey_item")
    ) |>
    mutate(fo_v_product = fo_percent * v_percent) |>
    group_by(across(all_of(group_cols))) |>
    mutate(
      sum_fo_v = sum(fo_v_product, na.rm = TRUE),
      iai = ifelse(sum_fo_v > 0, 100 * fo_v_product / sum_fo_v, NA_real_)
    ) |>
    ungroup()
  
  return(result)
}

# IRI = (N% + V%) * FO%
##### If count data are absent, n_percent defaults to 0 ####
calc_iri <- function(df,
                     group_cols = NULL,
                     diet_cols = attr(df, "diet_cols"),
                     count_cols = NULL,
                     id_col = attr(df, "id_col")) {
  
  if (is.null(group_cols)) group_cols <- character(0)
  
  v_tbl  <- calc_v_percent(df, group_cols = group_cols, diet_cols = diet_cols, id_col = id_col)
  fo_tbl <- calc_fo_percent(df, group_cols = group_cols, diet_cols = diet_cols, id_col = id_col)
  
  if (is.null(count_cols)) {
    n_tbl <- v_tbl |>
      select(all_of(group_cols), prey_item) |>
      mutate(n_percent = 0)
  } else {
    count_long <- pivot_diet_long(
      df,
      diet_cols = count_cols,
      id_col = id_col,
      metadata_cols = attr(df, "metadata_cols"),
      prey_col_name = "prey_item",
      value_col_name = "count_value"
    )
    
    n_tbl <- count_long |>
      group_by(across(all_of(c(group_cols, "prey_item")))) |>
      summarise(total_n = sum(count_value, na.rm = TRUE), .groups = "drop_last") |>
      mutate(group_total_n = sum(total_n, na.rm = TRUE),
             n_percent = ifelse(group_total_n > 0,
                                100 * total_n / group_total_n,
                                NA_real_)) |>
      ungroup()
  }
  
  result <- v_tbl |>
    select(all_of(group_cols), prey_item, v_percent) |>
    left_join(
      fo_tbl |> select(all_of(group_cols), prey_item, fo_percent),
      by = c(group_cols, "prey_item")
    ) |>
    left_join(
      n_tbl |> select(all_of(group_cols), prey_item, n_percent),
      by = c(group_cols, "prey_item")
    ) |>
    mutate(
      n_percent = coalesce(n_percent, 0),
      iri = (n_percent + v_percent) * fo_percent
    ) |>
    group_by(across(all_of(group_cols))) |>
    mutate(
      iri_percent = ifelse(sum(iri, na.rm = TRUE) > 0,
                           100 * iri / sum(iri, na.rm = TRUE),
                           NA_real_)
    ) |>
    ungroup()
  
  return(result)
}
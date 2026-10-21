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
pivot_diet_long <- function(
    df,
    diet_cols = NULL,
    id_col = NULL,
    metadata_cols = NULL,
    prey_col_name = "prey_item",
    value_col_name = "value",
    remove_zero = TRUE,
    remove_na = TRUE,
    strip_prefix = NULL
) {
  
  df <- as.data.frame(df)
  
  if (is.null(diet_cols)) {
    diet_cols <- attr(df, "diet_cols")
  }
  
  if (is.null(id_col)) {
    id_col <- attr(df, "id_col")
  }
  
  if (is.null(metadata_cols)) {
    metadata_cols <- attr(df, "metadata_cols")
  }
  
  if (is.null(diet_cols) || length(diet_cols) == 0) {
    stop("diet_cols must be supplied or available in attr(df, 'diet_cols').")
  }
  
  if (is.null(id_col) || length(id_col) != 1) {
    stop("id_col must be supplied or available in attr(df, 'id_col').")
  }
  
  if (!id_col %in% names(df)) {
    stop("id_col was not found in df.")
  }
  
  missing_diet <- setdiff(diet_cols, names(df))
  if (length(missing_diet) > 0) {
    stop(
      "The following diet_cols were not found in df: ",
      paste(missing_diet, collapse = ", ")
    )
  }
  
  if (is.null(metadata_cols)) {
    metadata_cols <- character(0)
  }
  
  missing_meta <- setdiff(metadata_cols, names(df))
  if (length(missing_meta) > 0) {
    warning(
      "The following metadata_cols were not found in df and will be ignored: ",
      paste(missing_meta, collapse = ", ")
    )
    metadata_cols <- intersect(metadata_cols, names(df))
  }
  
  keep_cols <- unique(c(id_col, metadata_cols, diet_cols))
  
  long_df <- df |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(diet_cols),
      names_to = prey_col_name,
      values_to = value_col_name,
      values_drop_na = FALSE
    )
  
  if (!is.null(strip_prefix)) {
    long_df[[prey_col_name]] <- ifelse(
      startsWith(long_df[[prey_col_name]], strip_prefix),
      substring(long_df[[prey_col_name]], nchar(strip_prefix) + 1),
      long_df[[prey_col_name]]
    )
  }
  
  if (remove_na) {
    long_df <- long_df |>
      dplyr::filter(!is.na(.data[[value_col_name]]))
  }
  
  if (remove_zero) {
    long_df <- long_df |>
      dplyr::filter(.data[[value_col_name]] > 0)
  }
  
  long_df <- tibble::as_tibble(long_df)
  
  return(long_df)
}
#### Percentage of volume (V%) ####
calc_v_percent <- function(
    df,
    group_cols = NULL,
    diet_cols = NULL,
    id_col = NULL,
    metadata_cols = NULL,
    strip_prefix = NULL
) {
  
  if (is.null(group_cols)) {
    group_cols <- character(0)
  }
  
  long_df <- pivot_diet_long(
    df = df,
    diet_cols = diet_cols,
    id_col = id_col,
    metadata_cols = metadata_cols,
    prey_col_name = "prey_item",
    value_col_name = "value",
    remove_zero = TRUE,
    remove_na = TRUE,
    strip_prefix = strip_prefix
  )
  
  result <- long_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "prey_item")))) |>
    dplyr::summarise(
      total_volume = sum(value, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::mutate(
      group_total_volume = sum(total_volume, na.rm = TRUE),
      v_percent = ifelse(
        group_total_volume > 0,
        100 * total_volume / group_total_volume,
        NA_real_
      )
    ) |>
    dplyr::ungroup()
  
  return(result)
}

#### Frequency of occurrence (FO%) ####
calc_fo_percent <- function(
    df,
    group_cols = NULL,
    diet_cols = NULL,
    id_col = NULL,
    metadata_cols = NULL,
    strip_prefix = NULL
) {
  
  if (is.null(group_cols)) {
    group_cols <- character(0)
  }
  
  if (is.null(id_col)) {
    id_col <- attr(df, "id_col")
  }
  
  long_df <- pivot_diet_long(
    df = df,
    diet_cols = diet_cols,
    id_col = id_col,
    metadata_cols = metadata_cols,
    prey_col_name = "prey_item",
    value_col_name = "value",
    remove_zero = TRUE,
    remove_na = TRUE,
    strip_prefix = strip_prefix
  )
  
  total_stomachs <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_stomachs = dplyr::n_distinct(.data[[id_col]]),
      .groups = "drop"
    )
  
  prey_occ <- long_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "prey_item")))) |>
    dplyr::summarise(
      occurrence = dplyr::n_distinct(.data[[id_col]]),
      .groups = "drop"
    )
  
  result <- prey_occ |>
    dplyr::left_join(total_stomachs, by = group_cols) |>
    dplyr::mutate(
      fo_percent = ifelse(
        n_stomachs > 0,
        100 * occurrence / n_stomachs,
        NA_real_
      )
    )
  
  return(result)
}

##### IAi = (FO% * V%) / sum(FO% * V%) * 100 ####
calc_iai <- function(
    df,
    group_cols = NULL,
    diet_cols = NULL,
    id_col = NULL,
    metadata_cols = NULL,
    strip_prefix = NULL
) {
  
  if (is.null(group_cols)) {
    group_cols <- character(0)
  }
  
  v_tbl <- calc_v_percent(
    df = df,
    group_cols = group_cols,
    diet_cols = diet_cols,
    id_col = id_col,
    metadata_cols = metadata_cols,
    strip_prefix = strip_prefix
  )
  
  fo_tbl <- calc_fo_percent(
    df = df,
    group_cols = group_cols,
    diet_cols = diet_cols,
    id_col = id_col,
    metadata_cols = metadata_cols,
    strip_prefix = strip_prefix
  )
  
  result <- v_tbl |>
    dplyr::select(
      dplyr::all_of(group_cols),
      prey_item,
      v_percent
    ) |>
    dplyr::left_join(
      fo_tbl |>
        dplyr::select(
          dplyr::all_of(group_cols),
          prey_item,
          fo_percent
        ),
      by = c(group_cols, "prey_item")
    ) |>
    dplyr::mutate(
      fo_v_product = fo_percent * v_percent
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      sum_fo_v = sum(fo_v_product, na.rm = TRUE),
      iai = ifelse(
        sum_fo_v > 0,
        100 * fo_v_product / sum_fo_v,
        NA_real_
      )
    ) |>
    dplyr::ungroup()
  
  return(result)
}

# IRI = (N% + V%) * FO%
##### Calculate IRI ####
calc_iri <- function(
    data_volume,
    data_count,
    group_cols = NULL,
    volume_cols = NULL,
    count_cols = NULL,
    id_col = NULL,
    volume_prefix = NULL,
    count_prefix = NULL
) {
  
  if (is.null(group_cols)) {
    group_cols <- character(0)
  }
  
  if (is.null(id_col)) {
    id_col <- attr(data_volume, "id_col")
  }
  
  if (is.null(volume_cols)) {
    volume_cols <- attr(data_volume, "diet_cols")
  }
  
  if (is.null(count_cols)) {
    count_cols <- attr(data_count, "diet_cols")
  }
  
  if (is.null(volume_cols) || length(volume_cols) == 0) {
    stop("volume_cols must be supplied or stored in data_volume.")
  }
  
  if (is.null(count_cols) || length(count_cols) == 0) {
    stop("count_cols must be supplied or stored in data_count.")
  }
  
  if (!id_col %in% names(data_volume)) {
    stop("id_col was not found in data_volume.")
  }
  
  if (!id_col %in% names(data_count)) {
    stop("id_col was not found in data_count.")
  }
  
  volume_ids <- sort(unique(data_volume[[id_col]]))
  count_ids  <- sort(unique(data_count[[id_col]]))
  
  if (!identical(volume_ids, count_ids)) {
    stop("data_volume and data_count do not contain the same fish IDs.")
  }
  
  v_tbl <- calc_v_percent(
    df = data_volume,
    group_cols = group_cols,
    diet_cols = volume_cols,
    id_col = id_col,
    metadata_cols = attr(data_volume, "metadata_cols"),
    strip_prefix = volume_prefix
  )
  
  fo_tbl <- calc_fo_percent(
    df = data_count,
    group_cols = group_cols,
    diet_cols = count_cols,
    id_col = id_col,
    metadata_cols = attr(data_count, "metadata_cols"),
    strip_prefix = count_prefix
  )
  
  count_long <- pivot_diet_long(
    df = data_count,
    diet_cols = count_cols,
    id_col = id_col,
    metadata_cols = attr(data_count, "metadata_cols"),
    prey_col_name = "prey_item",
    value_col_name = "count_value",
    remove_zero = FALSE,
    remove_na = FALSE,
    strip_prefix = count_prefix
  )
  
  count_long <- count_long |>
    dplyr::mutate(
      count_value = suppressWarnings(as.numeric(count_value)),
      count_value = dplyr::coalesce(count_value, 0)
    )
  
  n_tbl <- count_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "prey_item")))) |>
    dplyr::summarise(
      total_n = sum(count_value, na.rm = TRUE),
      .groups = "drop_last"
    ) |>
    dplyr::mutate(
      group_total_n = sum(total_n, na.rm = TRUE),
      n_percent = ifelse(
        group_total_n > 0,
        100 * total_n / group_total_n,
        NA_real_
      )
    ) |>
    dplyr::ungroup()
  
  result <- v_tbl |>
    dplyr::select(dplyr::all_of(group_cols), prey_item, v_percent) |>
    dplyr::left_join(
      fo_tbl |>
        dplyr::select(dplyr::all_of(group_cols), prey_item, fo_percent),
      by = c(group_cols, "prey_item")
    ) |>
    dplyr::left_join(
      n_tbl |>
        dplyr::select(dplyr::all_of(group_cols), prey_item, n_percent),
      by = c(group_cols, "prey_item")
    ) |>
    dplyr::mutate(
      v_percent = dplyr::coalesce(v_percent, 0),
      fo_percent = dplyr::coalesce(fo_percent, 0),
      n_percent = dplyr::coalesce(n_percent, 0),
      iri = (n_percent + v_percent) * fo_percent
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      iri_percent = ifelse(
        sum(iri, na.rm = TRUE) > 0,
        100 * iri / sum(iri, na.rm = TRUE),
        NA_real_
      )
    ) |>
    dplyr::ungroup()
  
  return(result)
}
#### Levin's niche breadth index (B) and standardized Levin's index (BA) ####
calc_levins_breadth <- function(
    df,
    group_cols = NULL,
    diet_cols = NULL,
    id_col = NULL,
    metadata_cols = NULL,
    strip_prefix = NULL,
    standardize_by = c("area", "group", "global")
) {
  
  standardize_by <- match.arg(standardize_by)
  
  if (is.null(group_cols)) {
    group_cols <- character(0)
  }
  
  if (is.null(diet_cols)) {
    diet_cols <- attr(df, "diet_cols")
  }
  
  if (is.null(id_col)) {
    id_col <- attr(df, "id_col")
  }
  
  if (is.null(metadata_cols)) {
    metadata_cols <- attr(df, "metadata_cols")
  }
  
  if (length(group_cols) == 0) {
    stop("At least one grouping column is required.")
  }
  
  long_df <- pivot_diet_long(
    df = df,
    diet_cols = diet_cols,
    id_col = id_col,
    metadata_cols = metadata_cols,
    prey_col_name = "prey_item",
    value_col_name = "diet_value",
    remove_zero = TRUE,
    remove_na = TRUE,
    strip_prefix = strip_prefix
  )
  
  group_key <- group_cols
  
  diet_summary <- long_df |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(group_cols, "prey_item"))
      )
    ) |>
    dplyr::summarise(
      total_diet_value = sum(diet_value, na.rm = TRUE),
      .groups = "drop"
    )
  
  result <- diet_summary |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::mutate(
      group_total_value = sum(total_diet_value, na.rm = TRUE),
      p_i = total_diet_value / group_total_value
    ) |>
    dplyr::summarise(
      levins_b = 1 / sum(p_i^2, na.rm = TRUE),
      n_prey_used = dplyr::n(),
      .groups = "drop"
    )
  
  if (standardize_by == "group") {
    
    result <- result |>
      dplyr::mutate(
        n_available = n_prey_used
      )
    
  } else if (standardize_by == "area") {
    
    area_cols <- intersect("sampling_area", group_cols)
    
    if (length(area_cols) != 1) {
      stop(
        "standardize_by = 'area' requires 'sampling_area' in group_cols."
      )
    }
    
    available_prey <- diet_summary |>
      dplyr::distinct(sampling_area, prey_item) |>
      dplyr::count(
        sampling_area,
        name = "n_available"
      )
    
    result <- result |>
      dplyr::left_join(
        available_prey,
        by = "sampling_area"
      )
    
  } else if (standardize_by == "global") {
    
    n_available <- diet_summary |>
      dplyr::distinct(prey_item) |>
      nrow()
    
    result <- result |>
      dplyr::mutate(
        n_available = n_available
      )
  }
  
  result <- result |>
    dplyr::mutate(
      levins_ba = dplyr::if_else(
        n_available > 1,
        (levins_b - 1) / (n_available - 1),
        0
      )
    )
  
  return(result)
}
##### Calculate item richness ####
calc_item_richness <- function(
    df,
    group_cols = NULL,
    diet_cols = NULL,
    id_col = NULL,
    metadata_cols = NULL,
    strip_prefix = NULL
) {
  
  if (is.null(group_cols)) {
    group_cols <- character(0)
  }
  
  if (is.null(diet_cols)) {
    diet_cols <- attr(df, "diet_cols")
  }
  
  if (is.null(id_col)) {
    id_col <- attr(df, "id_col")
  }
  
  if (is.null(metadata_cols)) {
    metadata_cols <- attr(df, "metadata_cols")
  }
  
  if (is.null(diet_cols) || length(diet_cols) == 0) {
    stop("diet_cols must be supplied or available in attr(df, 'diet_cols').")
  }
  
  if (length(group_cols) > 0) {
    missing_groups <- setdiff(group_cols, names(df))
    
    if (length(missing_groups) > 0) {
      stop(
        "The following group_cols were not found in df: ",
        paste(missing_groups, collapse = ", ")
      )
    }
  }
  
  long_df <- pivot_diet_long(
    df = df,
    diet_cols = diet_cols,
    id_col = id_col,
    metadata_cols = metadata_cols,
    prey_col_name = "prey_item",
    value_col_name = "diet_value",
    remove_zero = TRUE,
    remove_na = TRUE,
    strip_prefix = strip_prefix
  )
  
  result <- long_df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::summarise(
      item_richness = dplyr::n_distinct(prey_item),
      .groups = "drop"
    )
  
  return(result)
}


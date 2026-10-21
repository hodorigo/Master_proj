library(dplyr)
source("01_scripts/functions.R")

df <- read.csv("02_rawdata/generated_fish_data.csv", stringsAsFactors = FALSE)

data_volume <- prepare_diet_data(
  df = df,
  metadata_cols = c("sampling_area", "month", "region", "habitat",
                    "season", "sex", "unique_id", "species"),
  diet_cols = paste0("prey_vol_", 1:60),
  ignore_cols = c("class"),
  id_col = "unique_id",
  clean_names = TRUE)

data_count <- prepare_diet_data(
  df = df,
  metadata_cols = c("sampling_area", "month", "region", "habitat",
                    "season", "sex", "unique_id", "species"),
  diet_cols = paste0("prey_n_", 1:60),
  ignore_cols = c("class"),
  id_col = "unique_id",
  clean_names = TRUE)

v_out <- calc_v_percent(
  df = data_volume,
  group_cols = "species",
  strip_prefix = "prey_vol_")

fo_out <- calc_fo_percent(
  df = data_count,
  group_cols = "species",
  strip_prefix = "prey_n_")

iai_out <- calc_iai(
  df = data_volume,
  group_cols = "species",
  strip_prefix = "prey_vol_")

iri_out <- calc_iri(
  data_volume = data_volume,
  data_count = data_count,
  group_cols = "species",
  volume_prefix = "prey_vol_",
  count_prefix = "prey_n_")

levins_out <- calc_levins_breadth(
  df = data_count,
  group_cols = c("sampling_area", "species"),
  strip_prefix = "prey_n_",
  standardize_by = "area")

richness_out <- calc_item_richness(
  df = data_count,
  group_cols = c("sampling_area", "species"),
  strip_prefix = "prey_n_")


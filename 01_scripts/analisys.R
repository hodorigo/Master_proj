library(dplyr)
source("functions.R")

df <- read.csv("generated_fish_data.csv", stringsAsFactors = FALSE)

data_prepared <- prepare_diet_data(
  df = df,
  metadata_cols = c("samplingarea", "month", "region", "habitat",
                    "season", "sex", "uniqueid", "species"),
  diet_cols = paste0("prey", 1:60),
  ignore_cols = c("class"),
  id_col = "uniqueid",
  clean_names = TRUE
)

# Run only one index if desired
v_out  <- calc_v_percent(data_prepared, group_cols = c("species"))
fo_out <- calc_fo_percent(data_prepared, group_cols = c("species"))
iai_out <- calc_iai(data_prepared, group_cols = c("species"))
iri_out <- calc_iri(data_prepared, group_cols = c("species"))
#### Random dataset generator for IRI analysis creates metadata + prey abundance + prey volume ####


{
  n_fish = 2700
  n_prey = 60
  seed = 123
  absence_prob = 0.35
  min_volume = 0.01
  max_volume = 1.00
  max_abundance = 20
  output_file = "02_rawdata/generated_fish_data.csv"
  
  set.seed(seed = seed) # for reproducibility
  
  # Metadata pools
  sampling_areas <- c("area_01", "area_02", "area_03", "area_04",
                      "area_05", "area_06", "area_07", "area_08")
  
  month <- c("janvier", "fevrier", "mars", "avril", "mai", "juin",
             "juillet", "aout", "septembre", "octobre", "novembre", "decembre")
  
  region <- c("montain", "plains", "valley", "pothole")
  
  habitat <- c("river", "marine", "estuary", "lake",
               "puddle", "fast-stream", "slow-stream")
  
  seasons_list <- c("spring", "summer", "autumn", "winter")
  
  sex_list <- c("m", "f", "i")
  
  class_size <- c("class_1", "class_2", "class_3", "class_4",
                  "class_5", "class_6", "class_7")
  
  species_list <- c(
    "Astyanax_fasciatus",
    "Hoplias_malabaricus",
    "Leporinus_friderici",
    "Prochilodus_lineatus",
    "Pimelodus_maculatus",
    "Cichla_ocellaris",
    "Geophagus_brasiliensis",
    "Rhamdia_quelen"
  )
  
  # Main metadata table
  fish_data <- data.frame(
    sampling_area = sample(sampling_areas, n_fish, replace = TRUE),
    month = sample(month, n_fish, replace = TRUE),
    region = sample(region, n_fish, replace = TRUE),
    habitat = sample(habitat, n_fish, replace = TRUE),
    season = sample(seasons_list, n_fish, replace = TRUE),
    sex = sample(sex_list, n_fish, replace = TRUE),
    class = sample(class_size, n_fish, replace = TRUE),
    unique_id = sprintf("B%04d", 1:n_fish),
    species = sample(species_list, n_fish, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add prey abundance and prey volume columns
  for (i in seq_len(n_prey)) {
    
    present <- runif(n_fish) >= absence_prob
    
    abundance_values <- ifelse(
      present,
      sample(1:max_abundance, n_fish, replace = TRUE),
      0
    )
    
    volume_values <- ifelse(
      present,
      round(runif(n_fish, min_volume, max_volume), 2),
      0
    )
    
    fish_data[[paste0("prey_n_", i)]] <- abundance_values
    fish_data[[paste0("prey_vol_", i)]] <- volume_values
  }
  
  {
  write.csv(fish_data, file = output_file, row.names = FALSE)
  }
  
   }

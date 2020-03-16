#################################################################
# Data

get_data_aqui <- function(file_xlsx){
  
  # ARS nouvelle aquitaine
  end_idx <- which(readxl::read_xlsx(file_xlsx)[[1]] == "TOTAL")
  data_aqui <- readxl::read_xlsx(file, range = paste0("A4:C", end_idx),
                                 col_names = c("date", "dpt", "n"),
                                 col_types = c("date", "text", "numeric")) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
  # Geography --------------------------------------------------------------------
  # Departements nouvelle aquitaine
  regions <- read.csv("data/departements-region.csv")
  # Map france dep
  mfr <- map_data("france")
  #subset map france
  map_aqui <- regions %>%
    filter(region_name == "Nouvelle-Aquitaine") %>%
    mutate(dep_name = case_when(
      dep_name == "Pyrénées-Atlantiques" ~ "Pyrenees-Atlantiques",
      dep_name == "Deux-Sèvres" ~ "Deux-Sevres",
      dep_name == "Corrèze" ~ "Correze",
      TRUE ~ as.character(dep_name)
    )) %>%
    inner_join(mfr, by = c("dep_name" = "region")) 
  
  
  # Data for the plot ------------------------------------------------------------
  range_date_aqui <- range(data_aqui$date, na.rm = T) # min and max date observed
  seq_date <- seq(range_date_aqui[1], range_date_aqui[2], by = "day") # all the observed date by day
  all_dates <- tibble(date = rep(seq_date, nrow(regions)),
                      dpt = rep(regions$num_dep, each = length(seq_date))) %>%
    filter(dpt %in% map_aqui$num_dep)
  
  # Join ARS data and map france
  data_aqui_2 <- data_aqui %>%
    full_join(all_dates, by = c("dpt", "date")) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    group_by(dpt) %>%
    arrange(dpt, date) %>%
    mutate(n_sum = cumsum(n)) %>% # add total cases incident
    ungroup() %>%
    right_join(map_aqui, by = c("dpt" = "num_dep")) %>% # join map data
    group_by(dpt, date) %>%
    mutate(tooltip = paste0(dep_name,
                            "\n", n_sum, " cas",
                            "\n", n, " nouveaux cas")) %>%
    ungroup() %>%
    rename(id = dpt)
  
  return(data_aqui_2)
}

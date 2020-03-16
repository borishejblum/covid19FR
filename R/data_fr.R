#################################################################
# Data

# SPF france
get_data_spf_FR <- function(file_xlsx){
  data_fr <- readxl::read_xlsx(file_xlsx, skip = 1,
                               col_names = c("region", "n", "zone", "date"),
                               col_types = c("text", "numeric", "text", "date")) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  # Geography --------------------------------------------------------------------
  # Map france reg
  map_fr <- read_csv("data/regions-contours.csv") %>%
    filter(str_detect(group, "^.*\\.1$")) # simplification of the map (1 polygon/region)
  reg_metro <- unique(map_fr$id)
  
  range_date_fr <- range(data_fr$date, na.rm = T) # min and max date observed
  seq_date <- seq(range_date_fr[1], range_date_fr[2], by = "day") # all the observed date by day
  all_dates <- tibble(date = rep(seq_date, length(reg_metro)),
                      region = rep(reg_metro, each = length(seq_date)))
  
  # Join ARS data and map france
  data_fr_1 <- data_fr %>%
    filter(region %in% reg_metro) %>%
    full_join(all_dates, by = c("region", "date")) %>% 
    arrange(region, date)
  
  values <- data_fr_1$n
  for (i in 1:length(values)){
    if(is.na(values[i])){
      values[i] <- values[i-1]
    }
  }
  data_fr_1$n <- values
  
  data_fr_2 <- data_fr_1 %>%
    group_by(region) %>%
    mutate(n_sum = n) %>%
    mutate(n = n_sum - lag(n_sum)) %>% # add daily cases incident
    mutate(n = ifelse(is.na(n), n_sum, n)) %>%
    ungroup() %>%
    right_join(map_fr, by = c("region" = "id")) %>% # join map data
    mutate(tooltip = paste0(region,
                            "\n", n_sum, " cas",
                            "\n", n, " nouveaux cas")) %>%
    rename(id = region)
}


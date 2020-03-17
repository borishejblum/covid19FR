#################################################################
# Functions for the shiny app


# Function to make the map plot
fun_plot_map <- function(data, input_date){
  
  color_gradient <- colorRampPalette(colors = c("#e0e0e0", #white
                                                "#0090de")) #blue
  max_cases <- max(data$n_sum, na.rm = T) # max number of cases /dpt
  
  data %>%
    filter(date == input_date) %>%
    ggplot(aes(x = long, y = lat, group = group, tooltip = tooltip, fill = n_sum)) +
    geom_polygon_interactive(color = "black") +
    scale_fill_gradientn_interactive(colors = color_gradient(max_cases),
                                     limits = c(0, max_cases),
                                     name = "Nombre de cas") +
    coord_map() +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(caption = "Sources: Santé Publique France / ARS Nouvelle-Aquitaine")
}

# Function to make the barplot
fun_plot_bar <- function(data, input_date, range_date){
  
  max_cases <- data %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    summarise(n = max(n, na.rm = T)) %>%
    pull(n)
  
  data %>%
    filter(date <= input_date) %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(tooltip = paste0("Le ", format(date, "%A %e %B"),
                            "\n", n, " nouveaux cas")) %>%
    ggplot(aes(x = date, y = n, tooltip = tooltip)) +
    geom_bar_interactive(stat = "identity", fill = "#0090de", alpha = .3) +
    ylim(0, max_cases) +
    scale_x_date(date_breaks = "1 week", 
                 labels=scales::date_format("%d-%m"),
                 limits = range_date) +
    labs(x = "Date", y = "Nombre de cas",
         title = "Nombre de cas diagnostiqués par jour") +
    theme_classic() +
    theme(axis.title.y = element_text(angle = 0, vjust = .5))
}

# Function to make the geom_point (total cases)
fun_plot_point <- function(data, input_date, range_date){
  
  max_cases <- data %>%
    select(date, n_sum, id) %>%
    distinct() %>%
    filter(date == max(date, na.rm = T)) %>%
    summarise(n_sum = sum(n_sum, na.rm = T)) %>%
    pull(n_sum)
  
  data %>%
    filter(date <= input_date) %>%
    select(date, n_sum, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n_sum = sum(n_sum, na.rm = T)) %>%
    ggplot(aes(x = date, y = n_sum)) +
    geom_point_interactive(mapping = aes(tooltip = n_sum), stat = "identity",
                           color = "#0090de", size = 5) +
    geom_line(alpha = .5, color = "#0090de", size = 1) +
    ylim(0, max_cases) +
    scale_x_date(date_breaks = "1 week", 
                 labels=scales::date_format("%d-%m"),
                 limits = range_date) +
    labs(x = "Date", y = "Nombre de cas",
         title = "Nombre de cas total") +
    theme_classic() +
    theme(axis.title.y = element_text(angle = 0, vjust = .5))
}

# Functions to calculate valuebox values
fun_cases_total <- function(data, input_date) {
  data %>%
    filter(date <= input_date) %>%
    select(date, n, id) %>%
    distinct() %>%
    summarise(n = sum(n, na.rm = T)) %>%
    pull(n)
}
fun_cases_day <- function(data, input_date) {
  data %>%
    filter(date == input_date) %>%
    select(date, n, id) %>%
    distinct() %>%
    summarise(n = sum(n, na.rm = T)) %>%
    pull(n)
}
fun_fct_exp <- function(data, input_date) {
  rate <- data %>%
    filter(date %in% c(input_date - 1, input_date)) %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    arrange(date) %>%
    pull(n) %>%
    {paste0("x", round({.[2]/.[1]}, 2))}
  if(is.na(rate)| is.infinite(rate)) rate <- 0
  return(rate)
}


#################################################################
# Functions for the shiny app


#' Map plot
#' 
#' @import dplyr
#' @import ggiraph
#' @import ggplot2
#' @import grDevices
fun_plot_map <- function(data, input_date){
  
  color_gradient <- grDevices::colorRampPalette(colors = c("#e0e0e0", #white
                                                "#0090de")) #blue
  max_cases <- max(data$n_sum, na.rm = T) # max number of cases /dpt
  #cap_src <- cas
  
  data %>%
    filter(date == input_date) %>%
    ggplot(aes(x = long, y = lat, group = group, tooltip = tooltip, fill = n_sum)) +
    ggiraph::geom_polygon_interactive(color = "black") +
    ggiraph::scale_fill_gradientn_interactive(colors = color_gradient(max_cases),
                                     limits = c(0, max_cases),
                                     name = "Nombre de cas") +
    coord_map() +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(caption = "Sources: Santé Publique France / ARS Nouvelle-Aquitaine")
}

#'barplot and points
#'
#'@import patchwork
fun_plot_bar <- function(data, input_date, range_date){
  
  max_cases <- data %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    summarise(n = max(n, na.rm = T)) %>%
    pull(n)
  
  max_cases_cum <- data %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(cumn = cumsum(n)) %>%
    summarise(cumn = max(cumn, na.rm = T)) %>%
    pull(cumn)
  
  p1 <- data %>%
    filter(date <= input_date) %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    mutate(cumn = cumsum(n)) %>%
    mutate(tooltip = paste0("Le ", format(date, "%A %e %B"),
                            "\n", n, " nouveaux cas")) %>%
    ggplot(aes(x = date, y = n, tooltip = tooltip)) +
    geom_bar_interactive(stat = "identity", fill = "#0090de", alpha = .5) +
    theme_classic() +
    ylim(0, max_cases) +
    scale_x_date(date_breaks = "1 week",  
                 date_minor_breaks = "1 day",
                 labels=scales::date_format("%d %B"),
                 limits = range_date) +
    labs(x = "Date", y = "Nombre de cas",
         title = "Nombre de cas confirmés chaque jour") +
    theme(axis.title.y = element_text(angle = 0, vjust = .5))
  
    p2 <- data %>%
      filter(date <= input_date) %>%
      select(date, n, id) %>%
      distinct() %>%
      group_by(date) %>%
      summarise(n = sum(n, na.rm = T)) %>%
      mutate(cumn = cumsum(n)) %>%
      mutate(tooltip = paste0("Le ", format(date, "%A %e %B"),
                              "\n", cumn, " cas confirmés")) %>%
      ggplot(aes(x = date, y = cumn)) +
      geom_line(stat = "identity", color = "#0090de", size = 1, alpha = .5) +
      geom_point_interactive(aes(tooltip = tooltip), 
                             stat = "identity", color = "#0090de", size = 3) +
      theme_classic() +
      ylim(0, max_cases_cum) +
      scale_x_date(date_breaks = "1 week", 
                   date_minor_breaks = "1 day",
                   labels=scales::date_format("%d %B"),
                   limits = range_date) +
      labs(x = "Date", y = "Nombre de cas cumulés",
           title = "Nombre de cas confirmés") +
      theme(axis.title.y = element_text(angle = 0, vjust = .5))
  return(p1 / p2)
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

#'@import glue
fun_fct_exp <- function(data, input_date) {
  
  rate <- data %>%
    filter(date %in% c(input_date - 1, input_date)) %>%
    select(date, n, id) %>%
    distinct() %>%
    group_by(date) %>%
    summarise(n = sum(n, na.rm = T)) %>%
    arrange(date) %>%
    pull(n) %>% 
    {round({.[2]/.[1]}, 2)}
  
  if(is.na(rate) | is.infinite(rate)){
    rate <- 0
  } 
  
  return(paste0("x ", rate))
}


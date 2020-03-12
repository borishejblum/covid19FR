#################################################################
# First version Shiny with ggiraph:: package



# packages ---------------------------------------------------------------------
library(ggplot2)
library(maps) # + require mapproj
library(ggiraph) # interactive plot
library(shiny)
library(RColorBrewer) # more colors to use and construct personalized gradient



# data -------------------------------------------------------------------------

# ARS nouvelle aquitaine
file <- "data/raw/Tableau_decompte_des_cas_confirmes_NA_1103.xlsx"
end_idx <- which(readxl::read_xlsx(file, skip = 3)[[1]] == "TOTAL")
data_aqui <- readxl::read_xlsx(file, range = paste0("A4:C", end_idx),
                               col_names = c("date", "dpt", "n"),
                               col_types = c("date", "text", "numeric")) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Departements nouvelle aquitaine
regions <- read.csv("data/departements-region.csv") %>%
  filter(region_name == "Nouvelle-Aquitaine")

# Map france
mfr <- map_data("france")



# Data for the plot ------------------------------------------------------------
rna <- inner_join(mfr, regions, by = c("region" = "dep_name")) #subset map france
range_date <- range(data_aqui$date, na.rm = T) # min and max date observed
seq_date <- seq(range_date[1], range_date[2], by = "day") # all the observed date by day
all_dates <- tibble(date = rep(seq_date, nrow(regions)),
                    dpt = rep(regions$num_dep, each = length(seq_date)))

# Join ARS data and map france
data_aqui_2 <- data_aqui %>%
  full_join(all_dates, by = c("dpt", "date")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  group_by(dpt) %>%
  arrange(dpt, date) %>%
  mutate(n_sum = cumsum(n)) %>% # add total cases incident
  ungroup() %>%
  right_join(rna, by = c("dpt" = "num_dep")) %>% # join map data
  group_by(dpt, date) %>%
  mutate(tooltip = paste0(region, "\n", n_sum, " cas")) %>%
  ungroup()

# Function to make the plot
fun_plot_map <- function(data, input_date){
  
  color_gradient <- colorRampPalette(colors = c("#ffffcc", #yellow
                                                "#890000")) #red
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
    theme(legend.position = "bottom")
}

# Shiny ------------------------------------------------------------------------

# user interface
ui <- fluidPage(
  sliderInput(inputId = "date", label = "Date",
              value = range_date[1], min = range_date[1], max = range_date[2],
              timeFormat = "%Y-%m-%d",
              animate = T),
  girafeOutput("plot_map")
)

# shiny server
server <- function(input, output, session) {
  output$plot_map <- renderggiraph({
    girafe(ggobj = fun_plot_map(data_aqui_2, input$date),
           options = list(opts_tooltip(css = "font-size: 11px; color: white; background: black; border-radius: 3px; padding: 3px;",
                                       opacity = 0.7))
           )
    })
}

# run the shiny app
shinyApp(ui, server)


################################################################################
# Import COVID-19 data from regional report
# Source: ARS Nouvelle Aquitaine
################################################################################

file <- "data/raw/Tableau_decompte_des_cas_confirmes_COVID19_NA_09_03_2020.xlsx"
end_idx <- which(readxl::read_xlsx(file, skip = 3)[[1]] == "TOTAL")

data_aqui <- readxl::read_xlsx(file, range = paste0("A4:C", end_idx),
                  col_names = c("date", "dpt", "n"),
                  col_types = c("date", "text", "numeric"))


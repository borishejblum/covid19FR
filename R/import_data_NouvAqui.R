#' Import COVID-19 data from regional report
#'
#' @source Source: ARS Nouvelle Aquitaine

import_data_NouvAqui <- function(file_xlsx){
  end_idx <- which(readxl::read_xlsx(file_xlsx, skip = 3)[[1]] == "TOTAL")
  data_aqui <- readxl::read_xlsx(file, range = paste0("A4:C", end_idx),
                                 col_names = c("date", "dpt", "n"),
                                 col_types = c("date", "text", "numeric"))
  return(data_aqui)
}


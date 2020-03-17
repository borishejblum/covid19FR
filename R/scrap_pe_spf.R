#' Extract data from the daily "point epidemiologique COVID-19"
#' 
#' @source Source: Sante Publique France
#' 
#' 

scrap_pe_spf <- function(){
  list_pdf <- tribble(
    ~date, ~file, ~lines, 
    "04/03/20", "COVID19_PE_20200304.pdf", "12:47",
    "05/03/20", "COVID19_PE_20200305.pdf", "12:47",
    "06/03/20", "COVID19_PE_20200306.pdf", "12:49",
    "07/03/20", "COVID19_PE_20200307.pdf", "12:49")
  read_1 <- tm::readPDF(control = list(text = "-layout"))
  path <- "data/raw/"
  pdf_readed <- Corpus(URISource(paste0(path, list_pdf$file)),
                       readerControl = list(reader = read_1))
  
  # Extract lines related to table 1
  raw_data <- map2(set_names(1:nrow(list_pdf), nm = list_pdf$date),
                   rlang::parse_exprs(list_pdf$lines),
                   function(x, y) {
                     idx <- eval(y)
                     strsplit(content(pdf_readed[[x]])[2], split = "\n")[[1]][idx]
                   })
  
  # Number of patient / severe / death
  data_cas <- map(raw_data, function(x) {
    start_idx <- which(str_detect(x, "Nombre de cas"))
    end_idx <- which(str_detect(x, "Exposition identifiée")) -1
    str_split_fixed(x[start_idx:end_idx], "\\s{2,}", n = 3)[,2:3] %>%
      {tibble(area = .[,1], n = .[,2])}}) %>%
    bind_rows(.id = "date") %>%
    mutate(date = as.Date(date, format = "%d/%m/%y"),
           n = str_extract(n, "\\d+"))
  
  # Number by categorical age
  data_agec <- map(raw_data, function(x) {
    start_idx <- which(str_detect(x, "Classes.{4}ges")) + 1
    end_idx <- which(str_detect(x, "Région")) -1
    str_split_fixed(x[start_idx:end_idx], "\\s{2,}", n = 3)[,2:3] %>%
      {tibble(area = .[,1], n = .[,2])}}) %>%
    bind_rows(.id = "date") %>%
    mutate(date = as.Date(date, format = "%d/%m/%y"),
           n = str_extract(n, "\\d+"))
  
  # Number by region
  data_area <- map(raw_data, function(x) {
    start_idx <- which(str_detect(x, "Région")) + 1
    end_idx <- which(str_detect(x, "Total Métropole"))
    str_split_fixed(x[start_idx:end_idx], "\\s{2,}", n = 3)[,2:3] %>%
      {tibble(area = .[,1], n = .[,2])}}) %>%
    bind_rows(.id = "date") %>%
    mutate(date = as.Date(date, format = "%d/%m/%y"))
  
  # Number from cluster / foreign contamination
  # data_contam <- map(raw_data, function(x) {
  #   start_idx <- which(str_detect(x, "Exposition identifiée")) + 1
  #   end_idx <- which(str_detect(x, "Données démographiques")) -1
  #   str_split_fixed(x[start_idx:end_idx], "\\s{2,}", n = 3)[,2:3] %>%
  #     {tibble(area = .[,1], n = .[,2])}}) %>%
  #   bind_rows(.id = "date") %>%
  #   mutate(date = as.Date(date, format = "%d/%m/%y"))
  return(list(data_area = data_area,
              data_agec = data_agec,
              data_cas, data_cas
              )
  )
}



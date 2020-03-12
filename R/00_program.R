
# Packages
library(tm)
library(tidyverse)
# + readxl:: + rlang:: + pdftools::

# Run program
source("R/01_import_data_aquitaine.R")
source("R/01_scrap_pe_spf.R")

# Save data
save(data_aqui, # regional datasets
     data_cas, data_agec, data_area, # national datasets
     file = "data/covid19_fr.Rdata") # export

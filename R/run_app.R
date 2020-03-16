#################################################################
# First version Shiny with ggiraph:: package

# packages ---------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(maps) # + require mapproj
library(ggiraph) # interactive plot
library(shiny)
library(shinydashboard)
library(RColorBrewer) # more colors to use and construct personalized gradient

# source files
source("R/shiny/funs.R")
source("R/shiny/data_aqui.R")
source("R/shiny/data_fr.R")
source("R/shiny/ui.R")
source("R/shiny/server.R")

# run the shiny app
shinyApp(ui, server)


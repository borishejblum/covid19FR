#################################################################
# User interface

# user interface
ui <- dashboardPage(
  dashboardHeader(title = "Evolution de l'épidémie à COVID-19 en France et en Aquitaine",
                  titleWidth = "70vh"
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
    includeCSS("R/shiny/style.css"),
    fluidRow(
      column(12,
        fluidRow(
          valueBoxOutput("cases_total"),
          valueBoxOutput("cases_day"),
          valueBoxOutput("fct_exp")
        ),
        fluidRow(
          box(girafeOutput("plot_map", height = "70vh"), height = "70vh"),
          column(6,
            box(uiOutput("slideDate"),
                selectInput(inputId = "area", label = "Zone géographique",
                            choices = c("France", "Nouvelle Aquitaine"),
                            width = "40vh"),
                width = NULL),
            box(girafeOutput("plot_bar"), width = NULL, height = "47vh")
      )

    )
    )
  )
)
)


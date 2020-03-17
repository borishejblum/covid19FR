#################################################################
# User interface

# user interface
ui <- dashboardPage(
  dashboardHeader(title = "Evolution de l'épidémie à COVID-19 en France et en Aquitaine",
                  titleWidth = "70vh"
  ),
  dashboardSidebar(disable = T),
  dashboardBody(
    #includeCSS("R/shiny/style.css"),
    fluidRow(
      column(width = 12,
        fluidRow(
          valueBoxOutput("cases_total"),
          valueBoxOutput("cases_day"),
          valueBoxOutput("fct_exp")
        ),
        fluidRow(
          box(girafeOutput("plot_map", width = "100%", height = "85vh"), height = "85vh"),
          column(6,
            box(uiOutput("slideDate"),
                selectInput(inputId = "area", label = "Zone géographique",
                            choices = c("France", "Nouvelle Aquitaine"),
                            width = "40vh"),
                width = NULL),
            tabsetPanel(
              tabPanel(title = "Nombre de diagnostics par jour",
                       box(girafeOutput("plot_bar", height = "1000px"), height = "68vh", width = NULL)),
              tabPanel(title = "Nombre total de cas",
                       box(girafeOutput("plot_point"), height = "68vh", width = NULL)),
              type = "pills"
            )
      )

    )
    )
  )
)
)

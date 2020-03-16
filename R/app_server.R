#' @import shiny
#' @import ggplot2
#' @import ggiraph
app_server <- function(input, output, session) {

  dataPlot <- reactive({
    if(input$area == "France"){
      get_data_spf_FR("data/raw/COVID19data_SPF.xlsx")
    } else if (input$area == "Nouvelle Aquitaine") {
      get_data_aqui("data/raw/Tableau_decompte_des_cas_confirmes_NA_report_manuel_15-03-2020.xlsx")
    }
  })

  output$slideDate <- renderUI({
    sliderInput(inputId = "date", label = "Date",
                value = dataPlot()[["range"]][1],
                min = dataPlot()[["range"]][1], max = dataPlot()[["range"]][2],
                timeFormat = "%Y-%m-%d",
                step = 1,
                animate = animationOptions(interval = 1000),
                width = "60vh")
  })

  output$cases_total <- renderValueBox({
    valueBox(
      value = fun_cases_total(dataPlot()[["data"]], input$date),
      subtitle = paste0("Nombre total de cas le ", input$date),
      icon = icon("users")
    )
  })

  output$cases_day <- renderValueBox({
    valueBox(
      value = fun_cases_day(dataPlot()[["data"]], input$date),
      subtitle = paste0("Nombre de nouveaux cas le ", input$date),
      icon = icon("user")
    )
  })

  output$fct_exp <- renderValueBox({
    valueBox(
      value = fun_fct_exp(dataPlot()[["data"]], input$date),
      subtitle = "Taux d'augmentation de diagnostics positifs par rapport au jour précédent",
      icon = icon("chart-line")
    )
  })

  output$plot_map <- renderggiraph({
    girafe(ggobj = fun_plot_map(dataPlot()[["data"]], input$date),
           options = list(opts_tooltip(css = "font-size: 11px; color: white; background: black; border-radius: 3px; padding: 3px;",
                                       opacity = 0.7)),
           width_svg = 10, height_svg = 10)
  })

  output$plot_bar <- renderggiraph({
    girafe(ggobj = fun_plot_bar(dataPlot()[["data"]], input$date, dataPlot()[["range"]]),
           options = list(opts_tooltip(css = "font-size: 11px; color: white; background: black; border-radius: 3px; padding: 3px;",
                                       opacity = 0.7)),
           width_svg = 10, height_svg = 6)
  })
}

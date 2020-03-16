#################################################################
# Server side

server <- function(input, output, session) {
  
  dataPlot <- reactive({
    if(input$area == "France"){
      data_fr_2
    } else if (input$area == "Nouvelle Aquitaine") {
      data_aqui_2
    }
  })
  
  dateRange <- reactive({
    if(input$area == "France"){
      range_date_fr
    } else if (input$area == "Nouvelle Aquitaine") {
      range_date_aqui
    }
  })
  
  output$slideDate <- renderUI({
    sliderInput(inputId = "date", label = "Date",
                value = dateRange()[1],
                min = dateRange()[1], max = dateRange()[2],
                timeFormat = "%Y-%m-%d",
                animate = animationOptions(interval = 2000),
                width = "60vh")
  })
  
  output$cases_total <- renderValueBox({
    valueBox(
      value = fun_cases_total(dataPlot(), input$date),
      subtitle = paste0("Nombre total de cas le ", input$date),
      icon = icon("users")
    )
  })
  
  output$cases_day <- renderValueBox({
    valueBox(
      value = fun_cases_day(dataPlot(), input$date),
      subtitle = paste0("Nombre de nouveaux cas le ", input$date),
      icon = icon("user")
    )
  })
  
  output$fct_exp <- renderValueBox({
    valueBox(
      value = fun_fct_exp(dataPlot(), input$date),
      subtitle = "Taux d'augmentation de diagnostiques positifs par rapport au jours précédent",
      icon = icon("chart-line")
    )
  })
  
  output$plot_map <- renderggiraph({
    girafe(ggobj = fun_plot_map(dataPlot(), input$date),
           options = list(opts_tooltip(css = "font-size: 11px; color: white; background: black; border-radius: 3px; padding: 3px;",
                                       opacity = 0.7)),
           width_svg = 10, height_svg = 10)
  })
  
  output$plot_bar <- renderggiraph({
    girafe(ggobj = fun_plot_bar(dataPlot(), input$date, dateRange()),
           options = list(opts_tooltip(css = "font-size: 11px; color: white; background: black; border-radius: 3px; padding: 3px;",
                                       opacity = 0.7)),
           width_svg = 10, height_svg = 6)
  })
}

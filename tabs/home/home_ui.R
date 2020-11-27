home_ui <-
  tabPanel(
    title = "Home",
    value = "home",
    hr(),
    HTML("<h1><center><strong>Análise das Eleições de Porto Alegre - 2020 - 1º Turno</strong></center></h1>"),
    br(), br(), br(), br(),
    column(width = 2),
    column(width = 4,
           btn_landing(texto = "Análise dos Candidatos",
                       cor = "#434aa8",
                       id = "btn_candidatos")),
    column(width = 4,
           btn_landing(texto = "Análise por Bairros",
                       cor = "#434aa8",
                       id = "btn_bairros")),
    br()
  )
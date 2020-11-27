bairros_ui <-
  tabPanel(
    title = "Análise dos Bairros",
    value = "bairros",
    HTML("<h1><center><strong>Análise da votação por bairros</strong></center></h1>"),
    column(
      3,
      leafletOutput("mapa_bairros_filtro",
                    height = "600px")
      # awesomeCheckbox(
      #   inputId = "select_null_votes",
      #   label = "Apenas Votos Válidos",
      #   value = FALSE
      # )
    ),
    column(
      9,
      column(
        6,
        highchartOutput("hchart_votos_prefeito")
      ),
      column(
        6,
        dataTableOutput("tabela_votos_vereadores")
      )
    )
  )
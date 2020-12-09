bairros_ui <-
  tabPanel(
    title = "Análise dos Bairros",
    value = "bairros",
    HTML("<h1><center><strong>Análise da votação por bairros</strong></center></h1>"),
    fluidRow(
      column(
        width = 12,
        align = "center",
        awesomeRadio(
          inputId = "select_voting_turn_bairros",
          label = "Turno de votação",
          choices = c("1º Turno" = 1,
                      "2º Turno" = 2),
          selected = 1,
          inline = TRUE
        )
      )
    ),
    column(
      3,
      uiOutput("titulo_mapa_bairros"),
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
        HTML("<h4><center><strong>Votos para vereador na região selecionada</strong></center></h4>"),
        dataTableOutput("tabela_votos_vereadores")
      )
    )
  )
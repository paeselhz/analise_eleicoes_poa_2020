candidatos_ui <-
  tabPanel(
    title = "Análise dos Candidatos",
    value = "candidatos",
    HTML("<h1><center><strong>Análise individual dos candidatos</strong></center></h1>"),
    column(
      3,
      pickerInput(
        inputId = "select_voting_level",
        label = "Selecione o tipo de eleição: ",
        choices = votacao_poa %>% 
          dplyr::pull(ds_cargo) %>% 
          unique(),
        selected = "PREFEITO"
      ),
      pickerInput(
        inputId = "select_candidate",
        label = "Selecione o Candidato",
        choices = "",
        selected = "",
        options = pickerOptions(
          liveSearch = TRUE
        )
      ),
      pickerInput(
        inputId = "selected_zone",
        label = "Selecione a zona para analisar: ",
        choices = votacao_poa %>% 
          dplyr::pull(nr_zona) %>% 
          unique(),
        selected = 1
      ),
      # dataTableOutput("total_votos_zonas")
      highchartOutput("total_votos_zonas")
    ),
    column(
      9,
      column(
        6,
        leafletOutput("map_votos_secao",
                        height = "600px")
      ),
      column(
        6, 
        dataTableOutput("tabela_votos_secao")  
      )
    )
  )

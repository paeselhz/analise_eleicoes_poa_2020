shinyUI(
  fluidPage(
    h1("Analise das eleições Municipais de Porto Alegre - 2020 - 1 Turno"),
    br(),
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
      selectizeInput(
        inputId = "select_candidate",
        label = "Selecione o Candidato",
        choices = "",
        selected = ""
      ),
      pickerInput(
        inputId = "selected_zone",
        label = "Selecione a zona para analisar: ",
        choices = votacao_poa %>% 
          dplyr::pull(nr_zona) %>% 
          unique(),
        selected = 1
      ),
      dataTableOutput("total_votos_zonas")
    ),
    column(
      9,
      leafletOutput("map_votos_secao",
                    height = "600px"),
      dataTableOutput("tabela_votos_secao")
      # textOutput("tabela_votos_secao")
    )
  )
)
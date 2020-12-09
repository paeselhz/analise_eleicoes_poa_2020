click_evaluator <- 
  reactiveValues(clickedMarker = NULL)

observeEvent(
  input$map_votos_secao_marker_click,
  {
    click_evaluator$clickedMarker <- input$map_votos_secao_marker_click
  }
)

observeEvent(
  input$map_votos_secao_click,
  {
    click_evaluator$clickedMarker <- NULL
  }
)

is_candidatos <-
  reactive({
    
    if(input$navbar == "candidatos") {
      
      return(paste(input$navbar , click_evaluator$clickedMarker))
      
    } else {
      
      return(NULL)
    }
    
  })

observe({
  updatePickerInput(
    session,
    inputId = "select_voting_level",
    choices = votacao_poa %>% 
      dplyr::filter(nr_turno == input$select_voting_turn) %>% 
      dplyr::pull(ds_cargo) %>% 
      unique()
  )
})

observe({
  updatePickerInput(
    session,
    inputId = "select_candidate",
    choices = votacao_poa %>%
      dplyr::filter(ds_cargo == input$select_voting_level &
                      nr_turno == input$select_voting_turn) %>%
      dplyr::pull(nm_votavel) %>%
      unique()
  ) 
})

output$map_votos_secao <- renderLeaflet({
  
  df_filtered <-
    votacao_poa %>% 
    filter(
      ds_cargo == input$select_voting_level &
        nm_votavel == input$select_candidate &
        nr_zona == input$selected_zone &
        nr_turno == input$select_voting_turn
      # ds_cargo == "PREFEITO" &
      # nm_votavel == "VOTO NULO"
    )
  
  votos_zona <-
    df_filtered %>% 
    group_by(nr_zona) %>% 
    summarise(qt_votos = sum(qt_votos))
  
  votos_secao <-
    df_filtered %>% 
    mutate(
      nr_secao = as.character(nr_secao)
    ) %>% 
    group_by(nr_zona, nr_secao) %>% 
    summarise(qt_votos = sum(qt_votos))
  
  zona_eleitoral <-
    zona_secoes_geoloc %>% 
    filter(zona == input$selected_zone) %>% 
    select(colegio, zona, secoes, lat, long) %>% 
    distinct() %>% 
    left_join(
      votos_secao,
      by = c("zona" = "nr_zona",
             "secoes" = "nr_secao")
    ) %>% 
    group_by(colegio, zona, lat, long) %>% 
    summarise(
      qt_votos = sum(qt_votos, na.rm = TRUE)
    )
  
  leaflet(data = zona_eleitoral) %>% 
    addTiles() %>%
    addMarkers(layerId = ~paste0(colegio, "|", zona),
               lng = ~long,
               lat = ~lat, 
               popup = ~paste0(
                 "<h4>", as.character(colegio), "</h4>",
                 "<h4>Zona: ", as.character(zona), "</h4>",
                 "<hr>",
                 "Votos no local: ", as.character(qt_votos)
               ),
               label = ~as.character(colegio))
  
})

# click_local_votacao <- eventReactive(input$map_votos_secao_marker_click, {
# click_local_votacao <- eventReactive(input$map_votos_secao_click, {
click_local_votacao <- eventReactive(is_candidatos(), {
  
  marker_click <- click_evaluator$clickedMarker # O "_shape_click" é criado internamente pelo leaflet.
  
  marker_id <- marker_click$id
  
  return(marker_id)
  
})

output$tabela_votos_secao <-
  renderDataTable({
    
    click <- click_local_votacao()
    
    if(is.null(click)) {
      
      lst_secoes <- 
        zona_secoes_geoloc %>%
        dplyr::pull(secoes)
      
      df_secao <-
        votacao_poa %>%
        filter(
          ds_cargo == input$select_voting_level &
            nm_votavel == input$select_candidate &
            nr_zona == input$selected_zone &
            nr_turno == input$select_voting_turn
        )
      
      df_secao %>% 
        mutate(
          nr_secao = as.character(nr_secao)
        ) %>% 
        left_join(zona_secoes_geoloc,
                  by = c("nr_zona" = "zona",
                         "nr_secao" = "secoes")) %>% 
        select(
          colegio,
          nr_zona,
          nr_secao,
          qt_votos
        )
      
    } else {
      
      local <- unlist(strsplit(click_local_votacao(), "\\|"))
      
      lst_secoes <- 
        zona_secoes_geoloc %>%
        filter(
          colegio == local[1] & zona == local[2]
        ) %>% 
        dplyr::pull(secoes)
      
      df_secao <-
        votacao_poa %>%
        filter(
          # ds_cargo == "PREFEITO" &
          # nm_votavel == "VOTO NULO" &
          ds_cargo == input$select_voting_level &
            nm_votavel == input$select_candidate &
            nr_turno == input$select_voting_turn &
            nr_zona == local[2] &
            nr_secao %in% lst_secoes
        )
      
      df_secao %>% 
        mutate(
          nr_secao = as.character(nr_secao)
        ) %>% 
        left_join(zona_secoes_geoloc,
                  by = c("nr_zona" = "zona",
                         "nr_secao" = "secoes")) %>% 
        select(
          colegio,
          nr_zona,
          nr_secao,
          qt_votos
        )
      
    }
    
    
    
  })

output$total_votos_zonas <-
  # renderDataTable({
  renderHighchart({
    
    # browser()
    
    votacao_poa %>%
      filter(
        # ds_cargo == "PREFEITO" &
        # nm_votavel == "VOTO NULO"
        ds_cargo == input$select_voting_level &
          nm_votavel == input$select_candidate &
          nr_turno == input$select_voting_turn
      ) %>% 
      group_by(nr_zona) %>% 
      summarise(qt_votos = sum(qt_votos)) %>% 
      arrange(desc(qt_votos)) %>% 
      hchart(
        "bar",
        hcaes(x = paste0("Zona :", as.character(nr_zona)),
              y = qt_votos),
        name = "Quantidade de Votos"
      ) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "Quantidade de Votos"))
    
  })
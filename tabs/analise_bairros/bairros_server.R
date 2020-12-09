bairro_click <- 
  reactiveValues(clickedMarker = NULL)

observeEvent(
  input$mapa_bairros_filtro_shape_click,
  {
    bairro_click$clickedMarker <- input$mapa_bairros_filtro_shape_click
  }
)

observeEvent(
  input$mapa_bairros_filtro_click,
  {
    bairro_click$clickedMarker <- NULL
  }
)

is_bairros <-
  reactive({
    
    if(input$navbar == "bairros") {
      
      return(paste(input$navbar , bairro_click$clickedMarker))
      
    } else {
      
      return(NULL)
    }
    
  })


output$titulo_mapa_bairros <-
  renderUI({
    
    HTML(paste0("<h4><center><strong>Mapa da votação para prefeito no ", 
                input$select_voting_turn_bairros, "º Turno",
                "</strong></center></h4>")
    )
    
  })

output$mapa_bairros_filtro <-
  renderLeaflet({
    
    mais_votado_por_bairro <-
      sf_secoes_geoloc_bairro %>% 
      select(
        secoes, zona, NOME
      ) %>% 
      right_join(
        votacao_poa %>% 
          filter(ds_cargo == "PREFEITO" &
                   nr_turno == input$select_voting_turn_bairros) %>% 
          mutate(
            nr_secao = as.character(nr_secao)
          ),
        by = c("zona" = "nr_zona",
               "secoes" = "nr_secao")
      ) %>% 
      group_by(NOME, nm_votavel) %>% 
      summarise(
        qt_votos = sum(qt_votos, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      group_by(NOME) %>% 
      filter(
        qt_votos == max(qt_votos)
      )
    
    mapa_bairros_poa %>% 
      group_by(NOME) %>% 
      summarise(
        geometry = sf::st_union(geometry)
      ) %>% 
      ungroup() %>% 
      left_join(mais_votado_por_bairro,
                by = "NOME") %>% 
      mutate(
        cor_poligono = 
          case_when(
            nm_votavel == "SEBASTIÃO DE ARAÚJO MELO" ~ "green",
            nm_votavel == "MANUELA PINTO VIEIRA D AVILA" ~ "red",
            nm_votavel == "NELSON MARCHEZAN JÚNIOR" ~ "blue",
            TRUE ~ "grey"
          ),
        nm_votavel = ifelse(is.na(nm_votavel), "Não existem urnas nesse bairro.", nm_votavel)
      ) %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(color = "grey",
                  weight = 1,
                  opacity = 1,
                  fillColor = ~cor_poligono,
                  fillOpacity = 0.65,
                  layerId = ~paste0(NOME),
                  popup = ~paste0(
                    "<h4>", as.character(NOME), "</h4>",
                    "<hr>",
                    "<h4>Candidato a prefeito mais votado: <br>", stringr::str_to_title(nm_votavel), "</h4>"
                  )) %>% 
      addLegend(
        position = "bottomleft",
        labels = 
          c(
            "MANUELA PINTO VIEIRA D AVILA",
            "NELSON MARCHEZAN JÚNIOR",
            "SEBASTIÃO DE ARAÚJO MELO",
            "Não existem urnas nesse bairro."
          ),
        colors = c("red", "blue", "green", "grey")
      )
    
    
  })

click_bairro_votacao <- eventReactive(is_bairros(), {
  
  shape_click <- bairro_click$clickedMarker # O "_shape_click" é criado internamente pelo leaflet.
  
  shape_id <- shape_click$id
  
  return(shape_id)
  
})

output$hchart_votos_prefeito <-
  renderHighchart({
    
    if (is.null(click_bairro_votacao())) {
      bairro_filter <- sf_secoes_geoloc_bairro %>% 
        pull(NOME) %>% 
        unique()
      
      sf_secoes_geoloc_bairro %>% 
        select(
          secoes, zona, NOME
        ) %>%
        filter(
          # NOME == "CENTRO HISTÓRICO"
          NOME %in% bairro_filter
        ) %>% 
        right_join(
          votacao_poa %>% 
            filter(ds_cargo == "PREFEITO" &
                     nr_turno == input$select_voting_turn_bairros) %>% 
            mutate(
              nr_secao = as.character(nr_secao)
            ),
          by = c("zona" = "nr_zona",
                 "secoes" = "nr_secao")
        ) %>% 
        filter(
          !is.na(NOME)
        ) %>% 
        group_by(nm_votavel) %>% 
        summarise(
          qt_votos = sum(qt_votos, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        arrange(desc(qt_votos)) %>% 
        hchart(
          "column",
          hcaes(x = nm_votavel,
                y = qt_votos),
          name = "Quantidade de Votos"
        ) %>% 
        hc_xAxis(
          title = list(text = "Nome do Candidato")
        ) %>% 
        hc_yAxis(
          title = list(text = "Quantidade de Votos")
        ) %>% 
        hc_title(
          text = "Votação total para Porto Alegre"
        )
      
    } else {
      bairro_filter <- click_bairro_votacao()
      
      data_prep <- 
        sf_secoes_geoloc_bairro %>% 
        select(
          secoes, zona, NOME
        ) %>%
        filter(
          # NOME == "CENTRO HISTÓRICO"
          NOME %in% bairro_filter
        ) %>% 
        right_join(
          votacao_poa %>% 
            filter(ds_cargo == "PREFEITO" &
                     nr_turno == input$select_voting_turn_bairros) %>% 
            mutate(
              nr_secao = as.character(nr_secao)
            ),
          by = c("zona" = "nr_zona",
                 "secoes" = "nr_secao")
        ) %>% 
        filter(
          !is.na(NOME)
        ) 
      
      data_prep %>% 
        group_by(NOME, nm_votavel) %>% 
        summarise(
          qt_votos = sum(qt_votos, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        arrange(desc(qt_votos)) %>% 
        hchart(
          "column",
          hcaes(x = nm_votavel,
                y = qt_votos),
          name = "Quantidade de Votos"
        ) %>% 
        hc_xAxis(
          title = list(text = "Nome do Candidato")
        ) %>% 
        hc_yAxis(
          title = list(text = "Quantidade de Votos")
        ) %>% 
        hc_title(
          text = paste0("Votos no bairro: ", stringr::str_to_title(bairro_filter))
        ) %>% 
        hc_subtitle(
          text = paste0("Zonas: ", paste(c(data_prep %>% pull(zona) %>% unique %>% sort), collapse = ", "), "<br>", 
                        "Seções: ", paste(c(data_prep %>% pull(secoes) %>% unique %>% sort), collapse = ", "))
        )
    }
    
  })

output$tabela_votos_vereadores <-
  renderDataTable({
    
    if (is.null(click_bairro_votacao())) {
      bairro_filter <- sf_secoes_geoloc_bairro %>% 
        pull(NOME) %>% 
        unique()
      
      sf_secoes_geoloc_bairro %>% 
        select(
          secoes, zona, NOME
        ) %>%
        filter(
          # NOME == "CENTRO HISTÓRICO"
          NOME %in% bairro_filter
        ) %>% 
        right_join(
          votacao_poa %>% 
            filter(ds_cargo == "VEREADOR") %>% 
            mutate(
              nr_secao = as.character(nr_secao)
            ),
          by = c("zona" = "nr_zona",
                 "secoes" = "nr_secao")
        ) %>% 
        filter(
          !is.na(NOME)
        ) %>% 
        group_by(nm_votavel) %>% 
        summarise(
          qt_votos = sum(qt_votos, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        arrange(desc(qt_votos))
      
    } else {
      bairro_filter <- click_bairro_votacao()
      
      sf_secoes_geoloc_bairro %>% 
        select(
          secoes, zona, NOME
        ) %>%
        filter(
          # NOME == "CENTRO HISTÓRICO"
          NOME %in% bairro_filter
        ) %>% 
        right_join(
          votacao_poa %>% 
            filter(ds_cargo == "VEREADOR") %>% 
            mutate(
              nr_secao = as.character(nr_secao)
            ),
          by = c("zona" = "nr_zona",
                 "secoes" = "nr_secao")
        ) %>% 
        filter(
          !is.na(NOME)
        ) %>% 
        group_by(NOME, nm_votavel) %>% 
        summarise(
          qt_votos = sum(qt_votos, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        arrange(desc(qt_votos)) 
    }
    
  })
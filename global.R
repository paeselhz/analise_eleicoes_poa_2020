library(DT)
library(sf)
library(shiny)
library(dplyr)
library(leaflet)
library(highcharter)
library(shinyWidgets)

btn_landing <-
  function(texto, cor, id) {
    HTML(
      paste0(
        '<a id="', id,'" href="#" class="action-button">
                  <div class = "tab-block-landing tab-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', texto, '</span>
                  </div>
        </a>'
      )
    )
  }

zona_secoes_geoloc <-
  readr::read_rds('data/zona_secoes_geolocalizada.rds') %>% 
  mutate(
    geolocation = ifelse(colegio == "ESCOLA ESTADUAL INFANTE DOM HENRIQUE", list(c(-30.0592714,-51.2211452)), geolocation),
    geolocation = ifelse(colegio == "SOCIEDADE GINASTICA PORTO ALEGRE", list(c(-30.0083155,-51.1893282)), geolocation),
    geolocation = ifelse(colegio == "ESCOLA ESTADUAL OSCAR SCHMITT - I.DAS FLORES", list(c(-29.9863437,-51.2468938)), geolocation),
    geolocation = ifelse(colegio == "BENTO GONCALVES - ESC. EST. DE ENSINO FUNDAMENTAL", list(c(-30.0005489,-51.102625)), geolocation),
    geolocation = ifelse(colegio == "DECIO MARTINS COSTA - E. M. E. F - SANTO AGOSTINHO", list(c(-29.9898123,-51.1071067)), geolocation),
    geolocation = ifelse(colegio == "ESCOLA ESTADUAL ALVARENGA PEIXOTO - I.G.MARINHEIROS", list(c(-29.9917759,-51.2246585)), geolocation),
    geolocation = ifelse(colegio == "ESCOLA ESTADUAL MARIA JOSE MABILDE - I.PINTADA", list(c(-30.028018,-51.2535289)), geolocation),
    geolocation = ifelse(colegio == "ESCOLA ESTADUAL DE EDUCAÇÃO BÁSICA DOLORES ALCARAZ CALDAS", list(c(-30.0188076,-51.1616093)), geolocation),
    geolocation = ifelse(colegio == "ITAMARATI - ESC. EST. DE ENS. FUNDAMENTAL", list(c(-30.0074741,-51.1303127)), geolocation),
    geolocation = ifelse(colegio == "PRES. ARTHUR DA COSTA E SILVA - COLEGIO EST. DE ENS. MEDIO", list(c(-29.9994958,-51.1378135)), geolocation),
    geolocation = ifelse(colegio == "VERA CRUZ, ESC EST ENS FUND", list(c(-30.0735253,-51.2042364)), geolocation),
  ) %>% 
  filter(!is.na(geolocation)) %>% 
  tidyr::unnest_wider(geolocation) %>% 
  janitor::clean_names() %>% 
  rename(
    lat = x1,
    long = x2
  ) %>% 
  mutate(
    secoes = stringr::str_split(secoes, ",")
  ) %>% 
  tidyr::unnest_longer(secoes)

votacao_poa <-
  # readr::read_rds('data/votacoes_poa_2020_1_turno.rds')
  readr::read_rds('data/votacoes_poa_2020_1_turno_smaller.rds')

ui_files <-
  list.files(
    path = "tabs",
    pattern = "*_ui",
    recursive = TRUE,
    full.names = TRUE
  )

mapa_bairros_poa <-
  readr::read_rds("data/mapa_bairros_poa.rds")

sf_secoes_geoloc_bairro <-
  readr::read_rds("data/zonas_secoes_bairros.rds")

purrr::walk(ui_files, ~source(.x))

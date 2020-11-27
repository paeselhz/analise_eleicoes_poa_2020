library(sf)
library(dplyr)

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

mapa_bairros_poa <-
  readr::read_rds("data/mapa_bairros_poa.rds") %>% 
  st_transform(
    crs = 4674
  )

sf_secoes_geoloc_bairro <-
  zona_secoes_geoloc %>% 
  sf::st_as_sf(
    coords = c('long', 'lat'),
    agr = 'constant',
    crs = 4674,
    stringsAsFactors = FALSE,
    remove = FALSE
  ) %>% 
  sf::st_join(mapa_bairros_poa, join = sf::st_within) %>% 
  st_drop_geometry() %>% 
  select(
    zona, secoes, NOME
  ) %>% 
  mutate(
    NOME = ifelse(is.na(NOME), "LOMBA DO PINHEIRO", NOME)
  )

readr::write_rds(sf_secoes_geoloc_bairro, "data/zonas_secoes_bairros.rds")

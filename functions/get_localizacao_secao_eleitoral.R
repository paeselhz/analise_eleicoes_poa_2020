library(dplyr)
library(rvest)

source('functions/get_address.R')

get_location_secao_eleitoral <-
  function(zona_eleitoral) {
    
    url <-
      paste0(
        'https://eleitor.tre-rs.jus.br/locais-votacao/locais?q%5Bzona_numero_eq%5D=',
        zona_eleitoral,
        '&q%5Bmunicipio_id_eq%5D=7994&button='
      )
    
    tre_url <-
      xml2::read_html(url)
    
    tre_table <- 
      tre_url %>% 
      html_node(xpath = '//*[@id="content"]/table') %>% 
      html_table() %>% 
      janitor::clean_names()
    
    tbl_return <- 
      tre_table %>% 
      tidyr::separate(col = local, sep = '\n', into = c("colegio", "localizacao")) %>% 
      dplyr::mutate(
        zona = zona_eleitoral,
        colegio = trimws(colegio),
        localizacao = trimws(localizacao),
        localizacao = paste0(localizacao, ", Porto Alegre, RS"),
        secoes = stringr::str_replace_all(secoes, "\\n", ","),
        secoes = stringr::str_replace_all(secoes, " ", "")
      )
    
    return(tbl_return)
    
  }

list_secoes_eleitorais <-
  readr::read_rds('data/votacoes_poa_2020_1_turno.rds') %>% 
  dplyr::pull(nr_zona) %>% 
  unique()

tbl_localizacoes_zona_secao <-
  purrr::map_df(
    list_secoes_eleitorais,
    get_location_secao_eleitoral
  )

here_app_id <- "8ZUyu4KYphBTLj2Ww8pa"
here_api_key <- "ZjsVyStvxJM2AM1edGZ7Nxh7QGx2YQ6cM-OgTg3Ol8A"

zona_secao_geolocalizada <-
  tbl_localizacoes_zona_secao %>% 
  # sample_n(10) %>% 
  rowwise() %>% 
  mutate(
    geolocation = list(get_address(localizacao, here_api_key))
  ) %>% 
  ungroup()

readr::write_rds(zona_secao_geolocalizada, 'data/zona_secoes_geolocalizada.rds')


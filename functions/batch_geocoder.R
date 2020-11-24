batch_geocoder <-
  function (df, search_var, country, here_id, here_code) {
    `%>%` <- magrittr::`%>%`
    post_url <- paste0("https://batch.geocoder.api.here.com/6.2/jobs", 
                       "?apiKey=", here_code,
                       "&indelim=%7C", 
                       "&outdelim=%7C", "&action=run", "&outcols=displayLatitude,displayLongitude,", 
                       "state,country", "&outputcombined=false")
    df_search <- df %>% dplyr::select({
      {
        search_var
      }
    }, {
      {
        country
      }
    }) %>% dplyr::mutate(recId = stringr::str_pad(rank({
      {
        search_var
      }
    }), 5, pad = "0")) %>% dplyr::select(recId, searchText = {
      {
        search_var
      }
    }, country = {
      {
        country
      }
    })
    tmp <- tempfile()
    write.table(df_search, tmp, sep = "|", na = "", row.names = FALSE, 
                quote = FALSE)
    post_body <- readr::read_file(tmp)
    post_req <- httr::POST(url = post_url, body = post_body)
    request_id <- httr::content(post_req) %>% .$Response %>% 
      .$MetaInfo %>% .$RequestId
    while (here_get_job_status(request_id, here_id, here_code) != 
           "completed") {
      message("Waiting for job completion")
      Sys.sleep(5)
    }
    filename <- here_download_job(request_id, here_id, here_code)
    return_df <- df_search %>% dplyr::select(-country) %>% dplyr::mutate(recId = as.numeric(recId)) %>% 
      dplyr::left_join(read.table(filename, header = TRUE, 
                                  sep = "|") %>% dplyr::filter(SeqNumber == 1), by = "recId") %>% 
      dplyr::select(searchText, latitude, longitude, country)
    file.remove(filename)
    return(return_df)
  }

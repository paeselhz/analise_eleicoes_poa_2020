
get_address <-
  function(address, here_api_key) {
    print(address)
    url <-
      paste0("https://geocode.search.hereapi.com/v1/geocode?q=", URLencode(address), "&apiKey=", here_api_key)
    
    here_return <-
      httr::GET(
        url
      )
    
    if (here_return$status_code == 200) {
      here_content <- 
        here_return %>% 
        httr::content()
      
      if(length(here_content$items) == 0) {
        return(NA)
      } else {
        lat <- 
          here_content$items[[1]]$position$lat
        
        long <- 
          here_content$items[[1]]$position$lng
        
        return(c(lat, long))       
      }
    } else {
      return(NA)
    }
    
  }

#' @title Scrapes a page of entries
#' @description this function uses the information from getEntries
#'    in order to scrape a single page of data about stories
#' @param returns a list returned by getPages
#' @import rvest
#' @return returns a list of scraped entries from one page in a raw
#'    character string.
#' @examples
#' getEntries(list(noPages=FALSE, page_use="/cartoon/Invader-Zim/?&srt=1&r=103&p=",
#' page_num=11, url="https://www.fanfiction.net/cartoon/Invader-Zim/", max.entries=NA))
getEntries <- function(returns) {

  noPages <- returns[[1]]
  page_use <- returns[[2]]
  page_num <- returns[[3]]
  url <- returns[[4]]
  max.entries <- returns[[5]]

  totalOut<-0

  scrapeEntries <- function(url, total){
    info <- character(0)
    total <- totalOut
    for(i in 4:28){
      if(!is.na(max.entries)) {if(max.entries - totalOut == 0) return(info)}
      entry <- url %>% read_html() %>%
        html_nodes(xpath=paste('//*[@id="content_wrapper_inner"]/div[',i,']')) %>%
        html_text()
      info <- c(info, entry)
      total<-total+1
      totalOut <<- total
    }
    return(info)
  }

  if(noPages==TRUE){
    entries <- scrapeEntries(url, totalOut)
    return(entries)

  } else {
    entries <- lapply(paste0('https://www.fanfiction.net/',page_use, 1:page_num),
                      scrapeEntries, total=totalOut)
    return(entries)
  }

}

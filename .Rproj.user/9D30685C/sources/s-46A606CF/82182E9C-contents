#' @title Get information on the quantity and structure of fandom pages
#'
#' @description This function takes the output of the getURL function
#'    and generates information based on the amount of pages and the
#'    structure of the site.
#'
#' @param returns list passed from getUrl function
#'
#' @import rvest
#' @return returns a list with 4 elements
#'    a boolean that returns TRUE if there are no pages associated with the URL
#'    a string that gives the pattern of the URLs of the pages
#'    a numeric with the total number of pages
#'    a string with the url passed to the function
#'    max.entries passed from getUrl
#'
#' @examples
#' getPages(list(url="https://www.fanfiction.net/cartoon/Invader-Zim/", max.entries=NA))
#' getPages(list(url="https://www.fanfiction.net/game/Kingdom-Hearts/", max.entries=10))
getPages <- function(returns) {
  url <- returns[[1]]
  max.entries <- returns[[2]]
  returns <-list()
  library(rvest)

  page <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="content_wrapper_inner"]/center[1]/a[4]') %>%
    html_attr("href")

  if(length(page)>0){
    # if there are multiple pages of entries
    page.g <- strsplit(page, "=", " ")
    page_num <- as.numeric(page.g[[1]][4])
    page_use <- as.character(gsub(page_num,"", page))

    returns <- list(noPages=FALSE, page_use=page_use, page_num=page_num, url=url, max.entries=max.entries)
  } else {
    returns <- list(noPages=TRUE, page_use=NA, page_num=NA,url=url, max.entries= max.entries)
  }

  return(returns)
}

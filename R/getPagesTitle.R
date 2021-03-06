#' @title Get Pages to Iterate When Scraping Specific Story
#'
#' @description
#' \code{getPagesTitle.absentfan} Helper function of getChapter to get the total number of pages to iterate through
#' if the catalogue of entries is multiple pages.
#'
#' @param returns A list produced by \code{getUrlTitle.absentfan} including the full url of the associated fandom and the title of the fanfic being retrieved.
#'
#' @return A list including noPages (bool whether there are multiple pages of entries to scrape), page_use (href, if needed), page_num (total number of pages to iterate), url, and story title.
#' @examples
#' getPagesTitle(list(url="https://www.fanfiction.net/book/Harry-Potter/", title="Modern Marauders"))
getPagesTitle <- function(returns) {
  url <- returns[[1]]
  title <- returns[[2]]
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

    returns <- list(noPages=FALSE, page_use=page_use, page_num=page_num, url=url, title=title)
  } else {
    returns <- list(noPages=TRUE, page_use=NA, page_num=NA,url=url, title=title)
  }

  return(returns)
}

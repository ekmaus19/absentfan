#' @title Fanfiction Data Scraper
#' @description Scrapes the data on stories on Fanfiction.net by fandom.
#' @param story Which Fandom would you like to scrape?
#' @param type What media type is it?
#' @param max.entries How many Entries would you like to scrape?
#'    If NA, scrape all of the entries.
#' @import rvest
#' @import stringr
#' @import stringi
#' @import lubridate
#' @import dplyr
#' @import qdapRegex
#' @export
#' @return returns a dataframe with details about the entries scraped
#' @examples
#' scrapeAllEntries("Harry Potter", "book", max.entries=11)
scrapeAllEntries <- function(story, type, max.entries=NA) {
  return(all_entries(getEntries(getPages(getUrl(story,type, max.entries)))))
}

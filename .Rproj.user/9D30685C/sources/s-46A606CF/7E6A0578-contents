#' @title Get URL of a Fandom
#'
#' @description For a given fandom in a certain medium,
#'    this function gives the URL of its story page.
#'
#' @param story Which Fandom would you like the URL for?
#' @param type Which type of media? Must be one of the following
#'    ("anime", "book", "cartoon", "comic", "game", "misc", "tv")
#' @param max.entries How many Entries to scrape?
#' @return returns a list with 2 entries
#'    the URL of the fandom page as a string
#'    max.entries
#' @examples
#' getUrl("Zoolander","movie")
#' getUrl("Kingdom Hearts","game", max.entries=10)
getUrl <- function(story, type, max.entries = NA) {
  if (!(type %in% names(typeMedia))) stop("Unsupported Type: use from list")
  rowVal <- grep(story, typeMedia[[type]]$title)
  href <- typeMedia[[type]]$href[rowVal[1]]
  # make desired url
  url <- paste0("https://www.fanfiction.net", href)
  returns <- list(url = url, max.entries = max.entries)
  return(returns)
}

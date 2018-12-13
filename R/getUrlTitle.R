#' @title Get URL of Specific Story
#'
#' @description
#' \code{getUrlTitle.absentfan} Helper function of getChapter retrieving the full url of fandom from fanfiction.net while passing down the title
#' of a story to be scraped by later functions.
#'
#' @param story Name of fandom being searched. Ex, "Harry Potter."
#' @param type Whether the fandom is from the media type of anime, book, cartoon, comic, game, misc, movie, play, or tv.
#' @param title The title of the specific fanfic post to be scraped.
#'
#' @return A list including the full url and the title of the fanfic being retrieved.
#' @examples
#' getUrlTitle("Harry Potter", "book", "Modern Marauders")
getUrlTitle <- function(story, type, title) {
  rowVal <- grep(story,typeMedia[[type]]$title)
  href <- typeMedia[[type]]$href[rowVal[1]]
  # make desired url
  url <- paste0("https://www.fanfiction.net", href)
  returns <- list(url=url,title=title)
  return(returns)
}

#' Scraper of Chapter Content in Fanfiction
#'
#' @description
#' \code{getFullStory.absentfan} Scrapes fanfiction.net for a specific fanfiction entry (ie, user generated story)
#' based on the fandom (ie, general story narrative), type, and the verbatim title of the story.
#'
#' @param story Name of fandom being searched. Ex, "Harry Potter."
#' @param type Whether the fandom is from the media type of anime, book, cartoon, comic, game, misc, movie, play, or tv.
#' @param title The title of the specific fanfic post to be scraped.
#'
#' @import stringr
#' @import dplyr
#' @import qdapRegex
#' @import rvest
#'
#' @export
#' @return A data frame with two columns; the raw text of the scraped entries, and the chapter designations for each text vector.
#' @examples
#' getFullStory("Harry Potter", "book", "Modern Marauders")
getFullStory <- function(story, type, title) {
  getAllChapters(getTitles(getPagesTitle(getUrlTitle(story,type,title))))
}

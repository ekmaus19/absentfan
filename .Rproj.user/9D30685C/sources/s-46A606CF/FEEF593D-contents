#' @title Update Href tags for Type Media
#'
#' @description This function scrapes each media page of fanfiction.net
#'    and returns a list of the href tags of each fandom. This takes a
#'    few minutes.
#'
#' @details Use this function to update the href tags only if you believe
#'    you are missing a very recent story, as the tags do not change much
#'    if at all from day to day.
#'
#' @import stringr
#' @export
#' @return list with all href tags for each type of media
#' @examples
#' typeMedia <- updateTypeMedia()
updateTypeMedia <- function(){
  library(stringr)
  type <- c("anime", "book", "cartoon", "comic", "game", "misc", "movie", "play", "tv")
  typeMedia <- list()
  for(i in seq_along(type)){
    url <- paste0("https://www.fanfiction.net/", type[i])
    html <- paste(readLines(url), collapse="\n")
    matched <- str_match_all(html, "<a href=\"(.*?)\"")
    names <- str_match_all(html, "title=\"(.*?)\"")

    # temp<-data.frame()
    temp <- data.frame(href=matched[[1]][,2], title=names[[1]][,2])
    typeMedia[[i]] <- temp
    names(typeMedia)[i] <- type[i]
    temp<-NULL
  }
  return(typeMedia)
}

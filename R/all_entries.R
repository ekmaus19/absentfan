#' @title All Entry Scraper
#' @description This function Scrapes all of the pages in a particular fandom and turns it into a dataframe with information
#' @param entries a list passed from getEntries
#' @import dplyr
#' @import stringr
#' @import qdapRegex
#' @return returns a dataframe with relevant information encoded in the entries such as author, rating, follows, and, date published.
#' @examples
#' all_entries(getEntries(getPages(getUrl("NCIS", "tv"))))
all_entries<-function(entries) {
  makeDF <- function(entries) {
    full <- data.frame(matrix(NA, nrow = length(entries), ncol = 13))
    names(full) <- c("name","author","description","rating","language","theme",
                     "chapters","words","reviews","favs","follows","updated",
                     "published")
    t<- str_match(entries, "(.)+")
    t<- str_split(t[,1], " by ")
    summary <-  rm_between(entries[1], "\\n\t", "Rated:", extract=TRUE)
    test <- strsplit(entries, "Rated:")
    for(i in seq_along(entries)) {
      name <- trimws(t[[i]][1])
      author <- trimws(gsub(" reviews", "", t[[i]][2]))
      description <-  rm_between(entries[i], "\\n\t", "Rated:", extract=TRUE)

      summaries<-strsplit(test[[i]][2], " - ")
      rating <- trimws(summaries[[1]][1])
      language <- summaries[[1]][2]
      theme <- ifelse(grepl("Chapter",summaries[[1]][3])==T, NA, summaries[[1]][3])
      chapters <- trimws(gsub("Chapters: ", "",  str_match(test[[i]][2], "Chapters: [0-9,]+ ")))
      words <- trimws(gsub("Words: ", "",  str_match(test[[i]][2], "Words: [0-9,]+ ")))
      reviews <- trimws(gsub("Reviews: ", "",  str_match(test[[i]][2], "Reviews: [0-9,]+ ")))
      favs <- trimws(gsub("Favs: ", "",  str_match(test[[i]][2], "Favs: [0-9,]+ ")))
      follows <- trimws(gsub("Follows: ", "",  str_match(test[[i]][2], "Follows: [0-9,]+ ")))
      updated <-  rm_between(test[[i]][2], "Updated: ", " - ", extract=TRUE)
      published <-  rm_between(test[[i]][2], "Published: ", " - ", extract=TRUE)

      if(isTRUE(str_count(updated, "/")==1)){
        updated <- paste0(updated,"/",as.character(year(Sys.Date())))
      } else {
        updated<-updated
      }

      if(isTRUE(str_count(published, "/")==1)){
        published <- paste0(published,"/", as.character(year(Sys.Date())))
      } else {
        published<-published
      }
      ##### throwing error when making the data frame
      full$name[i]<- name
      full$author[i]<- author
      full$description[i]<- description
      full$rating[i]<- rating
      full$language[i]<- language
      full$theme[i]<- theme
      full$chapters[i]<- chapters
      full$words[i]<- words
      full$reviews[i] <- reviews
      full$favs[i]<- favs
      full$follows[i]<- follows
      full$updated[i]<- updated
      full$published[i]<- published
    }
    return(full)
  }
  if (typeof(entries) == "character") {
    full <- makeDF(entries)
  } else if (typeof(entries) == "list"){
    full <- lapply(entries, makeDF) %>% bind_rows()
  }
  return(full)
}

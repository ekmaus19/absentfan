#' Chapter to HTML Output
#'
#' @description Generate an easily readable html document with a fully scraped fanfiction retrieved from fanfiction.net.
#'
#' @param story Name of fandom being searched. Ex, "Harry Potter."
#' @param type Whether the fandom is from the media type of anime, book, cartoon, comic, game, misc, movie, play, or tv.
#' @param title The title of the specific fanfic post to be scraped.
#' @param browse If \code{TRUE}, use external browser
#'
#' @export
#' @return path to HTML file
#' @examples
#' storyPrint("Harry Potter", "book", "Modern Marauders")
storyPrint <- function(story, type, title,browse=FALSE){
  story <- deparse(substitute(story))
  type <- deparse(substitute(type))
  title <- deparse(substitute(title))

  # read template file and modify
  report <- readLines(system.file("templateStory.txt", package = "absentfan"))
  report <- gsub("xxxStory", story, report, fixed = TRUE)
  report <- gsub("xxxType", type, report, fixed = TRUE)
  report <- gsub("xxxTitle", title, report, fixed = TRUE)

  # output template and render
  tf <- tempfile(fileext = ".Rmd")
  to <- tempfile(fileext = ".html")
  writeLines(report, tf)
  library(rmarkdown)
  render(input=tf, output_format="html_document", output_file=to)
  file.show(to)
  if (browse){
    file.show(to)
  } else {
    viewer <- getOption("viewer")
    viewer(to)
  }
  invisible(to) # returns the file path (the object to) but doesnt print it to the console
}

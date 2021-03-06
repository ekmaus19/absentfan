% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAllChapters.R
\name{getAllChapters}
\alias{getAllChapters}
\title{Iterate Through All Chapters in Fanfiction Being Scraped}
\usage{
getAllChapters(returns)
}
\arguments{
\item{returns}{A list generated by getTitles including a url (of fanfiction.net) and the href of the story being queried.}
}
\value{
A data frame with the scraped text and chapter labels for each entry.
}
\description{
\code{getAllChapters.absentfan} Helper function of getChapter iterating through scraping chapters from specific entry in fanfiction.net.
}
\examples{
getAllChapters(list(url="https://www.fanfiction.net",href="/s/13084808/1/Modern-Marauders"))
}

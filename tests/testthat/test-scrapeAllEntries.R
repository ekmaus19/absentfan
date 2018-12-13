context("main entry scraper")
test_that("getUrl Works",{
  expect_equal(getUrl("Harry Potter", "book"),list(url="https://www.fanfiction.net/book/Harry-Potter/",max.entries=NA))
  expect_equal(getUrl("Zoolander", "movie", max.entries = 2),list(url="https://www.fanfiction.net/movie/Zoolander/",max.entries=2))
})

test_that("getPages for entries works",{
  expect_equal(getPages(list(url="https://www.fanfiction.net/book/Harry-Potter/",max.entries=NA)),
               list(noPages=FALSE, page_use="/book/Harry-Potter/?&srt=1&r=103&p=", page_num=11, url="https://www.fanfiction.net/book/Harry-Potter/", max.entries=NA))
  expect_equal(getPages(list(url="https://www.fanfiction.net/movie/Zoolander/",max.entries=2)),
               list(noPages=TRUE, page_use=NA, page_num=NA, url="https://www.fanfiction.net/movie/Zoolander/", max.entries=2))
})

test_that("all_entries returns a dataframe", {
  expect_equal(typeof(all_entries(getEntries(getPages(getUrl("Zoolander", "movie"))))), "list")
})


# News-feed reading (R/news.R): parsing, filtering and caching, with no network.
skip_if_not_installed("xml2")

feed <- '<?xml version="1.0"?><rss version="2.0"><channel><title>T</title>
<item><title>Metro line extended to the airport</title><link>https://example.org/a</link><pubDate>Thu, 15 Oct 2026 10:00:00 +0530</pubDate></item>
<item><title><![CDATA[Air quality worsens in the capital]]></title><link>https://example.org/b</link><pubDate>16 Oct 2026 08:00:00 +0000</pubDate></item>
<item><title>Cricket team wins series</title><link>https://example.org/c</link></item>
<item><title>No link here about hospitals</title><link>javascript:alert(1)</link></item>
<item><title>Old housing story</title><link>https://example.org/d</link><pubDate>Mon, 01 Jan 2024 10:00:00 +0000</pubDate></item>
</channel></rss>'

test_that("RSS items are parsed with dates in both formats and unsafe links dropped", {
  x <- parse_rss(feed, "Example")
  expect_equal(nrow(x), 4)                                   # the javascript: link is dropped
  expect_false(any(grepl("javascript", x$Link)))
  expect_equal(x$Title[2], "Air quality worsens in the capital")
  expect_equal(format(x$Date[1], "%Y-%m-%d %H:%M", tz = "UTC"), "2026-10-15 04:30")
  expect_equal(format(x$Date[2], "%Y-%m-%d %H:%M", tz = "UTC"), "2026-10-16 08:00")
  expect_true(is.na(x$Date[3]))
  expect_equal(x$Topic[1:3], c("Transport", "Environment", NA))
})

test_that("only recent livability headlines are kept, newest first", {
  x <- parse_rss(feed, "Example")
  s <- select_news(x, n = 4, now = as.POSIXct("2026-10-16 12:00", tz = "UTC"))
  expect_equal(s$Title, c("Air quality worsens in the capital", "Metro line extended to the airport"))
})

test_that("bad or empty input gives an empty result, not an error", {
  expect_equal(nrow(parse_rss("not xml at all", "X")), 0)
  expect_equal(nrow(parse_rss("<rss><channel></channel></rss>", "X")), 0)
})

test_that("the cache round-trips and expires", {
  f <- tempfile(fileext = ".rds")
  write_news_cache(list(items = parse_rss(feed, "Example"), errors = character(0), fetched_at = Sys.time()), f)
  expect_equal(nrow(read_news_cache(f)$items), 4)
  old <- readRDS(f); old$fetched_at <- Sys.time() - 3 * 3600; saveRDS(old, f)
  expect_null(read_news_cache(f, max_age_hours = 2))
  expect_false(is.null(read_news_cache(f, max_age_hours = 48)))
  writeLines("garbage", f)
  expect_null(read_news_cache(f))
  expect_null(read_news_cache(tempfile()))
})

test_that("the feature can be switched off and feeds overridden", {
  withr::with_envvar(c(LIVABILITY_NEWS = "off"), expect_false(news_enabled()))
  withr::with_envvar(c(LIVABILITY_NEWS_FEEDS = "A|https://a.example/rss;B|https://b.example/rss"), {
    f <- news_feeds()
    expect_equal(f$source, c("A", "B"))
  })
})

# ---- Retrieval over HTTP, against a local web server (no internet needed) ----
skip_if_not_installed("curl")
skip_if_not_installed("httpuv")
local_feed_server <- function() {
  good <- '<?xml version="1.0"?><rss version="2.0"><channel><item><title>New metro line opens</title><link>https://example.org/m</link><pubDate>16 Oct 2026 08:00:00 +0000</pubDate></item></channel></rss>'
  port <- httpuv::randomPort()
  srv <- httpuv::startServer("127.0.0.1", port, list(call = function(req) {
    switch(req$PATH_INFO,
           "/good.xml" = list(status = 200L, headers = list("Content-Type" = "application/rss+xml"), body = good),
           "/bad.xml"  = list(status = 200L, headers = list("Content-Type" = "text/plain"), body = "<rss><channel><item><title>broken"),
           list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "not found"))
  }))
  list(srv = srv, url = function(path) sprintf("http://127.0.0.1:%d%s", port, path))
}

test_that("fetch_news reads a feed, records 404s, malformed XML and unreachable hosts without failing", {
  s <- local_feed_server(); on.exit(httpuv::stopServer(s$srv))
  feeds <- data.frame(source = c("Good", "Missing", "Broken", "Nowhere"),
                      url = c(s$url("/good.xml"), s$url("/missing.xml"), s$url("/bad.xml"), "http://127.0.0.1:9/x.xml"),
                      stringsAsFactors = FALSE)
  # Run the request in a child process so this process can keep serving.
  p <- callr::r_bg(function(root, feeds) { source(file.path(root, "R", "news.R")); fetch_news(feeds, timeout = 5) },
                   args = list(root = normalizePath(ROOT_DIR), feeds = feeds))
  while (p$is_alive()) httpuv::service(50)
  res <- p$get_result()
  expect_equal(res$items$Title, "New metro line opens")
  expect_equal(res$items$Source, "Good")
  expect_equal(res$items$Topic, "Transport")
  expect_length(res$errors, 2)                          # 404 and unreachable host
  expect_true(any(grepl("HTTP 404", res$errors)))
  expect_true(any(grepl("127.0.0.1:9", res$errors)))    # the malformed feed simply yields no items
})

test_that("the background refresh writes the cache and only one refresh runs at a time", {
  s <- local_feed_server(); on.exit(httpuv::stopServer(s$srv))
  cache <- tempfile(fileext = ".rds")
  feeds <- data.frame(source = "Good", url = s$url("/good.xml"), stringsAsFactors = FALSE)
  state <- new.env()
  p1 <- ensure_news_refresh(state, ROOT_DIR, cache, feeds)
  p2 <- ensure_news_refresh(state, ROOT_DIR, cache, feeds)
  expect_identical(p1, p2)                              # no duplicate process while one is alive
  while (p1$is_alive()) httpuv::service(50)
  got <- read_news_cache(cache)
  expect_equal(got$items$Title, "New metro line opens")
  p3 <- ensure_news_refresh(state, ROOT_DIR, cache, feeds)
  expect_false(identical(p1, p3))                       # a new one may start once the first has finished
  while (p3$is_alive()) httpuv::service(50)
})

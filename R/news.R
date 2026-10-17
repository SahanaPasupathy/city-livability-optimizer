# ==============================================================================
# News-feed reading: a few current livability headlines from public RSS feeds
# (used by the app's News tab)
# ------------------------------------------------------------------------------
# * Headlines are external news. They must never be used in, or change, the
#   rankings.
# * start_news_refresh() fetches feeds in a background R process (callr) with
#   short timeouts and caches them on disk, so a slow or missing network cannot
#   block an app that uses it.
# * Nothing is invented: if no feed can be read, the result is empty.
# * Needs the xml2 and curl packages; news_enabled() is FALSE without them.
# * OFF BY DEFAULT (since the product review): the headlines are not about any
#   particular State/UT and are not used in the recommendations, and turning
#   the feature on starts a background process and writes a cache file in the
#   user's cache folder. Set LIVABILITY_NEWS=on to enable it.
# Environment variables (all optional):
#   LIVABILITY_NEWS=on             switch the feature on (default: off)
#   LIVABILITY_NEWS_FEEDS          "Source|URL;Source|URL" to replace the feeds
#   LIVABILITY_NEWS_CACHE          cache file (default: the user cache folder)
# ==============================================================================

# Public RSS feeds of established publishers (checked October 2026).
NEWS_FEEDS <- data.frame(
  source = c("The Hindu", "The Hindu", "Mongabay India"),
  url = c("https://www.thehindu.com/news/cities/feeder/default.rss",
          "https://www.thehindu.com/sci-tech/energy-and-environment/feeder/default.rss",
          "https://india.mongabay.com/feed/"),
  stringsAsFactors = FALSE)

# Livability topics and the words that identify them in a headline. A headline
# that matches none of them is not shown.
NEWS_TOPICS <- list(
  "Environment"    = c("air quality", "aqi", "caqm", "pollution", "smog", "pm2.5", "fuels",
                       "heatwave", "heat wave", "climate", "flood", "forest", "emission", "waste"),
  "Safety"         = c("police", "crime", "safety", "accident"),
  "Transport"      = c("metro", "railway", "rail ", "bus ", "buses", "airport", "highway",
                       "road", "traffic", "transport", "commut"),
  "Housing & urban" = c("housing", "urban", "smart city", "municipal", "civic", "slum", "real estate"),
  "Health"         = c("hospital", "health", "doctor", "vaccin", "disease", "dengue", "medical"),
  "Education"      = c("school", "education", "college", "university", "student", "exam"),
  "Economy & jobs" = c("employment", "jobs", "unemployment", "investment", "industr", "economy"),
  "Water & power"  = c("water supply", "drinking water", "sanitation", "electricity", "power cut",
                       "infrastructure")
)

#' Is the news feature available? Only when LIVABILITY_NEWS is "on" (or
#' "true"/"1") and the xml2 and curl packages are installed. Off by default.
news_enabled <- function() {
  tolower(Sys.getenv("LIVABILITY_NEWS")) %in% c("on", "true", "1") &&
    requireNamespace("xml2", quietly = TRUE) && requireNamespace("curl", quietly = TRUE)
}

#' Feeds to read: NEWS_FEEDS, or LIVABILITY_NEWS_FEEDS when set.
news_feeds <- function() {
  spec <- Sys.getenv("LIVABILITY_NEWS_FEEDS")
  if (!nzchar(spec)) return(NEWS_FEEDS)
  parts <- strsplit(strsplit(spec, ";", fixed = TRUE)[[1]], "|", fixed = TRUE)
  parts <- Filter(function(p) length(p) == 2, parts)
  data.frame(source = vapply(parts, `[`, "", 1), url = vapply(parts, `[`, "", 2),
             stringsAsFactors = FALSE)
}

#' The livability topic of a headline, or NA when it matches none.
news_topic <- function(title) {
  t <- paste0(" ", tolower(title), " ")
  for (topic in names(NEWS_TOPICS)) {
    if (any(vapply(NEWS_TOPICS[[topic]], grepl, logical(1), x = t, fixed = TRUE))) return(topic)
  }
  NA_character_
}

#' Parse an RSS 2.0 document into headlines.
#'
#' @param xml_text the feed as a single string.
#' @param source publisher name shown next to each headline.
#' @return data.frame(Title, Link, Date (POSIXct, NA if absent), Source, Topic);
#'   items without a title or an http(s) link are dropped.
parse_rss <- function(xml_text, source) {
  empty <- data.frame(Title = character(0), Link = character(0),
                      Date = as.POSIXct(character(0), tz = "UTC"),
                      Source = character(0), Topic = character(0), stringsAsFactors = FALSE)
  # NONET: never fetch anything referenced by the document; entities are not expanded.
  doc <- tryCatch(xml2::read_xml(xml_text, options = c("NONET", "NOBLANKS", "NOCDATA")),
                  error = function(e) NULL)
  if (is.null(doc)) return(empty)
  items <- xml2::xml_find_all(doc, "//item")
  if (length(items) == 0) return(empty)
  field <- function(name) {
    vapply(items, function(it) {
      node <- xml2::xml_find_first(it, name)
      if (inherits(node, "xml_missing")) NA_character_ else trimws(xml2::xml_text(node))
    }, character(1))
  }
  title <- gsub("\\s+", " ", gsub("<[^>]+>", "", field("title")))
  link <- field("link")
  old <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  raw_date <- sub("^[A-Za-z]{3}, *", "", field("pubDate"))   # the weekday is optional in practice
  date <- as.POSIXct(strptime(raw_date, "%d %b %Y %H:%M:%S %z", tz = "UTC"))
  out <- data.frame(Title = title, Link = link, Date = date, Source = source,
                    stringsAsFactors = FALSE)
  out <- out[!is.na(out$Title) & nzchar(out$Title) & grepl("^https?://", out$Link), , drop = FALSE]
  out$Topic <- vapply(out$Title, news_topic, character(1), USE.NAMES = FALSE)
  out
}

#' Keep recent livability headlines, newest first, without duplicates.
#' @param items rows from parse_rss(); @param n how many to keep.
#' @param max_age_days drop dated headlines older than this.
#' @param now reference time (for tests).
select_news <- function(items, n = 4, max_age_days = 14, now = Sys.time()) {
  items <- items[!is.na(items$Topic), , drop = FALSE]
  recent <- is.na(items$Date) | difftime(now, items$Date, units = "days") <= max_age_days
  items <- items[recent, , drop = FALSE]
  items <- items[!duplicated(tolower(items$Title)), , drop = FALSE]
  items <- items[order(is.na(items$Date), -as.numeric(items$Date)), , drop = FALSE]
  # Prefer variety: newest first, but at most two headlines per topic and three
  # per publisher among those actually shown.
  keep <- integer(0)
  for (i in seq_len(nrow(items))) {
    if (length(keep) >= n) break
    if (sum(items$Topic[keep] == items$Topic[i]) < 2 && sum(items$Source[keep] == items$Source[i]) < 3) {
      keep <- c(keep, i)
    }
  }
  items[keep, , drop = FALSE]
}

#' Download and parse the feeds (short timeouts; failures are recorded, not thrown).
#' @return list(items, errors, fetched_at).
fetch_news <- function(feeds = news_feeds(), timeout = 8) {
  all <- list(); errors <- character(0)
  for (k in seq_len(nrow(feeds))) {
    res <- tryCatch({
      h <- curl::new_handle(timeout = timeout, connecttimeout = 5, followlocation = TRUE,
                            useragent = "CityLivabilityOptimizer/1.0 (RSS reader)")
      r <- curl::curl_fetch_memory(feeds$url[k], handle = h)
      if (r$status_code != 200) stop("HTTP ", r$status_code)
      parse_rss(rawToChar(r$content), feeds$source[k])
    }, error = function(e) { errors <<- c(errors, sprintf("%s: %s", feeds$url[k], conditionMessage(e))); NULL })
    if (!is.null(res)) all[[length(all) + 1]] <- res
  }
  items <- if (length(all)) do.call(rbind, all) else parse_rss("", "")
  list(items = select_news(items), errors = errors, fetched_at = Sys.time())
}

#' Where the cached headlines are kept.
news_cache_file <- function() {
  p <- Sys.getenv("LIVABILITY_NEWS_CACHE")
  if (nzchar(p)) return(p)
  file.path(tools::R_user_dir("city-livability-optimizer", "cache"), "news.rds")
}

#' Cached result, or NULL when missing, unreadable or older than max_age_hours.
read_news_cache <- function(path = news_cache_file(), max_age_hours = 2) {
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(x) || is.null(x$fetched_at) || !is.data.frame(x$items)) return(NULL)
  if (difftime(Sys.time(), x$fetched_at, units = "hours") > max_age_hours) return(NULL)
  x
}

write_news_cache <- function(x, path = news_cache_file()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  saveRDS(x, tmp)
  file.rename(tmp, path)
}

#' Start a background refresh of the cache (returns the callr process).
#' @param root project root, so the child process can load this file.
start_news_refresh <- function(root, path = news_cache_file(), feeds = news_feeds()) {
  callr::r_bg(function(root, path, feeds) {
    source(file.path(root, "R", "news.R"))
    write_news_cache(fetch_news(feeds), path)
    TRUE
  }, args = list(root = normalizePath(root), path = path, feeds = feeds), supervise = TRUE)
}

#' Start a background refresh unless one is already running.
#' @param state an environment shared by all sessions of the app; the running
#'   process is kept in state$proc, so at most one refresh runs at a time.
#' @return the callr process, or NULL if it could not be started.
ensure_news_refresh <- function(state, root, path = news_cache_file(), feeds = news_feeds()) {
  if (!is.null(state$proc) && state$proc$is_alive()) return(state$proc)
  state$proc <- tryCatch(start_news_refresh(root, path, feeds), error = function(e) NULL)
  state$proc
}

#' "16 Oct" style date for display ("" when unknown).
news_date_label <- function(date) {
  ifelse(is.na(date), "", format(date, "%d %b", tz = "Asia/Kolkata"))
}

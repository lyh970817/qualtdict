args <- commandArgs(trailingOnly = TRUE)
site_dir <- if (length(args) >= 1) args[[1]] else "pkgdown"

internal_pages <- c("AGENTS.html", "CONTEXT.html")

is_internal_page <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    return(FALSE)
  }

  basename(path) %in% internal_pages
}

unlink(file.path(site_dir, internal_pages), force = TRUE)

search_path <- file.path(site_dir, "search.json")
if (file.exists(search_path)) {
  search <- jsonlite::fromJSON(search_path, simplifyVector = FALSE)
  search <- Filter(function(entry) {
    !is_internal_page(entry$path)
  }, search)
  jsonlite::write_json(search, search_path, auto_unbox = TRUE)
}

sitemap_path <- file.path(site_dir, "sitemap.xml")
if (file.exists(sitemap_path)) {
  sitemap <- xml2::read_xml(sitemap_path)
  urls <- xml2::xml_find_all(sitemap, ".//*[local-name()='url']")

  for (url in urls) {
    loc <- xml2::xml_find_first(url, ".//*[local-name()='loc']")
    if (inherits(loc, "xml_missing")) {
      next
    }

    if (is_internal_page(xml2::xml_text(loc))) {
      xml2::xml_remove(url)
    }
  }

  xml2::write_xml(sitemap, sitemap_path)
}

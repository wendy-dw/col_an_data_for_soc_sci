# A function to get all wikipedia links, starting from one wikipedia page, in a specific language
get_all_links <- function(language, language_short, url) {
  #start an instance of PhantomJS and create a new browser session that awaits to load URLs to render the corresponding websites
  urlstart <- str_c(str_split(url, "wikipedia.org")[[1]][1],
                    "wikipedia.org",
                    sep = "")
  pjs_instance <- run_phantomjs()
  pjs_session <- Session$new(port = pjs_instance$port)
  # go to URL
  pjs_session$go(url)
  # render page
  rendered_source <- pjs_session$getSource()
  # download text and parse the source code into an XML object
  html_document <- read_html(rendered_source)
  links <- html_document %>%
    html_elements("a") %>%
    html_attr(name = "href")
  links <- data.frame(link = links) %>%
    dplyr::filter(str_starts(link, "/wiki")) %>%#filter only internal wikipedia links
    dplyr::filter(!str_detect(link,
                              paste(
                                c("/wiki/Wikipedia:",
                                  "/wiki/Portal:",
                                  "/wiki/Special:",
                                  "/wiki/Help:",
                                  "/wiki/Talk:",
                                  "/wiki/File:"), #remove irrelevant links as shown for english page above. The ":" makes sure these web pages are also remove for other languages where they have a different name.
                                collapse = '|'))) %>%
    distinct(link) %>%
    mutate(link = str_c(urlstart, link, sep = ""),
           language = language,
           language_short = language_short)
  return(links)
}


get_content <- function(url){
  pjs_session$go(url)
  # retrieve the rendered source code of the page
  rendered_source <- pjs_session$getSource()
  # parse the dynamically rendered source code
  html_document <- read_html(rendered_source)
  title_text <- html_document %>%
    html_elements("h1") %>%
    html_text(trim = T)
  title_text <- title_text[1]
  body_text <- html_document %>%
    html_elements("p") %>%
    html_text(trim = T) %>%
    paste0(collapse = "\n")
  date_object <- html_document %>%
    html_elements("script") %>%
    html_text()
  locate_string <- str_locate(date_object[
    str_detect(date_object, "\"dateModified\":\"")],
    "\"dateModified\":\"")
  if (length(locate_string) != 0) {
    date_modified <- str_sub(
      date_object[
        str_detect(date_object, "\"dateModified\":\"")],
      start = locate_string[1, 2] + 1,
      end = locate_string[1, 2] + 10)
  } else {
    date_modified <- NA
  }
  locate_string <- str_locate(date_object[
    str_detect(date_object, "\"datePublished\":\"")],
    "\"datePublished\":\"")
  if (length(locate_string) != 0) {
    date_published <- str_sub(
      date_object[
        str_detect(date_object, "\"datePublished\":\"")],
      start = locate_string[1, 2] + 1,
      end = locate_string[1, 2] + 10)
  } else {
    date_published <- NA
  }
  return(c(title_text, body_text, date_modified, date_published))
}

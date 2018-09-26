extract_name <- function(x) {
  x %>%
    flatten() %>%
    flatten() %>%
    flatten() %>%
    .[names(.) %in% c("surname", "given-names", "string-name", "suffix")] %>%
    as_data_frame()
}

#' Autoren-Informationen extrahieren
#'
#' Dies ist eine interne Funktion, welche von [find_meta()] aufgerufen wird.
#'
#' Diese Funktion extrahiert die Informationen zu den Autoren. Dazu gehört nach
#' derzeitigem Wissensstand:
#' * surname == Nachname
#' * given_name == Vorname
#' * suffix == Nachgestellte Ausdrücke wie _Jr_ oder _III_
#'
#' Die Spalte _suffix_ ist nur vorhanden, wenn auch ein Suffix vorhanden ist.
#' Beim `unnesten` erhalten alle Fälle ohne Suffix automatisch ein `NA`.
#'
#' Für fehlende Autoren gibt die Funktion ein `NA` zurück. Dies scheint am
#' intuitivsten, macht aber Probleme, wenn anschließend die Autoren `unnested`
#' werden sollen. Hier hilft nur, die Fälle mit keinen Autoren zu filtern. Dies
#' scheint aber unproblematisch, da es sich hier meist um Dinge wie Errata handeln
#' müsste.
#'
#' @param contributors List of contributors from xml-data.
extract_contributors <- function(contributors) {
  if (is.null(contributors)) {
    return(NA)
  } else {
    contributors <- contributors %>%
      .[names(.) == "contrib"] %>%
      map(extract_name) %>%
      bind_rows()
    
    if (purrr::is_empty(contributors)) {
      return(NA)
    }
    
    contributors <- contributors %>%
      mutate(author_number = 1:n())
    
    contributors %>%
      names() %>% # find names
      stringr::str_replace_all("\\-", "_") %>% # replace dash with underscore
      purrr::set_names(contributors, .)
  }
}


page_finder <- function(page_num) {
  page_num <- page_num %>%
    stringr::str_extract(., "\\d+")
  
  if (purrr::is_empty(page_num)) {
    return(NA_integer_) # für Fälle, wo Felder für die Seitenzahl nicht vorhanden sind
  } else {
    as.integer(page_num)
  }
}


extract_pages <- function(article) {
  if (purrr::is_empty(article$fpage)) {
    return(
      data_frame(
        first_page = NA_integer_,
        last_page = NA_integer_,
        article_pages = NA_integer_
      )
    )
  } else {
    data_frame(first_page = page_finder(article$fpage[[1]]),
               last_page  = page_finder(article$lpage[[1]])) %>%
      mutate(article_pages =
               dplyr::case_when(
                 # Berechne die Seitenzahl:
                 # - Wenn nur erste Seite vorhanden, dann gibt es nur eine Seite
                 # - Wenn beide vorhanden wird normal berechnet
                 # - Ansonsten ein NA
                 is_integer(.$first_page) & is.na(.$last_page) ~ 1L,
                 is_integer(.$first_page) & is_integer(.$last_page) ~ .$last_page - .$first_page + 1L,
                 TRUE ~ NA_integer_)
      )
  }
}

extract_date <- function(article) {
  dates <- article$`pub-date` %>%
    flatten()
  
  sapply(unique(names(dates)),
         function(x) unname(unlist(dates[names(dates) == x])),
         simplify = FALSE) %>%
    as_data_frame() %>%
    dplyr::mutate_all(readr::parse_number)
}

extract_language <- function(article) {
  article$`custom-meta-group`$`custom-meta` %>%
    flatten() %>%
    .[["meta-value"]]
}


extract_vol_issue <- function(article, type) {
  if (is.null(article[[type]]) || is_empty(article[[type]])) {
    return(NA_character_)
  } else {
    article[[type]] %>%
      flatten_chr()
  }
}

extract_title <- function(title_group) {
  title_group %>%
    flatten() %>%
    flatten() %>%
    flatten_chr() %>%
    .[1]
}

#' Extract meta information
#'
#' `find_meta()` extrahiert die Meta-Informationen aus den XML-Dateien.
#'
#' Die Spalte der Autoren kann mit `purrr::unnest()` expandiert werden. Dafür
#' müssen vorher die fehlenden Werte in den Autoren gefiltert werden. Siehe dazu
#' auch [extract_contributors].
#'
#' @param meta_list Der Inhalt einer XML-Datei, welcher mit `as_list()` zu einer
#'   Liste konvertiert wurde.
#' @param .pb Zeige eine Fortschrittsbalken bei Anwendung mit `map`? Akzeptiert
#'   ein Objekt wie: [dplyr::progress_estimated]: `progress_estimand(length('dateien-zu-bearbeiten'))`.
#' @param show_warnings Sollen Warnungen gezeigt werden? Default ist `FALSE`.
#'
#' @return Eine `tibble` mit den folgenden Spalten:
#' * journal_id (`character`)
#' * article_id (`character`)
#' * article_title (`character`)
#' * authors (`list`)
#'
#' @seealso [extract_contributors]
#' @export
find_meta <- function(meta_list, .pb = NULL, show_warnings = F) {
  if (!is.list(meta_list)) {
    stop("`meta_list` must be a list", call. = FALSE)
  }
  
  # support for progress bar
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  article <- meta_list$article
  
  front <- article$front
  article <- front$`article-meta`
  
  # authors
  contributors <- article$`contrib-group`
  contributors <- extract_contributors(contributors)
  
  # title
  title <- extract_title(article$`title-group`$`article-title`)
  
  # publication date
  if (!show_warnings) {
    pub_date <- suppressWarnings(extract_date(article))
  } else {
    pub_date <- extract_date(article)
  }
  pub_year <- pub_date[[1, "year"]]
  
  # pages
  pages <- extract_pages(article)
  
  # piece together all elements
  data_frame(
    journal_id = front$`journal-meta`$`journal-id`[[1]],
    article_id = article$`article-id`[[1]],
    article_title = title,
    volume = extract_vol_issue(article, "volume"),
    issue = extract_vol_issue(article, "issue"),
    authors = list(contributors),
    article_pages = list(pages),
    pub_date = list(pub_date),
    pub_year = pub_year,
    language = extract_language(article)
  )
}


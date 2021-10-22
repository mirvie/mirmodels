#' Construct the bullet point bits for `custom_stop()`.
#'
#' @param string The message for the bullet point.
#'
#' @return A string with the bullet-pointed message nicely formatted for the
#'   console.
#'
#' @noRd
custom_stop_bullet <- function(string) {
  checkmate::assert_string(string)
  string %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_c("    * ", .)
}

#' Nicely formatted error message.
#'
#' Format an error message with bullet-pointed sub-messages with nice
#' line-breaks.
#'
#' Arguments should be entered as `glue`-style strings.
#'
#' @param main_message The main error message.
#' @param ... Bullet-pointed sub-messages.
#'
#' @noRd
custom_stop <- function(main_message, ..., .envir = parent.frame()) {
  checkmate::assert_string(main_message)
  main_message <- main_message %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_replace_all(stringr::coll("{code("), "{usethis::ui_code(") %>%
    glue::glue(.envir = .envir)
  out <- main_message
  dots <- as.character(unlist(list(...)))
  if (length(dots)) {
    dots <- dots %>%
      stringr::str_replace_all(
        stringr::coll("{code("),
        "{usethis::ui_code("
      ) %>%
      purrr::map_chr(glue::glue, .envir = .envir) %>%
      purrr::map_chr(custom_stop_bullet)
    out <- out %>% {
      stringr::str_c(c(., dots), collapse = "\n")
    }
  }
  rlang::abort(stringr::str_c(out, collapse = "\n"))
}

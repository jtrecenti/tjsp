#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

dvec <- function(.f, .l, lab = 'arq', ...) {
  fw <- {
    d_error <- dplyr::data_frame(result = 'error')
    ff <- function(...) {
      r <- .f(...)
      if (!'result' %in% names(r)) dplyr::mutate(r, result = 'OK')
      r
    }
    dplyr::failwith(d_error, ff)
  }
  dplyr::data_frame(.l) %>%
    dplyr::group_by(.l) %>%
    dplyr::do(fw(.$.l, ...)) %>%
    dplyr::ungroup() %>%
    setNames(c(lab, names(.)[-1]))
}

#' @export
desacentuar <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}

#' @export
rm_accent <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}

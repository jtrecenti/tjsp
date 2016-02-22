list_objects_1inst <- function(nm = 'varas') {
  u_base <- 'https://esaj.tjsp.jus.br/cjpg/%sTreeSelect.do?campoId=%s'
  u <- sprintf(u_base, nm, nm)
  r <- httr::GET(u, httr::config(ssl_verifypeer = FALSE))
  l <- XML::htmlTreeParse(httr::content(r, 'text'), asText = TRUE)
  ul <- l$children$html$children$body$children$div$children[[2]]$children$ul
  d <- html_to_list(ul)
  return(d)
}

xml2 <- function(h) {
  x <- xml2::read_html(capture.output(h))
  rvest::html_children(rvest::html_children(x))[[1]]
}

#' Funcao que lista as varas disponiveis
#'
#' @export
list_varas_1inst <- function() {
  d_varas <- list_objects_1inst('varas')
  f_obj <- dplyr::funs(iconv(., from = 'UTF-8', to = 'iso-8859-1'))
  d_varas <- dplyr::mutate_each(d_varas, f_obj, starts_with('n'))
  names(d_varas) <- c('cod_vara', 'nm_vara', 'cod_muni', 'cod_foro',
                      'nm_muni', 'nm_foro')
  return(d_varas)
}

#' Funcao que lista as classes disponiveis
#'
#' @export
list_classes_1inst <- function() {
  d_classe <- list_objects_1inst('classe')
  f_obj <- dplyr::funs(iconv(., from = 'UTF-8', to = 'iso-8859-1'))
  d_classe <- dplyr::mutate_each(d_classe, f_obj, starts_with('n'))
  return(d_classe)
}

#' Funcao que lista os assuntos disponiveis
#'
#' @export
list_assuntos_1inst <- function() {
  d_assunto <- list_objects_1inst('assunto')
  f_obj <- dplyr::funs(iconv(., from = 'UTF-8', to = 'iso-8859-1'))
  d_assunto <- dplyr::mutate_each(d_assunto, f_obj, starts_with('n'))
  return(d_assunto)
}

html_to_list <- function(l, codigos = NULL, nomes = NULL) {
  if ('ul' %in% names(l)) {
    li <- l$children$ul
    codigos <- append(codigos, rvest::html_attr(xml2(l$children[[2]]), 'value'))
    nomes <- append(nomes, rvest::html_text(xml2(l$children[[2]])))
    a <- list()
    for (i in 1:length(li)) {
      a[[i]] <- html_to_list(li[[i]], codigos, nomes)
    }
    empilhado <- dplyr::bind_rows(a)

    return(empilhado)
  } else if (!'span' %in% names(l)) {
    a <- list()
    for (i in 1:length(l)) {
      a[[i]] <- html_to_list(l[[i]], codigos, nomes)
    }
    empilhado <- dplyr::bind_rows(a)
    return(empilhado)
  } else {
    cod_leaf <- rvest::html_attr(xml2(l$children[[2]]), 'value')
    nm_leaf <- rvest::html_text(xml2(l$children[[2]]))

    if (length(codigos) > 0) {
      node_final <- data.frame(cod_leaf, nm_leaf, c0 = codigos, n0 = nomes,
                               stringsAsFactors = FALSE)
    } else {
      if (length(cod_leaf) == 0) cod_leaf <- ''
      if (length(nm_leaf) == 0) nm_leaf <- ''
      node_final <- data.frame(cod_leaf, nm_leaf, stringsAsFactors = F)
    }

    n <- nrow(node_final)
    if (n > 1) {
      node_final$cods_temp <- paste0('c', seq_len(n))
      node_final$noms_temp <- paste0('n', seq_len(n))
      node_final <- tidyr::spread(node_final, cods_temp, c0)
      node_final <- tidyr::spread(node_final, noms_temp, n0)
      node_final <- dplyr::summarise_each(node_final,
                                          dplyr::funs(unique(.[!is.na(.)])))
    }
    return(node_final)
  }
}



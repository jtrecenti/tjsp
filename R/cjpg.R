#' @export
cjpg <- function(livre = '', n_processo = '', classes = '', assuntos = '',
                 varas = '', datas = c('', ''),
                 min_pag = 1, max_pag = Inf, path = 'data-raw/cjpg',
                 verbose = TRUE) {
  suppressWarnings(dir.create(path, recursive = TRUE))
  if (class(datas) == 'Date') datas <- format(datas, '%d/%m/%Y')
  cjpg_pag <- function(pag, path, verbose) {
    config <- httr::config(ssl_verifypeer = FALSE)
    u_base <- 'https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=%d'
    u <- sprintf(u_base, pag)
    arq <- sprintf('%s/%06d.html', path, pag)
    if (verbose) cat(sprintf('pag: %06d...', pag))
    if (file.exists(arq)) {
      if (verbose) cat('ja existe!\n')
      return(dplyr::data_frame(result = 'ja existe'))
    }
    if (verbose) cat('downloading...')
    r_pag <- httr::GET(u, config = config, httr::write_disk(arq))
    if (verbose) cat('download realizado!\n')
    if (r_pag$status_code != 200) stop('bugou o download')
    dplyr::data_frame(result = 'OK')
  }
  l <- list(livre = livre, n_processo = n_processo, classes = classes,
            assuntos = assuntos, datas = datas, varas = varas)
  n_results <- n_cjpg(l)
  if (verbose) cat(sprintf('\nForam encontrados %d resultados...\n', n_results))
  p <- n_results %/% 10 + 1
  dvec(cjpg_pag, min_pag:min(max_pag, p), 'pag', path = path, verbose = verbose)
}

#' @export
n_cjpg <- function(l) {
  u <- build_url_cjpg(l)
  config <- httr::config(ssl_verifypeer = FALSE)
  r <- httr::GET(u, config = config)
  n <- r %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//td[@bgcolor="#EEEEEE"]') %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all('[\n\r\t]', '') %>%
    dplyr::first() %>%
    stringr::str_extract('[0-9]+$') %>%
    as.numeric()
  ifelse(is.na(n), 0, n)
}

build_url_cjpg <- function(l) {
  x <- list('conversationId' = '',
            'dadosConsulta.pesquisaLivre' = l$livre,
            'tipoNumero' = 'UNIFICADO',
            'numeroDigitoAnoUnificado' = '',
            'foroNumeroUnificado' = '',
            'dadosConsulta.nuProcesso' = l$n_processo,
            'dadosConsulta.nuProcessoAntigo' = '',
            'classeTreeSelection.values' = paste(l$classes, collapse = ','),
            'classeTreeSelection.text' = '',
            'assuntoTreeSelection.values' = paste(l$assuntos, collapse = ','),
            'assuntoTreeSelection.text' = '',
            'agenteSelectedEntitiesList' = '',
            'contadoragente' = '0', 'contadorMaioragente' = '0',
            'cdAgente' = '', 'nmAgente' = '',
            'dadosConsulta.dtInicio' = l$datas[1],
            'dadosConsulta.dtFim' = l$datas[2],
            'varasTreeSelection.values' = paste(l$varas, collapse = ','),
            'varasTreeSelection.text' = '',
            'dadosConsulta.ordenacao' = 'DESC')
  u_base <- 'https://esaj.tjsp.jus.br/cjpg/pesquisar.do?'
  httr::modify_url(u_base, query = x)
}

parse_cjpg <- function() {

}

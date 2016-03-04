#' Consolidar os dados.
#'
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import stringi
#' @import lubridate
#' @import tidyr
#'
#' @export
consolidar_dados <- function(d_cjpg, d_cpopg) {

  visualiza_um <- function(d) {
    d %>%
      filter(arq == sample(arq, 1)) %>%
      mutate(arq = gsub('[^0-9]', '', arq)) %>%
      select(n_processo = arq, everything()) %>%
      data.frame()
  }

  d_cpopg <- d_cpopg_keyval %>%
    group_by(key) %>%
    mutate(n = n()) %>%
    ungroup %>%
    filter(n > 25000) %>% # somente tags que aparecem 25k vezes ou mais
    select(-n) %>%
    group_by(key, arq) %>%
    summarise(val = paste(val, collapse = '\n')) %>%
    ungroup %>%
    spread(key, val) %>%
    mutate(arq = gsub('[^0-9]', '', arq)) %>%
    mutate(digital = is.na(local_fisico)) %>%
    select(n_processo = arq, distribuicao,
           digital, processo, reqdo:valor_da_acao) %>%
    mutate(codigo = str_sub(n_processo, 17L, 20L)) %>%
    inner_join(foros, 'codigo') %>%
    select(-codigo)

  # -----------------------------------------------------------------------------
  # filtros reqte NA, reqdo NA, valor NA
  d_cpopg <- d_cpopg %>%
    filter(!is.na(valor_da_acao)) %>%
    filter(!is.na(reqte)) %>%
    filter(!is.na(reqdo))

  # -----------------------------------------------------------------------------
  # filtros partes

  # Classificacao dos requeridos
  # BANCOS: BANCO DO BRASIL, ITAU, BRADESCO, SANTANDER.
  # TELEFONIA: TIM, VIVO, CLARO, NET, NEXTEL
  # OUTROS: ELETROPAULO

  bradesco <- 'BRADESCO SA|BRADESCAR|BRADESCO$|BRADESCO CARTOES|BRADESCO FINANC'

  itau <- 'UNIBANCO SA$|UNIBANCO ITAU|ITAU UNIBANCO BANCO MULTIPLO SA|'
  itau <- paste0(itau, 'ITAU CARD|ITAUCARD|ITAU UNIBANCO SA|ITAUCARD SA|')
  itau <- paste0(itau, 'ITAU$|UNIBANCO$|ITAU SA|ITAUCARD$|ITAUUNIBANCO|^ITAU$|')
  itau <- paste0(itau, 'ITAU FINAN|ITAU UNIBANCO F|FINANCEIRA ITAU')
  santander <- 'SANTANDER$|SANTANDER [^L]|SANTANDER[SA]'
  bb <- 'BANCO DO BRASIL'
  tim <- 'TIM CELUL?AR|^TIM SA|TIM BRASIL SA|TIM DO BRASIL SA|^TIM$| TIM$|'
  tim <- paste0(tim, 'TIM OPERADORA|TIM TELEFONIA')
  claro <- 'CLARO$|CLARO [^A]|EMBRATEL'
  net <- '^NET$|^NET | NET S|CENTRAL DA NET'
  nextel <- 'NEXTEL'
  vivo <- 'VIVO|TELESP|TELEFONICA'
  eletropaulo <- 'ELETROP'
  empresas <- paste(bradesco, itau, santander, bb, tim, claro,
                    net, nextel, vivo, eletropaulo, sep = '|')

  pj <- c('TELLERINA', 'ALLIANZ', 'INDÚSTRIA', 'CONSULTORIA',
          ' ME$', 'LTDA', 'CONDOMINIO', 'DEF PUB', 'LOGISTICA',
          'SPEED BEE', 'ASSOCIACAO', 'EUROTUBOS', 'TRANSPORTES') %>%
    paste(collapse='|')

  d_cpopg <- d_cpopg %>%
    mutate(reqdo_limpo = gsub('[^A-Za-z0-9 \n]', '', tjsp::rm_accent(reqdo)),
           reqdo_limpo = toupper(gsub('\\s+', ' ', reqdo_limpo)),
           reqte_limpo = gsub('[^A-Za-z0-9 \n]', '', tjsp::rm_accent(reqte)),
           reqte_limpo = toupper(gsub('\\s+', ' ', reqte_limpo))) %>%
    filter(str_detect(reqdo_limpo, empresas)) %>%
    filter(!str_detect(reqte_limpo, pj)) %>%
    mutate(q_bradesco = tjsp::add_key(reqdo_limpo, bradesco, 'BRADESCO'),
           q_itau = tjsp::add_key(reqdo_limpo, itau, 'ITAU'),
           q_santander = tjsp::add_key(reqdo_limpo, santander, 'SANTANDER'),
           q_bb = tjsp::add_key(reqdo_limpo, bb, 'BB'),
           q_tim = tjsp::add_key(reqdo_limpo, tim, 'TIM'),
           q_claro = tjsp::add_key(reqdo_limpo, claro, 'CLARO'),
           q_net = tjsp::add_key(reqdo_limpo, net, 'NET'),
           q_nextel = tjsp::add_key(reqdo_limpo, nextel, 'NEXTEL'),
           q_vivo = tjsp::add_key(reqdo_limpo, vivo, 'VIVO'),
           q_eletro = tjsp::add_key(reqdo_limpo, eletropaulo, 'ELETROPAULO')) %>%
    rowwise %>%
    mutate(empresa = paste(q_bradesco, q_itau, q_santander, q_bb, q_tim, q_claro,
                           q_net, q_nextel, q_vivo, q_eletro)) %>%
    ungroup %>%
    mutate(empresa = str_trim(empresa)) %>%
    filter(!str_detect(empresa, ' ')) %>%
    tbl_df %>%
    select(-starts_with('q_'))

  # -----------------------------------------------------------------------------
  # Cleaning

  ls2 <- ls()
  ls2 <- ls2[ls2 != 'd_cpopg']
  rm(list = ls2)
  rm(ls2)

  # -----------------------------------------------------------------------------
  imprime <- function(txt) {
    linha <- paste0('\n', str_dup('-', getOption('width') - 10), '\n')
    imp <- str_replace_all(txt, '\n', linha)
    cat(imp)
  }
  imprime_frase <- function(fr) {
    linha <- paste0('\n', str_dup('-', getOption('width') - 15), '\n')
    cat(paste(fr, collapse = linha))
  }

  salario_minimo <- function(d) {
    # https://www.domesticalegal.com.br/conteudo/utilidades/salario-minimo-das-empregadas-domesticas.aspx
    if(d >= '2014-01-01') return(810)
    if(d >= '2013-02-01' & d <= '2013-12-31') return(755)
    if(d >= '2012-03-01' & d <= '2013-01-31') return(690)
    if(d >= '2012-01-01' & d <= '2012-02-31') return(622)
    if(d >= '2011-04-01' & d <= '2011-12-31') return(600)
    if(d >= '2010-04-01' & d <= '2011-03-31') return(560)
    if(d >= '2010-01-01' & d <= '2010-03-31') return(510)
    if(d >= '2009-05-01' & d <= '2009-12-31') return(505)
    if(d >= '2009-02-01' & d <= '2009-04-31') return(465)
    if(d >= '2008-05-01' & d <= '2009-01-31') return(450)
    if(d >= '2008-03-01' & d <= '2008-04-31') return(415)
    if(d >= '2007-08-01' & d <= '2008-02-29') return(410)
    if(d >= '2007-04-01' & d <= '2007-07-31') return(350)
    if(d >= '2006-04-01' & d <= '2007-03-31') return(350)
    if(d >= '2005-05-01' & d <= '2006-03-31') return(300)
    if(d >= '2004-05-01' & d <= '2005-04-31') return(260)
    if(d >= '2003-04-01' & d <= '2004-04-31') return(240)
    if(d >= '2002-04-01' & d <= '2003-03-31') return(200)
    if(d >= '2001-04-01' & d <= '2002-03-31') return(180)
    if(d >= '2000-04-01' & d <= '2001-03-31') return(151)
    if(d >= '1999-05-01' & d <= '2000-03-31') return(136)
    if(d >= '1998-05-01' & d <= '1999-04-31') return(130)
    if(d >= '1997-05-01' & d <= '1998-04-31') return(120)
  }

  primeiro_andamento <- function(a) {
    res <- plyr::llply(a, function(y) {
      dat <- xml2::read_html(y) %>%
        rvest::html_node('#tabelaTodasMovimentacoes') %>%
        rvest::html_nodes('tr') %>%
        last() %>%
        rvest::html_nodes('td') %>%
        first() %>%
        rvest::html_text() %>%
        gsub('[\n\t\r ]', '', .) %>%
        lubridate::dmy() %>%
        as.Date()
      dat
    }, .progress = 'text')
    res <- unlist(res)
    data_frame(arq = a, data_inicial = as.Date(res, '1970-01-01')) %>%
      mutate(n_processo = gsub('[^0-9]', '', arq)) %>%
      select(n_processo, data_inicial)
  }

  # l <- dir('data-raw/html-cpopg/', full.names = TRUE)
  # datas_iniciais <- primeiro_andamento(l)
  # save(datas_iniciais, file = 'data/datas_iniciais.rda')
  load('data/datas_iniciais.rda')

  antiguidades_link <- c('http://www.tjsp.jus.br/Institucional/Pr',
                         'imeiraInstancia/Juizes/Default.aspx?',
                         'ctl00_ctl00_cphConteudoGeral_ContentPl',
                         'aceHolder1_GridJuizesChangePage=87_20')
  l <- dir('data-raw', full.names = TRUE)
  l <- l[grep('\\.xls', l)]
  antiguidade <- data_frame(arq = l) %>%
    group_by(arq) %>%
    do((function(f) {
      d <- readxl::read_excel(f) %>%
        `[`(-1, !names(.) %in% c('', 'Entrância')) %>%
        select(magistrado = Nome, data = `Início`)
      d
    })(.$arq)) %>%
    ungroup %>%
    mutate(magistrado = toupper(tjsp::rm_accent(magistrado))) %>%
    distinct(magistrado) %>%
    mutate(antiguidade = as.numeric(as.Date('2014-12-31') - as.Date(data))) %>%
    filter(antiguidade >= 0) %>%
    select(magistrado, antiguidade)


  #----------------------------------------------------------------------------
  # New approach
  # DECIDIDO! VAMOS OLHAR SOMENTE PARA AS PRIMEIRAS DECISOES
  # EXISTE UM VIES NA BASE DE ACORDOS POS SENTENCA ENTAO VAMOS IGNORAR
  # TUDO O QUE ACONTECE POS SENTENCA :))))
  data(d_cjpg_txt)
  data(d_cpopg)

  d_cjpg_one <- d_cjpg_txt %>%
    mutate(data_disp = as.Date(dmy(data_disponibilizacao))) %>%
    arrange(n_processo, data_disp) %>%
    group_by(n_processo) %>%
    slice(1) %>%
    ungroup

  split_phrases <- function(x) {
    before_dot <- c('[^ ][^I]')
    before_dot <- paste(before_dot, collapse = '|')
    resplit <- paste0('(?<=((', before_dot, ')\\. {1,10}[A-ZÀÉÁÚ]))')
    banned_phrases <- c('^(DOCUMENTO ASSINA)',
                        '^(Processo Fís)',
                        '^(Processo n.?\\: ?[0-9])',
                        '^(Classe +\\-)',
                        '^(São Paulo, )',
                        '^(Requerente\\:)',
                        '^(Requerid.{1,3}\\:)',
                        '^(Juiz(\\(a\\))? )',
                        '^(Eu[, ]).+',
                        '(crevi|subscr|digitei)\\.$',
                        '^P\\. ?R\\. ?I\\.',
                        '^R\\. ?P\\. ?I\\.')
    banned_phrases <- paste(banned_phrases, collapse = '|')

    re_dots <- c('Rel\\.$',
                 'Min\\.$',
                 'Col\\.$',
                 'Sra?\\.$',
                 'Exm[oa]\\.$',
                 'Dr(\\(a\\))?\\.$',
                 'inc\\.$')
    re_dots <- paste(re_dots, collapse = '|')

    linhas <- unlist(str_split(x, '\n'))
    frases <- stri_split_regex(linhas, resplit)
    frases <- lapply(frases, function(z) {
      if(length(z) > 1) {
        for(i in 2:length(z)) {
          z[i] <- paste0(str_sub(z[i-1], -1), z[i])
          z[i-1] <- str_sub(z[i-1], 1, -2)
        }
      }
      return(z)
    })
    frases <- unlist(frases)
    frases <- str_trim(frases[!frases %in% c('')])

    k <- 1
    colar <- list(1)
    for(i in 2:length(frases)) {
      if(str_detect(frases[i-1], re_dots)) {
        colar[[k]] <- append(colar[[k]], i)
      } else {
        colar <- append(colar, i)
        k <- k + 1
      }
    }
    frases <- sapply(colar, function(x) paste(frases[x], collapse = ' '))
    frases <- str_trim(frases[!frases %in% c('')])
    frases <- frases[!str_detect(frases, banned_phrases)]

    return(frases)
  }

  # Frases
  d_cjpg_frases <- d_cjpg_one %>%
    select(n_processo, cod_sentenca, txt) %>%
    mutate(txt = gsub(' +', ' ', txt)) %>%
    group_by(n_processo, cod_sentenca) %>%
    do(frase = split_phrases(.$txt)) %>%
    ungroup %>%
    unnest(frase) %>%
    tbl_df %>%
    filter(str_detect(frase, '[a-zA-Z0-9]'))

  # save(d_cjpg_frases, file = 'data/d_cjpg_frases.rda')

  # -----------------------------------------------------------------------------
  # Vamos pegar os resultados dos processos
  # load('data/d_cjpg_frases.rda')

  inicio_decisao <- c('^(.{0,8}Ante a?o exposto)',
                      '^(.{0,8}Frente ao exposto)',
                      '^(.{0,8}Diante dis[st]o)',
                      '^(.{0,8}Is[st]o posto)',
                      '^(.{0,8}Em razão do exposto)',
                      '^(.{0,8}Do exposto)',
                      '^(.{0,8}Pelo exposto)',
                      '^(.{0,8}Posto isso)',
                      '^(.{0,8}Diante, pois, de todo o exposto)',
                      '^(.{0,8}Diante d?o exposto)',
                      '^(.{0,8}Feitas as considerações supra)',
                      '^(.{0,8}Por todo o exposto)',
                      '^(.{0,8}Posto is[st]o|POSTO IS[T]O)',
                      '^(.{0,8}Por es[st]as razões)',
                      '^(.{0,8}Ante todo o exposto)',
                      '^(.{0,8}De acordo com o exposto)',
                      '^(.{0,8}De acordo com todo o exposto)',
                      '^(.{0,8}Pelas razões expostas)',
                      '^(.{0,8}Por tais motivos)',
                      '^(.{0,8}Por tudo o quanto exposto)',
                      '^(.{0,8}Nes[ts]es termos,? julgo)',
                      '^(.{0,8}Em face do exposto)',
                      '^(.{0,8}Pelos fundamentos alinhavados)',
                      '^(.{0,8}Dessarte)',
                      '^(.{0,8}Assim,)',
                      '^(.{0,8}Ante o acima exposto)')
  inicio_decisao <- paste(inicio_decisao, collapse = '|')

  possiveis_resultados <- c(
    'julg[oa]r? (.{1,10})?procedente',
    'julg[oa]r? (.{1,10})?procedente em parte',
    'julg[oa]r? (.{1,10})?parcialmente',
    'julg[oa]r? (.{1,10})?(liminarmente |totalmente )?improcedente',
    'julg[oa]r? (.{1,10})?extint[oa]',
    'homologo.{5,30}'
  )
  possiveis_resultados <- paste(possiveis_resultados, collapse = '|')

  sujeira <- c('^(@ )', 'JULG[OA]R?', 'LIMINARMENTE', '[0-9()\n:]', '(A )?AÇÃO',
               'PEDIDO', 'TOTALMENTE', 'DEMANDA', 'AINDA', 'TAMBÉM')
  sujeira <- paste(sujeira, collapse = '|')
  sujeira2 <- '( |^)[A-Z] '

  d_cjpg_decisao <- d_cjpg_frases %>%
    mutate(
      tem = str_detect(frase, regex(inicio_decisao, ignore_case = TRUE)) &
        str_detect(frase, regex('julg[oa]r?', ignore_case = TRUE)),
      tem_a = str_detect(frase, regex('homologo', ignore_case = TRUE)),
      ext = str_detect(frase, regex('sem julgamento de m[eé]rito',
                                    ignore_case = TRUE))
    ) %>%
    group_by(n_processo, cod_sentenca) %>%
    mutate(
      n = n(),
      result = ifelse(any(ext),
                      paste(frase[last(which(ext)):first(n)], collapse = '\n'),
                      'nao tem'),
      result = ifelse(any(tem_a),
                      paste(frase[last(which(tem_a)):first(n)], collapse = '\n'),
                      result),
      result = ifelse(any(tem),
                      paste(frase[last(which(tem)):first(n)], collapse = '\n'),
                      result)) %>%
    ungroup %>%
    mutate(
      result_txt = sapply(
        str_extract_all(result, regex(possiveis_resultados, ignore_case = TRUE)),
        paste, collapse = ' @ '
      ),
      result_txt = toupper(result_txt),
      result_txt = gsub('PROCEDENTE EM PARTE', 'PARCIALMENTE', result_txt),
      result_txt = ifelse(str_detect(result_txt,
                                     regex('homolog', ignore_case = TRUE)) &
                            str_detect(result,
                                       regex('acordo|concilia',
                                             ignore_case = TRUE)),
                          'ACORDO', result_txt)
    ) %>%
    # daqui vou tirar todo o resto e falar que é extincao de execucao.
    filter(result_txt != '',
           result_txt != 'JULGO EXTINTO',
           result_txt != 'JULGO EXTINTA',
           !(str_detect(result_txt, regex('homolog', ignore_case = TRUE)) &
               !str_detect(result_txt, regex('proced', ignore_case = TRUE)))) %>%
    mutate(result_txt = gsub('(@ )?JULG[AO]R? (O FEITO )?EXTINT[OA]', '',
                             result_txt),
           result_txt = gsub('^(@ )', '', result_txt),
           result_txt = str_trim(result_txt)) %>%
    filter(result_txt != '') %>%
    mutate(result_txt = str_trim(result_txt)) %>%
    mutate(result_txt = gsub(sujeira, '', result_txt),
           result_txt = gsub(sujeira2, ' ', result_txt),
           result_txt = gsub(' +', ' ', result_txt),
           result_txt = gsub('HOMOLOGO[^@]+(@ |$)', '', result_txt),
           result_txt = gsub('( @) *$', '', result_txt),
           result_txt = gsub('INPRO', 'IMPRO', result_txt),
           result_txt = str_trim(result_txt)) %>%
    filter(!str_detect(result_txt, 'EXTINT|IMPARCIALM'))

  # -----------------------------------------------------------------------------

  d_cjpg_decisao_complete <- d_cjpg_decisao %>%
    inner_join(
      select(d_cjpg_one, cod_sentenca, n_processo, classe, assunto, magistrado,
             foro, vara, data_disp),
      c('cod_sentenca', 'n_processo')
    ) %>%
    inner_join(select(d_cpopg, n_processo, digital, valor_da_acao, empresa,
                      reqte_adv, reqdo_adv),
               'n_processo')


  # -----------------------------------------------------------------------------
  # VARIAVEIS RELACIONADAS A RESULTADO

  # tipo_pedido: paste de dano_moral, dano_material
  # dano_moral: pediu dano moral?
  # dano_material: pediu dano material?
  # obrig_fazer: pediu obrigação de fazer? (usado somente para classificacao)

  # resultado_moral: resultado para moral (proced, improc, parcial, nao sei, NA)
  # resultado_material: result p material (proced, improc, parcial, nao sei, NA)
  # resultado_acordo: teve acordo ou nao

  # valor_moral
  # valor_material
  # valor_acordo
  # tempo processo

  mascara_dinheiro <- 'R\\$ ?[0-9.]+,[0-9]{2}'
  mascara_dano <- c('danos? +mora[li]s?',
                    'danos? +materia[li]s?',
                    'inexig[ií](bilidade|vel)')
  mascara_dano <- paste(mascara_dano, collapse = '|')

  re_serasa <- c('[oó]rg[aã]o.{1,20}prote[cç][aã]o.{1,20}cr[eé]d',
                 'cadastro.{1,20}prote[cç][aã]o.{1,20}cr[eé]d',
                 'serasa', '( |^)spc( |$)', 'negativa[çc][ãa]o')
  re_serasa <- paste(re_serasa, collapse = '|')
  re_terceiro <- c('terceiro')
  re_consumo <- c('rela[çc][aã]o de consumo', 'consumidor')
  re_consumo <- paste(re_consumo, collapse = '|')
  re_conciliar <- c('tentativa de concil',  'tentou conciliar')
  re_conciliar <- paste(re_conciliar, collapse = '|')
  re_gratuidade <- c('gratuidade', 'justi[çc]a gratuita')
  re_gratuidade <- paste(re_gratuidade, collapse = '|')

  somar <- function(z, tr = 100) {
    x <- as.numeric(unlist(strsplit(z[1], ' @ ')))
    x <- x[!is.na(x)]
    x <- x[x > tr]
    if(length(x) > 0) {
      return(sum(x))
    } else {
      return(NA_real_)
    }
  }

  d_cjpg_classif <- d_cjpg_decisao_complete %>%
    group_by(cod_sentenca, n_processo) %>%
    mutate(
      dano_moral = ifelse(
        any(str_detect(frase, regex('danos? +mora[il]', ignore_case = TRUE))),
        'dano moral', 'nao sei'
      ),
      dano_material = ifelse(
        any(str_detect(frase, regex('danos? +materia[il]', ignore_case = TRUE))),
        'dano material', 'nao sei'
      ),
      dano_material = ifelse(
        any(str_detect(result, regex('conden[oa]r?', ignore_case = TRUE)) &
              str_detect(result, regex('ressar|devol|reemb|restit',
                                       ignore_case = TRUE))),
        'dano material', first(dano_material)
      )) %>%
    ungroup() %>%
    mutate(dano_moral = ifelse(dano_moral == 'nao sei', '', dano_moral),
           dano_material = ifelse(dano_material == 'nao sei', '', dano_material),
           assunto = ifelse(is.na(assunto), '', assunto),
           dano_moral = ifelse(dano_moral == '' &
                                 str_detect(assunto, '[mM]ora[li]'),
                               'dano moral', dano_moral),
           dano_material = ifelse(dano_material == '' &
                                    str_detect(assunto, '[mM]ateria[li]|Perdas'),
                                  'dano material', dano_material),
           tipo_dano = paste(dano_moral, dano_material, sep = ', '),
           tipo_dano = gsub('(, )$|^(, )', '', tipo_dano)) %>%
    filter(!str_detect(tolower(assunto), 'de fazer')) %>%
    mutate(
      result_moral = ifelse(str_detect(tipo_dano, 'moral'), 'nao sei', NA),
      result_material = ifelse(str_detect(tipo_dano, 'material'), 'nao sei', NA),
      result_moral = ifelse(result_txt == 'IMPROCEDENTE', 'IMPROCEDENTE',
                            result_moral),
      result_material = ifelse(result_txt == 'IMPROCEDENTE', 'IMPROCEDENTE',
                               result_material),
      result_moral = ifelse(result_txt == 'ACORDO',
                            'ACORDO', result_moral),
      result_material = ifelse(result_txt == 'ACORDO', 'ACORDO', result_material)
    ) %>%
    mutate(tipo_dano = ifelse(tipo_dano == '', NA, tipo_dano),
           result_material = ifelse(!is.na(tipo_dano) &
                                      str_detect(tipo_dano, 'moral') &
                                      !str_detect(tipo_dano, 'material'),
                                    NA, result_material),
           result_moral = ifelse(!is.na(tipo_dano) &
                                   str_detect(tipo_dano, 'material') &
                                   !str_detect(tipo_dano, 'moral'),
                                 NA, result_moral)) %>%
    mutate(
      result_moral = ifelse(!is.na(tipo_dano) & tipo_dano == 'dano moral',
                            result_txt, result_moral),
      result_material = ifelse(!is.na(tipo_dano) & tipo_dano == 'dano material',
                               result_txt, result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, 'PARCIALM'),
                               'PARCIALMENTE', result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, '([^I]|^)PROCED') &
                                 str_detect(result_material, 'IMPROCED'),
                               'PARCIALMENTE', result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, '([^I]|^)PROCED'),
                               'PROCEDENTE', result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, 'IMPROCED'),
                               'IMPROCEDENTE', result_material),
      result_moral = ifelse(!is.na(tipo_dano) &
                              tipo_dano == 'dano moral' &
                              str_detect(result_moral, '@') &
                              str_detect(result_moral, 'PARCIALM'),
                            'PARCIALMENTE', result_moral),
      result_moral = ifelse(!is.na(tipo_dano) &
                              tipo_dano == 'dano moral' &
                              str_detect(result_moral, '@') &
                              str_detect(result_moral, '([^I]|^)PROCED') &
                              str_detect(result_moral, 'IMPROCED'),
                            'PARCIALMENTE', result_moral),
      result_moral = ifelse(!is.na(tipo_dano) &
                              tipo_dano == 'dano moral' &
                              str_detect(result_moral, '@') &
                              str_detect(result_moral, '([^I]|^)PROCED'),
                            'PROCEDENTE', result_moral),
      result_moral = ifelse(!is.na(tipo_dano) &
                              tipo_dano == 'dano moral' &
                              str_detect(result_moral, '@') &
                              str_detect(result_moral, 'IMPROCED'),
                            'IMPROCEDENTE', result_moral),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano moral, dano material',
                               result_txt, result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano moral, dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, 'PARCIALM'),
                               'PARCIALMENTE', result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano moral, dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, '([^I]|^)PROCED') &
                                 str_detect(result_material, 'IMPROCED'),
                               'PARCIALMENTE', result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano moral, dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, '([^I]|^)PROCED'),
                               'PROCEDENTE', result_material),
      result_material = ifelse(!is.na(tipo_dano) &
                                 tipo_dano == 'dano moral, dano material' &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, 'IMPROCED'),
                               'IMPROCEDENTE', result_material),
      result_moral = ifelse(!is.na(tipo_dano) &
                              tipo_dano == 'dano moral, dano material',
                            result_material, result_moral),
      result_material = ifelse(is.na(tipo_dano), result_txt, result_material),
      result_material = ifelse(is.na(tipo_dano) &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, 'PARCIALM'),
                               'PARCIALMENTE', result_material),
      result_material = ifelse(is.na(tipo_dano) &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, '([^I]|^)PROCED') &
                                 str_detect(result_material, 'IMPROCED'),
                               'PARCIALMENTE', result_material),
      result_material = ifelse(is.na(tipo_dano) &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, '([^I]|^)PROCED'),
                               'PROCEDENTE', result_material),
      result_material = ifelse(is.na(tipo_dano) &
                                 str_detect(result_material, '@') &
                                 str_detect(result_material, 'IMPROCED'),
                               'IMPROCEDENTE', result_material),
      result_moral = ifelse(is.na(tipo_dano), result_material, result_moral)
    ) %>%
    mutate(resultado = ifelse(is.na(result_moral),
                              result_material, result_moral)) %>%
    mutate(valor_acao = as.numeric(gsub(',', '.',
                                        gsub('[^0-9,]', '', valor_da_acao)))) %>%
    mutate(adv_autor = ifelse(is.na(reqte_adv), 'não', 'sim'),
           adv_autor = ifelse(!str_detect(classe, 'Juizado'), 'sim', adv_autor),
           adv_reu = ifelse(is.na(reqdo_adv), 'não', 'sim')) %>%
    mutate(magistrado = toupper(tjsp::rm_accent(magistrado))) %>%
    left_join(antiguidade, 'magistrado') %>%
    select(-magistrado, -assunto, -vara) %>%
    mutate(foro = gsub('[^\\-]+\\- ', '', foro),
           foro = ifelse(str_detect(foro, 'Central'), 'Central', foro)) %>%
    mutate(tipo_vara = ifelse(str_detect(classe, 'Juizado'), 'JEC', 'Comum'),
           tipo_rito = ifelse(str_detect(classe, 'Sum'), 'Sumário',
                              'Ordinário')) %>%
    mutate(digital = ifelse(digital, 'sim', 'não')) %>%
    mutate(tipo_vara = ifelse(tipo_vara == 'Comum', 'Comum com advogado',
                              ifelse(adv_autor == 'sim', 'JEC com advogado',
                                     'JEC sem advogado'))) %>%
    left_join(datas_iniciais, 'n_processo') %>%
    mutate(tempo = as.numeric(data_disp - data_inicial)) %>%
    filter(tempo > 1, tempo < 10000) %>%
    mutate(ano = lubridate::year(data_inicial)) %>%
    filter(ano >= 1998) %>%
    select(-ano) %>%
    mutate(sm = sapply(as.character(data_inicial), salario_minimo)) %>%
    mutate(valor_acao = ifelse(tipo_vara == 'JEC sem advogado' &
                                 valor_acao > 20 * sm, 20 * sm, valor_acao),
           valor_acao = ifelse(tipo_vara == 'JEC com advogado' &
                                 valor_acao > 40 * sm, 40 * sm, valor_acao)) %>%
    select(-data_inicial, -data_disp) %>%
    mutate(olhar = ifelse(result_txt == 'ACORDO', frase, result)) %>%
    group_by(n_processo, cod_sentenca) %>%
    mutate(
      result_vl = sapply(
        str_extract_all(olhar, regex(mascara_dinheiro, ignore_case = TRUE)),
        function(x) {
          x <- as.numeric(gsub(',', '.', gsub('[^0-9,]', '', x)))
          if(length(x) > 0) return(paste(x, collapse = ' @ '))
          return(NA_character_)
        }),
      result_sum = somar(result_vl)
    ) %>%
    ungroup %>%
    mutate(
      resultado_vl = ifelse(resultado == 'PROCEDENTE', valor_acao, NA),
      resultado_vl = ifelse(resultado == 'IMPROCEDENTE' &
                              str_detect(classe, 'Juizado'),
                            0, resultado_vl),
      resultado_vl = ifelse(resultado == 'IMPROCEDENTE' &
                              !str_detect(classe, 'Juizado'),
                            -result_sum, resultado_vl),
      resultado_vl = ifelse(resultado != 'IMPROCEDENTE',
                            result_sum, resultado_vl),
      resultado_vl = ifelse(!is.na(resultado_vl) & resultado_vl > valor_acao,
                            valor_acao, resultado_vl)
    ) %>%
    mutate(valor_acao = valor_acao / sm, resultado_vl = resultado_vl / sm) %>%
    select(n_processo, cod_sentenca, frase, classe:digital, empresa, reqte_adv,
           tipo_vara, adv_reu, reqdo_adv, tipo_dano, resultado,
           antiguidade, valor_acao, resultado_vl, tempo) %>%
    group_by(n_processo) %>%
    mutate(serasa = any(str_detect(frase, regex(re_serasa,
                                                ignore_case = TRUE))),
           terceiro = any(str_detect(frase, regex(re_terceiro,
                                                  ignore_case = TRUE))),
           consumo = any(str_detect(frase, regex(re_consumo,
                                                 ignore_case = TRUE))),
           gratuidade = any(str_detect(frase, regex(re_gratuidade,
                                                    ignore_case = TRUE)))) %>%
    ungroup %>%
    mutate(
      tentou_conciliar = ifelse(
        str_detect(classe, 'Juizado') | resultado == 'ACORDO', 'sim',
        ifelse(str_detect(frase, regex(re_conciliar,
                                       ignore_case = TRUE)), 'sim', 'não')
      ),
      serasa = ifelse(serasa, 'sim', 'não'),
      terceiro = ifelse(terceiro, 'sim', 'não'),
      consumo = ifelse(consumo, 'sim', 'não'),
      gratuidade = ifelse(gratuidade, 'sim', 'não')
    )

  # save(d_cjpg_classif, file = 'data/d_cjpg_classif.rda')
  # load('data/d_cjpg_classif.rda')

  d_cjpg_final <- d_cjpg_classif %>%
    distinct(n_processo, cod_sentenca) %>%
    select(-frase, -tentou_conciliar,
           -classe, -reqte_adv, -reqdo_adv)


  # -----------------------------------------------------------------------------
  ls2 <- ls()
  ls2 <- ls2[ls2 != 'd_cjpg_final']
  rm(list = ls2)
  rm(ls2)
  save(d_cjpg_final, file = 'data/d_cjpg_final.rda')

  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # Precisa de bases externas
  d_cjpg_final2 <- d_cjpg_final %>%
    left_join(select(d_cpopg_processos, n_processo, r), 'n_processo') %>%
    mutate(resultado = ifelse(
      !is.na(resultado_vl) & !is.na(r) &
        resultado %in% c('IMPROCEDENTE', 'PARCIALMENTE', 'PROCEDENTE') &
        r %in% c('ACORDO', 'EXTINTO'),
      'ACORDO', resultado
    )
    ) %>%
    select(-r) %>%
    mutate(resultado_vl = ifelse(resultado == 'ACORDO' & !is.na(resultado_vl),
                                 abs(resultado_vl), resultado_vl)) %>%
    mutate(resultado_vl = ifelse(abs(resultado_vl) > valor_acao,
                                 sign(resultado_vl) * valor_acao,
                                 resultado_vl))


  d_cjpg_final2 <- d_cjpg_final2 %>%
    mutate(empresa = ifelse(empresa %in% c('BB', 'BRADESCO', 'ITAU', 'SANTANDER'),
                            'Bancos', ifelse(empresa %in% c('VIVO', 'TIM', 'CLARO', 'NEXTEL'),
                                             'Telefonia', 'Net/Eletropaulo'))) %>%
    mutate(resultado_vl = sign(resultado_vl) * log10(abs(resultado_vl) + 1)) %>%
    mutate(valor_acao = as.character(cut(valor_acao, c(0, 5, 10, 20, 40, Inf))))
  d_cjpg_final2
}

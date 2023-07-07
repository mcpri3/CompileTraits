library("stringr")
library("purrr")
library("dplyr")

prefixes <- c(obo="http://purl.obolibrary.org/obo/",
              dwc="http://rs.tdwg.org/dwc/terms/",
              rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#",
              rdfs="http://www.w3.org/2000/01/rdf-schema#",
              xsd="http://www.w3.org/2001/XMLSchema#",
              onto="http://www.ontotext.com/",
              NCBITaxon="http://purl.obolibrary.org/obo/NCBITaxon_",
              NCBI="https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
              RO="http://purl.obolibrary.org/obo/RO_",
              GRATIN="https://leca.osug.fr/gratin/",
              SFWO="http://purl.org/sfwo/SFWO_",
              ECOCORE="http://purl.obolibrary.org/obo/ECOCORE_",
              OBI="http://purl.obolibrary.org/obo/OBI_",
              BFO="http://purl.obolibrary.org/obo/BFO_",
              GBIF="http://www.gbif.org/species/",
              ITIS="http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=",
              SILVA="https://www.arb-silva.de/",
              IF= "http://www.indexfungorum.org/names/NamesRecord.asp?RecordID=",
              sesame="http://www.openrdf.org/schema/sesame#",
              dispersal="http://leca.osug.fr/"
)

add_prefixes <- function(query) {
  format.namespace = function(x) sprintf("PREFIX %s: <%s>", x, prefixes[[x]])
  ns.str <- unlist(lapply(names(prefixes), format.namespace), use.names = FALSE)
  ns.str <- paste(ns.str, collapse = '\n')
  paste(ns.str, query, sep ="\n")
}

#' Create SPARQL values given a variable name and a vector of values.
#'
#' @param var.name The variable name.
#' @param var.values A vector containing the variable values.
#' @param is.iri Set to TRUE if var.values contain IRIs, FALSE if it contains strings. Default is TRUE.
#' @return A string.
#' @example \dontrun{get.sparql.values("?group_name", c("microbivore", "detritivore"), is.iri=FALSE)}
get.sparql.values <- function(var.name, var.values, is.iri=TRUE) {
  values.string = ""
  if(!is.iri)
    var.values = sapply(var.values, function(x) sprintf('"%s"', x))
  var.values.string = paste0(var.values, collapse=" ")
  values.string = sprintf("VALUES ?%s { %s }", var.name, var.values.string)
  return(values.string)
}

send.sparql <- function (.query, endpoint = "http://129.88.204.79:7200/repositories/dispersal")
{
  .query <- add_prefixes(.query)
  # writeLines(.query)
  url=endpoint
  resp = httr2::request(url) %>% httr2::req_url_query(query = .query) %>%
    httr2::req_method("GET") %>% httr2::req_headers(Accept = "application/sparql-results+json") %>%
    httr2::req_user_agent("rdfox") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(resp)
  if (httr2::resp_content_type(resp) != "application/sparql-results+json") {
    rlang::abort("Not right response type")
  }
  content = httr2::resp_body_json(resp)
  if (length(content$results$bindings) > 0) {
    parse_binding = function(binding, name) {
      type <- sub("http://www.w3.org/2001/XMLSchema#", "", binding[["datatype"]] %||% "http://www.w3.org/2001/XMLSchema#character")
      parse = function(x, type) {
        switch(type, character = x, integer = x, datetime = anytime::anytime(x))
      }
      value = parse(binding[["value"]], type)
      tibble::tibble(.rows = 1) %>% dplyr::mutate(`:=`({
        {
          name
        }
      }, value))
    }
    parse_result = function(result) {
      purrr::map2(result, names(result), parse_binding) %>%
        dplyr::bind_cols()
    }
    data_frame <- purrr::map_df(content$results$bindings, parse_result)
  }
  else {
    data_frame <- dplyr::as_tibble(matrix(character(), nrow = 0, ncol = length(content$head$vars), dimnames = list(c(), unlist(content$head$vars))))
  }
  return(data_frame)
}

get.traits <- function(taxid = NULL, sciName = NULL) {
  query = "
    SELECT DISTINCT ?queryName ?queryId ?matchName ?matchId ?trait_value ?trait ?unit ?trait_value_label ?graph
    WHERE{
      %s
      GRAPH ?graph {
        ?observation obo:OBI_0000293 ?organism.
      }
      ?organism rdf:type ?queryId.
      ?queryId rdfs:label ?queryName.
      ?organism sesame:directType ?matchId.
      ?matchId rdfs:label ?matchName.
      ?observation obo:OBI_0000299 ?datum.
      ?datum obo:IAO_0000136 ?quality.
      ?datum obo:OBI_0001938 ?value.
      ?value obo:OBI_0002135 ?trait_value.
      ?quality sesame:directType ?trait_iri.
      OPTIONAL {?value obo:IAO_0000039 [rdfs:label ?unit].}
      OPTIONAL {?trait_value rdfs:label ?trait_value_label.}
      OPTIONAL {?trait_iri rdfs:label ?trait.}
      FILTER(?trait_iri!=owl:Thing)
      FILTER(?trait_iri!=obo:PATO_0000001)
      FILTER(?matchId!=obo:CARO_0001010)
    }
  "
  
  values = paste(
    { if(!is.null(sciName)) get.sparql.values("queryName", sciName, is.iri=FALSE) else "" },
    { if(!is.null(taxid)) get.sparql.values("queryId", taxid, is.iri=TRUE) else "" },
    sep = " "
  )
  query <- sprintf(query, values)
  df <- send.sparql(query)
  
  cols <- c( "queryName","queryId","matchName","matchId", "trait", "trait_value", "unit", "graph" )
  df[cols[!(cols %in% colnames(df))]] = NA
  if(nrow(df) == 0)
    return(df)
  df <- df %>%
    group_by(matchName) %>%
    summarise(matchId = list(matchId), across())
  df$matchId <- lapply(df$matchId, function(x) {lapply(unique(x), to.short.iri)})
  df$queryId <- apply(df, 1, function(x) { if(!is.null(taxid)) to.short.iri(x$queryId) else NA })
  df$trait <- apply(df, 1, function(x) { to.short.iri(x$trait) })
  df$graph <- apply(df, 1, function(x) { to.short.iri(x$graph) })
  distinct(df[,cols])
}

get.provenance <- function(graph = NULL) {
  query = "
    SELECT DISTINCT ?field ?value
    WHERE{
      %s
      ?graph ?field ?value.
    }
  "
  values = get.sparql.values("graph", graph, is.iri=TRUE)
  query <- sprintf(query, values)
  df <- send.sparql(query)
}

#' Return short-form IRI.
#'
#' @param full.iri A full IRI.
#' @param namespaces A dictionary of namespaces.
#' @return A short version of the IRI in the form PREFIX:ID.
#' @example \dontrun{to.short.iri(full.iri="<http://purl.obolibrary.org/obo/STWO_0000024>", namespaces)}
#' @import stringr
#' @import memoise
to.short.iri <- function(full.iri, namespaces=prefixes) {
  if (!is.na(full.iri)) {
    max.long.form.length <- 0
    long.form <- NULL
    prefix <- NULL
    for (ns in names(namespaces)) {
      long.form.candidate <- namespaces[[ns]]
      if (startsWith(full.iri, long.form.candidate)) {
        if (nchar(long.form.candidate) > max.long.form.length) {
          max.long.form.length <- nchar(long.form.candidate)
          long.form <- long.form.candidate
          prefix <- ns
        }
      }
    }
    if (!is.null(long.form)) {
      return(paste0(prefix, ":", str_replace(full.iri, fixed(long.form), "")))
    }
  }
  return(full.iri)
}




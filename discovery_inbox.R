# ============================================================
# UPSTREAM HEALTH — ADMIN DISCOVERY + PROPOSAL HELPERS
# ============================================================
# Admin-only evidence discovery. Nothing in this module writes
# directly to the public graph. It creates review candidates and
# proposal drafts that require explicit admin review.
# ============================================================

empty_discovery_candidates <- function() {
  data.frame(
    candidate_id = character(),
    source_type = character(),
    source_org = character(),
    source_title = character(),
    source_url = character(),
    publication_date = character(),
    publication_types = character(),
    discovery_query_id = character(),
    discovery_query_label = character(),
    existing_node_hits = character(),
    priority = character(),
    status = character(),
    detected_at = character(),
    reviewed_at = character(),
    admin_notes = character(),
    proposal_kind = character(),
    proposal_summary = character(),
    proposed_nodes = character(),
    proposed_relationships = character(),
    proposed_interventions = character(),
    proposal_evidence = character(),
    proposal_review_status = character(),
    stringsAsFactors = FALSE
  )
}

ensure_candidate_columns <- function(df) {
  template <- empty_discovery_candidates()
  for (nm in names(template)) {
    if (!nm %in% names(df)) df[[nm]] <- rep(template[[nm]], length.out = nrow(df))
  }
  df[, names(template), drop = FALSE]
}

ensure_watchlist_columns <- function(df) {
  needed <- list(
    query_id = character(), label = character(), query = character(),
    cdc_query = character(), active = logical(), source_tier = integer()
  )
  for (nm in names(needed)) {
    if (!nm %in% names(df)) {
      if (nm == "cdc_query") df[[nm]] <- "" else df[[nm]] <- rep(needed[[nm]], length.out = nrow(df))
    }
  }
  df[, names(needed), drop = FALSE]
}

safe_json_from_url <- function(url) {
  tryCatch(jsonlite::fromJSON(url, simplifyVector = TRUE), error = function(e) NULL)
}

build_ncbi_common_params <- function() {
  params <- c("tool=upstream_health")
  ncbi_email <- trimws(Sys.getenv("NCBI_EMAIL", ""))
  ncbi_key <- trimws(Sys.getenv("NCBI_API_KEY", ""))
  if (nzchar(ncbi_email)) params <- c(params, paste0("email=", utils::URLencode(ncbi_email, reserved = TRUE)))
  if (nzchar(ncbi_key)) params <- c(params, paste0("api_key=", utils::URLencode(ncbi_key, reserved = TRUE)))
  paste(params, collapse = "&")
}

pubmed_search_ids <- function(query, days_back = 30, retmax = 8) {
  common <- build_ncbi_common_params()
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?",
    "db=pubmed&retmode=json&sort=pub+date&retmax=", as.integer(retmax),
    "&datetype=pdat&reldate=", as.integer(days_back),
    "&term=", utils::URLencode(query, reserved = TRUE), "&", common
  )
  out <- safe_json_from_url(url)
  if (is.null(out) || is.null(out$esearchresult$idlist)) return(character())
  as.character(out$esearchresult$idlist)
}

pubmed_summaries <- function(ids) {
  if (length(ids) == 0) return(list())
  common <- build_ncbi_common_params()
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?",
    "db=pubmed&retmode=json&id=", paste(ids, collapse = ","), "&", common
  )
  out <- safe_json_from_url(url)
  if (is.null(out) || is.null(out$result)) return(list())
  result <- out$result
  uids <- result$uids
  if (is.null(uids)) uids <- ids
  lapply(as.character(uids), function(id) result[[id]])
}

pubmed_abstract_text <- function(candidate_id) {
  pmid <- sub("^pubmed_", "", candidate_id)
  if (!grepl("^[0-9]+$", pmid)) return("")
  common <- build_ncbi_common_params()
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?",
    "db=pubmed&id=", pmid, "&rettype=abstract&retmode=text&", common
  )
  txt <- tryCatch(readLines(url, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  txt <- trimws(paste(txt, collapse = " "))
  gsub("[[:space:]]+", " ", txt)
}

flatten_pub_types <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  paste(unique(as.character(x)), collapse = "; ")
}

candidate_priority <- function(title, pubtypes) {
  text <- tolower(paste(title, pubtypes))
  if (grepl("systematic review|meta-analysis|meta analysis|practice guideline|guideline", text)) return("HIGH")
  if (grepl("randomized|randomised|clinical trial|consensus", text)) return("MEDIUM-HIGH")
  "MEDIUM"
}

find_existing_node_hits <- function(text, nodes) {
  if (is.null(nodes) || nrow(nodes) == 0 || !"node_name" %in% names(nodes)) return("")
  text_l <- tolower(text)
  names_vec <- as.character(nodes$node_name)
  ids_vec <- as.character(nodes$node_id)
  hits <- vapply(seq_along(names_vec), function(i) {
    nm <- tolower(trimws(names_vec[i]))
    if (nchar(nm) >= 5 && grepl(nm, text_l, fixed = TRUE)) ids_vec[i] else NA_character_
  }, character(1))
  paste(unique(hits[!is.na(hits)]), collapse = "; ")
}

scan_pubmed_watchlist <- function(watchlist, nodes, days_back = 30, retmax = 8) {
  watchlist <- ensure_watchlist_columns(watchlist)
  active <- watchlist[tolower(as.character(watchlist$active)) %in% c("true","1","yes","y"), , drop = FALSE]
  if (nrow(active) == 0) return(empty_discovery_candidates())
  rows <- list(); idx <- 1L
  for (i in seq_len(nrow(active))) {
    query <- trimws(as.character(active$query[i])); if (!nzchar(query)) next
    ids <- pubmed_search_ids(query, days_back = days_back, retmax = retmax); Sys.sleep(0.40)
    summaries <- pubmed_summaries(ids); Sys.sleep(0.40)
    for (rec in summaries) {
      if (is.null(rec) || is.null(rec$uid)) next
      pmid <- as.character(rec$uid)
      title <- if (!is.null(rec$title)) as.character(rec$title) else paste("PubMed", pmid)
      pubdate <- if (!is.null(rec$pubdate)) as.character(rec$pubdate) else ""
      pubtypes <- flatten_pub_types(rec$pubtype)
      rows[[idx]] <- data.frame(
        candidate_id=paste0("pubmed_",pmid), source_type="PubMed article", source_org="PubMed / NCBI",
        source_title=title, source_url=paste0("https://pubmed.ncbi.nlm.nih.gov/",pmid,"/"),
        publication_date=pubdate, publication_types=pubtypes,
        discovery_query_id=as.character(active$query_id[i]), discovery_query_label=as.character(active$label[i]),
        existing_node_hits=find_existing_node_hits(title,nodes), priority=candidate_priority(title,pubtypes),
        status="NEW", detected_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"), reviewed_at="", admin_notes="",
        proposal_kind="", proposal_summary="", proposed_nodes="", proposed_relationships="", proposed_interventions="",
        proposal_evidence="", proposal_review_status="", stringsAsFactors=FALSE
      ); idx <- idx+1L
    }
  }
  if (!length(rows)) return(empty_discovery_candidates())
  ensure_candidate_columns(do.call(rbind, rows))
}

# CDC Content Services is an official CDC API. This is a discovery source,
# not automatic evidence approval. CDC candidates still require admin review.
scan_cdc_watchlist <- function(watchlist, nodes, retmax = 6) {
  watchlist <- ensure_watchlist_columns(watchlist)
  active <- watchlist[tolower(as.character(watchlist$active)) %in% c("true","1","yes","y"), , drop=FALSE]
  rows <- list(); idx <- 1L
  for (i in seq_len(nrow(active))) {
    q <- trimws(as.character(active$cdc_query[i])); if (!nzchar(q)) next
    url <- paste0(
      "https://tools.cdc.gov/api/v2/resources/media?max=", as.integer(retmax),
      "&q=", utils::URLencode(q, reserved=TRUE)
    )
    out <- safe_json_from_url(url)
    results <- if (!is.null(out$results)) out$results else NULL
    if (is.null(results)) next
    if (is.data.frame(results)) recs <- split(results, seq_len(nrow(results))) else if (is.list(results)) recs <- results else next
    for (rec in recs) {
      getv <- function(nms, default="") {
        for (nm in nms) if (!is.null(rec[[nm]]) && length(rec[[nm]]) > 0 && !is.na(rec[[nm]][1])) return(as.character(rec[[nm]][1]))
        default
      }
      rid <- getv(c("id","mediaId")); title <- getv(c("name","title"), paste("CDC content",rid))
      surl <- getv(c("sourceUrl","url","targetUrl"), "https://www.cdc.gov/")
      pdate <- getv(c("dateModified","datePublished","lastModified"), "")
      if (!nzchar(rid)) rid <- substr(gsub("[^A-Za-z0-9]","",title),1,50)
      rows[[idx]] <- data.frame(
        candidate_id=paste0("cdc_",rid), source_type="CDC official content", source_org="CDC",
        source_title=title, source_url=surl, publication_date=pdate, publication_types="Official CDC content",
        discovery_query_id=as.character(active$query_id[i]), discovery_query_label=as.character(active$label[i]),
        existing_node_hits=find_existing_node_hits(title,nodes), priority="HIGH", status="NEW",
        detected_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"), reviewed_at="", admin_notes="",
        proposal_kind="", proposal_summary="", proposed_nodes="", proposed_relationships="", proposed_interventions="",
        proposal_evidence="", proposal_review_status="", stringsAsFactors=FALSE
      ); idx <- idx+1L
    }
  }
  if (!length(rows)) return(empty_discovery_candidates())
  ensure_candidate_columns(do.call(rbind,rows))
}

make_manual_trusted_candidate <- function(org, title, url, notes, nodes) {
  slug <- substr(gsub("[^A-Za-z0-9]", "", paste(org,title,format(Sys.time(),"%Y%m%d%H%M%S"))), 1, 90)
  data.frame(
    candidate_id=paste0("manual_",slug), source_type="Trusted source — manual import", source_org=org,
    source_title=title, source_url=url, publication_date="", publication_types="Admin-imported trusted source",
    discovery_query_id="manual", discovery_query_label="Manual trusted-source import",
    existing_node_hits=find_existing_node_hits(title,nodes), priority="HIGH", status="KEEP",
    detected_at=format(Sys.time(),"%Y-%m-%d %H:%M:%S"), reviewed_at="", admin_notes=notes,
    proposal_kind="", proposal_summary="", proposed_nodes="", proposed_relationships="", proposed_interventions="",
    proposal_evidence="", proposal_review_status="", stringsAsFactors=FALSE
  )
}

build_candidate_proposal <- function(row, nodes, abstract_text="") {
  text <- paste(row$source_title, abstract_text)
  text_l <- tolower(text)
  hits <- find_existing_node_hits(text,nodes)
  hit_names <- character()
  if (nzchar(hits)) {
    hit_ids <- trimws(unlist(strsplit(hits,";",fixed=TRUE)))
    hit_names <- nodes$node_name[match(hit_ids,nodes$node_id)]
    hit_names <- hit_names[!is.na(hit_names)]
  }
  kind <- "Evidence update / possible network expansion"
  if (grepl("guideline|recommendation|intervention|trial|program|treatment|prevention",text_l)) kind <- "Intervention or guidance update"
  if (grepl("risk factor|associated|association|relationship|predict|determinant|exposure",text_l)) kind <- "Relationship / evidence update"
  if (grepl("terminology|nomenclature|definition|renam",text_l)) kind <- "Terminology update"
  if (grepl("systematic review|meta-analysis|meta analysis",text_l)) kind <- paste0(kind," — synthesis evidence")
  summary <- paste0(
    "Automated triage found ", if(length(hit_names)) paste(length(hit_names),"existing graph concept(s)") else "no exact existing graph concept match",
    ". This is a proposal draft only; confirm the source supports any graph change before publishing."
  )
  proposed_nodes <- if(length(hit_names)) {
    paste0("Reuse/check existing concepts: ",paste(hit_names,collapse="; "),". Review source for genuinely new concepts before adding a node.")
  } else "No automatic node match. Review title/abstract for a new canonical concept or alias."
  proposed_rel <- if(length(hit_names)>=2) paste0("Review whether the source supports a relationship or evidence-strength update among: ",paste(hit_names,collapse="; "),".") else if(length(hit_names)==1) paste0("Review upstream/downstream relationships involving ",hit_names[1],".") else "Review for new or updated relationships; do not infer causality from the title alone."
  proposed_int <- if(grepl("intervention|program|treatment|prevention|guideline|recommendation",text_l)) "Potential intervention/guidance content detected. Extract actor, setting, target node, effect, and evidence strength before approval." else "No strong intervention signal detected automatically."
  evidence <- paste0("Source: ",row$source_org," • Type: ",row$publication_types," • Priority: ",row$priority,". Automated triage does not replace source review.")
  list(kind=kind, summary=summary, nodes=proposed_nodes, relationships=proposed_rel, interventions=proposed_int, evidence=evidence, hits=hits)
}

merge_discovery_candidates <- function(existing, incoming) {
  existing <- ensure_candidate_columns(existing); incoming <- ensure_candidate_columns(incoming)
  if (nrow(incoming)==0) return(existing)
  if (nrow(existing)==0) return(incoming[!duplicated(incoming$candidate_id),,drop=FALSE])
  new_rows <- incoming[!incoming$candidate_id %in% existing$candidate_id,,drop=FALSE]
  out <- rbind(existing,new_rows); out[!duplicated(out$candidate_id),,drop=FALSE]
}

save_discovery_candidates <- function(df, path="data/discovery_candidates.csv") {
  utils::write.csv(ensure_candidate_columns(df),path,row.names=FALSE,na=""); invisible(TRUE)
}

save_discovery_watchlist <- function(df,path="data/discovery_watchlist.csv") {
  utils::write.csv(ensure_watchlist_columns(df),path,row.names=FALSE,na=""); invisible(TRUE)
}

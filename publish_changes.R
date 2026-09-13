# ============================================================
# UPSTREAM HEALTH — ADMIN PUBLISH HELPERS
# ============================================================
# Converts an explicitly approved proposal change set into rows
# in the graph data tables. Nothing here runs unless the admin
# clicks Publish approved changes.
# ============================================================

uh_slugify_id <- function(x, prefix = "") {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- paste0("item_", format(Sys.time(), "%Y%m%d%H%M%S"))
  paste0(prefix, x)
}

uh_unique_id <- function(base, existing) {
  if (!base %in% existing) return(base)
  i <- 2L
  repeat {
    candidate <- paste0(base, "_", i)
    if (!candidate %in% existing) return(candidate)
    i <- i + 1L
  }
}

uh_next_relationship_id <- function(existing_ids) {
  nums <- suppressWarnings(as.integer(sub("^rel_", "", existing_ids)))
  nums <- nums[!is.na(nums)]
  next_num <- if (length(nums)) max(nums) + 1L else 1L
  sprintf("rel_%03d", next_num)
}

uh_source_id_for_candidate <- function(candidate_row, sources) {
  url <- trimws(as.character(candidate_row$source_url[1]))
  if (nzchar(url) && nrow(sources) > 0 && "url" %in% names(sources)) {
    hit <- which(tolower(trimws(sources$url)) == tolower(url))
    if (length(hit)) return(as.character(sources$source_id[hit[1]]))
  }
  base <- uh_slugify_id(as.character(candidate_row$candidate_id[1]), prefix = "src_discovery_")
  uh_unique_id(base, as.character(sources$source_id))
}

uh_make_source_row <- function(candidate_row, source_id) {
  data.frame(
    source_id = source_id,
    organization = as.character(candidate_row$source_org[1]),
    title = as.character(candidate_row$source_title[1]),
    url = as.character(candidate_row$source_url[1]),
    accessed_date = format(Sys.Date(), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
}

uh_backup_graph_files <- function(data_dir = "data") {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_dir <- file.path(data_dir, "backups", stamp)
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    "nodes.csv", "node_content.csv", "relationships.csv",
    "relationship_content.csv", "relationship_sources.csv", "sources.csv"
  )
  for (nm in files) {
    src <- file.path(data_dir, nm)
    if (file.exists(src)) file.copy(src, file.path(backup_dir, nm), overwrite = TRUE)
  }
  backup_dir
}

uh_build_publish_preview <- function(
  candidate_row,
  add_node = FALSE,
  node_name = "",
  node_type = "disease",
  sdoh_domain = "",
  modifiable = FALSE,
  badge = "",
  description = "",
  why_it_matters = "",
  importance = 2,
  add_relationship = FALSE,
  from_node = "",
  to_node = "",
  relationship_type = "associated_with",
  evidence_strength = "moderate",
  short_explanation = "",
  rel_badge = "EVIDENCE UPDATE",
  primary_text = "",
  secondary_label = "IMPORTANT DISTINCTION",
  secondary_text = "",
  nodes,
  node_content,
  relationships,
  relationship_content,
  relationship_sources,
  sources,
  interventions,
  local_metric_mappings,
  local_metric_notes,
  qa_source_domains,
  qa_metric_rules
) {
  new_nodes <- nodes
  new_node_content <- node_content
  new_relationships <- relationships
  new_relationship_content <- relationship_content
  new_relationship_sources <- relationship_sources
  new_sources <- sources
  messages <- character()
  created_node_id <- ""
  created_relationship_id <- ""

  source_id <- uh_source_id_for_candidate(candidate_row, new_sources)
  if (!source_id %in% new_sources$source_id) {
    new_sources <- rbind(new_sources, uh_make_source_row(candidate_row, source_id))
    messages <- c(messages, paste0("Add source: ", source_id))
  } else {
    messages <- c(messages, paste0("Reuse source: ", source_id))
  }

  if (isTRUE(add_node)) {
    node_name <- trimws(node_name)
    if (!nzchar(node_name)) stop("A node name is required when Add a new node is checked.")
    duplicate_name <- which(tolower(trimws(new_nodes$node_name)) == tolower(node_name))
    if (length(duplicate_name)) {
      created_node_id <- as.character(new_nodes$node_id[duplicate_name[1]])
      messages <- c(messages, paste0("Reuse existing node because the canonical name already exists: ", created_node_id))
    } else {
      created_node_id <- uh_unique_id(uh_slugify_id(node_name), as.character(new_nodes$node_id))
      node_row <- data.frame(
        node_id = created_node_id,
        node_name = node_name,
        node_type = as.character(node_type),
        sdoh_domain = as.character(sdoh_domain),
        modifiable = isTRUE(modifiable),
        stringsAsFactors = FALSE
      )
      content_row <- data.frame(
        node_id = created_node_id,
        badge = trimws(as.character(badge)),
        description = trimws(as.character(description)),
        why_it_matters = trimws(as.character(why_it_matters)),
        importance = suppressWarnings(as.numeric(importance)),
        stringsAsFactors = FALSE
      )
      if (!nzchar(content_row$badge)) content_row$badge <- toupper(gsub("_", " ", node_type))
      if (!nzchar(content_row$description)) stop("A description is required for a new node.")
      if (!nzchar(content_row$why_it_matters)) stop("'Why it matters' is required for a new node.")
      if (is.na(content_row$importance)) content_row$importance <- 2
      new_nodes <- rbind(new_nodes, node_row)
      new_node_content <- rbind(new_node_content, content_row)
      messages <- c(messages, paste0("Add node: ", node_name, " [", created_node_id, "]"))
    }
  }

  resolve_node <- function(x) {
    x <- trimws(as.character(x))
    if (identical(x, "__NEW__")) return(created_node_id)
    x
  }

  if (isTRUE(add_relationship)) {
    from_id <- resolve_node(from_node)
    to_id <- resolve_node(to_node)
    if (!nzchar(from_id) || !nzchar(to_id)) stop("Choose both a FROM node and a TO node for the relationship.")
    if (!from_id %in% new_nodes$node_id) stop(paste0("Relationship FROM node does not exist: ", from_id))
    if (!to_id %in% new_nodes$node_id) stop(paste0("Relationship TO node does not exist: ", to_id))
    if (from_id == to_id) stop("FROM and TO cannot be the same node for this publishing workflow.")

    pair_exists <- which(new_relationships$from_node == from_id & new_relationships$to_node == to_id)
    if (length(pair_exists)) {
      stop(paste0("That directed relationship already exists as ", new_relationships$relationship_id[pair_exists[1]], ". Edit the existing relationship instead of creating a duplicate."))
    }

    if (!nzchar(trimws(short_explanation))) stop("A short relationship explanation is required.")
    if (!nzchar(trimws(primary_text))) stop("Detailed relationship text is required.")

    created_relationship_id <- uh_next_relationship_id(as.character(new_relationships$relationship_id))
    rel_row <- data.frame(
      relationship_id = created_relationship_id,
      from_node = from_id,
      to_node = to_id,
      relationship_type = as.character(relationship_type),
      evidence_strength = as.character(evidence_strength),
      short_explanation = trimws(as.character(short_explanation)),
      stringsAsFactors = FALSE
    )
    rel_content_row <- data.frame(
      relationship_key = paste(from_id, to_id, sep = "__"),
      badge = trimws(as.character(rel_badge)),
      primary_text = trimws(as.character(primary_text)),
      secondary_label = trimws(as.character(secondary_label)),
      secondary_text = trimws(as.character(secondary_text)),
      stringsAsFactors = FALSE
    )
    rel_source_row <- data.frame(
      relationship_id = created_relationship_id,
      source_id = source_id,
      stringsAsFactors = FALSE
    )
    new_relationships <- rbind(new_relationships, rel_row)
    new_relationship_content <- rbind(new_relationship_content, rel_content_row)
    new_relationship_sources <- rbind(new_relationship_sources, rel_source_row)
    messages <- c(messages, paste0("Add relationship: ", from_id, " → ", to_id, " [", created_relationship_id, "]"))
  }

  qa <- run_static_qa(
    nodes = new_nodes,
    node_content = new_node_content,
    relationships = new_relationships,
    relationship_content = new_relationship_content,
    relationship_sources = new_relationship_sources,
    sources = new_sources,
    interventions = interventions,
    local_metric_mappings = local_metric_mappings,
    local_metric_notes = local_metric_notes,
    qa_source_domains = qa_source_domains,
    qa_metric_rules = qa_metric_rules
  )

  list(
    nodes = new_nodes,
    node_content = new_node_content,
    relationships = new_relationships,
    relationship_content = new_relationship_content,
    relationship_sources = new_relationship_sources,
    sources = new_sources,
    source_id = source_id,
    created_node_id = created_node_id,
    created_relationship_id = created_relationship_id,
    messages = messages,
    qa = qa,
    worst = qa_worst_status(qa)
  )
}

uh_commit_publish_preview <- function(preview, data_dir = "data") {
  backup_dir <- uh_backup_graph_files(data_dir)
  utils::write.csv(preview$nodes, file.path(data_dir, "nodes.csv"), row.names = FALSE, na = "")
  utils::write.csv(preview$node_content, file.path(data_dir, "node_content.csv"), row.names = FALSE, na = "")
  utils::write.csv(preview$relationships, file.path(data_dir, "relationships.csv"), row.names = FALSE, na = "")
  utils::write.csv(preview$relationship_content, file.path(data_dir, "relationship_content.csv"), row.names = FALSE, na = "")
  utils::write.csv(preview$relationship_sources, file.path(data_dir, "relationship_sources.csv"), row.names = FALSE, na = "")
  utils::write.csv(preview$sources, file.path(data_dir, "sources.csv"), row.names = FALSE, na = "")
  backup_dir
}

uh_append_publish_log <- function(candidate_row, preview, path = "data/publish_log.csv") {
  new_row <- data.frame(
    published_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    candidate_id = as.character(candidate_row$candidate_id[1]),
    source_id = preview$source_id,
    created_node_id = preview$created_node_id,
    created_relationship_id = preview$created_relationship_id,
    changes = paste(preview$messages, collapse = " | "),
    stringsAsFactors = FALSE
  )
  if (file.exists(path)) {
    old <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(old)) new_row <- rbind(old, new_row)
  }
  utils::write.csv(new_row, path, row.names = FALSE, na = "")
  invisible(TRUE)
}

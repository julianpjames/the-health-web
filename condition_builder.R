# ============================================================
# UPSTREAM HEALTH — ADMIN CONDITION PACKAGE BUILDER
# ============================================================
# Admin-only bulk condition expansion helpers.
# A package can create a central disease/condition node, optional
# supporting nodes, and multiple relationships in one reviewed batch.
# Nothing is written until the admin runs QA and explicitly publishes.
# ============================================================

uh_cb_slug <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

uh_cb_split_lines <- function(x) {
  x <- if (is.null(x)) "" else as.character(x)
  z <- unlist(strsplit(x, "\\r?\\n"))
  trimws(z[nzchar(trimws(z))])
}

uh_cb_parse_pipe_rows <- function(text, min_fields = 1L) {
  lines <- uh_cb_split_lines(text)
  if (!length(lines)) return(list())
  lapply(lines, function(line) {
    fields <- trimws(unlist(strsplit(line, "|", fixed = TRUE)))
    if (length(fields) < min_fields) fields <- c(fields, rep("", min_fields - length(fields)))
    fields
  })
}

uh_cb_find_node <- function(name_or_id, nodes) {
  q <- trimws(as.character(name_or_id))
  if (!nzchar(q)) return(NA_character_)
  hit <- which(tolower(nodes$node_id) == tolower(q) | tolower(nodes$node_name) == tolower(q))
  if (length(hit)) as.character(nodes$node_id[hit[1]]) else NA_character_
}

uh_cb_next_rel_id <- function(existing) {
  nums <- suppressWarnings(as.integer(sub("^rel_", "", existing)))
  nums <- nums[!is.na(nums)]
  sprintf("rel_%03d", if(length(nums)) max(nums)+1L else 1L)
}

uh_cb_guess_relationship_rows <- function(condition_id, existing_hit_ids, nodes) {
  if (!length(existing_hit_ids)) return(character())
  out <- character()
  for (nid in existing_hit_ids) {
    if (!nid %in% nodes$node_id || nid == condition_id) next
    typ <- as.character(nodes$node_type[match(nid, nodes$node_id)])
    nm <- as.character(nodes$node_name[match(nid, nodes$node_id)])
    if (typ %in% c("symptom","complication")) {
      out <- c(out, paste(condition_id, nid, "associated_with", "moderate", paste0("Review whether the source supports a relationship between the condition and ", nm, "."), sep=" | "))
    } else if (typ %in% c("structural_determinant","social_determinant","environmental_determinant","behavioral_pathway","behavioral_biological_pathway","biological_pathway")) {
      out <- c(out, paste(nid, condition_id, "associated_with", "moderate", paste0("Review whether the source supports ", nm, " as an upstream factor for this condition."), sep=" | "))
    } else {
      out <- c(out, paste(nid, condition_id, "associated_with", "moderate", paste0("Review whether the source supports an association between ", nm, " and this condition."), sep=" | "))
    }
  }
  out
}

uh_cb_build_preview <- function(candidate_row, condition_name, condition_description, condition_why,
                                condition_badge, condition_importance,
                                supporting_nodes_text, relationships_text,
                                nodes, node_content, relationships, relationship_content,
                                relationship_sources, sources) {
  condition_name <- trimws(as.character(condition_name))
  if (!nzchar(condition_name)) stop("Enter the disease or condition name.")

  exact_id <- uh_cb_find_node(condition_name, nodes)
  add_condition <- is.na(exact_id)
  condition_id <- if(add_condition) uh_cb_slug(condition_name) else exact_id
  if (!nzchar(condition_id)) stop("Could not create a valid condition ID.")
  if (add_condition && condition_id %in% nodes$node_id) condition_id <- paste0(condition_id, "_condition")

  new_nodes <- data.frame()
  new_content <- data.frame()
  messages <- character()

  if (add_condition) {
    new_nodes <- data.frame(
      node_id=condition_id, node_name=condition_name, node_type="disease",
      sdoh_domain="", modifiable=FALSE, stringsAsFactors=FALSE
    )
    new_content <- data.frame(
      node_id=condition_id,
      badge=if(nzchar(trimws(condition_badge))) trimws(condition_badge) else "DISEASE / CONDITION",
      description=trimws(condition_description), why_it_matters=trimws(condition_why),
      importance=as.numeric(condition_importance), stringsAsFactors=FALSE
    )
    messages <- c(messages, paste0("ADD condition node: ", condition_name, " [", condition_id, "]"))
  } else {
    messages <- c(messages, paste0("REUSE existing condition node: ", condition_name, " [", condition_id, "]"))
  }

  # Supporting-node syntax:
  # name | node_type | badge | description | why it matters | importance
  sn_rows <- uh_cb_parse_pipe_rows(supporting_nodes_text, 6L)
  for (f in sn_rows) {
    nm <- f[1]; if (!nzchar(nm)) next
    existing <- uh_cb_find_node(nm, nodes)
    if (!is.na(existing)) {
      messages <- c(messages, paste0("REUSE supporting node: ", nm, " [", existing, "]"))
      next
    }
    nid <- uh_cb_slug(nm)
    if (!nzchar(nid)) next
    used <- c(nodes$node_id, if(nrow(new_nodes)) new_nodes$node_id else character())
    if (nid %in% used) nid <- paste0(nid, "_new")
    typ <- if(nzchar(f[2])) f[2] else "environmental_determinant"
    imp <- suppressWarnings(as.numeric(f[6])); if(is.na(imp)) imp <- 2
    nr <- data.frame(node_id=nid,node_name=nm,node_type=typ,sdoh_domain="",modifiable=TRUE,stringsAsFactors=FALSE)
    cr <- data.frame(node_id=nid,badge=f[3],description=f[4],why_it_matters=f[5],importance=imp,stringsAsFactors=FALSE)
    new_nodes <- rbind(new_nodes,nr); new_content <- rbind(new_content,cr)
    messages <- c(messages,paste0("ADD supporting node: ",nm," [",nid,"]"))
  }

  all_nodes <- rbind(nodes, new_nodes)
  source_id <- uh_source_id_for_candidate(candidate_row, sources)
  new_source <- if(source_id %in% sources$source_id) data.frame() else uh_make_source_row(candidate_row,source_id)

  rel_rows <- uh_cb_parse_pipe_rows(relationships_text, 5L)
  new_relationships <- data.frame(); new_rel_content <- data.frame(); new_rel_sources <- data.frame()
  next_existing <- as.character(relationships$relationship_id)
  for (f in rel_rows) {
    from_raw <- f[1]; to_raw <- f[2]
    from_id <- uh_cb_find_node(from_raw, all_nodes); to_id <- uh_cb_find_node(to_raw, all_nodes)
    # Allow central condition token even when newly created.
    if (is.na(from_id) && tolower(trimws(from_raw)) %in% c("condition",tolower(condition_name),tolower(condition_id))) from_id <- condition_id
    if (is.na(to_id) && tolower(trimws(to_raw)) %in% c("condition",tolower(condition_name),tolower(condition_id))) to_id <- condition_id
    if (is.na(from_id) || is.na(to_id)) stop(paste0("Could not resolve relationship endpoints: ",from_raw," → ",to_raw))
    if (from_id == to_id) stop("A relationship cannot point from a node to itself.")
    dup <- relationships$from_node == from_id & relationships$to_node == to_id
    if (any(dup)) { messages <- c(messages,paste0("SKIP existing relationship: ",from_id," → ",to_id)); next }
    rid <- uh_cb_next_rel_id(next_existing); next_existing <- c(next_existing,rid)
    rtype <- if(nzchar(f[3])) f[3] else "associated_with"
    estr <- if(nzchar(f[4])) tolower(f[4]) else "moderate"
    short <- f[5]
    new_relationships <- rbind(new_relationships,data.frame(relationship_id=rid,from_node=from_id,to_node=to_id,relationship_type=rtype,evidence_strength=estr,short_explanation=short,stringsAsFactors=FALSE))
    key <- paste0(from_id,"__",to_id)
    new_rel_content <- rbind(new_rel_content,data.frame(relationship_key=key,badge="CONDITION PACKAGE",primary_text=short,secondary_label="EVIDENCE REVIEW",secondary_text="This relationship was added through the admin condition-package workflow after source review.",stringsAsFactors=FALSE))
    new_rel_sources <- rbind(new_rel_sources,data.frame(relationship_id=rid,source_id=source_id,stringsAsFactors=FALSE))
    messages <- c(messages,paste0("ADD relationship: ",from_id," → ",to_id," (",rtype,", ",estr,")"))
  }

  # Basic package QA.
  issues <- data.frame(status=character(), item=character(), message=character(), stringsAsFactors=FALSE)
  add_issue <- function(status,item,message) issues <<- rbind(issues,data.frame(status=status,item=item,message=message,stringsAsFactors=FALSE))
  if (add_condition && !nzchar(trimws(condition_description))) add_issue("FAIL","Condition description","A new condition must have a description before publication.")
  if (add_condition && !nzchar(trimws(condition_why))) add_issue("FAIL","Why it matters","A new condition must explain why it matters before publication.")
  if (nrow(new_relationships)==0) add_issue("REVIEW","Relationships","The package currently adds/reuses the condition but contains no new relationships.")
  if (nrow(new_relationships)>0 && any(!nzchar(trimws(new_relationships$short_explanation)))) add_issue("FAIL","Relationship wording","Every relationship needs a careful short explanation.")
  if (nrow(new_relationships)>0 && any(!new_relationships$evidence_strength %in% c("weak","moderate","strong"))) add_issue("FAIL","Evidence strength","Use weak, moderate, or strong for evidence strength.")
  if (nrow(new_nodes)>0 && any(duplicated(c(nodes$node_id,new_nodes$node_id)))) add_issue("FAIL","Node IDs","A proposed node ID duplicates an existing node ID.")
  if (!nzchar(trimws(as.character(candidate_row$source_url[1])))) add_issue("FAIL","Source","The condition package must retain a source URL.")
  if (nrow(issues)==0) add_issue("PASS","Condition package","No blocking structural problems found. Scientific interpretation still requires admin review.")
  rank <- c(PASS=1,WARNING=2,REVIEW=3,FAIL=4)
  worst <- names(rank)[which.max(rank[issues$status])]

  list(condition_id=condition_id, add_condition=add_condition,
       new_nodes=new_nodes,new_content=new_content,new_relationships=new_relationships,
       new_rel_content=new_rel_content,new_rel_sources=new_rel_sources,new_source=new_source,
       source_id=source_id,messages=messages,qa=issues,worst=worst)
}



# ============================================================
# AUTOMATIC CONDITION-PACKAGE DRAFTING
# ============================================================
# This is deliberately conservative. It extracts a draft from the reviewed
# source and existing graph concepts, but it does not treat co-mention as
# proof of causality. The admin can accept, edit, or reject the whole draft.

uh_cb_sentence_split <- function(x) {
  x <- gsub("[\r\n]+", " ", as.character(x))
  x <- gsub("[[:space:]]+", " ", x)
  z <- unlist(strsplit(x, "(?<=[.!?])\\s+", perl=TRUE))
  trimws(z[nzchar(trimws(z))])
}

uh_cb_condition_dictionary <- function() {
  data.frame(
    canonical=c(
      "Chronic obstructive pulmonary disease (COPD)", "Major depressive disorder", "Depression",
      "Heart failure", "Coronary heart disease", "Chronic kidney disease", "Chronic liver disease",
      "Rheumatoid arthritis", "Osteoarthritis", "Migraine", "Epilepsy", "Dementia", "Alzheimer's disease",
      "Obstructive sleep apnea", "Atrial fibrillation", "Peripheral artery disease", "Cancer",
      "Colorectal cancer", "Lung cancer", "Breast cancer", "Prostate cancer", "Inflammatory bowel disease",
      "Crohn's disease", "Ulcerative colitis", "Endometriosis", "Polycystic ovary syndrome",
      "Nonalcoholic fatty liver disease", "Metabolic dysfunction-associated steatotic liver disease",
      "Osteoporosis", "Anxiety disorder"
    ),
    aliases=c(
      "copd|chronic obstructive pulmonary disease", "major depressive disorder|mdd", "depression|depressive disorder",
      "heart failure|congestive heart failure", "coronary heart disease|coronary artery disease", "chronic kidney disease|ckd", "chronic liver disease",
      "rheumatoid arthritis", "osteoarthritis", "migraine|migraine disorder", "epilepsy|seizure disorder", "dementia", "alzheimer's disease|alzheimer disease",
      "obstructive sleep apnea|sleep apnoea|sleep apnea", "atrial fibrillation|afib|a-fib", "peripheral artery disease|peripheral arterial disease", "cancer|malignancy",
      "colorectal cancer|colon cancer|rectal cancer", "lung cancer", "breast cancer", "prostate cancer", "inflammatory bowel disease|ibd",
      "crohn's disease|crohn disease", "ulcerative colitis", "endometriosis", "polycystic ovary syndrome|pcos",
      "nonalcoholic fatty liver disease|nafld", "metabolic dysfunction-associated steatotic liver disease|masld",
      "osteoporosis", "anxiety disorder|anxiety"
    ), stringsAsFactors=FALSE
  )
}

uh_cb_detect_condition_name <- function(candidate_row, abstract_text, nodes) {
  text <- tolower(paste(candidate_row$source_title[1], candidate_row$discovery_query_label[1], abstract_text))
  dict <- uh_cb_condition_dictionary()
  for (i in seq_len(nrow(dict))) {
    aliases <- trimws(unlist(strsplit(dict$aliases[i], "|", fixed=TRUE)))
    if (any(vapply(aliases, function(a) nzchar(a) && grepl(tolower(a), text, fixed=TRUE), logical(1)))) {
      existing <- uh_cb_find_node(dict$canonical[i], nodes)
      # Prefer conditions that are not already present; if present, still return it so the
      # admin can deliberately expand an existing condition instead of creating a duplicate.
      return(list(name=dict$canonical[i], confidence=if(is.na(existing)) "HIGH" else "MEDIUM", already_exists=!is.na(existing)))
    }
  }
  label <- trimws(as.character(candidate_row$discovery_query_label[1]))
  label <- sub("\\s*[—–-]\\s*.*$", "", label)
  if (nzchar(label) && nchar(label) <= 80) {
    return(list(name=label, confidence="LOW", already_exists=!is.na(uh_cb_find_node(label,nodes))))
  }
  list(name="", confidence="LOW", already_exists=FALSE)
}

uh_cb_extract_description <- function(condition_name, abstract_text, title) {
  s <- uh_cb_sentence_split(abstract_text)
  if (length(s)) {
    key <- tolower(gsub("\\s*\\([^)]*\\)", "", condition_name))
    hit <- s[grepl(key, tolower(s), fixed=TRUE)]
    if (length(hit)) return(substr(hit[1],1,700))
    return(substr(s[1],1,700))
  }
  paste0("Condition discussed in the reviewed trusted source: ", title, ". Verify or edit this definition before publication.")
}

uh_cb_extract_why <- function(condition_name, abstract_text) {
  s <- uh_cb_sentence_split(abstract_text)
  if (length(s)) {
    pat <- "burden|prevalence|mortality|morbidity|hospital|quality of life|disability|risk|complication|outcome"
    hit <- s[grepl(pat, tolower(s))]
    if (length(hit)) return(substr(hit[1],1,700))
  }
  paste0(condition_name, " was identified by the discovery system as potentially relevant to the Upstream Health network. Confirm the burden and public-health significance in the source before publication.")
}

uh_cb_auto_draft <- function(candidate_row, abstract_text, nodes) {
  det <- uh_cb_detect_condition_name(candidate_row, abstract_text, nodes)
  cname <- det$name
  hits <- trimws(as.character(candidate_row$existing_node_hits[1]))
  hit_ids <- if(nzchar(hits)) trimws(unlist(strsplit(hits,";",fixed=TRUE))) else character()
  hit_ids <- unique(hit_ids[hit_ids %in% nodes$node_id])
  if (nzchar(cname)) {
    central_existing <- uh_cb_find_node(cname,nodes)
    if (!is.na(central_existing)) hit_ids <- setdiff(hit_ids,central_existing)
  }
  rels <- if(nzchar(cname)) uh_cb_guess_relationship_rows("condition",hit_ids,nodes) else character()
  title <- as.character(candidate_row$source_title[1])
  list(
    condition_name=cname,
    condition_badge="DISEASE / CONDITION",
    condition_description=if(nzchar(cname)) uh_cb_extract_description(cname,abstract_text,title) else "",
    condition_why=if(nzchar(cname)) uh_cb_extract_why(cname,abstract_text) else "",
    condition_importance=5,
    supporting_nodes="",
    relationships=paste(rels,collapse="\n"),
    confidence=det$confidence,
    already_exists=det$already_exists,
    detected_existing_ids=hit_ids,
    note=if(nzchar(cname)) paste0("Automatic draft generated from the reviewed source. Condition detection confidence: ",det$confidence,
      if(det$already_exists) ". An existing condition node appears to match, so the package will reuse it rather than duplicate it." else ". A new condition node is proposed unless you edit the canonical name to match an existing node.")
    else "The source did not yield a confident condition name. Edit the draft before QA."
  )
}

uh_cb_commit <- function(preview, data_dir="data") {
  backup <- uh_backup_graph_files(data_dir)
  append_rows <- function(path, rows) {
    if (is.null(rows) || nrow(rows)==0) return()
    old <- utils::read.csv(path,stringsAsFactors=FALSE)
    # align columns defensively
    for (nm in setdiff(names(old),names(rows))) rows[[nm]] <- ""
    for (nm in setdiff(names(rows),names(old))) old[[nm]] <- ""
    rows <- rows[,names(old),drop=FALSE]
    utils::write.csv(rbind(old,rows),path,row.names=FALSE,na="")
  }
  append_rows(file.path(data_dir,"nodes.csv"),preview$new_nodes)
  append_rows(file.path(data_dir,"node_content.csv"),preview$new_content)
  append_rows(file.path(data_dir,"relationships.csv"),preview$new_relationships)
  append_rows(file.path(data_dir,"relationship_content.csv"),preview$new_rel_content)
  append_rows(file.path(data_dir,"relationship_sources.csv"),preview$new_rel_sources)
  append_rows(file.path(data_dir,"sources.csv"),preview$new_source)
  backup
}

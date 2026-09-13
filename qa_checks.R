# ============================================================
# UPSTREAM HEALTH QA CHECKER
# ============================================================
# This module intentionally uses base R only.
# It performs four types of checks:
#   1. Structural integrity
#   2. Source / citation quality signals
#   3. Scientific wording / evidence consistency
#   4. Live local-statistic sanity checks
#
# IMPORTANT:
# A QA "PASS" does not prove a scientific claim is true.
# Automated source checks identify suspicious patterns and items
# that deserve human review; they do not replace source reading.
# ============================================================

qa_empty <- function() {
  data.frame(
    status = character(),
    area = character(),
    item = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

qa_row <- function(status, area, item, message) {
  data.frame(
    status = as.character(status),
    area = as.character(area),
    item = as.character(item),
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

qa_bind <- function(...) {
  pieces <- list(...)
  pieces <- pieces[
    vapply(
      pieces,
      function(x) !is.null(x) && nrow(x) > 0,
      logical(1)
    )
  ]

  if (length(pieces) == 0) {
    return(qa_empty())
  }

  result <- do.call(rbind, pieces)
  rownames(result) <- NULL
  result
}

qa_status_rank <- function(status) {
  ranks <- c(
    "PASS" = 1,
    "WARNING" = 2,
    "REVIEW" = 3,
    "FAIL" = 4
  )

  out <- unname(ranks[status])
  out[is.na(out)] <- 3
  out
}

qa_worst_status <- function(report) {
  if (is.null(report) || nrow(report) == 0) {
    return("PASS")
  }

  report$status[
    which.max(
      qa_status_rank(report$status)
    )
  ]
}

qa_counts <- function(report) {
  statuses <- c("PASS", "WARNING", "REVIEW", "FAIL")
  counts <- setNames(rep(0L, length(statuses)), statuses)

  if (!is.null(report) && nrow(report) > 0) {
    tab <- table(report$status)
    counts[names(tab)] <- as.integer(tab)
  }

  counts
}

qa_domain <- function(url) {
  if (length(url) == 0 || is.na(url) || !nzchar(url)) {
    return("")
  }

  url <- tolower(trimws(url))
  domain <- sub("^https?://", "", url)
  domain <- sub("/.*$", "", domain)
  domain <- sub(":.*$", "", domain)
  sub("^www\\.", "", domain)
}

qa_domain_matches <- function(domain, allowed_domain) {
  if (
    is.na(domain) ||
    is.na(allowed_domain) ||
    !nzchar(domain) ||
    !nzchar(allowed_domain)
  ) {
    return(FALSE)
  }

  domain <- tolower(domain)
  allowed_domain <- tolower(allowed_domain)

  identical(domain, allowed_domain) ||
    endsWith(domain, paste0(".", allowed_domain))
}

qa_nonempty <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

qa_duplicate_rows <- function(x) {
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

qa_normalize_text <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

qa_numeric_from_display <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(NA_real_)
  }

  suppressWarnings(
    as.numeric(
      gsub("[^0-9.\\-]", "", x)
    )
  )
}

qa_rule_for_metric <- function(dataset, metric_key, qa_metric_rules) {
  if (is.null(qa_metric_rules) || nrow(qa_metric_rules) == 0) {
    return(NULL)
  }

  candidates <- qa_metric_rules[
    qa_metric_rules$dataset == dataset,
    ,
    drop = FALSE
  ]

  if (nrow(candidates) == 0) {
    return(NULL)
  }

  for (i in seq_len(nrow(candidates))) {
    pattern <- candidates$metric_key_pattern[i]

    if (
      !is.na(pattern) &&
      nzchar(pattern) &&
      grepl(pattern, metric_key, ignore.case = TRUE)
    ) {
      return(candidates[i, , drop = FALSE])
    }
  }

  NULL
}

qa_check_numeric_value <- function(
  value,
  dataset,
  metric_key,
  label,
  qa_metric_rules
) {
  rows <- qa_empty()

  if (is.na(value)) {
    return(
      qa_row(
        "REVIEW",
        "Live statistics",
        label,
        "The metric matched, but its numeric value could not be interpreted."
      )
    )
  }

  rule <- qa_rule_for_metric(
    dataset,
    metric_key,
    qa_metric_rules
  )

  if (is.null(rule)) {
    return(
      qa_row(
        "REVIEW",
        "Live statistics",
        label,
        paste0(
          "No QA sanity range is defined yet for this metric (",
          metric_key,
          ")."
        )
      )
    )
  }

  fail_min <- suppressWarnings(as.numeric(rule$fail_min[1]))
  fail_max <- suppressWarnings(as.numeric(rule$fail_max[1]))
  warn_min <- suppressWarnings(as.numeric(rule$warn_min[1]))
  warn_max <- suppressWarnings(as.numeric(rule$warn_max[1]))

  if (!is.na(fail_min) && value < fail_min) {
    return(
      qa_row(
        "FAIL",
        "Live statistics",
        label,
        paste0(
          "Value ", value,
          " is below the hard sanity limit of ", fail_min,
          ". Check the measure, units, and API response."
        )
      )
    )
  }

  if (!is.na(fail_max) && value > fail_max) {
    return(
      qa_row(
        "FAIL",
        "Live statistics",
        label,
        paste0(
          "Value ", value,
          " is above the hard sanity limit of ", fail_max,
          ". Check the measure, units, and API response."
        )
      )
    )
  }

  if (!is.na(warn_min) && value < warn_min) {
    return(
      qa_row(
        "REVIEW",
        "Live statistics",
        label,
        paste0(
          "Value ", value,
          " is unusually low relative to the QA review range (",
          warn_min, "–", warn_max,
          "). This may be real, but it deserves verification."
        )
      )
    )
  }

  if (!is.na(warn_max) && value > warn_max) {
    return(
      qa_row(
        "REVIEW",
        "Live statistics",
        label,
        paste0(
          "Value ", value,
          " is unusually high relative to the QA review range (",
          warn_min, "–", warn_max,
          "). This may be real, but it deserves verification."
        )
      )
    )
  }

  qa_row(
    "PASS",
    "Live statistics",
    label,
    paste0(
      "Value ", value,
      " is inside the configured sanity range."
    )
  )
}

# ============================================================
# STATIC / STARTUP QA
# ============================================================

run_static_qa <- function(
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
  report <- qa_empty()

  add <- function(status, area, item, message) {
    report <<- qa_bind(
      report,
      qa_row(status, area, item, message)
    )
  }

  # ----------------------------------------------------------
  # Nodes
  # ----------------------------------------------------------

  duplicate_node_ids <- unique(
    nodes$node_id[qa_duplicate_rows(nodes$node_id)]
  )

  if (length(duplicate_node_ids) == 0) {
    add("PASS", "Structure", "Node IDs", "No duplicate node IDs found.")
  } else {
    for (id in duplicate_node_ids) {
      add("FAIL", "Structure", id, "Duplicate node_id found in nodes.csv.")
    }
  }

  duplicate_node_names <- unique(
    nodes$node_name[qa_duplicate_rows(tolower(nodes$node_name))]
  )

  if (length(duplicate_node_names) == 0) {
    add("PASS", "Structure", "Node names", "No duplicate canonical node names found.")
  } else {
    for (name in duplicate_node_names) {
      add(
        "REVIEW",
        "Structure",
        name,
        "More than one node has the same canonical display name. Confirm these are not duplicate concepts."
      )
    }
  }

  missing_node_content <- setdiff(nodes$node_id, node_content$node_id)
  extra_node_content <- setdiff(node_content$node_id, nodes$node_id)

  if (length(missing_node_content) == 0) {
    add("PASS", "Content", "Node content", "Every node has a node_content.csv row.")
  } else {
    for (id in missing_node_content) {
      add("FAIL", "Content", id, "Node is missing from node_content.csv.")
    }
  }

  if (length(extra_node_content) > 0) {
    for (id in extra_node_content) {
      add("WARNING", "Content", id, "node_content.csv contains a node_id that does not exist in nodes.csv.")
    }
  }

  required_node_content <- c(
    "badge",
    "description",
    "why_it_matters",
    "importance"
  )

  for (column in required_node_content) {
    if (!column %in% names(node_content)) {
      add("FAIL", "Content", column, paste0("node_content.csv is missing required column '", column, "'."))
    } else {
      bad <- node_content$node_id[!qa_nonempty(node_content[[column]])]
      if (length(bad) > 0) {
        for (id in bad) {
          add("FAIL", "Content", id, paste0("Missing node content field: ", column, "."))
        }
      }
    }
  }

  # ----------------------------------------------------------
  # Relationships
  # ----------------------------------------------------------

  duplicate_rel_ids <- unique(
    relationships$relationship_id[
      qa_duplicate_rows(relationships$relationship_id)
    ]
  )

  if (length(duplicate_rel_ids) == 0) {
    add("PASS", "Structure", "Relationship IDs", "No duplicate relationship IDs found.")
  } else {
    for (id in duplicate_rel_ids) {
      add("FAIL", "Structure", id, "Duplicate relationship_id found.")
    }
  }

  bad_from <- setdiff(unique(relationships$from_node), nodes$node_id)
  bad_to <- setdiff(unique(relationships$to_node), nodes$node_id)

  if (length(bad_from) == 0 && length(bad_to) == 0) {
    add("PASS", "Structure", "Relationship endpoints", "Every relationship points to valid nodes.")
  }

  for (id in bad_from) {
    add("FAIL", "Structure", id, "Relationship from_node does not exist in nodes.csv.")
  }

  for (id in bad_to) {
    add("FAIL", "Structure", id, "Relationship to_node does not exist in nodes.csv.")
  }

  self_loops <- relationships[
    relationships$from_node == relationships$to_node,
    ,
    drop = FALSE
  ]

  if (nrow(self_loops) > 0) {
    for (i in seq_len(nrow(self_loops))) {
      add(
        "REVIEW",
        "Structure",
        self_loops$relationship_id[i],
        "Relationship points from a node back to itself. Confirm that this is intentional."
      )
    }
  }

  rel_pairs <- paste(
    relationships$from_node,
    relationships$to_node,
    sep = "__"
  )

  duplicate_pairs <- unique(rel_pairs[qa_duplicate_rows(rel_pairs)])

  if (length(duplicate_pairs) > 0) {
    for (pair in duplicate_pairs) {
      add(
        "REVIEW",
        "Structure",
        pair,
        "Multiple relationships use the same from_node → to_node pair. Confirm that they are not duplicates."
      )
    }
  }

  relationship_keys <- paste(
    relationships$from_node,
    relationships$to_node,
    sep = "__"
  )

  missing_rel_content <- setdiff(
    relationship_keys,
    relationship_content$relationship_key
  )

  if (length(missing_rel_content) == 0) {
    add("PASS", "Content", "Relationship explanations", "Every relationship has detailed explanatory content.")
  } else {
    for (key in missing_rel_content) {
      add("FAIL", "Content", key, "Relationship is missing from relationship_content.csv.")
    }
  }

  # ----------------------------------------------------------
  # Sources and citation integrity
  # ----------------------------------------------------------

  missing_relationship_sources <- setdiff(
    relationships$relationship_id,
    relationship_sources$relationship_id
  )

  if (length(missing_relationship_sources) == 0) {
    add("PASS", "Sources", "Relationship citations", "Every relationship has at least one source mapping.")
  } else {
    for (id in missing_relationship_sources) {
      add("FAIL", "Sources", id, "Relationship has no source mapping in relationship_sources.csv.")
    }
  }

  invalid_relationship_source_ids <- setdiff(
    unique(relationship_sources$source_id),
    sources$source_id
  )

  if (length(invalid_relationship_source_ids) == 0) {
    add("PASS", "Sources", "Source IDs", "All relationship source IDs resolve to sources.csv.")
  } else {
    for (id in invalid_relationship_source_ids) {
      add("FAIL", "Sources", id, "relationship_sources.csv points to a source_id that is missing from sources.csv.")
    }
  }

  invalid_intervention_sources <- setdiff(
    unique(interventions$source_id),
    sources$source_id
  )

  if (length(invalid_intervention_sources) == 0) {
    add("PASS", "Sources", "Intervention citations", "All intervention source IDs resolve to sources.csv.")
  } else {
    for (id in invalid_intervention_sources) {
      add("FAIL", "Sources", id, "An intervention points to a missing source_id.")
    }
  }

  invalid_intervention_targets <- setdiff(
    unique(interventions$target_node),
    nodes$node_id
  )

  if (length(invalid_intervention_targets) == 0) {
    add("PASS", "Structure", "Intervention targets", "Every intervention points to a valid node.")
  } else {
    for (id in invalid_intervention_targets) {
      add("FAIL", "Structure", id, "Intervention target_node does not exist in nodes.csv.")
    }
  }

  duplicate_source_ids <- unique(
    sources$source_id[qa_duplicate_rows(sources$source_id)]
  )

  if (length(duplicate_source_ids) > 0) {
    for (id in duplicate_source_ids) {
      add("FAIL", "Sources", id, "Duplicate source_id found in sources.csv.")
    }
  }

  for (i in seq_len(nrow(sources))) {
    source <- sources[i, , drop = FALSE]
    source_id <- source$source_id[1]
    org <- source$organization[1]
    url <- source$url[1]

    if (!qa_nonempty(url)) {
      add("FAIL", "Sources", source_id, "Source URL is missing.")
      next
    }

    if (!grepl("^https://", url, ignore.case = TRUE)) {
      add("WARNING", "Sources", source_id, "Source URL is not HTTPS.")
    }

    domain <- qa_domain(url)

    expected <- qa_source_domains[
      qa_source_domains$organization == org,
      ,
      drop = FALSE
    ]

    if (nrow(expected) == 0) {
      add(
        "REVIEW",
        "Sources",
        source_id,
        paste0(
          "Organization '", org,
          "' is not yet in qa_source_domains.csv. Review the source manually before treating it as authoritative."
        )
      )
    } else {
      allowed <- expected$allowed_domain
      matches <- vapply(
        allowed,
        function(x) qa_domain_matches(domain, x),
        logical(1)
      )

      if (!any(matches)) {
        add(
          "REVIEW",
          "Sources",
          source_id,
          paste0(
            "The URL domain '", domain,
            "' does not match the approved domain(s) for ", org,
            ". Verify that the citation is authentic and appropriate."
          )
        )
      }
    }

    if ("accessed_date" %in% names(sources)) {
      accessed <- suppressWarnings(as.Date(source$accessed_date[1]))

      if (is.na(accessed)) {
        add("WARNING", "Sources", source_id, "accessed_date is missing or not a valid date.")
      } else {
        age_days <- as.numeric(Sys.Date() - accessed)
        if (!is.na(age_days) && age_days > 730) {
          add(
            "REVIEW",
            "Sources",
            source_id,
            paste0("Source was last checked ", age_days, " days ago. Re-open it and confirm the page still supports the claim.")
          )
        }
      }
    }
  }

  # ----------------------------------------------------------
  # Scientific wording / evidence consistency
  # ----------------------------------------------------------

  allowed_evidence <- c("strong", "moderate", "limited", "emerging")
  invalid_evidence <- relationships[
    !tolower(relationships$evidence_strength) %in% allowed_evidence,
    ,
    drop = FALSE
  ]

  if (nrow(invalid_evidence) == 0) {
    add("PASS", "Scientific", "Evidence labels", "All relationship evidence labels use recognized QA categories.")
  } else {
    for (i in seq_len(nrow(invalid_evidence))) {
      add(
        "WARNING",
        "Scientific",
        invalid_evidence$relationship_id[i],
        paste0("Unrecognized evidence_strength: ", invalid_evidence$evidence_strength[i], ".")
      )
    }
  }

  hard_causal_types <- c("can_cause", "disease_process")
  caution_rows <- relationships[
    tolower(relationships$evidence_strength) %in% c("moderate", "limited", "emerging") &
      relationships$relationship_type %in% hard_causal_types,
    ,
    drop = FALSE
  ]

  if (nrow(caution_rows) > 0) {
    for (i in seq_len(nrow(caution_rows))) {
      add(
        "REVIEW",
        "Scientific",
        caution_rows$relationship_id[i],
        paste0(
          "Relationship type '", caution_rows$relationship_type[i],
          "' is relatively causal while evidence is labeled '",
          caution_rows$evidence_strength[i],
          "'. Confirm that the cited source supports this strength of wording."
        )
      )
    }
  }

  strong_words <- "\\b(causes|caused by|results in|directly causes|proves)\\b"
  moderate_rows <- relationships[
    tolower(relationships$evidence_strength) != "strong",
    ,
    drop = FALSE
  ]

  if (nrow(moderate_rows) > 0) {
    for (i in seq_len(nrow(moderate_rows))) {
      text <- paste(
        moderate_rows$short_explanation[i],
        collapse = " "
      )

      if (grepl(strong_words, text, ignore.case = TRUE)) {
        add(
          "REVIEW",
          "Scientific",
          moderate_rows$relationship_id[i],
          "The short explanation uses strong causal wording even though the evidence is not labeled strong. Review the wording against the cited source."
        )
      }
    }
  }

  # ----------------------------------------------------------
  # Local-metric mapping integrity
  # ----------------------------------------------------------

  invalid_mapping_nodes <- setdiff(
    unique(local_metric_mappings$node_id),
    nodes$node_id
  )

  if (length(invalid_mapping_nodes) == 0) {
    add("PASS", "Local data", "Metric mapping nodes", "Every local metric mapping points to a valid node.")
  } else {
    for (id in invalid_mapping_nodes) {
      add("FAIL", "Local data", id, "local_metric_mappings.csv references a node that does not exist.")
    }
  }

  invalid_note_nodes <- setdiff(
    unique(local_metric_notes$node_id),
    nodes$node_id
  )

  if (length(invalid_note_nodes) > 0) {
    for (id in invalid_note_nodes) {
      add("FAIL", "Local data", id, "local_metric_notes.csv references a node that does not exist.")
    }
  }

  valid_datasets <- c("cdc", "census")
  bad_datasets <- local_metric_mappings[
    !local_metric_mappings$dataset %in% valid_datasets,
    ,
    drop = FALSE
  ]

  if (nrow(bad_datasets) > 0) {
    for (i in seq_len(nrow(bad_datasets))) {
      add(
        "FAIL",
        "Local data",
        bad_datasets$node_id[i],
        paste0("Unknown local dataset: ", bad_datasets$dataset[i], ".")
      )
    }
  }

  valid_sections <- c("primary", "related")
  bad_sections <- local_metric_mappings[
    !local_metric_mappings$section %in% valid_sections,
    ,
    drop = FALSE
  ]

  if (nrow(bad_sections) > 0) {
    for (i in seq_len(nrow(bad_sections))) {
      add(
        "FAIL",
        "Local data",
        bad_sections$node_id[i],
        paste0("Unknown metric section: ", bad_sections$section[i], ".")
      )
    }
  }

  valid_formats <- c("percent", "dollars")
  bad_formats <- local_metric_mappings[
    !local_metric_mappings$value_format %in% valid_formats,
    ,
    drop = FALSE
  ]

  if (nrow(bad_formats) > 0) {
    for (i in seq_len(nrow(bad_formats))) {
      add(
        "FAIL",
        "Local data",
        bad_formats$node_id[i],
        paste0("Unknown value_format: ", bad_formats$value_format[i], ".")
      )
    }
  }

  mapping_keys <- paste(
    local_metric_mappings$node_id,
    local_metric_mappings$section,
    local_metric_mappings$dataset,
    local_metric_mappings$metric_key,
    sep = "__"
  )

  duplicate_mappings <- unique(
    mapping_keys[qa_duplicate_rows(mapping_keys)]
  )

  if (length(duplicate_mappings) > 0) {
    for (key in duplicate_mappings) {
      add("WARNING", "Local data", key, "Duplicate local metric mapping found.")
    }
  }

  census_keys <- c(
    "labor_force",
    "unemployment",
    "median_household_income",
    "snap",
    "uninsured",
    "poverty",
    "median_gross_rent",
    "renter_cost_burden"
  )

  census_mappings <- local_metric_mappings[
    local_metric_mappings$dataset == "census",
    ,
    drop = FALSE
  ]

  unknown_census <- census_mappings[
    !census_mappings$metric_key %in% census_keys,
    ,
    drop = FALSE
  ]

  if (nrow(unknown_census) == 0) {
    add("PASS", "Local data", "Census fields", "All Census metric keys exist in the connected ACS response mapping.")
  } else {
    for (i in seq_len(nrow(unknown_census))) {
      add(
        "FAIL",
        "Local data",
        unknown_census$node_id[i],
        paste0("Unknown Census metric_key: ", unknown_census$metric_key[i], ".")
      )
    }
  }

  # Every metric should have a sanity rule.
  for (i in seq_len(nrow(local_metric_mappings))) {
    mapping <- local_metric_mappings[i, , drop = FALSE]
    rule <- qa_rule_for_metric(
      mapping$dataset[1],
      mapping$metric_key[1],
      qa_metric_rules
    )

    if (is.null(rule)) {
      add(
        "REVIEW",
        "Local data",
        mapping$node_id[1],
        paste0(
          "No QA range exists yet for ",
          mapping$dataset[1], " metric '",
          mapping$metric_key[1], "'."
        )
      )
    }
  }

  # If a node has related metrics but no primary direct metric, require
  # a caveat explaining that the displayed values are contextual/proxy.
  for (id in nodes$node_id) {
    maps <- local_metric_mappings[
      local_metric_mappings$node_id == id,
      ,
      drop = FALSE
    ]

    has_primary <- any(maps$section == "primary")
    has_note <- id %in% local_metric_notes$node_id

    if (!has_primary && !has_note) {
      add(
        "REVIEW",
        "Local data",
        id,
        "This node has no direct primary local metric and no caveat explaining the limitation. Add a local_metric_notes.csv entry."
      )
    }
  }

  # Add a positive summary line only if there are no serious issues.
  serious <- report$status %in% c("FAIL", "REVIEW")

  if (!any(serious)) {
    add(
      "PASS",
      "Overall",
      "Static QA",
      "No structural, source-integrity, scientific-wording, or mapping issues were detected by the automated checker."
    )
  }

  report <- report[
    order(
      -qa_status_rank(report$status),
      report$area,
      report$item
    ),
    ,
    drop = FALSE
  ]

  rownames(report) <- NULL
  report
}

# ============================================================
# LIVE LOCAL-DATA QA
# ============================================================

run_live_qa <- function(
  node_id,
  cdc_result,
  census_result,
  local_metric_mappings,
  qa_metric_rules
) {
  report <- qa_empty()

  add <- function(status, area, item, message) {
    report <<- qa_bind(
      report,
      qa_row(status, area, item, message)
    )
  }

  mappings <- local_metric_mappings[
    local_metric_mappings$node_id == node_id,
    ,
    drop = FALSE
  ]

  if (nrow(mappings) == 0) {
    add(
      "PASS",
      "Live statistics",
      node_id,
      "This node intentionally has no configured local metric mapping."
    )
    return(report)
  }

  for (i in seq_len(nrow(mappings))) {
    mapping <- mappings[i, , drop = FALSE]
    dataset <- mapping$dataset[1]
    metric_key <- mapping$metric_key[1]
    label <- mapping$label[1]

    if (dataset == "cdc") {
      if (!isTRUE(cdc_result$ok)) {
        add(
          "WARNING",
          "Live statistics",
          label,
          "CDC PLACES was unavailable, so this metric could not be QA-checked."
        )
        next
      }

      cdc <- cdc_result$data

      if (!"measure" %in% names(cdc)) {
        add(
          "FAIL",
          "Live statistics",
          label,
          "CDC response does not contain a 'measure' field."
        )
        next
      }

      matches <- which(
        grepl(
          metric_key,
          cdc$measure,
          ignore.case = TRUE
        )
      )

      if (length(matches) == 0) {
        add(
          "FAIL",
          "Live statistics",
          label,
          paste0(
            "Configured CDC metric pattern '", metric_key,
            "' matched no returned measure. The app may be showing no data because the measure name changed."
          )
        )
        next
      }

      if (length(matches) > 1) {
        add(
          "REVIEW",
          "Live statistics",
          label,
          paste0(
            "Configured CDC pattern '", metric_key,
            "' matched ", length(matches),
            " measures. The app currently uses the first match, so verify that the pattern is specific enough. Matches: ",
            paste(head(cdc$measure[matches], 4), collapse = " | ")
          )
        )
      } else {
        add(
          "PASS",
          "Live statistics",
          label,
          paste0(
            "CDC metric mapping matched exactly one returned measure: '",
            cdc$measure[matches[1]], "'."
          )
        )
      }

      row <- cdc[matches[1], , drop = FALSE]
      value <- suppressWarnings(as.numeric(row$data_value[1]))

      report <- qa_bind(
        report,
        qa_check_numeric_value(
          value,
          "cdc",
          metric_key,
          label,
          qa_metric_rules
        )
      )

      if (
        "low_confidence_limit" %in% names(row) &&
        "high_confidence_limit" %in% names(row)
      ) {
        low <- suppressWarnings(as.numeric(row$low_confidence_limit[1]))
        high <- suppressWarnings(as.numeric(row$high_confidence_limit[1]))

        if (
          !is.na(low) &&
          !is.na(high) &&
          !is.na(value)
        ) {
          if (low > high || value < low || value > high) {
            add(
              "FAIL",
              "Live statistics",
              label,
              paste0(
                "CDC confidence interval is internally inconsistent (",
                low, "–", high,
                " with estimate ", value, ")."
              )
            )
          } else {
            width <- high - low

            if (width > 25) {
              add(
                "REVIEW",
                "Live statistics",
                label,
                paste0(
                  "The CDC confidence interval is wide (",
                  round(width, 1),
                  " percentage points). Treat this local estimate cautiously."
                )
              )
            }
          }
        }
      }

    } else if (dataset == "census") {
      if (!isTRUE(census_result$ok)) {
        add(
          "WARNING",
          "Live statistics",
          label,
          "Census ACS was unavailable, so this metric could not be QA-checked."
        )
        next
      }

      census <- census_result$data

      if (!metric_key %in% names(census)) {
        add(
          "FAIL",
          "Live statistics",
          label,
          paste0(
            "Configured Census metric_key '", metric_key,
            "' was not present in the parsed ACS response."
          )
        )
        next
      }

      value <- suppressWarnings(as.numeric(census[[metric_key]]))

      report <- qa_bind(
        report,
        qa_check_numeric_value(
          value,
          "census",
          metric_key,
          label,
          qa_metric_rules
        )
      )
    }
  }

  report <- report[
    order(
      -qa_status_rank(report$status),
      report$item
    ),
    ,
    drop = FALSE
  ]

  rownames(report) <- NULL
  report
}

library(shiny)
library(visNetwork)
library(jsonlite)


# ============================================================
# LOAD DATA
# ============================================================

nodes <- read.csv(
  "data/nodes.csv",
  stringsAsFactors = FALSE
)

node_flow_stages <- read.csv(
  "data/node_flow_stages.csv",
  stringsAsFactors = FALSE
)

relationships <- read.csv(
  "data/relationships.csv",
  stringsAsFactors = FALSE
)

sources <- read.csv(
  "data/sources.csv",
  stringsAsFactors = FALSE
)

relationship_sources <- read.csv(
  "data/relationship_sources.csv",
  stringsAsFactors = FALSE
)

interventions <- read.csv(
  "data/interventions.csv",
  stringsAsFactors = FALSE
)

node_content <- read.csv(
  "data/node_content.csv",
  stringsAsFactors = FALSE
)

relationship_content <- read.csv(
  "data/relationship_content.csv",
  stringsAsFactors = FALSE
)

local_metric_mappings <- read.csv(
  "data/local_metric_mappings.csv",
  stringsAsFactors = FALSE
)

local_metric_notes <- read.csv(
  "data/local_metric_notes.csv",
  stringsAsFactors = FALSE
)

qa_source_domains <- read.csv(
  "data/qa_source_domains.csv",
  stringsAsFactors = FALSE
)

qa_metric_rules <- read.csv(
  "data/qa_metric_rules.csv",
  stringsAsFactors = FALSE
)

discovery_watchlist <- read.csv(
  "data/discovery_watchlist.csv",
  stringsAsFactors = FALSE
)

discovery_candidates <- read.csv(
  "data/discovery_candidates.csv",
  stringsAsFactors = FALSE
)

expansion_condition_catalog <- read.csv(
  "data/expansion_condition_catalog.csv",
  stringsAsFactors = FALSE
)

auto_condition_queue <- read.csv(
  "data/auto_condition_queue.csv",
  stringsAsFactors = FALSE
)

# ============================================================
# EDITABLE SITE CONTENT + FEEDBACK STORAGE
# ============================================================

SITE_CONTENT_PATH <- "data/site_content.csv"
FEEDBACK_PATH <- "data/feedback_submissions.csv"

default_site_content <- data.frame(
  key = c(
    "about_intro",
    "about_sdoh",
    "about_motivation",
    "about_project",
    "about_closing"
  ),
  value = c(
    "Hi! My name is Julian. I’m a high school junior from Portland, Maine, with an interest in public health.",
    "I was recently introduced to the Social Determinants of Health — the non-medical factors that shape health, including the conditions in which people are born, grow, work, live, and age.",
    "Learning about them made me think about how much of the U.S. healthcare system focuses on treating illness after it appears, rather than the upstream conditions that can influence health long before someone reaches a doctor’s office.",
    "That idea led to this project. I wanted to create a way to show how everyday experiences, environments, behaviors, and health conditions connect, while also offering people a starting point for understanding possible solutions and next steps toward care.",
    "I hope you enjoy this project as much as I did making it (which is a lot)."
  ),
  stringsAsFactors = FALSE
)

read_site_content <- function() {
  if (!file.exists(SITE_CONTENT_PATH)) {
    return(default_site_content)
  }
  out <- tryCatch(
    read.csv(SITE_CONTENT_PATH, stringsAsFactors = FALSE),
    error = function(e) default_site_content
  )
  if (!all(c("key", "value") %in% names(out))) {
    return(default_site_content)
  }
  missing_keys <- setdiff(default_site_content$key, out$key)
  if (length(missing_keys) > 0) {
    out <- rbind(
      out,
      default_site_content[default_site_content$key %in% missing_keys, , drop = FALSE]
    )
  }
  out
}

site_content <- read_site_content()

if (file.exists(FEEDBACK_PATH)) {
  feedback_submissions <- tryCatch(
    read.csv(FEEDBACK_PATH, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
} else {
  feedback_submissions <- data.frame(
    submitted_at = character(),
    name = character(),
    email = character(),
    category = character(),
    message = character(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

source("qa_checks.R")
source("discovery_inbox.R")
source("publish_changes.R")
source("condition_builder.R")
AUTO_EXPANSION_AVAILABLE <- file.exists("autonomous_expansion.R")

if (AUTO_EXPANSION_AVAILABLE) {
  source("autonomous_expansion.R")
}

# The paid/model-backed autonomous expansion helper is OPTIONAL.
# If it is not installed, the rest of The Health Web still runs normally.
if (!AUTO_EXPANSION_AVAILABLE) {
  uh_auto_ensure_queue <- function(x) x
}

discovery_watchlist <- ensure_watchlist_columns(discovery_watchlist)
discovery_candidates <- ensure_candidate_columns(discovery_candidates)
auto_condition_queue <- uh_auto_ensure_queue(auto_condition_queue)


# ============================================================
# STARTUP QA
# ============================================================

static_qa_report <- run_static_qa(
  nodes = nodes,
  node_content = node_content,
  relationships = relationships,
  relationship_content = relationship_content,
  relationship_sources = relationship_sources,
  sources = sources,
  interventions = interventions,
  local_metric_mappings = local_metric_mappings,
  local_metric_notes = local_metric_notes,
  qa_source_domains = qa_source_domains,
  qa_metric_rules = qa_metric_rules
)


# ============================================================
# API KEY
# ============================================================

CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY")

# Internal QA/admin controls are hidden unless this environment
# variable is explicitly enabled. For local development, add
# UPSTREAM_ADMIN_MODE=true to .Renviron and restart R.
UPSTREAM_PUBLIC_DEPLOYMENT <-
  tolower(trimws(Sys.getenv("UPSTREAM_PUBLIC_DEPLOYMENT", "false"))) %in%
  c("true", "1", "yes", "on")

# Safety guard: a public deployment can never expose the local/admin tools,
# even if UPSTREAM_ADMIN_MODE is accidentally left enabled in the host settings.
UPSTREAM_ADMIN_MODE <-
  !UPSTREAM_PUBLIC_DEPLOYMENT &&
  tolower(trimws(Sys.getenv("UPSTREAM_ADMIN_MODE", "false"))) %in%
  c("true", "1", "yes", "on")

# Optional durable feedback endpoint (for example, a Formspree-style HTTPS
# endpoint). Public deployments should configure this rather than relying on
# the local CSV, which may not persist across cloud restarts/redeploys.
UPSTREAM_FEEDBACK_ENDPOINT <- trimws(Sys.getenv("UPSTREAM_FEEDBACK_ENDPOINT", ""))

# Optional background discovery while the app process is running.
# Keep this FALSE until you intentionally enable it in .Renviron.
UPSTREAM_DISCOVERY_AUTO_SCAN <-
  tolower(trimws(Sys.getenv("UPSTREAM_DISCOVERY_AUTO_SCAN", "false"))) %in%
  c("true", "1", "yes", "on")

# Admin-only autonomous condition expansion. It defaults ON in admin mode.
# Set UPSTREAM_AUTO_EXPANSION=false in .Renviron if you want to pause it.
UPSTREAM_AUTO_EXPANSION <-
  AUTO_EXPANSION_AVAILABLE &&
  UPSTREAM_ADMIN_MODE &&
  tolower(trimws(Sys.getenv("UPSTREAM_AUTO_EXPANSION", "false"))) %in%
  c("true", "1", "yes", "on")


# ============================================================
# LIVE DATA SETTINGS
# ============================================================

CDC_PLACES_API <-
  "https://data.cdc.gov/resource/qnzd-25i4.json"

CDC_PLACES_PAGE <-
  "https://data.cdc.gov/d/qnzd-25i4"

CENSUS_ACS_API <-
  "https://api.census.gov/data/2024/acs/acs5/profile"

CENSUS_ACS_PAGE <-
  "https://www.census.gov/programs-surveys/acs"

# Small in-memory cache for public API results. This keeps repeated ZIP lookups
# fast and reduces unnecessary requests to CDC/Census/Zippopotam. The cache is
# intentionally process-local and can be safely rebuilt after a restart.
UPSTREAM_CACHE_TTL_SECONDS <- suppressWarnings(
  as.numeric(Sys.getenv("UPSTREAM_CACHE_TTL_SECONDS", "21600"))
)
if (is.na(UPSTREAM_CACHE_TTL_SECONDS) || UPSTREAM_CACHE_TTL_SECONDS < 60) {
  UPSTREAM_CACHE_TTL_SECONDS <- 21600
}

.upstream_api_cache <- new.env(parent = emptyenv())

cache_get <- function(key) {
  item <- get0(key, envir = .upstream_api_cache, inherits = FALSE)
  if (is.null(item) || is.null(item$saved_at) || is.null(item$value)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), item$saved_at, units = "secs"))
  if (!is.finite(age) || age > UPSTREAM_CACHE_TTL_SECONDS) {
    rm(list = key, envir = .upstream_api_cache)
    return(NULL)
  }
  item$value
}

cache_set <- function(key, value) {
  assign(key, list(saved_at = Sys.time(), value = value), envir = .upstream_api_cache)
  invisible(value)
}


# ============================================================
# GENERAL HELPERS
# ============================================================

safe_numeric <- function(x) {
  
  value <- suppressWarnings(as.numeric(x))
  
  if (
    length(value) == 0 ||
    is.na(value) ||
    value < -1000
  ) {
    return(NA_real_)
  }
  
  value
}




# ============================================================
# ZIP -> PLACE LOOKUP
#
# Uses the free Zippopotam.us postal-code service. No API key
# is required. This is only for a friendly city/state label;
# CDC PLACES and Census remain the sources for health/context data.
# ============================================================

lookup_zip_place <- function(zip_code) {
  cache_key <- paste0("zip_place:", zip_code)
  cached <- cache_get(cache_key)
  if (!is.null(cached)) return(cached)

  url <- paste0("https://api.zippopotam.us/us/", zip_code)

  result_out <- tryCatch({
    result <- jsonlite::fromJSON(url)

    if (is.null(result$places) || nrow(result$places) == 0) {
      return(list(ok = FALSE, message = "No place name found for this ZIP."))
    }

    place <- result$places[1, , drop = FALSE]

    list(
      ok = TRUE,
      place_name = as.character(place[["place name"]][1]),
      state = as.character(place[["state"]][1]),
      state_abbr = as.character(place[["state abbreviation"]][1]),
      latitude = suppressWarnings(as.numeric(place[["latitude"]][1])),
      longitude = suppressWarnings(as.numeric(place[["longitude"]][1])),
      source = "Zippopotam.us / GeoNames"
    )
  }, error = function(e) {
    list(ok = FALSE, message = "Place-name lookup is temporarily unavailable.")
  })

  if (isTRUE(result_out$ok)) cache_set(cache_key, result_out)
  result_out
}


# ============================================================
# CDC PLACES
# ============================================================

fetch_cdc_places <- function(zip_code) {
  cache_key <- paste0("cdc_places:", zip_code)
  cached <- cache_get(cache_key)
  if (!is.null(cached)) return(cached)
  
  where_clause <-
    paste0(
      "locationname='",
      zip_code,
      "'"
    )
  
  api_url <-
    paste0(
      CDC_PLACES_API,
      "?$limit=100&$where=",
      URLencode(
        where_clause,
        reserved = TRUE
      )
    )
  
  result_out <- tryCatch(
    
    {
      
      result <-
        jsonlite::fromJSON(
          api_url
        )
      
      if (
        is.null(result) ||
        nrow(result) == 0
      ) {
        
        return(
          list(
            ok = FALSE,
            message =
              "CDC PLACES did not return data for this ZIP/ZCTA."
          )
        )
      }
      
      numeric_columns <-
        c(
          "data_value",
          "low_confidence_limit",
          "high_confidence_limit"
        )
      
      for (column_name in numeric_columns) {
        
        if (column_name %in% names(result)) {
          
          result[[column_name]] <-
            suppressWarnings(
              as.numeric(
                result[[column_name]]
              )
            )
        }
      }
      
      list(
        ok = TRUE,
        data = result
      )
    },
    
    error = function(e) {
      
      list(
        ok = FALSE,
        message =
          paste(
            "CDC PLACES could not be reached:",
            conditionMessage(e)
          )
      )
    }
  )

  if (isTRUE(result_out$ok)) cache_set(cache_key, result_out)
  result_out
}


# ============================================================
# CENSUS ACS
# ============================================================

fetch_census_acs <- function(zip_code) {
  cache_key <- paste0("census_acs:", zip_code)
  cached <- cache_get(cache_key)
  if (!is.null(cached)) return(cached)
  
  if (!nzchar(CENSUS_API_KEY)) {
    
    return(
      list(
        ok = FALSE,
        message =
          "Census context is temporarily unavailable because this deployment is missing its Census API configuration."
      )
    )
  }
  
  variables <-
    c(
      "NAME",
      "DP03_0003PE",
      "DP03_0009PE",
      "DP03_0062E",
      "DP03_0074PE",
      "DP03_0099PE",
      "DP03_0128PE",
      "DP04_0134E",
      "DP04_0141PE",
      "DP04_0142PE"
    )
  
  api_url <-
    paste0(
      CENSUS_ACS_API,
      "?get=",
      paste(
        variables,
        collapse = ","
      ),
      "&for=zip%20code%20tabulation%20area:",
      zip_code,
      "&key=",
      URLencode(
        CENSUS_API_KEY,
        reserved = TRUE
      )
    )
  
  result_out <- tryCatch(
    
    {
      
      raw <-
        jsonlite::fromJSON(
          api_url
        )
      
      if (
        is.null(raw) ||
        nrow(raw) < 2
      ) {
        
        return(
          list(
            ok = FALSE,
            message =
              "Census ACS did not return data for this ZIP Code Tabulation Area."
          )
        )
      }
      
      headers <- as.character(raw[1, ])
      values <- as.character(raw[2, ])
      
      row <- as.list(values)
      names(row) <- headers
      
      rent_30_34 <-
        safe_numeric(
          row[["DP04_0141PE"]]
        )
      
      rent_35_plus <-
        safe_numeric(
          row[["DP04_0142PE"]]
        )
      
      rent_burden <- NA_real_
      
      if (
        !is.na(rent_30_34) &&
        !is.na(rent_35_plus)
      ) {
        
        rent_burden <-
          rent_30_34 +
          rent_35_plus
      }
      
      data <- list(
        
        place_name =
          row[["NAME"]],
        
        labor_force =
          safe_numeric(
            row[["DP03_0003PE"]]
          ),
        
        unemployment =
          safe_numeric(
            row[["DP03_0009PE"]]
          ),
        
        median_household_income =
          safe_numeric(
            row[["DP03_0062E"]]
          ),
        
        snap =
          safe_numeric(
            row[["DP03_0074PE"]]
          ),
        
        uninsured =
          safe_numeric(
            row[["DP03_0099PE"]]
          ),
        
        poverty =
          safe_numeric(
            row[["DP03_0128PE"]]
          ),
        
        median_gross_rent =
          safe_numeric(
            row[["DP04_0134E"]]
          ),
        
        renter_cost_burden =
          rent_burden
      )
      
      list(
        ok = TRUE,
        data = data
      )
    },
    
    error = function(e) {
      
      list(
        ok = FALSE,
        message =
          paste(
            "U.S. Census ACS could not be retrieved:",
            conditionMessage(e)
          )
      )
    }
  )

  if (isTRUE(result_out$ok)) cache_set(cache_key, result_out)
  result_out
}


# ============================================================
# LOCAL METRIC HELPERS
# ============================================================

empty_metric_table <- function() {
  
  data.frame(
    label = character(),
    value = character(),
    source = character(),
    year = character(),
    note = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
}


make_metric <- function(
    label,
    value,
    source,
    year,
    note = "",
    url = ""
) {
  
  if (
    length(value) == 0 ||
    is.null(value) ||
    is.na(value)
  ) {
    
    return(
      empty_metric_table()
    )
  }
  
  data.frame(
    label = label,
    value = as.character(value),
    source = source,
    year = year,
    note = note,
    url = url,
    stringsAsFactors = FALSE
  )
}


format_percent <- function(value) {
  
  if (is.na(value)) {
    return(NA_character_)
  }
  
  paste0(
    round(value, 1),
    "%"
  )
}


format_dollars <- function(value) {
  
  if (is.na(value)) {
    return(NA_character_)
  }
  
  paste0(
    "$",
    format(
      round(value),
      big.mark = ",",
      scientific = FALSE
    )
  )
}


find_cdc_row <- function(
    data,
    pattern
) {
  
  if (
    is.null(data) ||
    nrow(data) == 0 ||
    !"measure" %in% names(data)
  ) {
    return(NULL)
  }
  
  matches <-
    which(
      grepl(
        pattern,
        data$measure,
        ignore.case = TRUE
      )
    )
  
  if (length(matches) == 0) {
    return(NULL)
  }
  
  data[
    matches[1],
    ,
    drop = FALSE
  ]
}


cdc_metric <- function(
    data,
    pattern,
    label = NULL,
    note = ""
) {
  
  row <-
    find_cdc_row(
      data,
      pattern
    )
  
  if (is.null(row)) {
    return(
      empty_metric_table()
    )
  }
  
  if (is.null(label)) {
    label <- row$measure[1]
  }
  
  make_metric(
    
    label = label,
    
    value =
      paste0(
        row$data_value[1],
        "%"
      ),
    
    source =
      "CDC PLACES",
    
    year =
      if ("year" %in% names(row)) {
        as.character(row$year[1])
      } else {
        ""
      },
    
    note = note,
    
    url =
      CDC_PLACES_PAGE
  )
}


bind_metrics <- function(...) {
  
  pieces <- list(...)
  
  pieces <-
    pieces[
      vapply(
        pieces,
        function(x) {
          !is.null(x) &&
            nrow(x) > 0
        },
        logical(1)
      )
    ]
  
  if (length(pieces) == 0) {
    
    return(
      empty_metric_table()
    )
  }
  
  result <-
    do.call(
      rbind,
      pieces
    )
  
  rownames(result) <- NULL
  
  result
}


# ============================================================
# LOCAL CONTEXT
#
# Data-driven: node-to-metric mappings live in
# data/local_metric_mappings.csv.
# Caveats / "no direct measure" notes live in
# data/local_metric_notes.csv.
# ============================================================

build_local_context <- function(
  node_id,
  cdc_result,
  census_result
) {

  cdc <-
    if (isTRUE(cdc_result$ok)) {
      cdc_result$data
    } else {
      NULL
    }

  census <-
    if (isTRUE(census_result$ok)) {
      census_result$data
    } else {
      NULL
    }

  primary <- empty_metric_table()
  related <- empty_metric_table()

  note_row <-
    local_metric_notes[
      local_metric_notes$node_id == node_id,
      ,
      drop = FALSE
    ]

  direct_message <-
    if (nrow(note_row) > 0) {
      note_row$direct_message[1]
    } else {
      NULL
    }

  mappings <-
    local_metric_mappings[
      local_metric_mappings$node_id == node_id,
      ,
      drop = FALSE
    ]

  if (nrow(mappings) > 0) {

    for (i in seq_len(nrow(mappings))) {

      mapping <- mappings[i, ]

      metric <- empty_metric_table()

      if (mapping$dataset == "cdc") {

        metric <-
          cdc_metric(
            cdc,
            mapping$metric_key,
            mapping$label,
            mapping$note
          )

      } else if (
        mapping$dataset == "census" &&
        !is.null(census)
      ) {

        raw_value <-
          census[[mapping$metric_key]]

        formatted_value <-
          if (mapping$value_format == "dollars") {
            format_dollars(raw_value)
          } else {
            format_percent(raw_value)
          }

        metric <-
          make_metric(
            mapping$label,
            formatted_value,
            "U.S. Census ACS",
            "2024 ACS 5-year",
            mapping$note,
            CENSUS_ACS_PAGE
          )
      }

      if (mapping$section == "primary") {

        primary <-
          bind_metrics(
            primary,
            metric
          )

      } else {

        related <-
          bind_metrics(
            related,
            metric
          )
      }
    }
  }

  list(
    primary = primary,
    related = related,
    direct_message = direct_message
  )
}


# ============================================================
# INTERVENTION HELPERS
# ============================================================

get_display_level <- function(x) {
  
  x <- tolower(x)
  
  if (grepl("policy|structural", x)) {
    return("Policy / Structural")
  }
  
  if (grepl("healthcare", x)) {
    return("Healthcare")
  }
  
  if (grepl("community", x)) {
    return("Community")
  }
  
  if (grepl("individual", x)) {
    return("Individual")
  }
  
  "Other"
}


interventions$display_level <-
  vapply(
    interventions$intervention_level,
    get_display_level,
    character(1)
  )


intervention_level_order <-
  c(
    "Individual",
    "Healthcare",
    "Community",
    "Policy / Structural",
    "Other"
  )


get_intervention_heading <- function(node_type) {
  
  if (node_type == "symptom") {
    return("What can be done?")
  }
  
  if (node_type == "complication") {
    return("Management & prevention")
  }
  
  "What can be changed?"
}


get_intervention_intro <- function(node_type) {
  
  if (node_type == "symptom") {
    
    return(
      paste(
        "Symptoms often need evaluation of the underlying cause.",
        "These actions are not a diagnosis and are not a substitute",
        "for individualized medical care."
      )
    )
  }
  
  if (node_type == "complication") {
    
    return(
      paste(
        "Strategies can help prevent progression, reduce risk,",
        "or manage complications. Clinical decisions should be",
        "individualized with a health care professional."
      )
    )
  }
  
  paste(
    "Potential strategies are grouped",
    "by the level where action can occur."
  )
}


# ============================================================
# NODE CONTENT
#
# Definitions, badges, and "why it matters" text now live in
# data/node_content.csv instead of app.R.
# ============================================================

node_order <- nodes$node_id

nodes <-
  merge(
    nodes,
    node_content,
    by = "node_id",
    all.x = TRUE,
    sort = FALSE
  )

nodes <-
  nodes[
    match(
      node_order,
      nodes$node_id
    ),
  ]


# ============================================================
# NODE IMPORTANCE
#
# Importance now lives with node content in data/node_content.csv.
# New nodes therefore do not require app.R edits just to change
# their visual prominence.
# ============================================================

nodes$importance <-
  suppressWarnings(
    as.numeric(
      nodes$importance
    )
  )

nodes$importance[
  is.na(nodes$importance)
] <- 2


# ============================================================
# NETWORK LEVELS
# ============================================================

nodes$level <- NA_real_

nodes$level[
  nodes$node_type == "structural_determinant"
] <- -4

nodes$level[
  nodes$node_type %in%
    c(
      "social_determinant",
      "environmental_determinant"
    )
] <- -3

nodes$level[
  nodes$node_type == "behavioral_pathway"
] <- -2

nodes$level[
  nodes$node_type %in%
    c(
      "behavioral_biological_pathway",
      "biological_pathway"
    )
] <- -1

nodes$level[
  nodes$node_type == "disease"
] <- 0

nodes$level[
  nodes$node_type == "symptom"
] <- 1

nodes$level[
  nodes$node_type == "complication"
] <- 2


# ============================================================
# GENERIC GRAPH HELPERS
#
# These are the important new pieces.
#
# They do not know or care that this graph is currently
# about Type 2 diabetes.
# ============================================================

get_neighbors <- function(node_ids) {
  
  unique(
    c(
      relationships$to_node[
        relationships$from_node %in%
          node_ids
      ],
      relationships$from_node[
        relationships$to_node %in%
          node_ids
      ]
    )
  )
}


get_neighborhood <- function(
    center_id,
    degrees = 2
) {
  
  visible <- center_id
  frontier <- center_id
  
  if (degrees <= 0) {
    return(visible)
  }
  
  for (step in seq_len(degrees)) {
    
    next_nodes <-
      setdiff(
        get_neighbors(frontier),
        visible
      )
    
    if (length(next_nodes) == 0) {
      break
    }
    
    visible <-
      unique(
        c(
          visible,
          next_nodes
        )
      )
    
    frontier <- next_nodes
  }
  
  visible
}


# ============================================================
# NODE HOVER CARDS
# ============================================================

node_tooltips <-
  paste0(
    
    "<div class='network-tooltip-card node-tooltip-card'>",
    
    "<div class='node-tooltip-badge'>",
    nodes$badge,
    "</div>",
    
    "<div class='node-tooltip-title'>",
    nodes$node_name,
    "</div>",
    
    "<div class='node-tooltip-description'>",
    nodes$description,
    "</div>",
    
    "<div class='node-tooltip-footer'>",
    "Click to explore →",
    "</div>",
    
    "</div>"
  )


# ============================================================
# NODE COLORS
# ============================================================

node_style <- data.frame(
  
  node_type = c(
    "structural_determinant",
    "social_determinant",
    "environmental_determinant",
    "behavioral_pathway",
    "behavioral_biological_pathway",
    "biological_pathway",
    "disease",
    "symptom",
    "complication"
  ),
  
  background = c(
    "#15242C",
    "#162824",
    "#17281C",
    "#292419",
    "#302A1A",
    "#2D291D",
    "#E8EEF1",
    "#23232C",
    "#2A2025"
  ),
  
  border = c(
    "#426678",
    "#426B60",
    "#4B704F",
    "#706143",
    "#827347",
    "#776B4E",
    "rgba(230,238,241,0.78)",
    "#5B5B70",
    "#72505D"
  ),
  
  hover_background = c(
    "#1E3440",
    "#203A34",
    "#213A28",
    "#3B3424",
    "#433B25",
    "#403A29",
    "#F5F8FA",
    "#343442",
    "#3E2E35"
  ),
  
  hover_border = c(
    "#79A5B9",
    "#72A795",
    "#7CA87F",
    "#AB966B",
    "#B8A66E",
    "#A89772",
    "#F1F5F6",
    "#8989A0",
    "#A97888"
  ),
  
  font_color = c(
    "#DCEEF6",
    "#DCF0EA",
    "#E0F0E1",
    "#EFE6CE",
    "#F1E7BE",
    "#F2E8C9",
    "#172126",
    "#E6E6F0",
    "#F0DFE5"
  ),
  
  stringsAsFactors = FALSE
)


styled_nodes <-
  merge(
    nodes,
    node_style,
    by = "node_type",
    all.x = TRUE,
    sort = FALSE
  )


styled_nodes <-
  styled_nodes[
    match(
      nodes$node_id,
      styled_nodes$node_id
    ),
  ]


# ============================================================
# AUDITED LEFT-TO-RIGHT FLOW STAGES
#
# Stage placement is now explicit in data/node_flow_stages.csv rather
# than inferred only from node_type. This prevents concepts whose role
# in the causal story differs from their broad visual type from being
# placed in the wrong part of the network.
#
# Rules:
#   upstream  = social / structural / environmental context & exposures
#   pathway   = behaviors / mechanisms / intermediate risk states
#   condition = diagnosed diseases and chronic health conditions
#   outcome   = symptoms / complications / downstream events
# ============================================================

styled_nodes$flow_stage <-
  node_flow_stages$flow_stage[
    match(styled_nodes$node_id, node_flow_stages$node_id)
  ]

# Safe fallback for a future node that has not yet been explicitly audited.
fallback_flow_stage <-
  ifelse(
    styled_nodes$node_type %in% c(
      "structural_determinant",
      "social_determinant",
      "environmental_determinant"
    ),
    "upstream",
    ifelse(
      styled_nodes$node_type %in% c(
        "behavioral_pathway",
        "behavioral_biological_pathway",
        "biological_pathway"
      ),
      "pathway",
      ifelse(styled_nodes$node_type == "disease", "condition", "outcome")
    )
  )

missing_flow_stage <- is.na(styled_nodes$flow_stage) | !nzchar(styled_nodes$flow_stage)
styled_nodes$flow_stage[missing_flow_stage] <- fallback_flow_stage[missing_flow_stage]


# ============================================================
# FAST, DETERMINISTIC MAP POSITIONS
#
# The network is now positioned before it reaches the browser. This avoids
# a long physics/stabilization phase on every launch. Nodes are locked to
# these map coordinates so they cannot be dragged out of their audited
# upstream/pathway/condition/outcome stage. The VIEW itself remains draggable
# so users can pan around the map while retaining zoom.
# ============================================================

node_degree <- setNames(rep(0, nrow(styled_nodes)), styled_nodes$node_id)
for (nid in names(node_degree)) {
  node_degree[[nid]] <- sum(relationships$from_node == nid | relationships$to_node == nid)
}

stage_centers <- c(
  upstream = -1350,
  pathway = -450,
  condition = 450,
  outcome = 1350
)

styled_nodes$map_x <- 0
styled_nodes$map_y <- 0

for (stage_name in names(stage_centers)) {
  idx <- which(styled_nodes$flow_stage == stage_name)
  if (length(idx) == 0) next

  # Put highly connected concepts near the vertical center so the largest
  # number of relationships have shorter, cleaner paths.
  ord <- idx[order(-node_degree[styled_nodes$node_id[idx]], styled_nodes$node_name[idx])]
  n_stage <- length(ord)

  # Alternating center-out placement: 0, +1, -1, +2, -2 ...
  slots <- numeric(n_stage)
  if (n_stage > 1) {
    for (j in seq_len(n_stage)) {
      if (j == 1) {
        slots[j] <- 0
      } else {
        k <- ceiling((j - 1) / 2)
        slots[j] <- ifelse(j %% 2 == 0, k, -k)
      }
    }
  }

  # Vertical spacing is intentionally generous. A small deterministic
  # horizontal offset keeps the columns from looking mechanically rigid.
  styled_nodes$map_y[ord] <- slots * 165
  styled_nodes$map_x[ord] <- stage_centers[[stage_name]] +
    rep(c(-95, 0, 95, -45, 45), length.out = n_stage)
}


vis_nodes <- data.frame(
  
  id =
    styled_nodes$node_id,
  
  label =
    styled_nodes$node_name,
  
  group =
    styled_nodes$node_type,

  flowStage =
    styled_nodes$flow_stage,

  x =
    styled_nodes$map_x,

  y =
    styled_nodes$map_y,

  fixed =
    TRUE,

  physics =
    FALSE,
  
  level =
    styled_nodes$level,
  
  value =
    styled_nodes$importance,
  
  title =
    node_tooltips,
  
  hidden =
    FALSE,
  
  shape =
    "box",
  
  color.background =
    styled_nodes$background,
  
  color.border =
    styled_nodes$border,
  
  color.highlight.background =
    styled_nodes$hover_background,
  
  color.highlight.border =
    styled_nodes$hover_border,
  
  color.hover.background =
    styled_nodes$hover_background,
  
  color.hover.border =
    styled_nodes$hover_border,
  
  font.color =
    styled_nodes$font_color,
  
  borderWidth =
    ifelse(
      styled_nodes$node_id == "t2d",
      2.5,
      1.35
    ),
  
  borderWidthSelected =
    ifelse(
      styled_nodes$node_id == "t2d",
      3.5,
      2.5
    ),
  
  stringsAsFactors = FALSE
)


# ============================================================
# RELATIONSHIPS + SOURCES
# ============================================================

edges_with_sources <-
  merge(
    relationships,
    relationship_sources,
    by = "relationship_id",
    all.x = TRUE
  )

edges_with_sources <-
  merge(
    edges_with_sources,
    sources,
    by = "source_id",
    all.x = TRUE
  )

edges_with_sources$from_name <-
  nodes$node_name[
    match(
      edges_with_sources$from_node,
      nodes$node_id
    )
  ]

edges_with_sources$to_name <-
  nodes$node_name[
    match(
      edges_with_sources$to_node,
      nodes$node_id
    )
  ]

edges_with_sources$relationship_key <-
  paste(
    edges_with_sources$from_node,
    edges_with_sources$to_node,
    sep = "__"
  )


# ============================================================
# RELATIONSHIP CONTENT
#
# Relationship explanations now live in
# data/relationship_content.csv instead of app.R.
# ============================================================

edges_with_sources <-
  merge(
    edges_with_sources,
    relationship_content,
    by = "relationship_key",
    all.x = TRUE,
    sort = FALSE
  )

edges_with_sources <-
  edges_with_sources[
    match(
      relationships$relationship_id,
      edges_with_sources$relationship_id
    ),
  ]

edges_with_sources$badge[
  is.na(edges_with_sources$badge)
] <- "RELATIONSHIP"

edges_with_sources$primary_text[
  is.na(edges_with_sources$primary_text)
] <-
  edges_with_sources$short_explanation[
    is.na(edges_with_sources$primary_text)
  ]

edges_with_sources$secondary_label[
  is.na(edges_with_sources$secondary_label)
] <- ""

edges_with_sources$secondary_text[
  is.na(edges_with_sources$secondary_text)
] <- ""

# Optional quantitative evidence snapshots for selected relationships.
# Most relationships do not have a single honest number that summarizes them,
# so statistics are shown only where a clear authoritative estimate exists.
for (field in c("statistic_label", "statistic_text", "statistic_source_id")) {
  if (!field %in% names(edges_with_sources)) {
    edges_with_sources[[field]] <- ""
  }
  edges_with_sources[[field]][is.na(edges_with_sources[[field]])] <- ""
}

stat_source_match <- match(
  edges_with_sources$statistic_source_id,
  sources$source_id
)

edges_with_sources$statistic_source_org <-
  ifelse(
    is.na(stat_source_match),
    "",
    sources$organization[stat_source_match]
  )

edges_with_sources$statistic_source_title <-
  ifelse(
    is.na(stat_source_match),
    "",
    sources$title[stat_source_match]
  )

edges_with_sources$statistic_source_url <-
  ifelse(
    is.na(stat_source_match),
    "",
    sources$url[stat_source_match]
  )


# ============================================================
# EDGE HOVER CARDS
# ============================================================

edge_tooltips <-
  vapply(
    seq_len(nrow(edges_with_sources)),
    function(i) {
      edge <- edges_with_sources[i, ]

      context_html <- ""
      if (nzchar(edge$secondary_text)) {
        context_html <- paste0(
          "<div class='edge-callout'>",
          "<div class='edge-callout-label'>",
          ifelse(nzchar(edge$secondary_label), edge$secondary_label, "CONTEXT / LIMITATION"),
          "</div>",
          "<div class='edge-callout-text'>", edge$secondary_text, "</div>",
          "</div>"
        )
      }

      statistic_html <- ""
      if (nzchar(edge$statistic_text)) {
        statistic_source <-
          if (nzchar(edge$statistic_source_org)) {
            paste0("<div class='edge-stat-source'>Source: ", edge$statistic_source_org, "</div>")
          } else {
            ""
          }

        statistic_html <- paste0(
          "<div class='edge-stat-card'>",
          "<div class='edge-stat-label'>",
          ifelse(nzchar(edge$statistic_label), edge$statistic_label, "BY THE NUMBERS"),
          "</div>",
          "<div class='edge-stat-text'>", edge$statistic_text, "</div>",
          statistic_source,
          "</div>"
        )
      }

      source_name <-
        if (!is.na(edge$organization) && nzchar(edge$organization)) {
          edge$organization
        } else {
          "Source available"
        }

      paste0(
        "<div class='network-tooltip-card edge-tooltip-card'>",
        "<div class='edge-tooltip-top'>",
        "<div class='relationship-badge'>", edge$badge, "</div>",
        "<div class='edge-evidence-pill'>● ", tools::toTitleCase(edge$evidence_strength), " evidence</div>",
        "</div>",
        "<div class='edge-tooltip-path'>",
        "<span>", edge$from_name, "</span>",
        "<span class='edge-tooltip-arrow'>→</span>",
        "<span>", edge$to_name, "</span>",
        "</div>",
        "<div class='edge-section-label'>WHY THEY'RE LINKED</div>",
        "<div class='edge-why-text'>", edge$short_explanation, "</div>",
        "<div class='edge-section-label edge-section-spaced'>HOW THE CONNECTION WORKS</div>",
        "<div class='edge-main-text'>", edge$primary_text, "</div>",
        statistic_html,
        context_html,
        "<div class='edge-tooltip-footer'>",
        "<span class='edge-source'>", source_name, "</span>",
        "<span class='edge-click-hint'>Click for full evidence & sources →</span>",
        "</div>",
        "</div>"
      )
    },
    character(1)
  )

vis_edges <- data.frame(
  
  id =
    edges_with_sources$relationship_id,
  
  from =
    edges_with_sources$from_node,
  
  to =
    edges_with_sources$to_node,
  
  title =
    edge_tooltips,
  
  hidden =
    FALSE,
  
  stringsAsFactors = FALSE
)


# ============================================================
# SEARCH CHOICES
# ============================================================

search_choices <-
  setNames(
    nodes$node_id,
    nodes$node_name
  )


# ============================================================
# UI
# ============================================================

ui <- tagList(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
    tags$meta(name = "theme-color", content = "#090D0F"),
    tags$meta(
      name = "description",
      content = "Explore how social conditions, environments, behaviors, biology, chronic disease, and health outcomes connect."
    ),
    
    tags$script(
      
      HTML("

        Shiny.addCustomMessageHandler(
          'scrollToLocalResults',
          function(message) {

            var attempts = 0;
            var lastTop = null;
            var stableFrames = 0;

            function glideWhenStable() {
              var panel = document.querySelector('.side-panel');
              if (!panel) return;

              var target = panel.querySelector('.local-results-anchor');
              if (!target) {
                attempts += 1;
                if (attempts < 24) requestAnimationFrame(glideWhenStable);
                return;
              }

              var panelRect = panel.getBoundingClientRect();
              var targetRect = target.getBoundingClientRect();
              var desiredTop = panel.scrollTop + targetRect.top - panelRect.top - 18;

              if (lastTop !== null && Math.abs(desiredTop - lastTop) < 2) {
                stableFrames += 1;
              } else {
                stableFrames = 0;
              }

              lastTop = desiredTop;
              attempts += 1;

              if (stableFrames >= 2 || attempts >= 24) {
                panel.scrollTo({top:desiredTop, behavior:'smooth'});
                return;
              }

              requestAnimationFrame(glideWhenStable);
            }

            requestAnimationFrame(glideWhenStable);
          }
        );

        // Durable public feedback: when an HTTPS endpoint is configured, post
        // from the browser so the Shiny server does not need an extra HTTP package.
        Shiny.addCustomMessageHandler(
          'postFeedback',
          function(message) {
            if (!message || !message.endpoint) {
              Shiny.setInputValue('feedback_remote_result', {ok:false, message:'Feedback service is not configured.'}, {priority:'event'});
              return;
            }

            fetch(message.endpoint, {
              method: 'POST',
              headers: {
                'Content-Type': 'application/json',
                'Accept': 'application/json'
              },
              body: JSON.stringify(message.payload || {})
            })
            .then(function(response) {
              if (!response.ok) throw new Error('Request failed');
              Shiny.setInputValue('feedback_remote_result', {ok:true}, {priority:'event'});
            })
            .catch(function() {
              Shiny.setInputValue('feedback_remote_result', {ok:false, message:'Feedback could not be sent. Please try again later.'}, {priority:'event'});
            });
          }
        );

      ")
    ),
    
    
    tags$style(
      
      HTML("

        @import url('https://fonts.googleapis.com/css2?family=Manrope:wght@400;500;600;700&display=swap');

        html, body {
          background:#050505 !important;
          color:#f2f2f2;
          font-family:'Manrope','Avenir Next','Segoe UI',Arial,sans-serif;
          overflow-x:hidden;
        }

        .container-fluid {
          background:#050505;
          padding-left:25px;
          padding-right:25px;
        }

        .title-box {
          text-align:center;
          padding-top:24px;
        }

        .title-box h1 {
          color:#ffffff;
          font-weight:500;
          letter-spacing:1px;
          margin-bottom:8px;
        }

        .subtitle {
          text-align:center;
          color:#aaaaaa;
        }

        .mission-line {
          text-align:center;
          color:#D7E3E7;
          font-size:16px;
          font-weight:600;
          letter-spacing:.15px;
          margin:4px auto 7px auto;
          max-width:760px;
        }

        .hint {
          text-align:center;
          color:#777777;
          font-size:15px;
          line-height:1.45;
          margin-bottom:8px;
        }



        @media (prefers-reduced-motion: reduce) {
          *, *::before, *::after {
            scroll-behavior:auto !important;
            transition-duration:.01ms !important;
            animation-duration:.01ms !important;
            animation-iteration-count:1 !important;
          }
        }

        /* ===================================================
           SEARCH
           =================================================== */

        .graph-search-area {
          width:100%;
          max-width:650px;
          margin:18px auto 8px auto;
        }

        .graph-search-label {
          color:#AFC3CA;
          font-size:10px;
          font-weight:700;
          letter-spacing:.8px;
          text-transform:uppercase;
          margin-bottom:7px;
        }

        .graph-search-row {
          display:flex;
          align-items:flex-start;
          gap:9px;
        }

        .graph-search-input {
          flex:1;
          min-width:0;
        }

        .graph-search-input .form-group {
          margin:0;
          width:100% !important;
        }

        .graph-search-input .shiny-input-container {
          width:100% !important;
          max-width:none !important;
        }

        .graph-search-input .selectize-control {
          margin:0;
        }

        .graph-search-input .selectize-input {
          min-height:44px;
          background:#101516 !important;
          border:1px solid #304047 !important;
          border-radius:11px !important;
          color:#E8EFF1 !important;
          box-shadow:none !important;
          padding:11px 13px !important;
        }

        .graph-search-input .selectize-input.focus {
          border-color:#527482 !important;
          box-shadow:
            0 0 0 2px rgba(111,162,181,.09) !important;
        }

        .graph-search-input .selectize-input input {
          color:#E8EFF1 !important;
        }

        .graph-search-input .selectize-dropdown {
          background:#111617 !important;
          border:1px solid #304047 !important;
          border-radius:10px !important;
          color:#D6E0E3 !important;
          overflow:hidden;
        }

        .graph-search-input .selectize-dropdown-content {
          max-height:300px;
        }

        .graph-search-input .option {
          padding:10px 12px !important;
          color:#BFCBCD !important;
        }

        .graph-search-input .option.active {
          background:#182326 !important;
          color:#FFFFFF !important;
        }

        .full-network-btn {
          min-height:44px;
          white-space:nowrap;
          background:#101516 !important;
          border:1px solid #304047 !important;
          border-radius:11px !important;
          color:#AFC3CA !important;
          padding:10px 14px !important;
        }

        .full-network-btn:hover {
          background:#182023 !important;
          border-color:#49616B !important;
          color:#FFFFFF !important;
        }

        .qa-button {
          min-height:44px;
          white-space:nowrap;
          background:#142019 !important;
          border:1px solid #365744 !important;
          border-radius:11px !important;
          color:#B9D8C4 !important;
          padding:10px 14px !important;
        }

        .qa-button:hover {
          background:#1B2B21 !important;
          border-color:#4C765D !important;
          color:#FFFFFF !important;
        }

        .qa-modal-note {
          padding:11px 12px;
          margin-bottom:14px;
          border:1px solid #384349;
          border-radius:9px;
          background:#121719;
          color:#AEBBC0;
          font-size:12px;
          line-height:1.5;
        }



        /* Proposal review uses Bootstrap's light modal background.
           Force readable dark text inside that admin-only review screen. */
        .proposal-review-panel {
          color:#243238 !important;
        }

        .proposal-review-panel h4,
        .proposal-review-panel h5,
        .proposal-review-panel p,
        .proposal-review-panel label,
        .proposal-review-panel .control-label,
        .proposal-review-panel .shiny-text-output {
          color:#243238 !important;
        }

        .proposal-review-panel b,
        .proposal-review-panel strong {
          color:#101719 !important;
        }

        .proposal-review-panel textarea.form-control {
          background:#FFFFFF !important;
          color:#182226 !important;
          border:1px solid #AEB9BD !important;
          box-shadow:none !important;
        }

        .proposal-review-panel textarea.form-control:focus {
          border-color:#607D88 !important;
          box-shadow:0 0 0 2px rgba(96,125,136,.12) !important;
        }

        .qa-summary-grid {
          display:grid;
          grid-template-columns:repeat(4,1fr);
          gap:8px;
          margin:11px 0 15px 0;
        }

        .qa-summary-card {
          padding:9px 10px;
          border-radius:9px;
          border:1px solid #343A3D;
          background:#111415;
          text-align:center;
        }

        .qa-summary-number {
          color:#FFFFFF;
          font-size:20px;
          font-weight:600;
        }

        .qa-summary-label {
          margin-top:2px;
          color:#879398;
          font-size:9px;
          text-transform:uppercase;
          letter-spacing:.6px;
        }

        .qa-section-title {
          margin-top:17px;
          margin-bottom:8px;
          color:#D6E0E3;
          font-size:15px;
          font-weight:600;
        }

        .qa-row {
          margin:7px 0;
          padding:10px 11px;
          border-radius:9px;
          background:#111516;
          border:1px solid #30383B;
        }

        .qa-row-top {
          display:flex;
          align-items:center;
          gap:8px;
          margin-bottom:5px;
        }

        .qa-status-pill {
          display:inline-block;
          padding:3px 7px;
          border-radius:20px;
          font-size:8px;
          font-weight:700;
          letter-spacing:.55px;
        }

        .qa-pass {
          color:#9ED6B0;
          border:1px solid #315D40;
          background:#102018;
        }

        .qa-warning {
          color:#D7C98F;
          border:1px solid #62572E;
          background:#201D11;
        }

        .qa-review {
          color:#E0B987;
          border:1px solid #6A4E30;
          background:#24190F;
        }

        .qa-fail {
          color:#E2AAAA;
          border:1px solid #693A3A;
          background:#251313;
        }

        .qa-row-item {
          color:#D8E0E2;
          font-size:12px;
          font-weight:600;
        }

        .qa-row-area {
          margin-left:auto;
          color:#78888E;
          font-size:9px;
          text-transform:uppercase;
          letter-spacing:.55px;
        }

        .qa-row-message {
          color:#AAB6BA;
          font-size:11px;
          line-height:1.45;
        }

        .live-qa-badge {
          display:inline-block;
          margin-left:6px;
          padding:3px 7px;
          border-radius:20px;
          font-size:9px;
          font-weight:700;
          letter-spacing:.45px;
          text-transform:uppercase;
        }

        .qa-investigate-btn {
          margin-top:8px;
          padding:6px 9px !important;
          border-radius:8px !important;
          border:1px solid #43545B !important;
          background:#182024 !important;
          color:#C9D6DA !important;
          font-size:10px !important;
        }

        .qa-investigate-btn:hover {
          background:#202B30 !important;
          border-color:#607680 !important;
          color:#FFFFFF !important;
        }

        .qa-detail-grid {
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:9px;
          margin:12px 0;
        }

        .qa-detail-card {
          padding:10px 11px;
          border:1px solid #343F44;
          border-radius:9px;
          background:#111617;
        }

        .qa-detail-label {
          color:#78888E;
          font-size:9px;
          text-transform:uppercase;
          letter-spacing:.6px;
          margin-bottom:4px;
        }

        .qa-detail-value {
          color:#D8E0E2;
          font-size:12px;
          line-height:1.45;
          overflow-wrap:anywhere;
        }

        .qa-recommendation {
          margin:12px 0;
          padding:11px 12px;
          border-left:3px solid #5D7782;
          border-radius:7px;
          background:#11181B;
          color:#C2CED2;
          font-size:12px;
          line-height:1.5;
        }

        .qa-raw-table {
          width:100%;
          border-collapse:collapse;
          margin-top:8px;
          font-size:10px;
        }

        .qa-raw-table th,
        .qa-raw-table td {
          border-bottom:1px solid #2D3538;
          padding:6px 7px;
          text-align:left;
          vertical-align:top;
          color:#AEBBC0;
        }

        .qa-raw-table th {
          color:#D5DEE1;
          font-weight:600;
        }

        .qa-reviewed-badge {
          display:inline-block;
          margin-left:6px;
          padding:3px 7px;
          border-radius:20px;
          font-size:8px;
          font-weight:700;
          letter-spacing:.5px;
          color:#9ED6B0;
          border:1px solid #315D40;
          background:#102018;
        }

        .graph-mode-status {
          min-height:19px;
          margin-top:7px;
          text-align:center;
          color:#73848A;
          font-size:10px;
          letter-spacing:.2px;
        }


        .focus-label {
          text-align:center;
          color:#cccccc;
          font-size:14px;
          min-height:20px;
        }


        /* NETWORK */

        .network-wrapper {
          position:relative;
          width:100%;
          min-height:1200px;
          margin-top:15px;
        }

        /*
         * Persistent map-reading key. This is screen-space UI, not a fake
         * category boundary. It stays readable while the user pans/zooms
         * the fixed network underneath it.
         */
        .flow-key {
          position:absolute;
          top:16px;
          left:16px;
          z-index:14;
          width:min(500px, calc(100% - 120px));
          padding:14px 16px 13px 16px;
          border:1px solid rgba(176,202,210,0.16);
          border-radius:12px;
          background:rgba(10,14,16,0.90);
          box-shadow:0 6px 22px rgba(0,0,0,0.32);
          backdrop-filter:blur(8px);
          -webkit-backdrop-filter:blur(8px);
          pointer-events:none;
        }

        .flow-key-title {
          color:rgba(225,237,240,0.75);
          font-size:11px;
          font-weight:700;
          letter-spacing:1px;
          text-transform:uppercase;
          margin-bottom:7px;
        }

        .flow-key-path {
          display:flex;
          align-items:center;
          gap:5px;
          flex-wrap:wrap;
          color:rgba(205,220,225,0.76);
          font-size:13px;
          line-height:1.4;
        }

        .flow-key-step {
          border:1px solid rgba(155,185,195,0.14);
          border-radius:999px;
          padding:4px 7px;
          background:rgba(32,40,43,0.70);
          white-space:nowrap;
        }

        .flow-key-arrow {
          color:rgba(196,215,221,0.52);
          font-size:13px;
        }

        .flow-key-note {
          margin-top:7px;
          color:rgba(158,181,188,0.56);
          font-size:11px;
          line-height:1.45;
        }

        .flow-key-blue { color:rgba(120,203,235,0.95); }
        .flow-key-gold { color:rgba(239,205,132,0.95); }

        .network-box {
          position:relative;
          z-index:1;
          width:100%;
          background:#050505;
          border:1px solid #171A1B;
          border-radius:22px;
          overflow:hidden;
          box-shadow:
            inset 0 0 80px rgba(255,255,255,0.012),
            0 12px 45px rgba(0,0,0,0.35);
        }


        /* NETWORK LOADING STATE */

        .network-box {
          position:relative;
        }

        .network-loading {
          position:absolute;
          inset:0;
          z-index:12;
          display:flex;
          align-items:center;
          justify-content:center;
          pointer-events:none;
          background:#050505;
          opacity:1;
          transition:opacity .28s ease;
        }

        .network-loading.is-ready {
          opacity:0;
        }

        .network-loading-card {
          padding:14px 18px;
          border:1px solid #2D383C;
          border-radius:12px;
          background:#0D1214;
          color:#C8D5D9;
          font-size:14px;
          letter-spacing:.15px;
          box-shadow:0 10px 30px rgba(0,0,0,.35);
        }

        /* SHARED GLASS STYLE */

        .map-glass {
          background:rgba(13,16,17,.91);
          border:1px solid #272D2F;
          border-radius:13px;
          box-shadow:0 8px 28px rgba(0,0,0,.45);
          backdrop-filter:blur(9px);
          -webkit-backdrop-filter:blur(9px);
        }


        /* NETWORK KEY */

        .color-key {
          position:absolute;
          z-index:20;
          left:18px;
          bottom:18px;
          width:305px;
          padding:15px 16px;
          pointer-events:none;
        }

        .color-key-title {
          color:#C4D0D4;
          font-size:12px;
          font-weight:700;
          text-transform:uppercase;
          letter-spacing:.8px;
          margin-bottom:9px;
        }

        .color-key-grid {
          display:grid;
          grid-template-columns:1fr 1fr;
          gap:7px 11px;
        }

        .color-key-item {
          display:flex;
          align-items:center;
          gap:8px;
          color:#AAB5B9;
          font-size:13px;
          line-height:1.3;
        }

        .key-dot {
          width:12px;
          height:12px;
          border-radius:3px;
          flex:0 0 12px;
          border:1px solid rgba(255,255,255,.20);
        }

        .key-structural {
          background:#15242C;
          border-color:#426678;
        }

        .key-social {
          background:#162824;
          border-color:#426B60;
        }

        .key-environment {
          background:#17281C;
          border-color:#4B704F;
        }

        .key-behavioral {
          background:#292419;
          border-color:#706143;
        }

        .key-biological {
          background:#302A1A;
          border-color:#827347;
        }

        .key-disease {
          background:#E8EEF1;
          border-color:#D5E0E5;
        }

        .key-symptom {
          background:#23232C;
          border-color:#5B5B70;
        }

        .key-complication {
          background:#2A2025;
          border-color:#72505D;
        }


        /* CUSTOM ZOOM CONTROLS */

        .map-controls {
          position:absolute;
          z-index:30;
          right:18px;
          bottom:18px;
          padding:7px;
          display:flex;
          flex-direction:column;
          gap:6px;
        }

        .map-control-btn {
          appearance:none;
          -webkit-appearance:none;
          width:38px;
          height:38px;
          margin:0;
          padding:0;
          border:1px solid #2B3538;
          border-radius:9px;
          background:#111617;
          color:#AEBEC3;
          display:flex;
          align-items:center;
          justify-content:center;
          cursor:pointer;
          font-family:'Manrope','Avenir Next','Segoe UI',Arial,sans-serif;
          line-height:1;
          transition:
            background .15s ease,
            border-color .15s ease,
            color .15s ease,
            transform .08s ease;
        }

        .map-control-btn:hover {
          background:#182023;
          border-color:#42535A;
          color:#EDF4F6;
        }

        .map-control-btn:active {
          transform:scale(.95);
        }

        .map-control-btn:focus {
          outline:none;
          box-shadow:0 0 0 2px rgba(126,174,191,.15);
        }

        .zoom-symbol {
          font-size:22px;
          font-weight:300;
          transform:translateY(-1px);
        }

        .home-symbol {
          font-size:18px;
          line-height:1;
          transform:translateY(-1px);
        }

        .control-divider {
          height:1px;
          background:#273033;
          margin:1px 3px;
        }


        /* SIDE PANEL */

        .side-panel {
          position:fixed;
          top:105px;
          right:22px;
          width:420px;
          max-width:calc(100vw - 44px);
          max-height:calc(100vh - 135px);
          overflow-y:auto;
          overflow-x:hidden;
          z-index:999;
          box-sizing:border-box;
          padding:20px 21px;
          background:rgba(14,14,14,0.97);
          border:1px solid #353535;
          border-radius:14px;
          box-shadow:0 10px 35px rgba(0,0,0,0.75);
          color:#dddddd;
          line-height:1.5;
          backdrop-filter:blur(8px);
          -webkit-backdrop-filter:blur(8px);
        }

        .side-panel * {
          max-width:100%;
          box-sizing:border-box;
          overflow-wrap:anywhere;
        }

        .side-panel-header {
          display:flex;
          justify-content:space-between;
          align-items:flex-start;
          gap:12px;
          margin-bottom:5px;
        }

        .side-panel-title {
          color:#ffffff;
          font-size:27px;
          line-height:1.18;
          font-weight:600;
          margin:0;
        }

        .panel-node-badge {
          display:inline-block;
          padding:5px 8px;
          margin-bottom:11px;
          border-radius:20px;
          background:#18262B;
          border:1px solid #334A53;
          color:#9BC0CC;
          font-size:8.5px;
          font-weight:700;
          letter-spacing:.8px;
          line-height:1;
        }

        .panel-description {
          margin-top:12px;
          color:#B7C0C2;
          font-size:15px;
          line-height:1.6;
        }

        .why-box {
          margin-top:17px;
          padding:13px 14px;
          background:#111719;
          border:1px solid #29383E;
          border-radius:11px;
        }

        .why-label {
          color:#8FB9C7;
          font-size:9px;
          font-weight:700;
          letter-spacing:.75px;
          text-transform:uppercase;
          margin-bottom:6px;
        }

        .why-text {
          color:#C3CCCE;
          font-size:12px;
          line-height:1.5;
        }

        .close-panel-button {
          flex:0 0 auto;
          border:none;
          background:transparent;
          color:#888888;
          font-size:25px;
          line-height:1;
          padding:0 2px;
          margin:0;
          cursor:pointer;
        }

        .close-panel-button:hover {
          color:#ffffff;
          background:transparent;
        }


        /* INTERVENTIONS */

        .interventions-title {
          color:#A9D7B6;
          margin-top:24px;
          margin-bottom:5px;
        }

        .intervention-intro {
          color:#999999;
          font-size:13px;
          margin-bottom:15px;
        }

        .intervention-section {
          width:100%;
          margin-top:20px;
          margin-bottom:22px;
        }

        .level-heading {
          width:100%;
          font-size:16px;
          font-weight:600;
          padding-bottom:6px;
          margin-bottom:9px;
          border-bottom:1px solid #303030;
        }

        .level-individual {
          color:#B7D9D2;
        }

        .level-healthcare {
          color:#9FCAD5;
        }

        .level-community {
          color:#A9D7B6;
        }

        .level-policy {
          color:#D2C89B;
        }

        .level-other {
          color:#CFCFCF;
        }

        .intervention-card {
          width:100%;
          padding:12px 13px;
          margin:9px 0;
          background:#111814;
          border:1px solid #27402f;
          border-radius:9px;
        }

        .intervention-name {
          color:#DDF0E2;
          font-size:16px;
          font-weight:650;
          line-height:1.4;
        }

        .intervention-summary {
          margin-top:6px;
          color:#c7c7c7;
          font-size:14px;
          line-height:1.55;
        }

        details {
          width:100%;
          margin-top:8px;
        }

        summary {
          display:inline-block;
          cursor:pointer;
          color:#9FC7D9;
          font-size:12px;
        }

        .detail-content {
          width:100%;
          margin-top:9px;
          padding-top:9px;
          border-top:1px solid #29362E;
          color:#ADB7B0;
          font-size:12px;
          line-height:1.5;
        }

        .detail-content p {
          margin:5px 0;
        }

        .source-line {
          margin-top:9px;
          padding-top:8px;
          border-top:1px solid #252E28;
          font-size:11px;
          color:#929B95;
        }


        /* LOCAL */

        .local-help-box {
          margin-top:25px;
          padding:15px;
          width:100%;
          background:#11171A;
          border:1px solid #29414B;
          border-radius:11px;
          overflow:hidden;
        }

        .local-help-title {
          color:#B8DCE8;
          font-size:17px;
          font-weight:600;
          margin-bottom:4px;
        }

        .local-help-subtitle {
          color:#9AABB1;
          font-size:14px;
          line-height:1.55;
          margin-bottom:13px;
        }

        .zip-field .form-group {
          width:100%;
          margin-bottom:0;
        }

        .zip-field label {
          color:#BFC8CC;
          font-size:11px;
        }

        .zip-field input {
          width:100% !important;
          height:38px;
          background:#0B0F11;
          color:#ffffff;
          border:1px solid #3B5059;
          border-radius:7px;
          padding:8px 9px;
        }

        .local-search-button {
          display:block !important;
          width:100% !important;
          margin-top:9px !important;
          background:#17303A !important;
          color:#CDE8F1 !important;
          border:1px solid #3E6575 !important;
          border-radius:7px !important;
          padding:8px 10px !important;
        }

        .search-preview {
          margin-top:14px;
          padding-top:12px;
          border-top:1px solid #26383F;
          color:#AAB8BD;
          font-size:12px;
        }

        .search-preview-title {
          color:#D3E5EA;
          font-weight:600;
          margin-bottom:6px;
        }

        .search-tag {
          display:inline-block;
          margin:3px 3px 3px 0;
          padding:4px 7px;
          background:#17252A;
          border:1px solid #2D454E;
          border-radius:20px;
          color:#B8CFD7;
          font-size:10px;
        }

        .local-results-anchor {
          margin-top:18px;
          padding-top:15px;
          border-top:1px solid #31434A;
        }

        .local-data-heading {
          color:#D5EDF5;
          font-size:17px;
          font-weight:600;
        }

        .data-section-label {
          margin-top:14px;
          margin-bottom:6px;
          color:#91BFCF;
          font-size:11px;
          font-weight:600;
          text-transform:uppercase;
          letter-spacing:.7px;
        }

        .live-badge {
          display:inline-block;
          padding:3px 7px;
          margin:4px 0 8px 0;
          border-radius:20px;
          background:#173328;
          border:1px solid #2E624B;
          color:#9EE0B7;
          font-size:9px;
          font-weight:600;
          letter-spacing:.5px;
          text-transform:uppercase;
        }

        .no-direct-box {
          margin:10px 0 13px 0;
          padding:11px 12px;
          background:#171614;
          border:1px solid #4A4130;
          border-radius:9px;
          color:#CABF9D;
          font-size:11px;
        }

        .local-data-card {
          width:100%;
          margin:9px 0;
          padding:11px 12px;
          background:#0C1417;
          border:1px solid #263E47;
          border-radius:9px;
        }

        .local-data-measure {
          color:#CBDDE3;
          font-size:12px;
        }

        .local-data-value {
          color:#FFFFFF;
          font-size:27px;
          font-weight:600;
          margin-top:3px;
        }

        .metric-note {
          color:#8EA0A6;
          font-size:10px;
          margin-top:5px;
        }

        .metric-source {
          color:#71858D;
          font-size:10px;
          margin-top:7px;
        }

        .data-note {
          color:#819198;
          font-size:10px;
          margin-top:11px;
        }

        .data-error {
          margin-top:12px;
          padding:10px;
          border:1px dashed #5E4444;
          border-radius:8px;
          color:#CFAAAA;
          font-size:12px;
        }

        .source-status {
          margin-top:8px;
          padding:8px 9px;
          border-radius:8px;
          font-size:10px;
        }

        .source-status-good {
          color:#A6D7B6;
          background:#101B15;
          border:1px solid #294634;
        }

        .source-status-bad {
          color:#CCA9A9;
          background:#1A1111;
          border:1px solid #4C3030;
        }

        .program-coming {
          margin-top:15px;
          padding:10px 11px;
          background:#101415;
          border:1px dashed #3D4A4F;
          border-radius:8px;
          color:#88979C;
          font-size:11px;
        }


        /* RELATIONSHIP PANEL */

        .relationship-big-path {
          color:#ffffff;
          font-size:22px;
          font-weight:600;
          line-height:1.3;
          margin-bottom:20px;
        }

        .relationship-big-arrow {
          color:#9FC7D9;
          margin:0 6px;
        }

        .relationship-panel-badge {
          display:inline-block;
          padding:5px 8px;
          margin-bottom:13px;
          border-radius:20px;
          background:#15262D;
          border:1px solid #345565;
          color:#AFCFD9;
          font-size:9px;
          font-weight:700;
          letter-spacing:.75px;
        }

        .evidence-section {
          margin-top:20px;
          padding-top:15px;
          border-top:1px solid #303030;
        }

        .evidence-section h4 {
          color:#B7D4DF;
          margin-top:0;
          margin-bottom:8px;
        }

        .stat-card {
          background:#11181B;
          border:1px solid #2D424B;
          border-radius:10px;
          padding:13px 14px;
          margin:15px 0;
        }

        .source-button {
          display:inline-block;
          margin-top:10px;
          padding:8px 11px;
          border:1px solid #3A5966;
          border-radius:7px;
          color:#B9DCE8 !important;
          text-decoration:none;
          font-size:12px;
          background:#11191D;
        }


        /* CONNECTIONS */

        .connections {
          margin-top:22px;
          padding-top:13px;
          border-top:1px solid #282828;
        }

        .connections h4 {
          color:#dddddd;
          font-size:16px;
        }

        .connections li {
          margin-bottom:7px;
          font-size:13px;
        }


        .reset-button {
          display:block;
          margin:9px auto 5px auto;
          border-radius:8px;
        }

        .btn-default {
          background:#171717;
          color:#dddddd;
          border-color:#333333;
        }

        a {
          color:#9FC7D9;
        }


        /* TOOLTIP RESET */

        .vis-tooltip,
        div.vis-tooltip {
          background:transparent !important;
          border:none !important;
          padding:0 !important;
          margin:0 !important;
          max-width:440px !important;
          white-space:normal !important;
          box-shadow:none !important;
          pointer-events:none;
        }

        .network-tooltip-card {
          box-sizing:border-box;
          background:rgba(17,22,23,0.985);
          border:1px solid #2C393D;
          box-shadow:
            0 18px 45px rgba(0,0,0,.68),
            inset 0 1px 0 rgba(255,255,255,.025);
          color:#D8DEDF;
          white-space:normal;
          overflow-wrap:anywhere;
          backdrop-filter:blur(12px);
          -webkit-backdrop-filter:blur(12px);
        }


        /* NODE HOVER */

        .node-tooltip-card {
          width:340px;
          max-width:340px;
          padding:16px 17px 13px 17px;
          border-radius:17px;
        }

        .node-tooltip-badge {
          display:inline-block;
          padding:5px 8px;
          margin-bottom:11px;
          border-radius:20px;
          background:#18262B;
          border:1px solid #334A53;
          color:#9BC0CC;
          font-size:8.5px;
          font-weight:700;
          letter-spacing:.8px;
          line-height:1;
        }

        .node-tooltip-title {
          color:#F1F4F4;
          font-size:17px;
          line-height:1.3;
          font-weight:600;
          margin-bottom:8px;
        }

        .node-tooltip-description {
          color:#BBC4C6;
          font-size:12px;
          line-height:1.5;
        }

        .node-tooltip-footer {
          margin-top:11px;
          padding-top:9px;
          border-top:1px solid #293235;
          color:#86B2C1;
          font-size:9.5px;
          text-align:right;
        }


        /* EDGE HOVER */

        .edge-tooltip-card {
          width:410px;
          max-width:410px;
          padding:17px 18px 15px 18px;
          border-radius:18px;
          border-color:#304046;
        }

        .edge-tooltip-top {
          display:flex;
          justify-content:space-between;
          align-items:center;
          gap:10px;
          margin-bottom:13px;
        }

        .relationship-badge {
          display:inline-block;
          padding:5px 8px;
          border-radius:20px;
          background:#15262D;
          border:1px solid #345565;
          color:#AFCFD9;
          font-size:8.5px;
          font-weight:700;
          letter-spacing:.8px;
          line-height:1;
        }

        .edge-evidence-pill {
          flex:0 0 auto;
          color:#81969D;
          font-size:9.5px;
        }

        .edge-tooltip-path {
          display:flex;
          flex-wrap:wrap;
          align-items:center;
          gap:5px;
          margin-bottom:16px;
          color:#F2F5F5;
          font-size:16.5px;
          line-height:1.35;
          font-weight:600;
        }

        .edge-tooltip-arrow {
          color:#7FAFC0;
          padding:0 2px;
        }

        .edge-section-label {
          color:#7FAFC0;
          font-size:9px;
          letter-spacing:.75px;
          font-weight:700;
          margin-bottom:6px;
        }

        .edge-main-text {
          color:#D3DBDD;
          font-size:13.5px;
          line-height:1.58;
        }

        .edge-why-text {
          color:#F0F4F5;
          font-size:14px;
          line-height:1.55;
          font-weight:650;
        }

        .edge-section-spaced {
          margin-top:14px;
        }

        .edge-stat-card {
          margin-top:14px;
          padding:11px 12px 12px 12px;
          background:#182327;
          border:1px solid #38525B;
          border-radius:12px;
        }

        .edge-stat-label {
          color:#A9D7B6;
          font-size:9px;
          font-weight:800;
          letter-spacing:.8px;
          margin-bottom:6px;
        }

        .edge-stat-text {
          color:#E2E9EA;
          font-size:13px;
          line-height:1.52;
        }

        .edge-stat-source {
          margin-top:7px;
          color:#81969D;
          font-size:10px;
        }

        .edge-callout {
          margin-top:13px;
          padding:10px 11px 11px 11px;
          background:#141C1F;
          border:1px solid #293B42;
          border-radius:11px;
        }

        .edge-callout-label {
          color:#90B8C5;
          font-size:8.5px;
          font-weight:700;
          letter-spacing:.75px;
          margin-bottom:5px;
        }

        .edge-callout-text {
          color:#B7C1C4;
          font-size:11.5px;
          line-height:1.48;
        }

        .edge-tooltip-footer {
          display:flex;
          justify-content:space-between;
          align-items:center;
          gap:15px;
          margin-top:14px;
          padding-top:10px;
          border-top:1px solid #273033;
          font-size:9.5px;
        }

        .edge-source {
          color:#74878D;
        }

        .edge-click-hint {
          color:#87B4C3;
          text-align:right;
        }


        .medical-disclaimer-banner {
          max-width:1120px;
          margin:16px auto 20px auto;
          padding:12px 16px;
          border:1px solid rgba(216,185,91,.42);
          border-radius:11px;
          background:rgba(97,76,20,.16);
          color:#D9D1B6;
          font-size:14px;
          line-height:1.5;
        }

        .medical-disclaimer-banner strong { color:#F1E5B8; }

        .admin-builder {
          max-width:980px;
          margin:22px auto;
          padding:22px;
          border:1px solid #334249;
          border-radius:14px;
          background:#0D1214;
        }

        .admin-builder h2, .admin-builder h3 { color:#EAF0F2; }
        .admin-builder p { color:#AAB6BB; line-height:1.55; }
        .admin-builder label { color:#D7E0E3; font-weight:700; }
        .admin-builder textarea, .admin-builder input, .admin-builder select {
          background:#151D20 !important;
          color:#EDF3F4 !important;
          border:1px solid #3A4B52 !important;
        }

        .feedback-form {
          max-width:760px;
          margin:0 auto;
          padding:24px;
          border:1px solid #2E3B40;
          border-radius:14px;
          background:#0C1113;
        }

        .feedback-form label { color:#DCE5E8; font-weight:700; }
        .feedback-form textarea, .feedback-form input, .feedback-form select {
          background:#151D20 !important;
          color:#EEF3F4 !important;
          border:1px solid #39494F !important;
        }

        .feedback-note {
          color:#8F9DA2;
          font-size:13px;
          line-height:1.5;
          margin:8px 0 16px 0;
        }

        .feedback-success {
          margin-top:14px;
          padding:12px 14px;
          border-radius:10px;
          background:rgba(53,112,77,.22);
          border:1px solid rgba(91,166,119,.5);
          color:#CDEAD8;
        }

        .feedback-error {
          margin-top:14px;
          padding:12px 14px;
          border-radius:10px;
          background:rgba(125,54,54,.20);
          border:1px solid rgba(185,91,91,.5);
          color:#F0CACA;
        }


        /* Keep the admin feedback inbox readable inside Bootstrap's table styles. */
        #feedback_admin_table table {
          width:100% !important;
          background:#0F1517 !important;
          color:#E8EFF1 !important;
          border-color:#334249 !important;
        }

        #feedback_admin_table table thead th {
          background:#182124 !important;
          color:#F3F7F8 !important;
          border-color:#3B4B51 !important;
          font-weight:700;
        }

        #feedback_admin_table table tbody td {
          color:#DCE5E8 !important;
          border-color:#2C393E !important;
          background:#0F1517 !important;
        }

        #feedback_admin_table .table-striped > tbody > tr:nth-of-type(odd) > td {
          background:#141C1F !important;
        }

        #feedback_admin_table .table-striped > tbody > tr:nth-of-type(even) > td {
          background:#0F1517 !important;
        }

        @media (max-width:800px) {

          .graph-search-row {
            flex-direction:column;
          }

          .graph-search-input {
            width:100%;
          }

          .full-network-btn {
            width:100%;
          }

          .qa-button {
            width:100%;
          }

          .qa-summary-grid {
            grid-template-columns:1fr 1fr;
          }

          .side-panel {
            top:auto;
            bottom:15px;
            left:15px;
            right:15px;
            width:auto;
            max-width:none;
            max-height:55vh;
          }

          .edge-tooltip-card {
            width:340px;
            max-width:340px;
          }

          .node-tooltip-card {
            width:310px;
            max-width:310px;
          }

          .color-key {
            width:230px;
          }

          .map-controls {
            right:12px;
            bottom:12px;
          }
        }


        /* ===================================================
           WEBSITE NAVIGATION + INFORMATION PAGES
           =================================================== */

        .navbar {
          background:#090D0F !important;
          border:0 !important;
          border-bottom:1px solid #202A2E !important;
          border-radius:0 !important;
          margin-bottom:0 !important;
          min-height:62px;
        }

        .navbar .container-fluid {
          background:#090D0F !important;
          max-width:1500px;
          margin:0 auto;
          padding-left:28px;
          padding-right:28px;
        }

        .navbar-brand,
        .navbar-inverse .navbar-brand {
          color:#F4F7F8 !important;
          font-size:19px;
          font-weight:750;
          letter-spacing:.2px;
          padding-top:20px;
        }

        .navbar-inverse .navbar-nav > li > a {
          color:#AAB7BC !important;
          font-size:14px;
          font-weight:650;
          letter-spacing:.1px;
          padding-top:21px;
          padding-bottom:20px;
          transition:color .16s ease, background .16s ease;
        }

        .navbar-inverse .navbar-nav > li > a:hover,
        .navbar-inverse .navbar-nav > .active > a,
        .navbar-inverse .navbar-nav > .active > a:hover,
        .navbar-inverse .navbar-nav > .active > a:focus {
          color:#FFFFFF !important;
          background:#12191C !important;
        }

        .site-page {
          max-width:1180px;
          margin:0 auto;
          padding:46px 24px 76px 24px;
        }

        .site-explore-page {
          max-width:none;
          padding:0 0 40px 0;
        }

        .info-hero {
          max-width:860px;
          margin:10px auto 40px auto;
          text-align:center;
        }

        .info-eyebrow {
          color:#8FB9C7;
          font-size:12px;
          font-weight:800;
          letter-spacing:1.2px;
          text-transform:uppercase;
          margin-bottom:10px;
        }

        .info-hero h1 {
          color:#F5F7F8;
          font-size:42px;
          line-height:1.12;
          font-weight:760;
          letter-spacing:-.7px;
          margin:0 0 14px 0;
        }

        .info-hero p {
          color:#B5C0C4;
          font-size:18px;
          line-height:1.65;
          margin:0 auto;
          max-width:760px;
        }

        .info-grid {
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:18px;
          margin:26px 0;
        }

        .info-card {
          background:#0D1315;
          border:1px solid #263237;
          border-radius:16px;
          padding:24px 25px;
        }

        .info-card h3 {
          color:#F2F5F5;
          font-size:20px;
          font-weight:730;
          margin:0 0 10px 0;
        }

        .info-card p,
        .info-card li {
          color:#B7C1C5;
          font-size:15.5px;
          line-height:1.65;
        }

        .info-card ul {
          padding-left:20px;
          margin-bottom:0;
        }

        .info-wide {
          margin:18px 0;
        }

        .info-stat-row {
          display:grid;
          grid-template-columns:repeat(4,minmax(0,1fr));
          gap:12px;
          margin:28px 0 34px 0;
        }

        .info-stat {
          background:#0D1315;
          border:1px solid #253238;
          border-radius:14px;
          padding:18px;
          text-align:center;
        }

        .info-stat-value {
          display:block;
          color:#F1F5F6;
          font-size:27px;
          font-weight:780;
          line-height:1.1;
          margin-bottom:6px;
        }

        .info-stat-label {
          color:#829299;
          font-size:12px;
          line-height:1.4;
          text-transform:uppercase;
          letter-spacing:.65px;
        }

        .flow-explainer {
          display:grid;
          grid-template-columns:repeat(4,minmax(0,1fr));
          gap:10px;
          align-items:stretch;
          margin:28px 0;
        }

        .flow-explainer-step {
          position:relative;
          background:#0D1416;
          border:1px solid #29383E;
          border-radius:14px;
          padding:20px;
        }

        .flow-explainer-step:not(:last-child)::after {
          content:'→';
          position:absolute;
          right:-13px;
          top:50%;
          transform:translateY(-50%);
          color:#688892;
          font-size:19px;
          z-index:2;
        }

        .flow-explainer-step strong {
          color:#E9F0F2;
          display:block;
          font-size:15px;
          margin-bottom:7px;
        }

        .flow-explainer-step span {
          color:#8E9DA3;
          font-size:13px;
          line-height:1.5;
        }

        .source-family {
          border-left:3px solid #476C78;
          padding:5px 0 5px 16px;
          margin:18px 0;
        }

        .source-family strong {
          color:#EAF0F1;
          font-size:15px;
        }

        .source-family p {
          color:#AAB6BB;
          margin:5px 0 0 0;
          font-size:14.5px;
          line-height:1.55;
        }

        @media (max-width:800px) {
          .navbar .container-fluid {
            padding-left:16px;
            padding-right:16px;
          }
          .site-page { padding:32px 15px 56px 15px; }
          .info-hero h1 { font-size:32px; }
          .info-hero p { font-size:16px; }
          .info-grid,
          .info-stat-row,
          .flow-explainer { grid-template-columns:1fr; }
          .flow-explainer-step:not(:last-child)::after { display:none; }
        }

        /* ===================================================
           FINAL WEBSITE-READINESS: ACCESSIBILITY + RESPONSIVE
           =================================================== */

        .skip-link {
          position:fixed;
          left:12px;
          top:-60px;
          z-index:5000;
          padding:10px 14px;
          border-radius:8px;
          background:#F4F7F8;
          color:#0A0D0E !important;
          font-weight:700;
          transition:top .15s ease;
        }

        .skip-link:focus { top:12px; }

        button:focus-visible,
        a:focus-visible,
        input:focus-visible,
        textarea:focus-visible,
        select:focus-visible,
        .selectize-input.focus {
          outline:3px solid rgba(120,203,235,.72) !important;
          outline-offset:2px;
        }

        .deployment-note {
          max-width:980px;
          margin:14px auto 0 auto;
          color:#91A0A5;
          font-size:13px;
          line-height:1.5;
        }

        #health_network { width:100% !important; }

        @media (max-width:980px) {
          .site-explore-page { padding-left:12px; padding-right:12px; }
          .network-wrapper { min-height:900px; }
          #health_network { height:900px !important; }
          .flow-key {
            top:10px;
            left:10px;
            right:70px;
            width:auto;
            padding:12px 13px;
          }
          .flow-key-title { font-size:12px; }
          .flow-key-path { font-size:13px; gap:4px; }
          .flow-key-note { font-size:12px; }
          .color-key { left:10px; bottom:10px; width:260px; }
        }

        @media (max-width:600px) {
          .container-fluid { padding-left:10px; padding-right:10px; }
          .title-box { padding-top:18px; }
          .title-box h1 { font-size:32px; }
          .mission-line { font-size:16px; padding:0 6px; }
          .subtitle { font-size:15px; padding:0 5px; }
          .medical-disclaimer-banner { margin:12px 2px 16px 2px; font-size:13px; }
          .graph-search-area { margin-top:14px; }
          .graph-search-label { font-size:12px; }
          .graph-search-input .selectize-input { min-height:48px; font-size:14px; }
          .network-wrapper { min-height:740px; margin-top:10px; }
          #health_network { height:740px !important; }
          .network-box { border-radius:14px; }
          .flow-key {
            top:8px;
            left:8px;
            right:58px;
            padding:10px;
            max-height:170px;
            overflow:hidden;
          }
          .flow-key-path { gap:4px; line-height:1.25; }
          .flow-key-step { padding:3px 6px; font-size:11.5px; }
          .flow-key-arrow { font-size:11px; }
          .flow-key-note { margin-top:5px; font-size:11.5px; }
          .color-key {
            left:8px;
            bottom:8px;
            width:205px;
            padding:10px 11px;
          }
          .color-key-title { font-size:12px; margin-bottom:6px; }
          .color-key-grid { grid-template-columns:1fr 1fr; gap:5px 8px; }
          .color-key-item { font-size:11.5px; gap:6px; }
          .key-dot { width:10px; height:10px; flex-basis:10px; }
          .map-controls { right:8px; bottom:8px; }
          .map-control-btn { width:44px; height:44px; }
          .side-panel {
            position:fixed;
            top:auto;
            left:0;
            right:0;
            bottom:0;
            width:100%;
            max-width:none;
            max-height:64vh;
            padding:18px 17px calc(20px + env(safe-area-inset-bottom));
            border-radius:18px 18px 0 0;
            border-left:0;
            border-right:0;
            border-bottom:0;
          }
          .side-panel-title { font-size:24px; }
          .panel-description { font-size:14px !important; }
          .edge-tooltip-card, .node-tooltip-card { width:290px; max-width:calc(100vw - 30px); }
          .feedback-form, .admin-builder { padding:17px; }
          .info-hero h1 { font-size:29px; }
          .info-hero p { font-size:15.5px; }
        }

      ")
    )
  ),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "redesign.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('revealNetwork', function() {
        var target = document.getElementById('health-network-section');
        if (!target) return;
        var reduced = window.matchMedia &&
          window.matchMedia('(prefers-reduced-motion: reduce)').matches;
        target.scrollIntoView({ behavior: reduced ? 'auto' : 'smooth', block: 'start' });
      });
    "))
  ),

  tags$a(class = "skip-link", href = "#main-explore-content", "Skip to Main Content"),

  navbarPage(
    title = "The Health Web",
    windowTitle = "The Health Web — Health Information in Context",
    id = "main_site_nav",
    inverse = TRUE,
    collapsible = TRUE,
    fluid = TRUE,

    tabPanel(
      "Explore",
      value = "explore",
      div(
        id = "main-explore-content",
        class = "site-explore-page",
        div(
          class = "medical-disclaimer-banner",
          tags$strong("Educational information only. "),
          "This project is a collection and visualization of public-health and medical sources. It is not medical advice, diagnosis, or treatment from trained medical professionals. For personal medical decisions, talk with a qualified health professional."
        ),
        div(
          class = "health-editorial-hero",
          div(class = "health-kicker", "An interactive atlas of public health"),
          h1("Health is connected to ", tags$em("everything.")),
          p(
            class = "health-lede",
            "Where we live, what we breathe, the opportunities around us — all of it shapes health. Follow the connections between daily life, social conditions, and health outcomes."
          ),
          div(
            class = "health-journey",
            div(class = "health-journey-label", "Choose a thread to follow"),
            div(
              class = "health-journey-grid",
              actionButton(
                "journey_food", 
                label = tagList(
                  span(class = "health-journey-no", "01 / EVERYDAY LIFE"),
                  span(class = "health-journey-name", "Food insecurity"),
                  span(class = "health-journey-arrow", "↗")
                ),
                class = "health-journey-card"
              ),
              actionButton(
                "journey_diabetes",
                label = tagList(
                  span(class = "health-journey-no", "02 / HEALTH CONDITIONS"),
                  span(class = "health-journey-name", "Type 2 diabetes"),
                  span(class = "health-journey-arrow", "↗")
                ),
                class = "health-journey-card"
              ),
              actionButton(
                "journey_asthma",
                label = tagList(
                  span(class = "health-journey-no", "03 / OUR ENVIRONMENT"),
                  span(class = "health-journey-name", "Asthma"),
                  span(class = "health-journey-arrow", "↗")
                ),
                class = "health-journey-card"
              )
            ),
            p(class = "health-editorial-note",
              "Each starting point reveals influences, pathways, and outcomes in the network below.")
          )
        ),
        div(
          id = "health-network-section",
          class = "health-explore-intro",
          div(
            h2("Explore the connections"),
            p("Start with a topic above or search the network. Select any concept or relationship to learn why it matters and where the evidence comes from.")
          )
        ),

        # ==========================================================
        # NEW SEARCH AREA
        # ==========================================================
  
        div(
    
          class = "graph-search-area",
    
          div(
            class = "graph-search-label",
            "Search the Health Network"
          ),
    
          div(
      
            class = "graph-search-row",
      
            div(
        
              class = "graph-search-input",
        
              selectizeInput(
                "node_search",
                label = NULL,
                choices = c(
                  "Search a condition, symptom, risk factor, or determinant..." = "",
                  search_choices
                ),
                selected = "",
                options =
                  list(
                    placeholder =
                      "Search a condition, symptom, risk factor, or determinant...",
                    allowEmptyOption = TRUE
                  )
              )
            ),
      
            actionButton(
              "show_full_network",
              "Clear Focus",
              class = "full-network-btn"
            ),

            if (UPSTREAM_ADMIN_MODE) {
              tagList(
                actionButton(
                  "run_qa",
                  "Run QA check",
                  class = "qa-button"
                ),
                actionButton(
                  "open_discovery_inbox",
                  "Discovery Inbox",
                  class = "qa-button"
                ),
                if (AUTO_EXPANSION_AVAILABLE) {
                  actionButton(
                    "open_expansion_queue",
                    "Expansion Queue",
                    class = "qa-button"
                  )
                }
              )
            }
          ),
    
          div(
            class = "graph-mode-status",
            textOutput(
              "graph_mode_text",
              inline = TRUE
            )
          )
        ),
  
  
        div(
          class = "hint",
          p(
            paste(
              "Read the network roughly left to right: upstream context → pathways → health conditions → outcomes.",
              "Search or click a concept to highlight direct upstream influences and downstream effects.",
              "Hover over any arrow to understand the connection."
            )
          )
        ),
  
        div(
          class = "focus-label",
          textOutput("focus_text")
        ),
  
        actionButton(
          "reset_view",
          "Return to Full Network",
          class = "reset-button"
        ),
  
        div(
    
          class = "network-wrapper",

          div(
            class = "flow-key",
            div(class = "flow-key-title", "How to Read the Map"),
            div(
              class = "flow-key-path",
              span(class = "flow-key-step", "1  Upstream Context"),
              span(class = "flow-key-arrow", "→"),
              span(class = "flow-key-step", "2  Intermediate Pathways"),
              span(class = "flow-key-arrow", "→"),
              span(class = "flow-key-step", "3  Health Conditions"),
              span(class = "flow-key-arrow", "→"),
              span(class = "flow-key-step", "4  Outcomes")
            ),
            div(
              class = "flow-key-note",
              span(class = "flow-key-blue", "Blue arrows"),
              " flow into the selected concept; ",
              span(class = "flow-key-gold", "gold arrows"),
              " flow out. Moving dots travel downstream along connections; hover or click an arrow to pause its flow. Drag the background to move around; nodes stay fixed."
            )
          ),

          div(
      
            class = "network-box",

            div(
              id = "network_loading",
              class = "network-loading",
              div(class = "network-loading-card", "Building the health network…")
            ),
      
            visNetworkOutput(
              "health_network",
              height = "1200px"
            )
          ),
    
    
          # ========================================================
          # COLOR KEY
          # ========================================================
    
          div(
      
            class = "color-key map-glass",
      
            div(
              class = "color-key-title",
              "Network Key"
            ),
      
            div(
        
              class = "color-key-grid",
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-structural"
                ),
                span("Structural")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-social"
                ),
                span("Social")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-environment"
                ),
                span("Environment")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-behavioral"
                ),
                span("Behavior")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-biological"
                ),
                span("Biology")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-disease"
                ),
                span("Disease")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-symptom"
                ),
                span("Symptoms")
              ),
        
              div(
                class = "color-key-item",
                span(
                  class = "key-dot key-complication"
                ),
                span("Complications")
              )
            )
          ),
    
    
          # ========================================================
          # CUSTOM ZOOM CONTROLS
          # ========================================================
    
          div(
      
            class = "map-controls map-glass",
      
            tags$button(
              id = "zoom_in_btn",
              class = "map-control-btn",
              `type` = "button",
              title = "Zoom in",
        
              tags$span(
                class = "zoom-symbol",
                "+"
              )
            ),
      
      
            tags$button(
              id = "zoom_out_btn",
              class = "map-control-btn",
              `type` = "button",
              title = "Zoom out",
        
              tags$span(
                class = "zoom-symbol",
                "−"
              )
            ),
      
      
            div(
              class = "control-divider"
            ),
      
      
            tags$button(
              id = "map_home_btn",
              class = "map-control-btn",
              `type` = "button",
              title = "Return to Full Network",
        
              tags$span(
                class = "home-symbol",
                "⌂"
              )
            )
          ),
    
    
          uiOutput(
            "side_panel"
          )
        )
      )
    ),

    tabPanel(
      "About This Project",
      value = "about",
      div(
        class = "site-page",
        uiOutput("about_public_ui")
      )
    ),

    tabPanel(
      "How It Works",
      value = "how",
      div(
        class = "site-page",
        div(
          class = "info-hero",
          div(class = "info-eyebrow", "How to use the network"),
          h1("Follow Health From Context to Outcome"),
          p(
            "The map is organized as a directional system. Read it roughly from upstream influences on the left toward pathways, diagnosed conditions, and downstream outcomes on the right."
          )
        ),
        div(
          class = "flow-explainer",
          div(class = "flow-explainer-step", strong("Upstream context"), span("Social, structural, and environmental conditions and exposures.")),
          div(class = "flow-explainer-step", strong("Intermediate pathways"), span("Behaviors, biological mechanisms, and modifiable risk states.")),
          div(class = "flow-explainer-step", strong("Health conditions"), span("Diagnosed diseases and chronic conditions.")),
          div(class = "flow-explainer-step", strong("Outcomes"), span("Symptoms, complications, acute events, and downstream effects."))
        ),
        div(
          class = "info-grid",
          div(
            class = "info-card",
            h3("Nodes"),
            p("Each node represents one concept. Node color describes the kind of concept — for example social, environmental, behavioral, biological, disease, symptom, or complication.")
          ),
          div(
            class = "info-card",
            h3("Arrows"),
            p("Arrows show the direction of the mapped relationship. Click or hover an arrow to read why the concepts are linked, how the pathway works, supporting statistics when appropriate, and the evidence source.")
          ),
          div(
            class = "info-card",
            h3("Search and Focus"),
            p("Searching or clicking a concept keeps the full network in place but fades unrelated information. Blue arrows flow into the selected concept; gold arrows flow out toward downstream effects.")
          ),
          div(
            class = "info-card",
            h3("ZIP-Code View"),
            p("For concepts with matching public datasets, a ZIP search uses CDC PLACES and U.S. Census ACS data to show local measures or clearly labeled contextual indicators. It does not assume that a ZIP-level statistic explains an individual's health.")
          ),
          div(
            class = "info-card",
            h3("Solutions and Management"),
            p("Concept pages include prevention, management, health-care, community, or structural actions when evidence supports them. The type of action depends on the node rather than forcing every concept into the same template.")
          ),
          div(
            class = "info-card",
            h3("A Relationship Is Not Always Causation"),
            p("Some arrows represent well-established causal mechanisms, while others represent consistent associations or multifactorial pathways. The evidence text and source should be used to interpret the strength and meaning of each connection.")
          )
        ),
        div(
          class = "info-card info-wide",
          h3("Medical-Information Disclaimer"),
          p("The Health Web is an educational project assembled from cited public-health, government, and research sources. It is not created by trained medical professionals and does not provide individualized medical advice, diagnosis, or treatment. If you have a medical concern, use this project as a starting point for questions — not as a substitute for professional care.")
        )
      )
    ),

    tabPanel(
      "Evidence & Sources",
      value = "evidence",
      div(
        class = "site-page",
        div(
          class = "info-hero",
          div(class = "info-eyebrow", "Evidence and transparency"),
          h1("Every Connection Should Be Explainable"),
          p(
            "The network is designed so users can move beyond the arrow itself and inspect the reasoning, evidence strength, quantitative context when appropriate, and original source."
          )
        ),
        div(
          class = "info-stat-row",
          div(class = "info-stat", span(class = "info-stat-value", as.character(nrow(nodes))), span(class = "info-stat-label", "Concepts")),
          div(class = "info-stat", span(class = "info-stat-value", as.character(nrow(relationships))), span(class = "info-stat-label", "Relationships")),
          div(class = "info-stat", span(class = "info-stat-value", as.character(nrow(interventions))), span(class = "info-stat-label", "Solution / management entries")),
          div(class = "info-stat", span(class = "info-stat-value", as.character(nrow(sources))), span(class = "info-stat-label", "Source records"))
        ),
        div(
          class = "info-grid",
          div(
            class = "info-card",
            h3("Source Priorities"),
            div(class = "source-family", strong("CDC and other public-health agencies"), p("Population measures, chronic-disease definitions, risk factors, prevention guidance, and local PLACES data.")),
            div(class = "source-family", strong("NIH institutes and clinical evidence"), p("Disease mechanisms, management, complications, and evidence-based prevention or treatment context.")),
            div(class = "source-family", strong("Peer-reviewed research"), p("Systematic reviews, meta-analyses, guidelines, and high-quality studies when agency guidance does not fully answer a relationship question.")),
            div(class = "source-family", strong("U.S. Census Bureau"), p("Local social and economic context from the American Community Survey."))
          ),
          div(
            class = "info-card",
            h3("Evidence Principles"),
            tags$ul(
              tags$li("Prefer direct, authoritative evidence over filler statistics."),
              tags$li("Do not present an association as proof of causation."),
              tags$li("Use relationship-specific numbers only when they are interpretable and reasonably generalizable."),
              tags$li("Label proxy or contextual ZIP measures instead of presenting them as direct local prevalence."),
              tags$li("Withhold suspicious live statistics when QA identifies a true failure."),
              tags$li("Link users to the underlying evidence wherever possible.")
            )
          )
        ),
        div(
          class = "info-card info-wide",
          h3("Important Limitation"),
          p(
            "Human health is complex. Relationships can vary by age, population, exposure intensity, disease subtype, time period, and many other factors. The map is a structured educational model of evidence — not a complete causal model of an individual person's health."
          )
        )
      )
    ),

    tabPanel(
      "Feedback",
      value = "feedback",
      div(
        class = "site-page",
        div(
          class = "info-hero",
          div(class = "info-eyebrow", "Help improve the project"),
          h1("Feedback & Recommendations"),
          p("Spot something confusing, missing, or worth adding? Send a note. Suggestions about the network, evidence, design, local resources, or new concepts are all welcome.")
        ),
        div(
          class = "feedback-form",
          textInput("feedback_name", "Name (optional)", placeholder = "Your name"),
          textInput("feedback_email", "Email (optional)", placeholder = "you@example.com"),
          selectInput(
            "feedback_category",
            "What is this about?",
            choices = c(
              "General feedback",
              "Possible error or inaccurate information",
              "Design / usability",
              "Suggested condition or connection",
              "Suggested resource / intervention",
              "Other"
            )
          ),
          textAreaInput(
            "feedback_message",
            "Your feedback",
            placeholder = "Tell me what you noticed or what you would recommend...",
            rows = 7
          ),
          div(
            class = "feedback-note",
            "Please do not include private medical information, diagnoses, or other sensitive personal details. This form is for project feedback, not medical questions."
          ),
          if (UPSTREAM_PUBLIC_DEPLOYMENT && !nzchar(UPSTREAM_FEEDBACK_ENDPOINT)) {
            div(
              class = "feedback-note",
              tags$strong("Launch setup note: "),
              "The public feedback endpoint has not been connected yet, so submissions are temporarily disabled on this deployment."
            )
          },
          actionButton("submit_feedback", "Send Feedback", class = "qa-button"),
          uiOutput("feedback_status_ui")
        )
      )
    ),

    if (UPSTREAM_ADMIN_MODE) {
      tabPanel(
        "Admin",
        value = "admin",
        div(
          class = "site-page",
          div(
            class = "info-hero",
            div(class = "info-eyebrow", "Admin tools"),
            h1("Site Editor"),
            p("Edit public-facing site content and review feedback without changing app.R.")
          ),
          div(
            class = "admin-builder",
            h2("Edit About This Project"),
            p("Changes are saved to data/site_content.csv and appear on the About tab immediately. For the public website, commit and push that CSV with the rest of your site update."),
            textAreaInput("admin_about_intro", "Opening introduction", value = "", rows = 3),
            textAreaInput("admin_about_sdoh", "Social Determinants of Health paragraph", value = "", rows = 4),
            textAreaInput("admin_about_motivation", "Why this interested you", value = "", rows = 4),
            textAreaInput("admin_about_project", "What the project does", value = "", rows = 4),
            textAreaInput("admin_about_closing", "Closing line", value = "", rows = 3),
            actionButton("save_about_content", "Save About Page", class = "qa-button"),
            uiOutput("about_save_status_ui")
          ),
          div(
            class = "admin-builder",
            h2("Deployment Readiness"),
            p("Public deployment safety is controlled by environment variables rather than code edits."),
            tags$ul(
              tags$li(paste0("Public deployment guard: ", if (UPSTREAM_PUBLIC_DEPLOYMENT) "ON" else "OFF (local development)")),
              tags$li(paste0("Census API configured: ", if (nzchar(CENSUS_API_KEY)) "YES" else "NO")),
              tags$li(paste0("Durable feedback endpoint configured: ", if (nzchar(UPSTREAM_FEEDBACK_ENDPOINT)) "YES" else "NO")),
              tags$li(paste0("API cache duration: ", round(UPSTREAM_CACHE_TTL_SECONDS / 3600, 1), " hours"))
            ),
            div(class = "deployment-note", "For a public host, set UPSTREAM_PUBLIC_DEPLOYMENT=true and UPSTREAM_ADMIN_MODE=false. Never store API keys in app.R or GitHub.")
          ),
          div(
            class = "admin-builder",
            h2("Feedback Inbox"),
            p("Submissions from the Feedback tab appear here while this prototype stores them locally."),
            tableOutput("feedback_admin_table")
          )
        )
      )
    }
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  selected_type <- reactiveVal(NULL)
  selected_id <- reactiveVal(NULL)
  searched_zip <- reactiveVal(NULL)
  live_local_data <- reactiveVal(NULL)
  qa_selected_issue <- reactiveVal(NULL)
  qa_reviewed_issues <- reactiveVal(character())
  discovery_candidates_rv <- reactiveVal(ensure_candidate_columns(discovery_candidates))
  discovery_watchlist_rv <- reactiveVal(discovery_watchlist)
  discovery_last_scan <- reactiveVal(NULL)
  auto_condition_queue_rv <- reactiveVal(uh_auto_ensure_queue(auto_condition_queue))
  auto_expansion_last_run <- reactiveVal(NULL)
  auto_expansion_busy <- reactiveVal(FALSE)
  auto_selected_package <- reactiveVal(NULL)
  
  graph_mode <- reactiveVal("full")
  graph_center <- reactiveVal(NULL)
  site_content_rv <- reactiveVal(site_content)
  feedback_rv <- reactiveVal(feedback_submissions)
  feedback_status_rv <- reactiveVal(NULL)
  about_save_status_rv <- reactiveVal(NULL)
  
  
  # ==========================================================
  # SITE CONTENT / FEEDBACK
  # ==========================================================

  site_value <- function(df, key, fallback = "") {
    idx <- match(key, df$key)
    if (is.na(idx)) return(fallback)
    as.character(df$value[idx])
  }

  output$about_public_ui <- renderUI({
    content <- site_content_rv()
    div(
      class = "info-hero",
      div(class = "info-eyebrow", "About the project"),
      h1("Why I Made This"),
      p(tags$b(site_value(content, "about_intro"))),
      p(site_value(content, "about_sdoh")),
      p(site_value(content, "about_motivation")),
      p(site_value(content, "about_project")),
      p(tags$b(site_value(content, "about_closing"))),
      div(
        class = "medical-disclaimer-banner",
        tags$strong("Important: "),
        "This project is educational. It compiles and visualizes information from cited sources, but it is not medical advice and was not created by trained medical professionals. It should not replace care from a qualified health professional."
      )
    )
  })

  observe({
    req(UPSTREAM_ADMIN_MODE)
    content <- site_content_rv()
    updateTextAreaInput(session, "admin_about_intro", value = site_value(content, "about_intro"))
    updateTextAreaInput(session, "admin_about_sdoh", value = site_value(content, "about_sdoh"))
    updateTextAreaInput(session, "admin_about_motivation", value = site_value(content, "about_motivation"))
    updateTextAreaInput(session, "admin_about_project", value = site_value(content, "about_project"))
    updateTextAreaInput(session, "admin_about_closing", value = site_value(content, "about_closing"))
  })

  if (UPSTREAM_ADMIN_MODE) {
    observeEvent(input$save_about_content, {
      updated <- data.frame(
        key = c("about_intro", "about_sdoh", "about_motivation", "about_project", "about_closing"),
        value = c(
          input$admin_about_intro,
          input$admin_about_sdoh,
          input$admin_about_motivation,
          input$admin_about_project,
          input$admin_about_closing
        ),
        stringsAsFactors = FALSE
      )

      result <- tryCatch({
        write.csv(updated, SITE_CONTENT_PATH, row.names = FALSE, fileEncoding = "UTF-8")
        site_content_rv(updated)
        TRUE
      }, error = function(e) e)

      if (isTRUE(result)) {
        about_save_status_rv(list(ok = TRUE, message = "Saved. The About page has been updated."))
      } else {
        about_save_status_rv(list(ok = FALSE, message = paste("Could not save:", conditionMessage(result))))
      }
    })
  }

  output$about_save_status_ui <- renderUI({
    status <- about_save_status_rv()
    if (is.null(status)) return(NULL)
    div(
      class = if (isTRUE(status$ok)) "feedback-success" else "feedback-error",
      status$message
    )
  })

  observeEvent(input$submit_feedback, {
    message_text <- trimws(input$feedback_message)
    if (!nzchar(message_text)) {
      feedback_status_rv(list(ok = FALSE, message = "Please enter a message before sending feedback."))
      return()
    }

    payload <- list(
      submitted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      name = trimws(input$feedback_name),
      email = trimws(input$feedback_email),
      category = as.character(input$feedback_category),
      message = message_text,
      project = "The Health Web"
    )

    # Public hosting: use a durable external form endpoint when configured.
    # This avoids pretending that an app-container CSV is permanent storage.
    if (UPSTREAM_PUBLIC_DEPLOYMENT) {
      if (!nzchar(UPSTREAM_FEEDBACK_ENDPOINT)) {
        feedback_status_rv(list(
          ok = FALSE,
          message = "Feedback is temporarily unavailable while the public submission service is being connected."
        ))
        return()
      }

      feedback_status_rv(list(ok = TRUE, message = "Sending feedback…"))
      session$sendCustomMessage(
        "postFeedback",
        list(endpoint = UPSTREAM_FEEDBACK_ENDPOINT, payload = payload)
      )
      return()
    }

    # Local/admin development fallback. This is convenient while building, but
    # public deployments deliberately do not rely on the CSV for persistence.
    new_row <- data.frame(
      submitted_at = payload$submitted_at,
      name = payload$name,
      email = payload$email,
      category = payload$category,
      message = payload$message,
      status = "NEW",
      stringsAsFactors = FALSE
    )

    current <- feedback_rv()
    if (nrow(current) == 0 || !all(names(new_row) %in% names(current))) {
      updated <- new_row
    } else {
      updated <- rbind(current[, names(new_row), drop = FALSE], new_row)
    }

    result <- tryCatch({
      write.csv(updated, FEEDBACK_PATH, row.names = FALSE, fileEncoding = "UTF-8")
      feedback_rv(updated)
      TRUE
    }, error = function(e) e)

    if (isTRUE(result)) {
      feedback_status_rv(list(ok = TRUE, message = "Thanks — your feedback was saved locally."))
      updateTextInput(session, "feedback_name", value = "")
      updateTextInput(session, "feedback_email", value = "")
      updateTextAreaInput(session, "feedback_message", value = "")
    } else {
      feedback_status_rv(list(ok = FALSE, message = "Feedback could not be saved. Please try again later."))
    }
  })

  observeEvent(input$feedback_remote_result, {
    result <- input$feedback_remote_result
    if (isTRUE(result$ok)) {
      feedback_status_rv(list(ok = TRUE, message = "Thanks — your feedback was sent."))
      updateTextInput(session, "feedback_name", value = "")
      updateTextInput(session, "feedback_email", value = "")
      updateTextAreaInput(session, "feedback_message", value = "")
    } else {
      feedback_status_rv(list(
        ok = FALSE,
        message = if (!is.null(result$message) && nzchar(result$message)) result$message else "Feedback could not be sent. Please try again later."
      ))
    }
  }, ignoreInit = TRUE)

  output$feedback_status_ui <- renderUI({
    status <- feedback_status_rv()
    if (is.null(status)) return(NULL)
    div(
      class = if (isTRUE(status$ok)) "feedback-success" else "feedback-error",
      status$message
    )
  })

  output$feedback_admin_table <- renderTable({
    req(UPSTREAM_ADMIN_MODE)
    df <- feedback_rv()
    if (nrow(df) == 0) {
      return(data.frame(Message = "No feedback submissions yet."))
    }
    show <- df[nrow(df):1, , drop = FALSE]
    names(show) <- c("Submitted", "Name", "Email", "Category", "Message", "Status")
    head(show, 100)
  }, striped = TRUE, bordered = TRUE, spacing = "s")


  # ==========================================================
  # GRAPH PROXY HELPERS
  #
  # IMPORTANT:
  # Search now swaps in a fully styled subset of the graph
  # instead of hiding/unhiding existing nodes. This preserves
  # every node's custom background, border, hover, and font
  # styling when the network changes.
  # ==========================================================

  # Build a full-network focus view. Nothing disappears: the selected
  # concept and its nearby pathways become prominent while the rest of
  # the graph fades into context.
  build_focus_graph <- function(center_id, degrees = 2) {

    one_hop <- get_neighborhood(center_id, 1)
    two_hop <- get_neighborhood(center_id, degrees)

    focused_nodes <- vis_nodes
    focused_edges <- vis_edges

    is_secondary <- focused_nodes$id %in% setdiff(two_hop, one_hop)
    is_context <- !(focused_nodes$id %in% two_hop)

    incoming_primary <-
      focused_edges$to == center_id

    outgoing_primary <-
      focused_edges$from == center_id

    incoming_nodes <-
      unique(
        focused_edges$from[incoming_primary]
      )

    outgoing_nodes <-
      unique(
        focused_edges$to[outgoing_primary]
      )

    # The whole graph remains visible. Unrelated concepts become quiet context.
    focused_nodes$font.color[is_context] <- "rgba(224,232,235,0.12)"
    focused_nodes$color.background[is_context] <- "rgba(35,42,45,0.08)"
    focused_nodes$color.border[is_context] <- "rgba(122,145,153,0.08)"
    focused_nodes$color.highlight.background[is_context] <- "rgba(60,70,74,0.14)"
    focused_nodes$color.highlight.border[is_context] <- "rgba(145,170,180,0.16)"
    focused_nodes$color.hover.background[is_context] <- "rgba(60,70,74,0.14)"
    focused_nodes$color.hover.border[is_context] <- "rgba(145,170,180,0.16)"
    focused_nodes$borderWidth[is_context] <- 0.7

    # Two-hop context stays readable, but secondary to the direct pathway.
    focused_nodes$font.color[is_secondary] <- "rgba(228,236,238,0.52)"
    focused_nodes$color.background[is_secondary] <- "rgba(42,50,53,0.42)"
    focused_nodes$color.border[is_secondary] <- "rgba(135,157,165,0.42)"
    focused_nodes$borderWidth[is_secondary] <- 1.0

    # Direct upstream influences and downstream effects get clearer borders.
    is_incoming_node <- focused_nodes$id %in% incoming_nodes
    is_outgoing_node <- focused_nodes$id %in% outgoing_nodes

    focused_nodes$borderWidth[is_incoming_node | is_outgoing_node] <- 2.0

    # Give the exact focus concept the strongest ring.
    is_center <- focused_nodes$id == center_id
    focused_nodes$borderWidth[is_center] <- 4.0
    focused_nodes$borderWidthSelected[is_center] <- 4.8

    edge_related <-
      focused_edges$from %in% two_hop &
      focused_edges$to %in% two_hop

    # Background connections nearly disappear, but remain available as context.
    focused_edges$color <- "rgba(108,132,140,0.055)"
    focused_edges$width <- 0.72

    # Related second-degree pathways are visible enough to preserve context.
    focused_edges$color[edge_related] <- "rgba(115,151,164,0.28)"
    focused_edges$width[edge_related] <- 1.55

    # Direct incoming influences use cool blue.
    focused_edges$color[incoming_primary] <- "rgba(120,203,235,0.98)"
    focused_edges$width[incoming_primary] <- 4.2

    # Direct downstream effects use warm pale gold so direction is immediately legible.
    focused_edges$color[outgoing_primary] <- "rgba(239,205,132,0.98)"
    focused_edges$width[outgoing_primary] <- 4.2

    list(nodes = focused_nodes, edges = focused_edges)
  }



  show_full_graph <- function(
    focus_id = NULL,
    focus_scale = 0.85
  ) {

    proxy <- visNetworkProxy("health_network")

    # Update styling in place instead of replacing the network dataset.
    # This preserves the stable node coordinates created by the initial
    # layout, so returning home/full network never collapses or overlaps
    # the graph.
    visUpdateNodes(
      proxy,
      nodes = vis_nodes
    )

    visUpdateEdges(
      proxy,
      edges = vis_edges
    )

    graph_mode("full")
    graph_center(NULL)

    session$onFlushed(
      function() {
        visFit(
          proxy,
          animation = list(
            duration = 500,
            easingFunction = "easeInOutQuad"
          )
        )
      },
      once = TRUE
    )
  }


  focus_node_network <- function(
    center_id,
    degrees = 2
  ) {

    focused <- build_focus_graph(center_id, degrees)
    proxy <- visNetworkProxy("health_network")

    # Focus is purely a visual emphasis change. Keep the underlying
    # node coordinates untouched so the user's mental map never moves.
    visUpdateNodes(
      proxy,
      nodes = focused$nodes
    )

    visUpdateEdges(
      proxy,
      edges = focused$edges
    )

    graph_mode("focus")
    graph_center(center_id)

    # Keep the whole network in view so search changes emphasis rather
    # than replacing the user's mental map with a different mini-tree.
    session$onFlushed(
      function() {
        visFit(
          proxy,
          animation = list(
            duration = 550,
            easingFunction = "easeInOutQuad"
          )
        )
      },
      once = TRUE
    )
  }


  # ==========================================================
  # GRAPH MODE STATUS
  # ==========================================================
  
  output$graph_mode_text <-
    renderText({
      
      if (graph_mode() == "full") {
        
        return(
          paste(
            "Showing the full health network •",
            nrow(nodes),
            "concepts"
          )
        )
      }
      
      center_id <-
        graph_center()
      
      if (
        is.null(center_id) ||
        !center_id %in% nodes$node_id
      ) {
        return("")
      }
      
      center_name <-
        nodes$node_name[
          match(
            center_id,
            nodes$node_id
          )
        ]
      
      highlighted_count <-
        length(
          get_neighborhood(
            center_id,
            2
          )
        )
      
      paste0(
        "Focused on ",
        center_name,
        " • blue arrows = upstream influences • gold arrows = downstream effects • ",
        highlighted_count,
        " related concepts highlighted"
      )
    })
  
  
  # Guided entry points reuse the existing search/focus interaction.
  # Avoid parallel graph logic so future network improvements stay consistent.
  journey_topics <- c(
    journey_food = "food_insecurity",
    journey_diabetes = "t2d",
    journey_asthma = "asthma"
  )
  for (button_id in names(journey_topics)) {
    local({
      current_button <- button_id
      current_node <- unname(journey_topics[[button_id]])
      observeEvent(input[[current_button]], {
        updateSelectizeInput(session, "node_search", selected = current_node)
        selected_type("node")
        selected_id(current_node)
        searched_zip(NULL)
        live_local_data(NULL)
        focus_node_network(current_node, degrees = 2)
        session$sendCustomMessage("revealNetwork", list())
      }, ignoreInit = TRUE)
    })
  }

  # ==========================================================
  # SEARCH
  # ==========================================================
  
  observeEvent(
    input$node_search,
    {
      
      node_id <-
        input$node_search
      
      if (
        is.null(node_id) ||
        !nzchar(node_id)
      ) {
        
        show_full_graph()
        
        return()
      }
      
      if (!node_id %in% nodes$node_id) {
        return()
      }
      
      selected_type("node")
      selected_id(node_id)
      
      searched_zip(NULL)
      live_local_data(NULL)
      
      focus_node_network(
        node_id,
        degrees = 2
      )
    },
    
    ignoreInit = TRUE
  )
  
  
  # ==========================================================
  # SHOW FULL NETWORK BUTTON
  # ==========================================================
  
  observeEvent(
    input$show_full_network,
    {
      
      updateSelectizeInput(
        session,
        "node_search",
        selected = ""
      )
      
      show_full_graph()
    }
  )
  
  
  # ==========================================================
  # SCROLL HELPER
  # ==========================================================
  
  scroll_to_local_results <- function() {
    
    session$onFlushed(
      
      function() {
        
        session$sendCustomMessage(
          "scrollToLocalResults",
          list()
        )
      },
      
      once = TRUE
    )
  }
  
  
  # ==========================================================
  # QA CHECKER
  # ==========================================================

  qa_issue_key <- function(scope, row_index, row) {
    paste(
      scope,
      row_index,
      row$status,
      row$area,
      row$item,
      row$message,
      sep = "||"
    )
  }


  qa_recommendation_text <- function(row) {

    msg <- tolower(row$message)
    area <- tolower(row$area)

    if (grepl("matched no returned measure|not present in the parsed acs response", msg)) {
      return(paste(
        "Check the metric mapping in data/local_metric_mappings.csv.",
        "Confirm that the metric_key still matches the source field or measure name,",
        "then rerun the ZIP lookup."
      ))
    }

    if (grepl("matched .* measures|uses the first match", msg)) {
      return(paste(
        "Tighten the metric_key in data/local_metric_mappings.csv so it matches",
        "one and only one CDC measure. Do not choose a value manually just because",
        "it looks plausible."
      ))
    }

    if (grepl("outside|impossible|percentage|guardrail|confidence interval", msg)) {
      return(paste(
        "Inspect the raw value and the exact source measure below.",
        "If the measure is correct, verify the source definition and geography before",
        "changing any guardrail. If the measure is wrong, fix the mapping rather than",
        "editing the returned statistic."
      ))
    }

    if (grepl("domain|url|citation|source", msg) || grepl("source", area)) {
      return(paste(
        "Open the cited source and verify that it directly supports the claim or metric.",
        "If it does not, replace the citation or weaken the claim to match the evidence."
      ))
    }

    if (grepl("caus|wording|evidence|association", msg)) {
      return(paste(
        "Compare the relationship wording with the cited evidence.",
        "Use causal language only when the evidence supports causation; otherwise use",
        "association or contribution language."
      ))
    }

    if (row$status == "FAIL") {
      return(paste(
        "Treat this as a blocking issue until the underlying data, mapping, or source",
        "has been verified and corrected."
      ))
    }

    if (row$status == "REVIEW") {
      return(paste(
        "Review the underlying source or raw data before deciding whether this is valid.",
        "If verified, mark it reviewed for this session."
      ))
    }

    paste(
      "Review the underlying data or citation and confirm that the result is expected.",
      "No automatic data replacement is recommended."
    )
  }


  qa_public_block_text <- function(row) {
    if (row$area == "Live statistics" && row$status == "FAIL") {
      return("YES — this live metric is withheld from public mode until the failure is resolved.")
    }

    if (row$status == "FAIL") {
      return("Potentially. This is a blocking QA issue, but it does not map to one specific live metric automatically.")
    }

    "NO — this item is visible to the admin for review but is not automatically blocked."
  }


  qa_raw_table <- function(df, columns = NULL) {

    if (is.null(df) || nrow(df) == 0) {
      return(
        tags$div(
          class = "qa-modal-note",
          "No raw rows are available for this item."
        )
      )
    }

    if (!is.null(columns)) {
      columns <- intersect(columns, names(df))
      df <- df[, columns, drop = FALSE]
    }

    if (nrow(df) > 8) {
      df <- df[seq_len(8), , drop = FALSE]
    }

    tags$table(
      class = "qa-raw-table",
      tags$thead(
        tags$tr(
          lapply(names(df), tags$th)
        )
      ),
      tags$tbody(
        lapply(
          seq_len(nrow(df)),
          function(i) {
            tags$tr(
              lapply(
                df[i, , drop = FALSE],
                function(value) {
                  text <- if (length(value) == 0 || is.na(value)) "" else as.character(value)
                  tags$td(text)
                }
              )
            )
          }
        )
      )
    )
  }


  qa_source_detail_ui <- function(row) {

    source_rows <- sources[0, , drop = FALSE]

    if (row$item %in% sources$source_id) {
      source_rows <- sources[sources$source_id == row$item, , drop = FALSE]
    }

    if (row$item %in% relationships$relationship_id) {
      rel_sources <- relationship_sources[
        relationship_sources$relationship_id == row$item,
        ,
        drop = FALSE
      ]

      if (nrow(rel_sources) > 0) {
        source_rows <- sources[
          sources$source_id %in% rel_sources$source_id,
          ,
          drop = FALSE
        ]
      }
    }

    if (nrow(source_rows) == 0) {
      return(NULL)
    }

    tagList(
      tags$div(class = "qa-section-title", "Linked source"),
      lapply(
        seq_len(nrow(source_rows)),
        function(i) {
          src <- source_rows[i, , drop = FALSE]
          tags$div(
            class = "qa-detail-card",
            tags$div(class = "qa-detail-value", tags$b(src$organization), " — ", src$title),
            if (!is.na(src$url) && nzchar(src$url)) {
              tags$a(
                href = src$url,
                target = "_blank",
                rel = "noopener noreferrer",
                "Open source ↗"
              )
            }
          )
        }
      )
    )
  }


  qa_live_detail_ui <- function(row, local_result) {

    if (is.null(local_result) || !isTRUE(local_result$ok)) {
      return(NULL)
    }

    mappings <- local_metric_mappings[
      local_metric_mappings$node_id == local_result$node_id &
        local_metric_mappings$label == row$item,
      ,
      drop = FALSE
    ]

    if (nrow(mappings) == 0) {
      return(NULL)
    }

    mapping <- mappings[1, , drop = FALSE]
    dataset <- mapping$dataset[1]
    metric_key <- mapping$metric_key[1]

    raw_ui <- NULL

    if (dataset == "cdc" && !is.null(local_result$cdc_result) && isTRUE(local_result$cdc_result$ok)) {
      cdc <- local_result$cdc_result$data
      matches <- which(grepl(metric_key, cdc$measure, ignore.case = TRUE))
      rows <- if (length(matches) > 0) cdc[matches, , drop = FALSE] else cdc[0, , drop = FALSE]

      raw_ui <- qa_raw_table(
        rows,
        c(
          "locationname",
          "measure",
          "data_value",
          "low_confidence_limit",
          "high_confidence_limit",
          "year"
        )
      )
    }

    if (dataset == "census" && !is.null(local_result$census_result) && isTRUE(local_result$census_result$ok)) {
      census <- local_result$census_result$data
      raw_df <- data.frame(
        metric_key = metric_key,
        value = if (metric_key %in% names(census)) as.character(census[[metric_key]]) else "<missing>",
        geography = if ("place_name" %in% names(census)) as.character(census$place_name) else local_result$zip,
        stringsAsFactors = FALSE
      )
      raw_ui <- qa_raw_table(raw_df)
    }

    tagList(
      tags$div(class = "qa-section-title", "Configured mapping"),
      tags$div(
        class = "qa-detail-grid",
        tags$div(
          class = "qa-detail-card",
          tags$div(class = "qa-detail-label", "Dataset"),
          tags$div(class = "qa-detail-value", toupper(dataset))
        ),
        tags$div(
          class = "qa-detail-card",
          tags$div(class = "qa-detail-label", "Metric key / pattern"),
          tags$div(class = "qa-detail-value", metric_key)
        ),
        tags$div(
          class = "qa-detail-card",
          tags$div(class = "qa-detail-label", "Section"),
          tags$div(class = "qa-detail-value", mapping$section[1])
        ),
        tags$div(
          class = "qa-detail-card",
          tags$div(class = "qa-detail-label", "ZIP / ZCTA"),
          tags$div(class = "qa-detail-value", local_result$zip)
        )
      ),
      tags$div(class = "qa-section-title", "Raw source result"),
      raw_ui
    )
  }


  qa_row_ui <- function(row, scope, row_index) {

    status <- toupper(row$status)
    status_class <-
      switch(
        status,
        "PASS" = "qa-pass",
        "WARNING" = "qa-warning",
        "REVIEW" = "qa-review",
        "FAIL" = "qa-fail",
        "qa-review"
      )

    issue_key <- qa_issue_key(scope, row_index, row)
    reviewed <- issue_key %in% qa_reviewed_issues()

    tags$div(
      class = "qa-row",

      tags$div(
        class = "qa-row-top",

        tags$span(
          class = paste("qa-status-pill", status_class),
          status
        ),

        if (reviewed) {
          tags$span(class = "qa-reviewed-badge", "REVIEWED")
        },

        tags$span(
          class = "qa-row-item",
          row$item
        ),

        tags$span(
          class = "qa-row-area",
          row$area
        )
      ),

      tags$div(
        class = "qa-row-message",
        row$message
      ),

      if (status != "PASS") {
        tags$button(
          type = "button",
          class = "qa-investigate-btn",
          onclick = sprintf(
            "Shiny.setInputValue('qa_investigate', '%s::%s', {priority:'event'});",
            scope,
            row_index
          ),
          "Investigate"
        )
      }
    )
  }


  qa_section_ui <- function(report, title, scope) {

    if (is.null(report) || nrow(report) == 0) {
      return(
        tagList(
          tags$div(
            class = "qa-section-title",
            title
          ),
          tags$div(
            class = "qa-modal-note",
            "No QA results are available for this section."
          )
        )
      )
    }

    counts <- qa_counts(report)
    issue_indices <- which(report$status != "PASS")
    pass_indices <- which(report$status == "PASS")

    tagList(

      tags$div(
        class = "qa-section-title",
        title
      ),

      tags$div(
        class = "qa-summary-grid",

        tags$div(
          class = "qa-summary-card",
          tags$div(class = "qa-summary-number", counts[["FAIL"]]),
          tags$div(class = "qa-summary-label", "Fail")
        ),

        tags$div(
          class = "qa-summary-card",
          tags$div(class = "qa-summary-number", counts[["REVIEW"]]),
          tags$div(class = "qa-summary-label", "Review")
        ),

        tags$div(
          class = "qa-summary-card",
          tags$div(class = "qa-summary-number", counts[["WARNING"]]),
          tags$div(class = "qa-summary-label", "Warning")
        ),

        tags$div(
          class = "qa-summary-card",
          tags$div(class = "qa-summary-number", counts[["PASS"]]),
          tags$div(class = "qa-summary-label", "Pass")
        )
      ),

      if (length(issue_indices) == 0) {
        tags$div(
          class = "qa-modal-note",
          "No automated issues were detected in this section."
        )
      } else {
        tagList(
          tags$div(
            style = "color:#B8C6CA;font-size:12px;font-weight:600;margin:7px 0;",
            "Items needing attention"
          ),
          lapply(
            issue_indices,
            function(i) qa_row_ui(report[i, , drop = FALSE], scope, i)
          )
        )
      },

      if (length(pass_indices) > 0) {
        tags$details(
          style = "margin-top:12px;",
          tags$summary(
            paste0("Show ", length(pass_indices), " passing checks")
          ),
          lapply(
            pass_indices,
            function(i) qa_row_ui(report[i, , drop = FALSE], scope, i)
          )
        )
      }
    )
  }


  output$qa_report <- renderUI({

    local_result <- live_local_data()

    live_report <-
      if (
        !is.null(local_result) &&
        isTRUE(local_result$ok) &&
        !is.null(local_result$qa_report)
      ) {
        local_result$qa_report
      } else {
        NULL
      }

    tagList(

      tags$div(
        class = "qa-modal-note",
        tags$b("QA repair console: "),
        paste(
          "FAIL, REVIEW, and WARNING rows now include an Investigate button.",
          "Use it to inspect the mapping/source, see the raw live value when available,",
          "and get a recommended next action. A PASS is still not proof that a scientific claim is correct."
        )
      ),

      qa_section_ui(
        static_qa_report,
        "Database & source QA",
        "static"
      ),

      if (!is.null(live_report)) {
        qa_section_ui(
          live_report,
          paste0(
            "Most recent live-statistic QA — ZIP ",
            local_result$zip
          ),
          "live"
        )
      } else {
        tags$div(
          class = "qa-modal-note",
          paste(
            "No live ZIP statistic has been tested yet in this session.",
            "Explore a ZIP from any node, then run QA again to inspect the returned",
            "CDC/Census measure names, values, and confidence intervals."
          )
        )
      }
    )
  })


  show_qa_console <- function() {
    showModal(
      modalDialog(
        title = "The Health Web QA repair console",
        uiOutput("qa_report"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  }


  output$qa_issue_detail <- renderUI({

    selected <- qa_selected_issue()

    if (is.null(selected)) {
      return(NULL)
    }

    scope <- selected$scope
    row_index <- selected$row_index
    local_result <- live_local_data()

    report <-
      if (scope == "live" && !is.null(local_result) && !is.null(local_result$qa_report)) {
        local_result$qa_report
      } else {
        static_qa_report
      }

    if (row_index < 1 || row_index > nrow(report)) {
      return(tags$div(class = "qa-modal-note", "This QA item is no longer available."))
    }

    row <- report[row_index, , drop = FALSE]
    issue_key <- qa_issue_key(scope, row_index, row)
    reviewed <- issue_key %in% qa_reviewed_issues()

    status_class <-
      switch(
        toupper(row$status),
        "PASS" = "qa-pass",
        "WARNING" = "qa-warning",
        "REVIEW" = "qa-review",
        "FAIL" = "qa-fail",
        "qa-review"
      )

    tagList(
      tags$div(
        class = "qa-row",
        tags$div(
          class = "qa-row-top",
          tags$span(class = paste("qa-status-pill", status_class), toupper(row$status)),
          if (reviewed) tags$span(class = "qa-reviewed-badge", "REVIEWED"),
          tags$span(class = "qa-row-item", row$item),
          tags$span(class = "qa-row-area", row$area)
        ),
        tags$div(class = "qa-row-message", row$message)
      ),

      tags$div(
        class = "qa-detail-grid",
        tags$div(
          class = "qa-detail-card",
          tags$div(class = "qa-detail-label", "Public display"),
          tags$div(class = "qa-detail-value", qa_public_block_text(row))
        ),
        tags$div(
          class = "qa-detail-card",
          tags$div(class = "qa-detail-label", "Review status"),
          tags$div(
            class = "qa-detail-value",
            if (reviewed) "Reviewed for this R session" else "Not yet reviewed"
          )
        )
      ),

      tags$div(
        class = "qa-recommendation",
        tags$b("Recommended next action: "),
        qa_recommendation_text(row)
      ),

      if (scope == "live") qa_live_detail_ui(row, local_result),
      if (scope == "static") qa_source_detail_ui(row)
    )
  })



  # ==========================================================
  # AUTONOMOUS EXPANSION QUEUE — ADMIN ONLY
  #
  # The worker researches missing conditions in the background,
  # assembles a multi-node evidence package, and only asks the
  # administrator to Accept, Edit, or Reject the finished package.
  # ==========================================================

  auto_package_badge <- function(status) {
    status <- toupper(trimws(as.character(status)))
    bg <- switch(status,
      "READY"="rgba(89,194,133,.18)",
      "ACCEPTED"="rgba(86,154,255,.17)",
      "REJECTED"="rgba(150,150,150,.13)",
      "EDITING"="rgba(238,188,84,.18)",
      "rgba(255,255,255,.08)"
    )
    tags$span(style=paste0("display:inline-block;padding:3px 8px;border-radius:999px;font-size:11px;font-weight:800;",
      "background:",bg,";border:1px solid rgba(255,255,255,.13);"),status)
  }

  auto_pkg_ui <- function(row) {
    pkg <- tryCatch(uh_auto_decode(row$package_json), error=function(e) NULL)
    if (is.null(pkg)) return(NULL)
    reln <- if(is.data.frame(pkg$relationships)) nrow(pkg$relationships) else 0
    supn <- if(is.data.frame(pkg$supporting_nodes)) nrow(pkg$supporting_nodes) else 0
    intn <- if(is.data.frame(pkg$interventions)) nrow(pkg$interventions) else 0
    metric <- if(!is.null(pkg$local_metric$status) && pkg$local_metric$status=="DIRECT") pkg$local_metric$match else "No direct CDC PLACES measure automatically confirmed"
    pid <- as.character(row$package_id)
    tags$div(
      style="margin:10px 0;padding:15px;border:1px solid #2b3438;border-radius:12px;background:rgba(15,19,20,.76);",
      tags$div(style="display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-bottom:8px;",
        auto_package_badge(row$status),
        tags$span(style="font-size:11px;font-weight:800;color:#aebbc0;",paste0("QA ",row$qa_status)),
        tags$span(style="font-size:11px;color:#819096;",paste0(" • ",row$evidence_count," evidence records • generated ",row$generated_at))
      ),
      tags$div(style="font-size:18px;font-weight:800;margin-bottom:6px;",row$condition_name),
      tags$div(style="font-size:12px;color:#b7c2c6;line-height:1.5;margin-bottom:9px;",row$summary),
      tags$div(style="font-size:12px;color:#aab6bb;line-height:1.45;",
        tags$b("Package contents: "),paste0(reln," relationships • ",supn," new supporting concepts • ",intn," interventions")
      ),
      tags$div(style="font-size:12px;color:#aab6bb;line-height:1.45;margin-bottom:11px;",
        tags$b("Local data: "),metric
      ),
      tags$div(style="display:flex;gap:7px;flex-wrap:wrap;",
        tags$button(type="button",class="qa-button",onclick=sprintf("Shiny.setInputValue('auto_pkg_open','%s',{priority:'event'});",pid),"Review package"),
        if(row$status=="READY") tags$button(type="button",class="qa-button",onclick=sprintf("Shiny.setInputValue('auto_pkg_accept','%s',{priority:'event'});",pid),"Accept & publish"),
        if(row$status=="READY") tags$button(type="button",class="full-network-btn",onclick=sprintf("Shiny.setInputValue('auto_pkg_reject','%s',{priority:'event'});",pid),"Reject")
      )
    )
  }

  output$expansion_queue_ui <- renderUI({
    req(UPSTREAM_ADMIN_MODE)
    q <- uh_auto_ensure_queue(auto_condition_queue_rv())
    pending <- q[q$status %in% c("READY","EDITING"),,drop=FALSE]
    if(nrow(pending)>0) pending <- pending[order(as.integer(pending$priority),pending$generated_at),,drop=FALSE]
    cards <- if(!nrow(pending)) tags$div(class="qa-modal-note",
      "No autonomous packages are waiting right now. The background worker will keep researching missing conditions while the admin app is running.")
      else lapply(seq_len(nrow(pending)),function(i) auto_pkg_ui(pending[i,,drop=FALSE]))
    tagList(
      tags$div(class="qa-modal-note",
        tags$b("Autonomous Expansion — ADMIN. "),
        "You do not need to pick the next disease or gather the evidence. The worker researches missing conditions, builds a package, and puts the finished proposal here. Nothing becomes public until you accept it."
      ),
      tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:10px;",
        actionButton("auto_expand_now","Find the next missing condition now",class="qa-button"),
        tags$span(style="font-size:12px;color:#9ba8ad;align-self:center;",
          paste0("Background worker: ",if(UPSTREAM_AUTO_EXPANSION) "ON" else "PAUSED",
                 if(!is.null(auto_expansion_last_run())) paste0(" • last run ",format(auto_expansion_last_run(),"%Y-%m-%d %H:%M")) else "")
        )
      ),
      if(isTRUE(auto_expansion_busy())) tags$div(class="qa-modal-note","Research worker is assembling a condition package…"),
      cards
    )
  })

  show_expansion_queue <- function() {
    showModal(modalDialog(title="Autonomous Expansion Queue — ADMIN",uiOutput("expansion_queue_ui"),size="l",easyClose=TRUE,footer=modalButton("Close")))
  }

  output$auto_package_detail_ui <- renderUI({
    req(UPSTREAM_ADMIN_MODE)
    pid <- auto_selected_package(); req(pid)
    q <- auto_condition_queue_rv(); i <- match(pid,q$package_id); req(!is.na(i))
    pkg <- uh_auto_decode(q$package_json[i])
    ev <- pkg$evidence; rels <- pkg$relationships; supp <- pkg$supporting_nodes; ints <- pkg$interventions; qau <- pkg$qa
    evidence_ui <- if(is.data.frame(ev) && nrow(ev)) lapply(seq_len(nrow(ev)),function(j)
      tags$li(tags$a(href=ev$source_url[j],target="_blank",ev$source_title[j]),tags$span(style="color:#65747a;",paste0(" — ",ev$source_org[j]))))
      else tags$li("No evidence records captured.")
    rel_ui <- if(is.data.frame(rels) && nrow(rels)) lapply(seq_len(nrow(rels)),function(j)
      tags$li(paste0(rels$from[j]," → ",rels$to[j]," [",rels$evidence_strength[j],"] — ",rels$short_explanation[j])))
      else tags$li("No relationships met the automatic evidence threshold.")
    supp_ui <- if(is.data.frame(supp) && nrow(supp)) lapply(seq_len(nrow(supp)),function(j)
      tags$li(paste0(supp$name[j]," (",supp$node_type[j],", supported in ",supp$evidence_docs[j]," evidence records)")))
      else tags$li("No new supporting concepts proposed.")
    int_ui <- if(is.data.frame(ints) && nrow(ints)) lapply(seq_len(nrow(ints)),function(j)
      tags$li(paste0(ints$intervention_name[j]," — ",ints$description[j])))
      else tags$li("No intervention passed the automatic keyword/evidence screen.")
    qa_ui <- if(is.data.frame(qau) && nrow(qau)) lapply(seq_len(nrow(qau)),function(j)
      tags$li(tags$b(paste0(qau$status[j],": ")),qau$message[j])) else NULL
    tags$div(
      class="proposal-review-light",
      tags$h3(pkg$condition_name),
      tags$p(tags$b("Definition draft: "),pkg$description),
      tags$p(tags$b("Why it matters: "),pkg$why_it_matters),
      tags$h4("Proposed graph relationships"),tags$ul(rel_ui),
      tags$h4("New supporting concepts"),tags$ul(supp_ui),
      tags$h4("Interventions"),tags$ul(int_ui),
      tags$h4("Local statistic mapping"),
      tags$p(if(!is.null(pkg$local_metric$status) && pkg$local_metric$status=="DIRECT") paste0("Direct CDC PLACES match: ",pkg$local_metric$match)
             else paste0("No direct measure automatically published. Status: ",pkg$local_metric$status)),
      tags$h4("Automated QA"),tags$ul(qa_ui),
      tags$h4("Evidence bundle"),tags$ul(evidence_ui),
      tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin-top:12px;",
        if(q$status[i]=="READY") actionButton("auto_accept_selected","Accept entire package & publish",class="qa-button"),
        if(q$status[i]=="READY") actionButton("auto_edit_selected","Edit before publishing",class="full-network-btn"),
        if(q$status[i]=="READY") actionButton("auto_reject_selected","Reject package",class="full-network-btn")
      )
    )
  })

  show_auto_package <- function(pid) {
    auto_selected_package(pid)
    showModal(modalDialog(title="Autonomous Condition Package — ADMIN",uiOutput("auto_package_detail_ui"),size="l",easyClose=TRUE,footer=modalButton("Close")))
  }

  output$auto_package_edit_ui <- renderUI({
    pid <- auto_selected_package(); req(pid)
    q <- auto_condition_queue_rv(); i <- match(pid,q$package_id); req(!is.na(i))
    pkg <- uh_auto_decode(q$package_json[i])
    reltxt <- if(is.data.frame(pkg$relationships) && nrow(pkg$relationships)) paste(apply(pkg$relationships,1,function(r) paste(r["from"],r["to"],r["relationship_type"],r["evidence_strength"],r["short_explanation"],sep=" | ")),collapse="\n") else ""
    suptxt <- if(is.data.frame(pkg$supporting_nodes) && nrow(pkg$supporting_nodes)) paste(apply(pkg$supporting_nodes,1,function(r) paste(r["name"],r["node_type"],r["role"],r["badge"],r["evidence_docs"],sep=" | ")),collapse="\n") else ""
    inttxt <- if(is.data.frame(pkg$interventions) && nrow(pkg$interventions)) paste(apply(pkg$interventions,1,function(r) paste(r["intervention_name"],r["intervention_level"],r["actor"],r["description"],sep=" | ")),collapse="\n") else ""
    tags$div(class="proposal-review-light",
      textInput("auto_edit_name","Condition name",value=pkg$condition_name),
      textAreaInput("auto_edit_description","Description",value=pkg$description,rows=4),
      textAreaInput("auto_edit_why","Why it matters",value=pkg$why_it_matters,rows=4),
      textAreaInput("auto_edit_supporting","Supporting concepts — name | node_type | role | badge | evidence docs",value=suptxt,rows=7),
      textAreaInput("auto_edit_relationships","Relationships — from | to | type | strength | explanation",value=reltxt,rows=10),
      textAreaInput("auto_edit_interventions","Interventions — name | level | actor | description",value=inttxt,rows=7),
      actionButton("auto_save_edits","Save edits and re-run package QA",class="qa-button")
    )
  })

  run_autonomous_expansion_once <- function(notify=FALSE) {
    if(!UPSTREAM_ADMIN_MODE || isTRUE(auto_expansion_busy())) return(invisible(NULL))
    auto_expansion_busy(TRUE); on.exit(auto_expansion_busy(FALSE),add=TRUE)
    result <- tryCatch(uh_auto_queue_one(
      expansion_condition_catalog,auto_condition_queue_rv(),nodes,node_content,relationships,sources,interventions,local_metric_mappings
    ),error=function(e)e)
    auto_expansion_last_run(Sys.time())
    if(inherits(result,"error")) {
      if(notify) showNotification(paste("Expansion worker error:",conditionMessage(result)),type="error",duration=8)
      return(invisible(NULL))
    }
    auto_condition_queue_rv(result$queue); uh_auto_save_queue(result$queue)
    if(notify) showNotification(result$message,type="message",duration=7)
    invisible(result)
  }

  # ==========================================================
  # ADMIN DISCOVERY INBOX
  #
  # Trusted-source discovery is intentionally admin-only.
  # It discovers candidate sources; it NEVER writes new health
  # claims directly into the public graph without review.
  # ==========================================================

  discovery_status_badge <- function(status) {
    status <- toupper(trimws(as.character(status)))
    bg <- switch(
      status,
      "NEW" = "rgba(86, 154, 255, .16)",
      "KEEP" = "rgba(89, 194, 133, .16)",
      "PROPOSAL" = "rgba(238, 188, 84, .18)",
      "DISMISSED" = "rgba(150, 150, 150, .12)",
      "rgba(255,255,255,.08)"
    )
    tags$span(
      style = paste0(
        "display:inline-block;padding:3px 8px;border-radius:999px;",
        "font-size:11px;font-weight:700;letter-spacing:.04em;",
        "background:", bg, ";border:1px solid rgba(255,255,255,.12);"
      ),
      status
    )
  }

  run_discovery_scan <- function(days_back = 30) {
    showNotification("Scanning trusted PubMed watch queries…", type = "message", duration = 3)

    incoming <- scan_pubmed_watchlist(
      watchlist = discovery_watchlist_rv(),
      nodes = nodes,
      days_back = days_back,
      retmax = 8
    )

    merged <- merge_discovery_candidates(discovery_candidates_rv(), incoming)
    discovery_candidates_rv(merged)
    save_discovery_candidates(merged)
    discovery_last_scan(Sys.time())

    # The merge is deduplicated; report how many records were seen in this scan.
    showNotification(
      paste0("Discovery scan complete. ", nrow(incoming), " recent source records checked; inbox deduplicated automatically."),
      type = "message",
      duration = 5
    )
  }

  discovery_view_rv <- reactiveVal("NEW")
  proposal_candidate_rv <- reactiveVal(NULL)

  discovery_card <- function(row) {
    cid <- as.character(row$candidate_id)
    hits <- trimws(as.character(row$existing_node_hits)); if (!nzchar(hits)) hits <- "No exact existing-node match detected"
    ptypes <- trimws(as.character(row$publication_types)); if (!nzchar(ptypes)) ptypes <- "Source type not supplied"
    open_label <- if (grepl("PubMed", row$source_type, ignore.case=TRUE)) "Open PubMed" else "Open source"
    tagList(
      tags$div(
        style=paste0("margin:10px 0;padding:14px;border:1px solid #2b3438;border-radius:12px;","background:rgba(15,19,20,.72);"),
        tags$div(style="display:flex;gap:8px;align-items:center;flex-wrap:wrap;margin-bottom:7px;",
          discovery_status_badge(row$status),
          tags$span(style="font-size:11px;font-weight:700;color:#aebbc0;",paste0("PRIORITY ",row$priority)),
          tags$span(style="font-size:11px;color:#819096;",paste0(" • ",row$source_org)),
          tags$span(style="font-size:11px;color:#819096;",paste0(" • ",row$discovery_query_label))
        ),
        tags$div(style="font-size:15px;font-weight:700;line-height:1.35;margin-bottom:7px;",row$source_title),
        tags$div(style="font-size:12px;color:#aab6bb;line-height:1.45;margin-bottom:5px;",paste0("Published: ",ifelse(nzchar(row$publication_date),row$publication_date,"date not captured"),"  •  ",ptypes)),
        tags$div(style="font-size:12px;color:#aab6bb;line-height:1.45;margin-bottom:10px;",tags$b("Existing graph matches: "),hits),
        if (nzchar(row$admin_notes)) tags$div(style="font-size:12px;color:#c2ccd0;margin-bottom:10px;",tags$b("Admin note: "),row$admin_notes),
        tags$div(style="display:flex;gap:7px;flex-wrap:wrap;align-items:center;",
          tags$a(href=row$source_url,target="_blank",class="full-network-btn",style="text-decoration:none;display:inline-block;",open_label),
          if (row$status != "PUBLISHED" && row$status != "KEEP") tags$button(type="button",class="qa-button",onclick=sprintf("Shiny.setInputValue('discovery_action', 'KEEP::%s', {priority:'event'});",cid),"Keep"),
          if (row$status != "PUBLISHED") tags$button(type="button",class="qa-button",onclick=sprintf("Shiny.setInputValue('discovery_action', 'PROPOSAL::%s', {priority:'event'});",cid),if(row$status=="PROPOSAL") "Refresh proposal" else "Promote to proposal"),
          if (row$status %in% c("PROPOSAL","PUBLISHED")) tags$button(type="button",class="qa-button",onclick=sprintf("Shiny.setInputValue('open_proposal_review', '%s', {priority:'event'});",cid),if(row$status=="PUBLISHED") "View published proposal" else "Open proposal review"),
          if (row$status != "PUBLISHED") {
            if (row$status != "DISMISSED") tags$button(type="button",class="full-network-btn",onclick=sprintf("Shiny.setInputValue('discovery_action', 'DISMISSED::%s', {priority:'event'});",cid),"Dismiss") else tags$button(type="button",class="full-network-btn",onclick=sprintf("Shiny.setInputValue('discovery_action', 'NEW::%s', {priority:'event'});",cid),"Restore to New")
          }
        )
      )
    )
  }

  output$discovery_inbox_ui <- renderUI({
    req(UPSTREAM_ADMIN_MODE)
    candidates <- discovery_candidates_rv(); watchlist <- ensure_watchlist_columns(discovery_watchlist_rv())
    view <- discovery_view_rv()
    visible <- if (view == "ALL") candidates else candidates[candidates$status == view,,drop=FALSE]
    if (nrow(visible)>0) {
      pr <- match(visible$priority,c("HIGH","MEDIUM-HIGH","MEDIUM","LOW")); pr[is.na(pr)] <- 99
      visible <- visible[order(pr,visible$detected_at,decreasing=FALSE),,drop=FALSE]
    }
    cards <- if(nrow(visible)==0) tags$div(class="qa-modal-note",paste0("No ",tolower(view)," discovery items yet.")) else lapply(seq_len(nrow(visible)),function(i) discovery_card(visible[i,,drop=FALSE]))
    tagList(
      tags$div(class="qa-modal-note",tags$b("Admin-only discovery pipeline. "),"Candidates never publish themselves. Review evidence, then deliberately promote approved knowledge into the graph."),
      tags$div(style="display:flex;gap:7px;flex-wrap:wrap;margin:12px 0;",
        actionButton("disc_view_new",paste0("New (",sum(candidates$status=="NEW"),")"),class="full-network-btn"),
        actionButton("disc_view_keep",paste0("Kept (",sum(candidates$status=="KEEP"),")"),class="full-network-btn"),
        actionButton("disc_view_proposal",paste0("Proposals (",sum(candidates$status=="PROPOSAL"),")"),class="qa-button"),
        actionButton("disc_view_published",paste0("Published (",sum(candidates$status=="PUBLISHED"),")"),class="qa-button"),
        actionButton("disc_view_dismissed",paste0("Dismissed (",sum(candidates$status=="DISMISSED"),")"),class="full-network-btn"),
        actionButton("disc_view_all",paste0("All (",nrow(candidates),")"),class="full-network-btn")
      ),
      tags$div(style="display:flex;gap:10px;flex-wrap:wrap;align-items:end;margin:12px 0;",
        numericInput("discovery_days_back","Look back (days)",value=30,min=1,max=365,step=1,width="150px"),
        actionButton("run_discovery_scan","Run PubMed + CDC scan",class="qa-button")
      ),
      tags$div(style="font-size:12px;color:#9ba8ad;margin-bottom:12px;",paste0("Current view: ",view,if(!is.null(discovery_last_scan())) paste0(" • Last scan: ",format(discovery_last_scan(),"%Y-%m-%d %H:%M")) else "")),
      tags$details(style="margin-bottom:14px;",tags$summary(style="cursor:pointer;font-weight:700;","Add a trusted source manually (NIH, WHO, USPSTF, Community Guide, etc.)"),
        tags$div(style="padding-top:10px;",
          selectInput("manual_source_org","Organization",choices=c("NIH","CDC","WHO","USPSTF","The Community Guide","Professional society / guideline","Other")),
          textInput("manual_source_title","Source title"), textInput("manual_source_url","Source URL"),
          textAreaInput("manual_source_notes","Admin note (optional)",rows=2), actionButton("add_manual_trusted_source","Add to Kept",class="qa-button")
        )
      ),
      tags$details(style="margin-bottom:14px;",tags$summary(style="cursor:pointer;font-weight:700;","Manage automated watch queries"),
        tags$div(style="padding-top:10px;",
          tags$div(style="font-size:12px;color:#9ba8ad;margin-bottom:8px;",paste0("Active watches: ",paste(watchlist$label[tolower(as.character(watchlist$active)) %in% c("true","1","yes","y")],collapse=" • "))),
          textInput("new_watch_label","New watch label",placeholder="e.g., COPD — upstream risk"),
          textInput("new_watch_query","PubMed query",placeholder="e.g., COPD AND (risk factor OR intervention)"),
          textInput("new_watch_cdc_query","CDC search terms",placeholder="e.g., COPD"),
          actionButton("add_discovery_watch","Add watch query",class="full-network-btn")
        )
      ),
      tags$div(cards)
    )
  })

  show_discovery_inbox <- function() {
    showModal(modalDialog(title="The Health Web Discovery Inbox — ADMIN",uiOutput("discovery_inbox_ui"),size="l",easyClose=TRUE,footer=modalButton("Close")))
  }

  publish_preview_rv <- reactiveVal(NULL)
  publish_status_rv <- reactiveVal("")

  output$proposal_review_ui <- renderUI({
    req(UPSTREAM_ADMIN_MODE); cid <- proposal_candidate_rv(); req(cid)
    df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id); req(!is.na(idx)); row <- df[idx,,drop=FALSE]
    reviewed <- identical(as.character(row$proposal_review_status), "EVIDENCE_REVIEWED") || identical(as.character(row$proposal_review_status), "PUBLISHED")
    published <- identical(as.character(row$status), "PUBLISHED") || identical(as.character(row$proposal_review_status), "PUBLISHED")
    hits <- trimws(as.character(row$existing_node_hits))
    hit_ids <- if (nzchar(hits)) trimws(unlist(strsplit(hits, ";", fixed=TRUE))) else character()
    hit_ids <- hit_ids[hit_ids %in% nodes$node_id]
    hit_names <- nodes$node_name[match(hit_ids,nodes$node_id)]
    node_choices <- c("— choose node —"="", setNames(as.character(nodes$node_id), as.character(nodes$node_name)), "NEW NODE ABOVE"="__NEW__")
    default_from <- if(length(hit_ids)>=1) hit_ids[1] else ""
    default_to <- if(length(hit_ids)>=2) hit_ids[2] else ""
    preview <- publish_preview_rv()

    publish_section <- if (published) {
      tags$div(
        style="margin-top:16px;padding:14px;border:1px solid #86a98c;border-radius:12px;background:#f2fbf4;color:#17331d;",
        tags$h5("Published to graph", style="color:#17331d;"),
        tags$p("This proposal has already been written into the graph data. Use the publish log / backups if you need to audit or undo it."),
        actionButton("reload_after_publish","Reload graph data",class="qa-button")
      )
    } else if (reviewed) {
      tagList(
        tags$hr(),
        tags$div(style="padding:12px;border:1px solid #9bb6c2;border-radius:10px;background:#eef7fa;color:#17323d;margin-bottom:14px;",
          tags$h4("New disease / condition package", style="color:#17323d;margin-top:0;"),
          tags$p("If this evidence points to a disease or condition The Health Web does not yet contain, build a multi-node condition package instead of publishing one change at a time."),
          actionButton("open_condition_builder","Auto-build condition draft",class="qa-button")
        ),
        tags$h4("Publish individual changes", style="color:#111827;"),
        tags$div(style="padding:10px 12px;background:#fff8df;border:1px solid #d9b95b;border-radius:9px;color:#4a3810;margin-bottom:12px;",
          tags$b("Nothing below is automatic. "),
          "The exact rows shown here are the only changes that can be published, and publication is blocked if pre-publish QA finds a FAIL."
        ),
        tags$p(tags$b("Existing graph concepts detected: "), if(length(hit_names)) paste(hit_names,collapse=" • ") else "None detected automatically."),
        tags$p(tags$b("Source that will be attached: "), paste0(row$source_org," — ",row$source_title)),
        checkboxInput("publish_add_node","Add a NEW node from this source",value=FALSE),
        tags$div(style="padding-left:12px;border-left:3px solid #d8dde0;margin-bottom:14px;",
          textInput("publish_node_name","New node canonical name",placeholder="e.g., Wildfire smoke exposure"),
          selectInput("publish_node_type","Node type",choices=c("Structural determinant"="structural_determinant","Social determinant"="social_determinant","Environmental determinant"="environmental_determinant","Behavioral pathway"="behavioral_pathway","Behavioral / biological pathway"="behavioral_biological_pathway","Biological pathway"="biological_pathway","Disease / condition"="disease","Symptom"="symptom","Complication / outcome"="complication"),selected="environmental_determinant"),
          selectInput("publish_sdoh_domain","SDOH domain",choices=c("None / not applicable"="","Economic Stability","Health Care Access and Quality","Neighborhood and Built Environment")),
          checkboxInput("publish_modifiable","Potentially modifiable",value=TRUE),
          textInput("publish_badge","Display badge",placeholder="e.g., ENVIRONMENTAL EXPOSURE"),
          textAreaInput("publish_description","Definition / description",rows=2),
          textAreaInput("publish_why","Why it matters",rows=2),
          numericInput("publish_importance","Graph importance",value=2,min=1,max=5,step=.5)
        ),
        checkboxInput("publish_add_relationship","Add a NEW directed relationship",value=FALSE),
        tags$div(style="padding-left:12px;border-left:3px solid #d8dde0;margin-bottom:14px;",
          tags$div(style="display:flex;gap:10px;flex-wrap:wrap;",
            tags$div(style="flex:1;min-width:220px;",selectInput("publish_from_node","FROM",choices=node_choices,selected=default_from)),
            tags$div(style="flex:1;min-width:220px;",selectInput("publish_to_node","TO",choices=node_choices,selected=default_to))
          ),
          selectInput("publish_relationship_type","Relationship type",choices=c("Contributes to"="contributes_to","Associated with"="associated_with","Increases risk"="increases_risk","Can trigger / worsen"="can_worsen","Leads to"="leads_to"),selected="associated_with"),
          selectInput("publish_evidence_strength","Evidence strength",choices=c("Weak"="weak","Moderate"="moderate","Strong"="strong"),selected="moderate"),
          textInput("publish_short_explanation","Short graph explanation",placeholder="One careful sentence; do not overstate causality."),
          textInput("publish_rel_badge","Relationship badge",value="EVIDENCE UPDATE"),
          textAreaInput("publish_primary_text","Detailed relationship explanation",rows=3),
          textInput("publish_secondary_label","Caution / distinction label",value="IMPORTANT DISTINCTION"),
          textAreaInput("publish_secondary_text","Caution, limits, or distinction",rows=2)
        ),
        tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin:10px 0;",
          actionButton("run_pre_publish_qa","Run pre-publish QA",class="qa-button"),
          if (!is.null(preview) && !identical(preview$worst,"FAIL")) actionButton("publish_approved_changes","Publish approved changes",class="qa-button")
        ),
        if (nzchar(publish_status_rv())) tags$div(style="padding:10px;border-radius:9px;background:#eef2f4;color:#172126;margin-top:8px;white-space:pre-wrap;",publish_status_rv()),
        if (!is.null(preview)) tagList(
          tags$h5("Exact proposed changes",style="color:#111827;margin-top:12px;"),
          tags$ul(lapply(preview$messages,tags$li)),
          tags$p(tags$b("Pre-publish QA worst status: "),preview$worst),
          if (nrow(preview$qa[preview$qa$status %in% c("FAIL","REVIEW","WARNING"),,drop=FALSE]))
            tags$div(style="max-height:180px;overflow:auto;border:1px solid #d8dde0;border-radius:8px;padding:8px;background:white;",
              lapply(seq_len(nrow(preview$qa[preview$qa$status %in% c("FAIL","REVIEW","WARNING"),,drop=FALSE])),function(i){
                rr<-preview$qa[preview$qa$status %in% c("FAIL","REVIEW","WARNING"),,drop=FALSE][i,]
                tags$div(style="font-size:12px;margin-bottom:6px;",tags$b(paste0(rr$status," — ",rr$item,": ")),rr$message)
              })
            )
        )
      )
    } else {
      tags$div(style="margin-top:16px;padding:12px;border:1px solid #d9b95b;border-radius:10px;background:#fff8df;color:#4a3810;",
        tags$b("Publishing is locked. "),"Read the evidence and click Mark evidence reviewed first."
      )
    }

    tags$div(
      class="proposal-review-panel",
      tags$div(class="qa-modal-note",tags$b("Proposal draft — admin only. "),"Nothing reaches the public graph until you explicitly publish an approved change set."),
      tags$h4(row$source_title),
      tags$div(style="font-size:12px;color:#526168;margin-bottom:10px;",paste0(row$source_org," • ",row$publication_types," • ",row$publication_date)),
      tags$a(href=row$source_url,target="_blank",class="full-network-btn",style="text-decoration:none;display:inline-block;margin-bottom:12px;","Open original source"),
      tags$h5("Automated triage"),
      tags$p(tags$b("Proposal type: "),row$proposal_kind), tags$p(row$proposal_summary),
      tags$p(tags$b("Nodes / concepts: "),row$proposed_nodes),
      tags$p(tags$b("Relationships: "),row$proposed_relationships),
      tags$p(tags$b("Interventions: "),row$proposed_interventions),
      tags$p(tags$b("Evidence note: "),row$proposal_evidence),
      if (grepl("^pubmed_",cid)) tagList(tags$h5("PubMed abstract / record text"),tags$div(style="max-height:240px;overflow:auto;padding:10px;border:1px solid #c8d0d4;border-radius:9px;background:#fff;color:#111827;font-size:12px;line-height:1.45;",textOutput("proposal_abstract_text"))),
      textAreaInput("proposal_admin_notes","Your review notes",value=row$admin_notes,rows=4),
      tags$div(style="display:flex;gap:8px;flex-wrap:wrap;",
        actionButton("save_proposal_notes","Save notes",class="qa-button"),
        if (!reviewed) actionButton("mark_proposal_reviewed","Mark evidence reviewed",class="qa-button"),
        if (!published) actionButton("return_proposal_to_keep","Return to Kept",class="full-network-btn")
      ),
      publish_section
    )
  })

  condition_package_preview_rv <- reactiveVal(NULL)
  condition_package_status_rv <- reactiveVal("")
  condition_auto_draft_rv <- reactiveVal(NULL)

  output$condition_builder_ui <- renderUI({
    req(UPSTREAM_ADMIN_MODE); cid <- proposal_candidate_rv(); req(cid)
    df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id); req(!is.na(idx)); row <- df[idx,,drop=FALSE]
    draft <- condition_auto_draft_rv()
    hits <- trimws(as.character(row$existing_node_hits))
    hit_ids <- if(nzchar(hits)) trimws(unlist(strsplit(hits,";",fixed=TRUE))) else character()
    hit_ids <- hit_ids[hit_ids %in% nodes$node_id]
    fallback_guess <- uh_cb_guess_relationship_rows("condition",hit_ids,nodes)
    preview <- condition_package_preview_rv()
    val <- function(name, fallback="") if(!is.null(draft) && !is.null(draft[[name]])) draft[[name]] else fallback
    confidence_box <- if(!is.null(draft)) tags$div(
      style="padding:10px 12px;background:#eef7fa;border:1px solid #9bb6c2;border-radius:9px;color:#17323d;margin-bottom:12px;",
      tags$b(paste0("Automatic draft • ",draft$confidence," confidence. ")), draft$note,
      tags$div(style="margin-top:6px;font-size:12px;","Everything below is editable. Accepting the draft only sends it to QA; it does not publish it.")
    ) else tags$div(
      style="padding:10px 12px;background:#fff8df;border:1px solid #d9b95b;border-radius:9px;color:#4a3810;margin-bottom:12px;",
      tags$b("No automatic draft loaded. "),"Generate one from the reviewed source, or enter a package manually."
    )
    tagList(
      tags$div(class="qa-modal-note",tags$b("Admin-only automatic condition builder. "),"The system prepares the package; your job is primarily Accept, Edit, or Reject. Nothing is public until QA passes and you explicitly publish."),
      tags$p(tags$b("Evidence source: "),paste0(row$source_org," — ",row$source_title)),
      confidence_box,
      tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:12px;",
        actionButton("generate_condition_draft","Regenerate automatic draft",class="qa-button"),
        actionButton("reject_condition_draft","Reject / clear draft",class="full-network-btn")
      ),
      textInput("cb_condition_name","Disease / condition canonical name",value=val("condition_name",""),placeholder="e.g., Chronic obstructive pulmonary disease (COPD)"),
      textInput("cb_condition_badge","Display badge",value=val("condition_badge","DISEASE / CONDITION")),
      textAreaInput("cb_condition_description","Definition / description",value=val("condition_description",""),rows=3),
      textAreaInput("cb_condition_why","Why it matters",value=val("condition_why",""),rows=3),
      numericInput("cb_condition_importance","Graph importance",value=as.numeric(val("condition_importance",5)),min=1,max=5,step=.5),
      tags$details(open=FALSE, tags$summary(style="cursor:pointer;font-weight:700;","Supporting NEW nodes — usually leave as generated unless editing is needed"),
        tags$p(style="font-size:12px;color:#526168;","One per line: name | node_type | badge | description | why it matters | importance. Existing names are automatically reused rather than duplicated."),
        textAreaInput("cb_supporting_nodes","Supporting nodes",value=val("supporting_nodes",""),rows=6,placeholder="Tobacco smoke exposure | environmental_determinant | EXPOSURE | ... | ... | 3")
      ),
      tags$h5("Candidate relationships",style="color:#111827;margin-top:14px;"),
      tags$p(style="font-size:12px;color:#526168;","These are generated as REVIEW prompts from concepts found in the source. Co-mention is not proof of causality. You can accept them for QA, edit them, or delete any row."),
      textAreaInput("cb_relationships","Relationship rows",value=val("relationships",paste(fallback_guess,collapse="\n")),rows=8),
      tags$div(style="display:flex;gap:8px;flex-wrap:wrap;margin:10px 0;",
        actionButton("accept_condition_draft_qa","Accept draft + run QA",class="qa-button"),
        actionButton("run_condition_package_qa","Run QA after edits",class="full-network-btn"),
        if(!is.null(preview) && !identical(preview$worst,"FAIL")) actionButton("publish_condition_package","Publish condition package",class="qa-button")
      ),
      if(nzchar(condition_package_status_rv())) tags$div(style="padding:10px;border-radius:9px;background:#eef2f4;color:#172126;margin-top:8px;white-space:pre-wrap;",condition_package_status_rv()),
      if(!is.null(preview)) tagList(
        tags$h5("Exact package changes",style="color:#111827;margin-top:12px;"),
        tags$ul(lapply(preview$messages,tags$li)),
        tags$p(tags$b("Package QA worst status: "),preview$worst),
        tags$div(style="max-height:190px;overflow:auto;border:1px solid #d8dde0;border-radius:8px;padding:8px;background:white;",
          lapply(seq_len(nrow(preview$qa)),function(i){rr<-preview$qa[i,];tags$div(style="font-size:12px;margin-bottom:6px;color:#111827;",tags$b(paste0(rr$status," — ",rr$item,": ")),rr$message)})
        )
      )
    )
  })

  proposal_abstract_rv <- reactiveVal("")
  output$proposal_abstract_text <- renderText(proposal_abstract_rv())

  show_proposal_review <- function(cid) {
    proposal_candidate_rv(cid); proposal_abstract_rv("")
    if (grepl("^pubmed_",cid)) proposal_abstract_rv(pubmed_abstract_text(cid))
    showModal(modalDialog(title="Proposal Review — ADMIN",uiOutput("proposal_review_ui"),size="l",easyClose=TRUE,footer=modalButton("Close")))
  }

  if (UPSTREAM_ADMIN_MODE) {
    observeEvent(input$open_expansion_queue,{ show_expansion_queue() })
    observeEvent(input$auto_expand_now,{ run_autonomous_expansion_once(notify=TRUE) })
    observeEvent(input$auto_pkg_open,{ show_auto_package(input$auto_pkg_open) })

    accept_auto_package <- function(pid) {
      q <- auto_condition_queue_rv(); i <- match(pid,q$package_id); if(is.na(i)) return()
      pkg <- tryCatch(uh_auto_decode(q$package_json[i]),error=function(e)e)
      if(inherits(pkg,"error")) {showNotification("Package could not be decoded.",type="error");return()}
      if(identical(as.character(pkg$worst),"FAIL")) {showNotification("This package has a blocking QA FAIL and cannot be published until edited.",type="error",duration=7);return()}
      backup <- tryCatch(uh_auto_commit_package(pkg,"data"),error=function(e)e)
      if(inherits(backup,"error")) {showNotification(paste("Publish failed:",conditionMessage(backup)),type="error",duration=8);return()}
      q$status[i] <- "ACCEPTED"; q$admin_notes[i] <- paste0("Published; backup: ",backup); auto_condition_queue_rv(q); uh_auto_save_queue(q)
      showNotification(paste0(pkg$condition_name," accepted and published. Restart/reload the app to display the expanded graph."),type="message",duration=9)
      removeModal()
    }

    observeEvent(input$auto_pkg_accept,{ accept_auto_package(input$auto_pkg_accept) })
    observeEvent(input$auto_accept_selected,{ pid<-auto_selected_package(); if(!is.null(pid)) accept_auto_package(pid) })

    reject_auto_package <- function(pid) {
      q <- auto_condition_queue_rv(); i <- match(pid,q$package_id); if(is.na(i)) return()
      q$status[i] <- "REJECTED"; q$admin_notes[i] <- paste0("Rejected ",format(Sys.time(),"%Y-%m-%d %H:%M:%S"))
      auto_condition_queue_rv(q); uh_auto_save_queue(q); showNotification("Package rejected. The graph was not changed.",type="message"); removeModal()
    }
    observeEvent(input$auto_pkg_reject,{ reject_auto_package(input$auto_pkg_reject) })
    observeEvent(input$auto_reject_selected,{ pid<-auto_selected_package(); if(!is.null(pid)) reject_auto_package(pid) })

    observeEvent(input$auto_edit_selected,{
      showModal(modalDialog(title="Edit Autonomous Package — ADMIN",uiOutput("auto_package_edit_ui"),size="l",easyClose=TRUE,footer=modalButton("Cancel")))
    })

    observeEvent(input$auto_save_edits,{
      pid<-auto_selected_package(); q<-auto_condition_queue_rv(); i<-match(pid,q$package_id); if(is.na(i)) return()
      pkg<-uh_auto_decode(q$package_json[i]); pkg$condition_name<-trimws(input$auto_edit_name); pkg$condition_key<-uh_auto_slug(pkg$condition_name)
      pkg$description<-trimws(input$auto_edit_description); pkg$why_it_matters<-trimws(input$auto_edit_why)
      sr<-uh_cb_parse_pipe_rows(input$auto_edit_supporting,5L)
      if(length(sr)) pkg$supporting_nodes<-do.call(rbind,lapply(sr,function(f) data.frame(name=f[1],node_type=f[2],role=f[3],badge=f[4],evidence_docs=suppressWarnings(as.integer(f[5])),stringsAsFactors=FALSE))) else pkg$supporting_nodes<-data.frame()
      rr<-uh_cb_parse_pipe_rows(input$auto_edit_relationships,5L)
      if(length(rr)) pkg$relationships<-do.call(rbind,lapply(rr,function(f) data.frame(from=f[1],to=f[2],relationship_type=f[3],evidence_strength=tolower(f[4]),short_explanation=f[5],evidence_docs=2,stringsAsFactors=FALSE))) else pkg$relationships<-data.frame()
      ir<-uh_cb_parse_pipe_rows(input$auto_edit_interventions,4L)
      if(length(ir)) pkg$interventions<-do.call(rbind,lapply(ir,function(f) data.frame(intervention_name=f[1],intervention_level=f[2],actor=f[3],description=f[4],stringsAsFactors=FALSE))) else pkg$interventions<-data.frame()
      qa<-data.frame(status=character(),item=character(),message=character(),stringsAsFactors=FALSE)
      aq<-function(st,it,msg) qa<<-rbind(qa,data.frame(status=st,item=it,message=msg,stringsAsFactors=FALSE))
      if(!nzchar(pkg$condition_name)||!nzchar(pkg$description)||!nzchar(pkg$why_it_matters)) aq("FAIL","Required content","Condition name, description, and why-it-matters are required.")
      if(is.data.frame(pkg$relationships)&&nrow(pkg$relationships)&&any(!pkg$relationships$evidence_strength %in% c("weak","moderate","strong"))) aq("FAIL","Evidence strength","Relationships must use weak, moderate, or strong.")
      if(!nrow(qa)) aq("PASS","Edited package","No blocking structural issue found after edit.")
      rank<-c(PASS=1,WARNING=2,REVIEW=3,FAIL=4); pkg$qa<-qa; pkg$worst<-names(rank)[which.max(rank[qa$status])]
      q$condition_name[i]<-pkg$condition_name; q$condition_key[i]<-pkg$condition_key; q$qa_status[i]<-pkg$worst
      q$summary[i]<-uh_auto_package_summary(pkg,nodes); q$package_json[i]<-jsonlite::toJSON(pkg,auto_unbox=TRUE,dataframe="rows",null="null",digits=NA)
      q$status[i]<-"READY"; auto_condition_queue_rv(q); uh_auto_save_queue(q)
      showNotification("Edits saved and QA rerun. Review the package, then accept or reject.",type="message"); show_auto_package(pid)
    })

    # First background research pass starts automatically after admin startup,
    # then repeats every hour while the app process is running.
    if (UPSTREAM_AUTO_EXPANSION) {
      auto_start_done <- reactiveVal(FALSE)
      observe({
        invalidateLater(1800,session)
        if(!isTRUE(auto_start_done())) {
          auto_start_done(TRUE)
          isolate(run_autonomous_expansion_once(notify=FALSE))
        }
      })
      auto_expansion_timer <- reactiveTimer(60*60*1000,session)
      observe({auto_expansion_timer(); isolate({
        if(!isTRUE(auto_start_done())) return()
        run_autonomous_expansion_once(notify=FALSE)
      })})
    }

    observeEvent(input$open_discovery_inbox,{ show_discovery_inbox() })
    observeEvent(input$disc_view_new,{discovery_view_rv("NEW")})
    observeEvent(input$disc_view_keep,{discovery_view_rv("KEEP")})
    observeEvent(input$disc_view_proposal,{discovery_view_rv("PROPOSAL")})
    observeEvent(input$disc_view_published,{discovery_view_rv("PUBLISHED")})
    observeEvent(input$disc_view_dismissed,{discovery_view_rv("DISMISSED")})
    observeEvent(input$disc_view_all,{discovery_view_rv("ALL")})

    observeEvent(input$run_discovery_scan,{
      days <- suppressWarnings(as.integer(input$discovery_days_back)); if(is.na(days)||days<1) days<-30L
      wl <- discovery_watchlist_rv()
      incoming_pubmed <- scan_pubmed_watchlist(wl,nodes,days_back=min(days,365L))
      incoming_cdc <- scan_cdc_watchlist(wl,nodes)
      incoming <- rbind(ensure_candidate_columns(incoming_pubmed),ensure_candidate_columns(incoming_cdc))
      merged <- merge_discovery_candidates(discovery_candidates_rv(),incoming)
      discovery_candidates_rv(merged); save_discovery_candidates(merged); discovery_last_scan(Sys.time())
      showNotification(paste0("Trusted-source scan complete: ",nrow(incoming_pubmed)," PubMed + ",nrow(incoming_cdc)," CDC candidates checked."),type="message",duration=6)
    })

    observeEvent(input$discovery_action,{
      parts <- strsplit(input$discovery_action,"::",fixed=TRUE)[[1]]; if(length(parts)!=2) return()
      new_status <- parts[1]; cid <- parts[2]; df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id); if(is.na(idx)) return()
      if (new_status == "PROPOSAL") {
        abs <- if(grepl("^pubmed_",cid)) pubmed_abstract_text(cid) else ""
        prop <- build_candidate_proposal(df[idx,,drop=FALSE],nodes,abs)
        df$proposal_kind[idx] <- prop$kind; df$proposal_summary[idx] <- prop$summary; df$proposed_nodes[idx] <- prop$nodes
        df$proposed_relationships[idx] <- prop$relationships; df$proposed_interventions[idx] <- prop$interventions; df$proposal_evidence[idx] <- prop$evidence
        df$proposal_review_status[idx] <- "UNREVIEWED"
        if(nzchar(prop$hits)) df$existing_node_hits[idx] <- prop$hits
      }
      df$status[idx] <- new_status; df$reviewed_at[idx] <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
      discovery_candidates_rv(df); save_discovery_candidates(df)
      showNotification(paste("Discovery item marked",tolower(new_status)),type="message")
    })

    observeEvent(input$open_proposal_review,{ show_proposal_review(input$open_proposal_review) })

    observeEvent(input$open_condition_builder,{
      condition_package_preview_rv(NULL); condition_package_status_rv("")
      cid <- proposal_candidate_rv(); df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id)
      if(!is.na(idx)) condition_auto_draft_rv(uh_cb_auto_draft(df[idx,,drop=FALSE],proposal_abstract_rv(),nodes))
      showModal(modalDialog(title="Automatic Condition Draft — ADMIN",uiOutput("condition_builder_ui"),size="l",easyClose=TRUE,footer=modalButton("Close")))
    })

    observeEvent(input$generate_condition_draft,{
      cid <- proposal_candidate_rv(); df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id); if(is.na(idx)) return()
      condition_auto_draft_rv(uh_cb_auto_draft(df[idx,,drop=FALSE],proposal_abstract_rv(),nodes))
      condition_package_preview_rv(NULL); condition_package_status_rv("Automatic condition draft regenerated. Review it, then Accept draft + run QA.")
    })

    observeEvent(input$reject_condition_draft,{
      condition_auto_draft_rv(NULL); condition_package_preview_rv(NULL); condition_package_status_rv("Draft rejected/cleared. Nothing was published.")
    })

    run_condition_package_qa_now <- function(){
      cid<-proposal_candidate_rv(); df<-discovery_candidates_rv(); idx<-match(cid,df$candidate_id); if(is.na(idx)) return()
      row<-df[idx,,drop=FALSE]
      if(!identical(as.character(row$proposal_review_status),"EVIDENCE_REVIEWED")){showNotification("Mark the evidence reviewed first.",type="warning");return()}
      preview<-tryCatch(uh_cb_build_preview(
        candidate_row=row,condition_name=input$cb_condition_name,condition_description=input$cb_condition_description,
        condition_why=input$cb_condition_why,condition_badge=input$cb_condition_badge,condition_importance=input$cb_condition_importance,
        supporting_nodes_text=input$cb_supporting_nodes,relationships_text=input$cb_relationships,
        nodes=nodes,node_content=node_content,relationships=relationships,relationship_content=relationship_content,
        relationship_sources=relationship_sources,sources=sources),error=function(e)e)
      if(inherits(preview,"error")){condition_package_preview_rv(NULL);condition_package_status_rv(paste0("Cannot build package: ",conditionMessage(preview)));showNotification(conditionMessage(preview),type="error",duration=8);return()}
      condition_package_preview_rv(preview)
      condition_package_status_rv(paste0("Condition-package QA complete. Worst status: ",preview$worst,if(preview$worst=="FAIL") "\nPublishing is blocked until the FAIL is resolved." else "\nReview every proposed row above before publishing."))
    
    }

    observeEvent(input$run_condition_package_qa,{ run_condition_package_qa_now() })
    observeEvent(input$accept_condition_draft_qa,{ run_condition_package_qa_now() })

    observeEvent(input$publish_condition_package,{
      cid<-proposal_candidate_rv(); df<-discovery_candidates_rv(); idx<-match(cid,df$candidate_id); if(is.na(idx)) return()
      preview<-condition_package_preview_rv(); if(is.null(preview)) return()
      if(identical(preview$worst,"FAIL")){showNotification("Publishing is blocked because package QA contains a FAIL.",type="error");return()}
      backup<-tryCatch(uh_cb_commit(preview,"data"),error=function(e)e)
      if(inherits(backup,"error")){showNotification(paste("Condition publish failed:",conditionMessage(backup)),type="error",duration=8);return()}
      row<-df[idx,,drop=FALSE]
      df$status[idx]<-"PUBLISHED"; df$proposal_review_status[idx]<-"PUBLISHED"; df$reviewed_at[idx]<-format(Sys.time(),"%Y-%m-%d %H:%M:%S")
      df$admin_notes[idx]<-paste(trimws(as.character(df$admin_notes[idx])),paste0("[Condition package published: ",preview$condition_id,"]"))
      discovery_candidates_rv(df);save_discovery_candidates(df)
      condition_package_status_rv(paste0("PUBLISHED condition package. Backup created at: ",backup,"\nReload the app to display the expanded graph."))
      showNotification("Condition package published. A graph backup was created automatically.",type="message",duration=8)
    })

    observeEvent(input$save_proposal_notes,{
      cid<-proposal_candidate_rv(); df<-discovery_candidates_rv(); idx<-match(cid,df$candidate_id); if(is.na(idx)) return()
      df$admin_notes[idx] <- input$proposal_admin_notes; discovery_candidates_rv(df); save_discovery_candidates(df); showNotification("Proposal notes saved.",type="message")
    })
    observeEvent(input$mark_proposal_reviewed,{
      cid<-proposal_candidate_rv(); df<-discovery_candidates_rv(); idx<-match(cid,df$candidate_id); if(is.na(idx)) return()
      df$admin_notes[idx] <- input$proposal_admin_notes; df$proposal_review_status[idx] <- "EVIDENCE_REVIEWED"; df$reviewed_at[idx] <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
      discovery_candidates_rv(df); save_discovery_candidates(df); publish_preview_rv(NULL); publish_status_rv(""); showNotification("Evidence review recorded. Publish controls are now unlocked; nothing has been published yet.",type="message")
    })
    observeEvent(input$run_pre_publish_qa,{
      cid <- proposal_candidate_rv(); df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id); if(is.na(idx)) return()
      row <- df[idx,,drop=FALSE]
      if (!identical(as.character(row$proposal_review_status),"EVIDENCE_REVIEWED")) {
        showNotification("Mark the evidence reviewed before building a publishable change set.",type="warning"); return()
      }
      preview <- tryCatch(
        uh_build_publish_preview(
          candidate_row=row,
          add_node=isTRUE(input$publish_add_node),
          node_name=input$publish_node_name,
          node_type=input$publish_node_type,
          sdoh_domain=input$publish_sdoh_domain,
          modifiable=isTRUE(input$publish_modifiable),
          badge=input$publish_badge,
          description=input$publish_description,
          why_it_matters=input$publish_why,
          importance=input$publish_importance,
          add_relationship=isTRUE(input$publish_add_relationship),
          from_node=input$publish_from_node,
          to_node=input$publish_to_node,
          relationship_type=input$publish_relationship_type,
          evidence_strength=input$publish_evidence_strength,
          short_explanation=input$publish_short_explanation,
          rel_badge=input$publish_rel_badge,
          primary_text=input$publish_primary_text,
          secondary_label=input$publish_secondary_label,
          secondary_text=input$publish_secondary_text,
          nodes=nodes,node_content=node_content,relationships=relationships,
          relationship_content=relationship_content,relationship_sources=relationship_sources,
          sources=sources,interventions=interventions,local_metric_mappings=local_metric_mappings,
          local_metric_notes=local_metric_notes,qa_source_domains=qa_source_domains,qa_metric_rules=qa_metric_rules
        ),
        error=function(e)e
      )
      if(inherits(preview,"error")) {
        publish_preview_rv(NULL); publish_status_rv(paste0("Cannot build change set: ",conditionMessage(preview)))
        showNotification(conditionMessage(preview),type="error",duration=7); return()
      }
      publish_preview_rv(preview)
      publish_status_rv(paste0("Pre-publish QA complete. Worst status: ",preview$worst,
        if(preview$worst=="FAIL") "\nPublishing is blocked until the FAIL is resolved." else "\nReview the exact changes below, then publish if they are correct."))
    })

    observeEvent(input$publish_approved_changes,{
      cid <- proposal_candidate_rv(); df <- discovery_candidates_rv(); idx <- match(cid,df$candidate_id); if(is.na(idx)) return()
      row <- df[idx,,drop=FALSE]; preview <- publish_preview_rv(); if(is.null(preview)) return()
      if (!identical(as.character(row$proposal_review_status),"EVIDENCE_REVIEWED")) {showNotification("Evidence review is required.",type="error");return()}
      if (identical(preview$worst,"FAIL")) {showNotification("Publishing is blocked because pre-publish QA contains a FAIL.",type="error");return()}
      if (length(preview$messages) == 1 && grepl("source",preview$messages[1],ignore.case=TRUE) && !isTRUE(input$publish_add_node) && !isTRUE(input$publish_add_relationship)) {
        showNotification("Choose at least one graph change (new node and/or relationship) before publishing.",type="warning");return()
      }
      backup_dir <- tryCatch(uh_commit_publish_preview(preview,"data"),error=function(e)e)
      if(inherits(backup_dir,"error")){showNotification(paste("Publish failed:",conditionMessage(backup_dir)),type="error",duration=8);return()}
      uh_append_publish_log(row,preview,"data/publish_log.csv")
      df$status[idx] <- "PUBLISHED"; df$proposal_review_status[idx] <- "PUBLISHED"; df$reviewed_at[idx] <- format(Sys.time(),"%Y-%m-%d %H:%M:%S")
      df$admin_notes[idx] <- input$proposal_admin_notes
      discovery_candidates_rv(df); save_discovery_candidates(df)
      publish_status_rv(paste0("PUBLISHED. Backup created at: ",backup_dir,"\nReload the app to display the new graph data."))
      showNotification("Approved graph changes published. A backup was created automatically.",type="message",duration=8)
    })

    observeEvent(input$reload_after_publish,{
      session$reload()
    })

    observeEvent(input$return_proposal_to_keep,{
      cid<-proposal_candidate_rv(); df<-discovery_candidates_rv(); idx<-match(cid,df$candidate_id); if(is.na(idx)) return()
      df$status[idx]<-"KEEP"; discovery_candidates_rv(df); save_discovery_candidates(df); removeModal(); showNotification("Proposal returned to Kept.",type="message")
    })

    observeEvent(input$add_manual_trusted_source,{
      org<-trimws(input$manual_source_org); title<-trimws(input$manual_source_title); url<-trimws(input$manual_source_url); notes<-trimws(input$manual_source_notes)
      if(!nzchar(title)||!nzchar(url)||!grepl("^https?://",url)){showNotification("Enter a source title and a full http/https URL.",type="warning");return()}
      row<-make_manual_trusted_candidate(org,title,url,notes,nodes); merged<-merge_discovery_candidates(discovery_candidates_rv(),ensure_candidate_columns(row))
      discovery_candidates_rv(merged); save_discovery_candidates(merged); discovery_view_rv("KEEP")
      updateTextInput(session,"manual_source_title",value=""); updateTextInput(session,"manual_source_url",value=""); updateTextAreaInput(session,"manual_source_notes",value="")
      showNotification("Trusted source added to Kept.",type="message")
    })

    observeEvent(input$add_discovery_watch,{
      label<-if(is.null(input$new_watch_label))"" else trimws(input$new_watch_label); query<-if(is.null(input$new_watch_query))"" else trimws(input$new_watch_query); cdcq<-if(is.null(input$new_watch_cdc_query))"" else trimws(input$new_watch_cdc_query)
      if(!nzchar(label)||(!nzchar(query)&&!nzchar(cdcq))){showNotification("Enter a label and at least one PubMed or CDC query.",type="warning");return()}
      df<-ensure_watchlist_columns(discovery_watchlist_rv()); qid<-paste0("q_custom_",format(Sys.time(),"%Y%m%d%H%M%S"))
      newrow<-data.frame(query_id=qid,label=label,query=query,cdc_query=cdcq,active=TRUE,source_tier=2,stringsAsFactors=FALSE)
      df<-rbind(df,newrow); discovery_watchlist_rv(df); save_discovery_watchlist(df)
      updateTextInput(session,"new_watch_label",value=""); updateTextInput(session,"new_watch_query",value=""); updateTextInput(session,"new_watch_cdc_query",value="")
      showNotification("Watch query added.",type="message")
    })

    if(UPSTREAM_DISCOVERY_AUTO_SCAN){
      auto_discovery_timer<-reactiveTimer(6*60*60*1000,session)
      observe({auto_discovery_timer(); isolate({
        wl<-discovery_watchlist_rv(); p<-scan_pubmed_watchlist(wl,nodes,days_back=30); c<-scan_cdc_watchlist(wl,nodes)
        merged<-merge_discovery_candidates(discovery_candidates_rv(),rbind(ensure_candidate_columns(p),ensure_candidate_columns(c)))
        discovery_candidates_rv(merged);save_discovery_candidates(merged);discovery_last_scan(Sys.time())
      })})
    }
  }


  if (UPSTREAM_ADMIN_MODE) {

    observeEvent(
      input$run_qa,
      {
        show_qa_console()
      }
    )

    observeEvent(
      input$qa_investigate,
      {
        parts <- strsplit(input$qa_investigate, "::", fixed = TRUE)[[1]]

        if (length(parts) != 2) {
          return()
        }

        qa_selected_issue(
          list(
            scope = parts[1],
            row_index = suppressWarnings(as.integer(parts[2]))
          )
        )

        showModal(
          modalDialog(
            title = "Investigate QA issue",
            uiOutput("qa_issue_detail"),
            size = "l",
            easyClose = TRUE,
            footer = tagList(
              actionButton(
                "qa_mark_reviewed",
                "Mark reviewed this session",
                class = "qa-button"
              ),
              actionButton(
                "qa_back_to_console",
                "Back to QA",
                class = "full-network-btn"
              ),
              modalButton("Close")
            )
          )
        )
      }
    )

    observeEvent(
      input$qa_mark_reviewed,
      {
        selected <- qa_selected_issue()

        if (is.null(selected)) {
          return()
        }

        local_result <- live_local_data()
        report <-
          if (selected$scope == "live" && !is.null(local_result) && !is.null(local_result$qa_report)) {
            local_result$qa_report
          } else {
            static_qa_report
          }

        if (selected$row_index < 1 || selected$row_index > nrow(report)) {
          return()
        }

        row <- report[selected$row_index, , drop = FALSE]
        issue_key <- qa_issue_key(selected$scope, selected$row_index, row)
        qa_reviewed_issues(unique(c(qa_reviewed_issues(), issue_key)))
        showNotification("Marked reviewed for this R session.", type = "message")
      }
    )

    observeEvent(
      input$qa_back_to_console,
      {
        show_qa_console()
      }
    )
  }


  # ==========================================================
  # FOCUS LABEL
  # ==========================================================
  
  output$focus_text <-
    renderText({
      
      type <- selected_type()
      id <- selected_id()
      
      if (
        is.null(type) ||
        is.null(id)
      ) {
        
        return(
          "Click a term or arrow to explore it"
        )
      }
      
      if (type == "node") {
        
        name <-
          nodes$node_name[
            nodes$node_id == id
          ]
        
        return(
          paste(
            "Currently exploring:",
            name
          )
        )
      }
      
      edge <-
        edges_with_sources[
          edges_with_sources$relationship_id == id,
        ]
      
      if (nrow(edge) > 0) {
        
        return(
          paste(
            edge$from_name[1],
            "→",
            edge$to_name[1]
          )
        )
      }
      
      ""
    })
  
  
  # ==========================================================
  # ZIP SEARCH
  # ==========================================================
  
  observeEvent(
    input$find_local_help,
    {
      
      entered_zip <-
        isolate(
          input$local_zip
        )
      
      current_node <-
        isolate(
          selected_id()
        )
      
      searched_zip(
        entered_zip
      )
      
      if (
        is.null(entered_zip) ||
        !grepl(
          "^[0-9]{5}$",
          entered_zip
        )
      ) {
        
        live_local_data(
          list(
            ok = FALSE,
            message =
              "Please enter a valid 5-digit ZIP code."
          )
        )
        
        scroll_to_local_results()
        
        return()
      }
      
      
      place_result <-
        lookup_zip_place(
          entered_zip
        )

      cdc_result <-
        fetch_cdc_places(
          entered_zip
        )
      
      census_result <-
        fetch_census_acs(
          entered_zip
        )
      
      
      if (
        !cdc_result$ok &&
        !census_result$ok
      ) {
        
        live_local_data(
          list(
            ok = FALSE,
            message =
              "Neither CDC PLACES nor Census ACS returned local data for this ZIP.",
            cdc_message =
              cdc_result$message,
            census_message =
              census_result$message,
            place = place_result
          )
        )
        
        scroll_to_local_results()
        
        return()
      }
      
      
      context <-
        build_local_context(
          current_node,
          cdc_result,
          census_result
        )

      live_qa_report <-
        run_live_qa(
          node_id = current_node,
          cdc_result = cdc_result,
          census_result = census_result,
          local_metric_mappings = local_metric_mappings,
          qa_metric_rules = qa_metric_rules
        )
      
      
      # In public mode, FAIL-level live metrics are withheld rather than
      # displayed as if they were trustworthy. Admin mode keeps them visible
      # so the underlying raw result can be investigated.
      if (!UPSTREAM_ADMIN_MODE && !is.null(live_qa_report)) {
        blocked_labels <- unique(
          live_qa_report$item[
            live_qa_report$status == "FAIL" &
              live_qa_report$area == "Live statistics"
          ]
        )

        if (length(blocked_labels) > 0) {
          context$primary <- context$primary[
            !context$primary$label %in% blocked_labels,
            ,
            drop = FALSE
          ]
          context$related <- context$related[
            !context$related$label %in% blocked_labels,
            ,
            drop = FALSE
          ]

          verification_note <-
            "One or more local statistics are temporarily unavailable while the source data are being verified."

          if (is.null(context$direct_message) || !nzchar(context$direct_message)) {
            context$direct_message <- verification_note
          } else {
            context$direct_message <- paste(context$direct_message, verification_note)
          }
        }
      }

      live_local_data(
        list(
          ok = TRUE,
          zip = entered_zip,
          node_id = current_node,
          context = context,
          qa_report = live_qa_report,
          cdc_result = cdc_result,
          census_result = census_result,
          place = place_result,
          cdc_ok = cdc_result$ok,
          census_ok = census_result$ok,
          cdc_message =
            if (cdc_result$ok) {
              NULL
            } else {
              cdc_result$message
            },
          census_message =
            if (census_result$ok) {
              NULL
            } else {
              census_result$message
            }
        )
      )
      
      scroll_to_local_results()
    }
  )
  
  
  # ==========================================================
  # METRIC CARDS
  # ==========================================================
  
  render_metric_cards <- function(metric_data) {
    
    if (
      is.null(metric_data) ||
      nrow(metric_data) == 0
    ) {
      return(NULL)
    }
    
    lapply(
      
      seq_len(
        nrow(metric_data)
      ),
      
      function(i) {
        
        row <- metric_data[i, ]
        
        tags$div(
          
          class = "local-data-card",
          
          tags$div(
            class = "local-data-measure",
            row$label
          ),
          
          tags$div(
            class = "local-data-value",
            row$value
          ),
          
          if (
            !is.na(row$note) &&
            nzchar(row$note)
          ) {
            
            tags$div(
              class = "metric-note",
              row$note
            )
          },
          
          tags$div(
            
            class = "metric-source",
            
            paste0(
              row$source,
              ifelse(
                nzchar(row$year),
                paste0(
                  " • ",
                  row$year
                ),
                ""
              )
            ),
            
            if (
              !is.na(row$url) &&
              nzchar(row$url)
            ) {
              
              tagList(
                " • ",
                tags$a(
                  href = row$url,
                  target = "_blank",
                  "source ↗"
                )
              )
            }
          )
        )
      }
    )
  }
  
  
  # ==========================================================
  # SIDE PANEL
  # ==========================================================
  
  output$side_panel <-
    renderUI({
      
      type <- selected_type()
      id <- selected_id()
      
      if (
        is.null(type) ||
        is.null(id)
      ) {
        return(NULL)
      }
      
      
      # ======================================================
      # EDGE PANEL
      # ======================================================
      
      if (type == "edge") {
        
        edge <-
          edges_with_sources[
            edges_with_sources$relationship_id == id,
          ]
        
        if (nrow(edge) == 0) {
          return(NULL)
        }
        
        edge <- edge[1, ]
        
        
        return(
          
          tags$div(
            
            class = "side-panel",
            
            tags$div(
              
              class = "side-panel-header",
              
              tags$div(
                
                tags$div(
                  class = "relationship-panel-badge",
                  edge$badge
                ),
                
                tags$div(
                  
                  class = "relationship-big-path",
                  
                  edge$from_name,
                  
                  tags$span(
                    class = "relationship-big-arrow",
                    "→"
                  ),
                  
                  edge$to_name
                )
              ),
              
              actionButton(
                "close_panel",
                HTML("&times;"),
                class = "close-panel-button"
              )
            ),
            
            
            tags$div(
              class = "evidence-section",
              tags$h4("Why These Are Linked"),
              tags$p(style = "font-weight:650;color:#F0F4F5;", edge$short_explanation)
            ),

            tags$div(
              class = "evidence-section",
              tags$h4("How the Connection Works"),
              tags$p(edge$primary_text)
            ),

            if (nzchar(edge$statistic_text)) {
              tags$div(
                class = "stat-card",
                tags$div(
                  style = "color:#A9D7B6;font-size:11px;font-weight:800;letter-spacing:.7px;margin-bottom:6px;",
                  ifelse(nzchar(edge$statistic_label), edge$statistic_label, "BY THE NUMBERS")
                ),
                tags$div(
                  style = "color:#E4EAEB;font-size:14px;line-height:1.55;",
                  edge$statistic_text
                ),
                if (nzchar(edge$statistic_source_url)) {
                  tags$div(
                    style = "margin-top:8px;font-size:11px;color:#81969D;",
                    paste0("Statistic source: ", edge$statistic_source_org),
                    " • ",
                    tags$a(
                      href = edge$statistic_source_url,
                      target = "_blank",
                      "open source ↗"
                    )
                  )
                }
              )
            },

            if (nzchar(edge$secondary_text)) {
              tags$div(
                class = "stat-card",
                tags$div(
                  style = "color:#8FB9C7;font-size:10px;font-weight:700;letter-spacing:.6px;margin-bottom:5px;",
                  ifelse(nzchar(edge$secondary_label), edge$secondary_label, "CONTEXT / LIMITATION")
                ),
                tags$div(
                  style = "color:#CFD8DA;font-size:13px;line-height:1.5;",
                  edge$secondary_text
                )
              )
            },

            tags$div(
              class = "evidence-section",
              tags$h4("Evidence & Relationship Type"),
              
              tags$p(
                tags$b(
                  "Relationship type: "
                ),
                gsub(
                  "_",
                  " ",
                  edge$relationship_type
                )
              ),
              
              tags$p(
                tags$b(
                  "Evidence strength: "
                ),
                edge$evidence_strength
              )
            ),
            
            
            if (
              !is.na(edge$url) &&
              nzchar(edge$url)
            ) {
              
              tags$div(
                
                class = "evidence-section",
                
                tags$h4(
                  "Source"
                ),
                
                tags$p(
                  edge$organization,
                  tags$br(),
                  edge$title
                ),
                
                tags$a(
                  class = "source-button",
                  href = edge$url,
                  target = "_blank",
                  "Open source ↗"
                )
              )
            }
          )
        )
      }
      
      
      # ======================================================
      # NODE PANEL
      # ======================================================
      
      node <-
        nodes[
          nodes$node_id == id,
        ]
      
      if (nrow(node) == 0) {
        return(NULL)
      }
      
      
      incoming <-
        edges_with_sources[
          edges_with_sources$to_node == id,
        ]
      
      outgoing <-
        edges_with_sources[
          edges_with_sources$from_node == id,
        ]
      
      node_interventions <-
        interventions[
          interventions$target_node == id,
        ]
      
      
      heading_text <-
        get_intervention_heading(
          node$node_type[1]
        )
      
      intro_text <-
        get_intervention_intro(
          node$node_type[1]
        )
      
      
      # ======================================================
      # INTERVENTION UI
      # ======================================================
      
      intervention_ui <- list()
      
      
      if (nrow(node_interventions) > 0) {
        
        sections <- list()
        
        for (level_name in intervention_level_order) {
          
          level_data <-
            node_interventions[
              node_interventions$display_level == level_name,
            ]
          
          if (nrow(level_data) == 0) {
            next
          }
          
          level_class <-
            switch(
              level_name,
              "Individual" =
                "level-individual",
              "Healthcare" =
                "level-healthcare",
              "Community" =
                "level-community",
              "Policy / Structural" =
                "level-policy",
              "level-other"
            )
          
          
          cards <-
            lapply(
              
              seq_len(
                nrow(level_data)
              ),
              
              function(i) {
                
                item <- level_data[i, ]
                
                source_info <-
                  sources[
                    sources$source_id ==
                      item$source_id,
                  ]
                
                source_ui <- NULL
                
                if (nrow(source_info) > 0) {
                  
                  source_ui <-
                    tags$div(
                      
                      class = "source-line",
                      
                      tags$b(
                        "Source: "
                      ),
                      
                      source_info$organization[1],
                      
                      " — ",
                      
                      tags$a(
                        href = source_info$url[1],
                        target = "_blank",
                        source_info$title[1]
                      )
                    )
                }
                
                
                tags$div(
                  
                  class = "intervention-card",
                  
                  tags$div(
                    class = "intervention-name",
                    item$intervention_name
                  ),
                  
                  tags$div(
                    class = "intervention-summary",
                    item$description
                  ),
                  
                  tags$details(
                    
                    tags$summary(
                      "Show more"
                    ),
                    
                    tags$div(
                      
                      class = "detail-content",
                      
                      tags$p(
                        tags$b("Who can act: "),
                        item$actor
                      ),
                      
                      tags$p(
                        tags$b("Barrier addressed: "),
                        item$barrier_addressed
                      ),
                      
                      tags$p(
                        tags$b("Setting: "),
                        item$setting
                      ),
                      
                      tags$p(
                        tags$b("Evidence: "),
                        item$evidence_rating
                      ),
                      
                      tags$p(
                        tags$b("Implementation complexity: "),
                        item$implementation_complexity
                      ),
                      
                      tags$p(
                        tags$b("Typical time horizon: "),
                        item$time_horizon
                      ),
                      
                      source_ui
                    )
                  )
                )
              }
            )
          
          
          sections[[level_name]] <-
            
            tags$div(
              
              class = "intervention-section",
              
              tags$div(
                class =
                  paste(
                    "level-heading",
                    level_class
                  ),
                level_name
              ),
              
              cards
            )
        }
        
        
        intervention_ui <-
          tagList(
            
            tags$h4(
              class = "interventions-title",
              heading_text
            ),
            
            tags$div(
              class = "intervention-intro",
              intro_text
            ),
            
            sections
          )
        
      } else {
        
        intervention_ui <-
          tagList(
            
            tags$h4(
              class = "interventions-title",
              heading_text
            ),
            
            tags$p(
              style =
                "color:#888888;font-size:13px;",
              "No specific strategies have been added yet."
            )
          )
      }
      
      
      # ======================================================
      # LIVE LOCAL RESULTS
      # ======================================================
      
      local_result <- live_local_data()
      live_data_ui <- NULL
      
      
      if (!is.null(local_result)) {
        
        if (!isTRUE(local_result$ok)) {
          
          live_data_ui <-
            tags$div(
              
              class = "local-results-anchor",
              
              if (!is.null(local_result$place) && isTRUE(local_result$place$ok)) {
                tags$div(
                  class = "local-data-heading",
                  paste0(
                    local_result$place$place_name,
                    ", ",
                    local_result$place$state,
                    " · ",
                    searched_zip()
                  )
                )
              },

              tags$div(
                class = "data-error",
                local_result$message
              ),
              
              if (
                !is.null(
                  local_result$census_message
                )
              ) {
                
                tags$div(
                  class = "data-error",
                  paste(
                    "Census:",
                    local_result$census_message
                  )
                )
              },
              
              if (
                !is.null(
                  local_result$cdc_message
                )
              ) {
                
                tags$div(
                  class = "data-error",
                  paste(
                    "CDC:",
                    local_result$cdc_message
                  )
                )
              }
            )
          
        } else if (
          identical(
            local_result$node_id,
            id
          )
        ) {
          
          context <- local_result$context
          
          primary_ui <-
            render_metric_cards(
              context$primary
            )
          
          related_ui <-
            render_metric_cards(
              context$related
            )
          
          
          live_data_ui <-
            tags$div(
              
              class = "local-results-anchor",
              
              tags$div(
                class = "local-data-heading",
                {
                  place <- local_result$place
                  if (!is.null(place) && isTRUE(place$ok)) {
                    paste0(
                      "Local snapshot for ",
                      place$place_name,
                      ", ",
                      place$state,
                      " · ",
                      local_result$zip
                    )
                  } else {
                    paste("Local snapshot for", local_result$zip)
                  }
                }
              ),

              if (!is.null(local_result$place) && isTRUE(local_result$place$ok)) {
                tags$div(
                  style = "color:#94A3A8;font-size:11px;margin-top:3px;margin-bottom:9px;",
                  paste0(
                    "ZIP location: ",
                    local_result$place$place_name,
                    ", ",
                    local_result$place$state_abbr
                  )
                )
              },
              
              tags$span(
                class = "live-badge",
                "Live public data"
              ),

              if (UPSTREAM_ADMIN_MODE && !is.null(local_result$qa_report)) {

                {
                  local_qa_status <- qa_worst_status(local_result$qa_report)
                  local_qa_class <-
                    switch(
                      local_qa_status,
                      "PASS" = "qa-pass",
                      "WARNING" = "qa-warning",
                      "REVIEW" = "qa-review",
                      "FAIL" = "qa-fail",
                      "qa-review"
                    )

                  tags$span(
                    class = paste("live-qa-badge", local_qa_class),
                    paste("QA", local_qa_status)
                  )
                }
              },
              
              
              if (isTRUE(local_result$census_ok)) {
                
                tags$div(
                  class =
                    "source-status source-status-good",
                  "✓ U.S. Census ACS connected"
                )
                
              } else {
                
                tags$div(
                  class =
                    "source-status source-status-bad",
                  paste(
                    "Census unavailable:",
                    local_result$census_message
                  )
                )
              },
              
              
              if (isTRUE(local_result$cdc_ok)) {
                
                tags$div(
                  class =
                    "source-status source-status-good",
                  "✓ CDC PLACES connected"
                )
                
              } else {
                
                tags$div(
                  class =
                    "source-status source-status-bad",
                  paste(
                    "CDC unavailable:",
                    local_result$cdc_message
                  )
                )
              },
              
              
              if (
                !is.null(
                  context$direct_message
                )
              ) {
                
                tags$div(
                  class = "no-direct-box",
                  context$direct_message
                )
              },
              
              
              if (
                nrow(
                  context$primary
                ) > 0
              ) {
                
                tagList(
                  
                  tags$div(
                    class = "data-section-label",
                    "Closest direct local measures"
                  ),
                  
                  primary_ui
                )
              },
              
              
              if (
                nrow(
                  context$related
                ) > 0
              ) {
                
                tagList(
                  
                  tags$div(
                    class = "data-section-label",
                    "Related local context"
                  ),
                  
                  related_ui
                )
              },
              
              
              tags$div(
                
                class = "data-note",
                
                paste(
                  "CDC PLACES values are model-based population estimates",
                  "for the ZIP Code Tabulation Area. Census values are ACS",
                  "5-year estimates. Related metrics are labeled as context",
                  "rather than presented as direct measures."
                )
              ),
              
              
              tags$div(
                
                class = "program-coming",
                
                tags$b(
                  "Local programs"
                ),
                
                tags$br(),
                
                paste(
                  "The 211 V2 connection will appear here when",
                  "your API subscription is approved."
                )
              )
            )
        }
      }
      
      
      # ======================================================
      # LOCAL SEARCH BOX
      # ======================================================
      
      search_terms <-
        unique(
          node_interventions$intervention_name
        )
      
      search_tags <-
        lapply(
          
          search_terms,
          
          function(term) {
            
            tags$span(
              class = "search-tag",
              term
            )
          }
        )
      
      
      local_help_ui <-
        tags$div(
          
          class = "local-help-box",
          
          tags$div(
            class = "local-help-title",
            "Explore your area"
          ),
          
          tags$div(
            class = "local-help-subtitle",
            paste(
              "Enter a ZIP code to see the most relevant",
              "local measures for this specific concept."
            )
          ),
          
          div(
            
            class = "zip-field",
            
            textInput(
              "local_zip",
              "ZIP code",
              value =
                ifelse(
                  is.null(
                    searched_zip()
                  ),
                  "",
                  searched_zip()
                ),
              placeholder =
                "e.g. 04101"
            )
          ),
          
          actionButton(
            "find_local_help",
            "Explore this ZIP",
            class =
              "local-search-button"
          ),
          
          if (
            length(
              search_terms
            ) > 0
          ) {
            
            tags$div(
              
              class = "search-preview",
              
              tags$div(
                class = "search-preview-title",
                "Programs will eventually be matched to:"
              ),
              
              search_tags
            )
          },
          
          live_data_ui
        )
      
      
      # ======================================================
      # FULL NODE PANEL
      # ======================================================
      
      tags$div(
        
        class = "side-panel",
        
        tags$div(
          
          class = "side-panel-header",
          
          tags$div(
            
            tags$div(
              class = "panel-node-badge",
              node$badge[1]
            ),
            
            tags$h2(
              class = "side-panel-title",
              node$node_name[1]
            )
          ),
          
          actionButton(
            "close_panel",
            HTML("&times;"),
            class = "close-panel-button"
          )
        ),
        
        
        tags$div(
          class = "panel-description",
          node$description[1]
        ),
        
        
        tags$div(
          
          class = "why-box",
          
          tags$div(
            class = "why-label",
            "Why this matters"
          ),
          
          tags$div(
            class = "why-text",
            node$why_it_matters[1]
          )
        ),
        
        
        intervention_ui,
        
        local_help_ui,
        
        
        if (nrow(incoming) > 0) {
          
          tags$div(
            
            class = "connections",
            
            tags$h4(
              "Upstream connections"
            ),
            
            tags$ul(
              
              lapply(
                
                seq_len(
                  nrow(incoming)
                ),
                
                function(i) {
                  
                  tags$li(
                    paste0(
                      incoming$from_name[i],
                      " — ",
                      incoming$short_explanation[i]
                    )
                  )
                }
              )
            )
          )
        },
        
        
        if (nrow(outgoing) > 0) {
          
          tags$div(
            
            class = "connections",
            
            tags$h4(
              "Downstream connections"
            ),
            
            tags$ul(
              
              lapply(
                
                seq_len(
                  nrow(outgoing)
                ),
                
                function(i) {
                  
                  tags$li(
                    paste0(
                      outgoing$to_name[i],
                      " — ",
                      outgoing$short_explanation[i]
                    )
                  )
                }
              )
            )
          )
        }
      )
    })
  
  
  # ==========================================================
  # NETWORK
  # ==========================================================
  
  output$health_network <-
    renderVisNetwork({
      
      network <-
        
        visNetwork(
          
          vis_nodes,
          vis_edges,
          
          width = "100%",
          height = "1200px",
          background = "#050505"
          
        ) %>%
        
        visLayout(
          randomSeed = 42,
          improvedLayout = FALSE
        ) %>%
        
        visNodes(
          
          shape = "box",
          
          margin =
            list(
              top = 12,
              right = 17,
              bottom = 12,
              left = 17
            ),
          
          widthConstraint =
            list(
              maximum = 210
            ),
          
          heightConstraint =
            list(
              minimum = 40
            ),
          
          scaling =
            list(
              min = 18,
              max = 35,
              label =
                list(
                  enabled = TRUE,
                  min = 19,
                  max = 19,
                  drawThreshold = 10
                )
            ),
          
          font =
            list(
              face = "Manrope",
              strokeWidth = 0
            ),
          
          shapeProperties =
            list(
              borderRadius = 14
            ),
          
          shadow =
            list(
              enabled = TRUE,
              color =
                "rgba(0,0,0,0.36)",
              size = 8,
              x = 0,
              y = 3
            )
          
        ) %>%
        
        visEdges(
          
          color =
            list(
              color =
                "rgba(105,137,149,0.24)",
              highlight =
                "rgba(172,213,229,0.98)",
              hover =
                "rgba(172,213,229,0.92)",
              inherit = FALSE
            ),
          
          width = 1.55,
          hoverWidth = 2.9,
          selectionWidth = 3.3,
          
          smooth =
            list(
              enabled = TRUE,
              type = "continuous",
              roundness = 0.18
            ),
          
          arrows =
            list(
              to =
                list(
                  enabled = TRUE,
                  scaleFactor = 0.58
                )
            )
          
        ) %>%
        
        visInteraction(
          hover = TRUE,
          navigationButtons = FALSE,
          keyboard = TRUE,
          zoomView = TRUE,
          dragView = TRUE,
          dragNodes = FALSE
        ) %>%
        
        visOptions(
          
          highlightNearest =
            list(
              enabled = TRUE,
              degree = 1,
              hover = TRUE,
              hideColor =
                "rgba(74,82,85,0.10)",
              labelOnly = FALSE
            )
        ) %>%
        
        visEvents(
          
          selectNode = "

            function(properties) {

              if (
                properties.nodes.length > 0
              ) {

                Shiny.setInputValue(
                  'clicked_node',
                  properties.nodes[0],
                  {priority:'event'}
                );
              }
            }

          ",
          
          selectEdge = "

            function(properties) {

              if (
                properties.edges.length > 0 &&
                properties.nodes.length == 0
              ) {

                Shiny.setInputValue(
                  'clicked_edge',
                  properties.edges[0],
                  {priority:'event'}
                );
              }
            }

          "
        ) %>%
        
        visPhysics(
          enabled = FALSE
        )
      
      
      # ======================================================
      # CUSTOM BUTTON LOGIC
      # ======================================================
      
      htmlwidgets::onRender(
        
        network,
        
        "

        function(el, x) {

          var network = this.network;

          if (!network) {
            console.log('The Health Web: network object not found');
            return;
          }

          requestAnimationFrame(function() {
            var loader = document.getElementById('network_loading');
            if (loader) {
              loader.classList.add('is-ready');
              setTimeout(function() { loader.style.display = 'none'; }, 320);
            }
          });


          /*
           * STABLE MAP INTERACTION
           *
           * Node coordinates are calculated in R before rendering, so there
           * is no browser-side stabilization delay. Nodes remain fixed, but
           * the viewport can be panned and zoomed like a normal map.
           */

          network.setOptions({
            physics:{enabled:false},
            interaction:{
              dragView:true,
              dragNodes:false,
              zoomView:true
            }
          });

          /*
           * DOWNSTREAM FLOW ANIMATION
           *
           * A small glowing particle follows each edge's actual drawn path from
           * its `from` node to its `to` node. That gives the network a constant, subtle sense of
           * direction without making the arrows thicker or more cluttered.
           *
           * Hovering an edge pauses that edge. Clicking/selecting an edge
           * keeps that edge paused while the evidence panel is open. People
           * who prefer reduced motion get the static network automatically.
           */

          if (el.__upstreamFlowFrame) {
            cancelAnimationFrame(el.__upstreamFlowFrame);
          }

          var reduceMotion = false;
          try {
            reduceMotion = window.matchMedia &&
              window.matchMedia('(prefers-reduced-motion: reduce)').matches;
          } catch (e) {}

          var flowState = {
            hoveredEdge: null,
            selectedEdge: null,
            lastRedraw: 0
          };

          function flowAlpha(colorValue) {
            if (!colorValue || typeof colorValue !== 'string') return 0.24;
            var match = colorValue.match(/rgba[(][^,]+,[^,]+,[^,]+,[ ]*([0-9.]+)[ ]*[)]/i);
            if (!match) return 1;
            var value = parseFloat(match[1]);
            return isNaN(value) ? 1 : value;
          }

          function flowParticleColor(edgeItem, alpha) {
            var c = edgeItem && edgeItem.color;
            if (typeof c === 'string') {
              if (c.indexOf('239,205,132') !== -1) {
                return 'rgba(247,214,145,0.96)';
              }
              if (c.indexOf('120,203,235') !== -1) {
                return 'rgba(130,211,242,0.98)';
              }
            }
            return alpha > 0.70
              ? 'rgba(177,220,236,0.96)'
              : 'rgba(151,190,205,0.62)';
          }

          function stablePhase(id) {
            var text = String(id || '');
            var h = 0;
            for (var i = 0; i < text.length; i++) {
              h = ((h << 5) - h) + text.charCodeAt(i);
              h |= 0;
            }
            return (Math.abs(h) % 1000) / 1000;
          }

          network.on('hoverEdge', function(params) {
            flowState.hoveredEdge = params && params.edge != null
              ? String(params.edge)
              : null;
          });

          network.on('blurEdge', function(params) {
            if (!params || params.edge == null ||
                flowState.hoveredEdge === String(params.edge)) {
              flowState.hoveredEdge = null;
            }
          });

          network.on('selectEdge', function(params) {
            flowState.selectedEdge = params && params.edges && params.edges.length
              ? String(params.edges[0])
              : null;
          });

          network.on('deselectEdge', function() {
            flowState.selectedEdge = null;
          });

          network.on('selectNode', function() {
            flowState.selectedEdge = null;
          });

          if (!reduceMotion) {
            network.on('afterDrawing', function(ctx) {
              var edgesData = network.body && network.body.data
                ? network.body.data.edges
                : null;
              if (!edgesData) return;

              var now = performance.now();
              // Slow, deliberate movement: one pass takes about 6.5 seconds.
              var base = (now % 6500) / 6500;
              var ids = edgesData.getIds();

              for (var i = 0; i < ids.length; i++) {
                var id = String(ids[i]);
                if (id === flowState.hoveredEdge || id === flowState.selectedEdge) {
                  continue;
                }

                var edgeItem = edgesData.get(ids[i]);
                if (!edgeItem || edgeItem.hidden) continue;

                var alpha = flowAlpha(edgeItem.color);
                if (alpha < 0.12) continue;

                var phase = stablePhase(id);
                var t = (base + phase) % 1;

                // Keep the particle away from the node centers. Most importantly,
                // use visNetwork's own edge geometry so it follows the actual
                // straight/curved arrow rather than cutting across beside it.
                t = 0.06 + (t * 0.88);

                var point = null;
                var liveEdge = network.body && network.body.edges
                  ? network.body.edges[ids[i]]
                  : null;

                try {
                  if (liveEdge && liveEdge.edgeType &&
                      typeof liveEdge.edgeType.getPoint === 'function') {
                    point = liveEdge.edgeType.getPoint(t);
                  } else if (liveEdge && typeof liveEdge.getPoint === 'function') {
                    point = liveEdge.getPoint(t);
                  }
                } catch (e) {
                  point = null;
                }

                // Fallback for any edge type that does not expose its path.
                if (!point || !isFinite(point.x) || !isFinite(point.y)) {
                  var positions = network.getPositions([edgeItem.from, edgeItem.to]);
                  var fromPos = positions[edgeItem.from];
                  var toPos = positions[edgeItem.to];
                  if (!fromPos || !toPos) continue;
                  point = {
                    x: fromPos.x + ((toPos.x - fromPos.x) * t),
                    y: fromPos.y + ((toPos.y - fromPos.y) * t)
                  };
                }

                var xPos = point.x;
                var yPos = point.y;
                var strong = alpha > 0.70;
                var radius = strong ? 2.8 : 1.65;

                ctx.save();
                ctx.beginPath();
                ctx.arc(xPos, yPos, radius, 0, Math.PI * 2, false);
                ctx.fillStyle = flowParticleColor(edgeItem, alpha);
                ctx.shadowColor = flowParticleColor(edgeItem, alpha);
                ctx.shadowBlur = strong ? 7 : 4;
                ctx.fill();
                ctx.restore();
              }
            });

            function animateDownstreamFlow(timestamp) {
              if (!document.documentElement.contains(el)) return;

              // ~30 FPS is smooth enough for flow while keeping CPU use low.
              if (timestamp - flowState.lastRedraw > 33) {
                if (el.offsetParent !== null) {
                  network.redraw();
                }
                flowState.lastRedraw = timestamp;
              }

              el.__upstreamFlowFrame = requestAnimationFrame(animateDownstreamFlow);
            }

            el.__upstreamFlowFrame = requestAnimationFrame(animateDownstreamFlow);
          }

          setTimeout(function() {
            network.fit({
              animation:{
                duration:180,
                easingFunction:'easeInOutQuad'
              }
            });

            // Start a little closer than a full-network fit so first-time
            // visitors can actually read node labels immediately. Users can
            // still zoom out or use Return to full network whenever they want.
            setTimeout(function() {
              var fittedScale = network.getScale();
              var currentPosition = network.getViewPosition();
              var isPhone = window.innerWidth <= 700;
              var minimumReadableScale = isPhone ? 0.54 : 0.58;
              var targetScale = Math.max(
                fittedScale * (isPhone ? 1.16 : 1.22),
                minimumReadableScale
              );

              network.moveTo({
                position: currentPosition,
                scale: targetScale,
                animation:{
                  duration:260,
                  easingFunction:'easeInOutQuad'
                }
              });
            }, 230);
          }, 20);

          /*
           * ZOOM IN
           */

          var zoomInButton =
            document.getElementById('zoom_in_btn');

          if (zoomInButton) {

            zoomInButton.onclick = function(event) {

              event.preventDefault();
              event.stopPropagation();

              var currentScale =
                network.getScale();

              var currentPosition =
                network.getViewPosition();

              network.moveTo({

                position:currentPosition,

                scale:Math.min(
                  currentScale * 1.20,
                  2.4
                ),

                animation:{
                  duration:220,
                  easingFunction:'easeInOutQuad'
                }

              });
            };
          }


          /*
           * ZOOM OUT
           */

          var zoomOutButton =
            document.getElementById('zoom_out_btn');

          if (zoomOutButton) {

            zoomOutButton.onclick = function(event) {

              event.preventDefault();
              event.stopPropagation();

              var currentScale =
                network.getScale();

              var currentPosition =
                network.getViewPosition();

              network.moveTo({

                position:currentPosition,

                scale:Math.max(
                  currentScale / 1.20,
                  0.35
                ),

                animation:{
                  duration:220,
                  easingFunction:'easeInOutQuad'
                }

              });
            };
          }


          /*
           * HOME
           *
           * Shiny handles restoring the full graph.
           * JavaScript just tells Shiny the button was clicked.
           */

          var homeButton =
            document.getElementById('map_home_btn');

          if (homeButton) {

            homeButton.onclick = function(event) {

              event.preventDefault();
              event.stopPropagation();

              Shiny.setInputValue(
                'map_home_clicked',
                Date.now(),
                {priority:'event'}
              );
            };
          }


          /*
           * PAGE SCROLL FIX
           */

          var canvas =
            el.querySelector('.vis-network');

          if (!canvas) {
            return;
          }

          canvas.addEventListener(

            'wheel',

            function(event) {

              if (
                !event.ctrlKey &&
                !event.metaKey
              ) {

                event.stopImmediatePropagation();
              }
            },

            true
          );
        }

        "
      )
    })
  
  
  # ==========================================================
  # NODE CLICK
  # ==========================================================
  
  observeEvent(
    input$clicked_node,
    {
      
      id <- input$clicked_node
      
      selected_type("node")
      selected_id(id)
      
      searched_zip(NULL)
      live_local_data(NULL)
      
      proxy <-
        visNetworkProxy(
          "health_network"
        )
      
      visFocus(
        
        proxy,
        
        id = id,
        
        scale = 1.15,
        
        animation =
          list(
            duration = 650,
            easingFunction =
              "easeInOutQuad"
          )
      )
    }
  )
  
  
  # ==========================================================
  # EDGE CLICK
  # ==========================================================
  
  observeEvent(
    input$clicked_edge,
    {
      
      selected_type("edge")
      
      selected_id(
        input$clicked_edge
      )
      
      searched_zip(NULL)
      live_local_data(NULL)
    }
  )
  
  
  # ==========================================================
  # CLOSE PANEL
  # ==========================================================
  
  observeEvent(
    input$close_panel,
    {
      
      selected_type(NULL)
      selected_id(NULL)
      searched_zip(NULL)
      live_local_data(NULL)
    }
  )
  
  
  # ==========================================================
  # CUSTOM HOME BUTTON
  # ==========================================================
  
  observeEvent(
    input$map_home_clicked,
    {
      
      selected_type(NULL)
      selected_id(NULL)
      searched_zip(NULL)
      live_local_data(NULL)
      
      updateSelectizeInput(
        session,
        "node_search",
        selected = ""
      )
      
      show_full_graph()
    }
  )
  
  
  # ==========================================================
  # TOP RESET BUTTON
  # ==========================================================
  
  observeEvent(
    input$reset_view,
    {
      
      selected_type(NULL)
      selected_id(NULL)
      searched_zip(NULL)
      live_local_data(NULL)
      
      updateSelectizeInput(
        session,
        "node_search",
        selected = ""
      )
      
      show_full_graph()
    }
  )
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

# Script to take raw depression overview data and clean for the Descriptive Findings Dashboard

# Purpose: run the original data import/clean/merge steps and export one CSV the app will load.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rio, here, readxl, stringi)

# Import data - Get all outcome domains
outcome_domains_sheets <- readxl::excel_sheets(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"))

# Remove "metadata" from outcome domains
outcome_domains_sheets <- outcome_domains_sheets[outcome_domains_sheets != "metadata"]
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which= "study_level")

# Load all domain sheets and combine
load_all_domains <- function(domains) {
  all_data <- list()
  for (domain in domains) {
    domain_data <- import(here("Data", "Depression_Overview_Meta_Analysis_Data.xlsx"), which = domain)
    if (!"outcome_domain" %in% names(domain_data)) {
      domain_data$outcome_domain <- domain
    }
    if ("icc" %in% names(domain_data)) {
      domain_data$icc <- as.numeric(as.character(domain_data$icc))
    }
    all_data[[domain]] <- domain_data
  }
  bind_rows(all_data)
}

# Load all domain data
all_domains_df <- load_all_domains(outcome_domains_sheets)

# Get unique outcome domains from the actual data (not sheet names)
outcome_domains <- sort(unique(all_domains_df$outcome_domain))

# Load studies data 
studies <- import(here("Data", "Depression_Overview_Primary_Study_Data.xlsx"), which = "study_level")

# Helper function to standardize 
standardize <- function(x) {
  x %>%
    tolower() %>%
    stri_trans_general("Latin-ASCII") %>%
    trimws()
}

# Aligning study years
studies$study_author_year[studies$study_author_year == "McLaughlin 2010"] <- "McLaughlin 2011"

# Standardize keys
all_domains_df <- all_domains_df %>% mutate(study_std = standardize(study))
studies <- studies %>% mutate(study_author_year_std = standardize(study_author_year))

# Create the full merged dataset with all domains
merged_all_domains <- all_domains_df %>%
  left_join(
    studies,
    by = c("study_std" = "study_author_year_std"),
    suffix = c(".df", ".studies")
  )

# Handle duplicate columns
dup_bases <- intersect(
  gsub("\\.df$", "", names(merged_all_domains)[endsWith(names(merged_all_domains), ".df")]),
  gsub("\\.studies$", "", names(merged_all_domains)[endsWith(names(merged_all_domains), ".studies")])
)
for (col in dup_bases) {
  merged_all_domains[[col]] <- merged_all_domains[[paste0(col, ".studies")]]
}
merged_all_domains <- merged_all_domains %>% select(-matches("\\.df$"), -matches("\\.studies$"))

# Helper functions used for deriving columns

# Grade level creation - Updated scheme
classify_grade_level <- function(x) {
  x <- gsub(" ", "", x)
  if (tolower(x) %in% c("cannot tell", "unclear", "")) return("Unclear")
  x <- gsub("K", "0", x, ignore.case = TRUE)
  grades_split <- unlist(strsplit(x, ","))
  if (any(is.na(suppressWarnings(as.numeric(grades_split))))) return("Unclear")
  grades_num <- as.numeric(grades_split)
  min_g <- min(grades_num)
  max_g <- max(grades_num)
  if (min_g >= 0 && max_g <= 8) return("K-8")
  if (min_g >= 9 && max_g <= 12) return("9-12")
  if (min_g <= 8 && max_g >= 9) return("K-12")
  return("Unclear")
}

# Urbanicity creation
clean_urbanicity <- function(x) {
  if (is.na(x) || trimws(x) == "") return("Unclear")
  if (tolower(trimws(x)) %in% c("cannot tell", "unclear")) return("Unclear")
  x <- tolower(x)
  x <- gsub("[0-9]+\\.", "", x)
  x <- gsub("[\r\n]+", ",", x)
  parts <- unlist(strsplit(x, "[,;]+"))
  parts <- trimws(parts)
  possible <- c("rural", "suburban", "urban")
  found <- unique(parts[parts %in% possible])
  if (length(found) == 0) return("Unclear")
  label <- paste(sort(found), collapse = "+")
  stringr::str_to_title(label)
}

# Grade level processing - Individual grades 1-12 plus Unclear
process_grade_levels <- function(grade_string) {
  if (is.na(grade_string) || tolower(trimws(grade_string)) %in% c("cannot tell", "unclear", "")) {
    return("Unclear")
  }
  grades_clean <- gsub(" ", "", grade_string)
  grades_split <- unlist(strsplit(grades_clean, ","))
  grades_numeric <- suppressWarnings(as.numeric(grades_split))
  valid_grades <- grades_numeric[!is.na(grades_numeric) & grades_numeric >= 1 & grades_numeric <= 12]
  if (length(valid_grades) == 0) return("Unclear")
  paste(sort(valid_grades), collapse = ",")
}

# School type processing - Individual types plus Unclear
process_school_types <- function(school_type_string) {
  if (is.na(school_type_string) || tolower(trimws(school_type_string)) %in% c("cannot tell", "unclear", "")) {
    return("Unclear")
  }
  school_types_clean <- gsub("[\r\n]+", ",", school_type_string)
  school_types_split <- unlist(strsplit(school_types_clean, "[,;]+"))
  school_types_split <- trimws(school_types_split)
  valid_types <- c("Public", "Private", "Charter")
  found_types <- unique(school_types_split[school_types_split %in% valid_types])
  if (length(found_types) == 0) return("Unclear")
  paste(sort(found_types), collapse = ",")
}

# Outcome measure choices - Root-based grouping
process_outcome_measures_roots <- function(all_outcome_measures) {
  clean_measures <- all_outcome_measures[!is.na(all_outcome_measures) & trimws(all_outcome_measures) != ""]
  if (length(clean_measures) == 0) return(rep("Other/Unclear", length(all_outcome_measures)))
  
  normalize_instrument_name <- function(measure) {
    lower_measure <- tolower(measure)
    instrument_patterns <- list(
      "Center for Epidemiologic Studies Depression Scale" = c("center for epidemiologic studies depression scale","ces-d","cesd"),
      "Children's Depression Inventory" = c("children's depression inventory","childrens depression inventory","child depression inventory","cdi"),
      "Revised Children's Anxiety and Depression Scale" = c("revised children's anxiety and depression scale","revised child anxiety and depression scale","rcads"),
      "Beck Depression Inventory" = c("beck depression inventory","bdi"),
      "Patient Health Questionnaire" = c("patient health questionnaire","phq"),
      "Depression Anxiety Stress Scale" = c("depression anxiety stress scale","dass"),
      "Reynolds Adolescent Depression Scale" = c("reynolds adolescent depression scale","rads"),
      "Reynolds Child Depression Scale" = c("reynolds child depression scale","rcds"),
      "Revised Children's Manifest Anxiety Scale" = c("revised children's manifest anxiety scale","revised child manifest anxiety scale","rcmas"),
      "Spence Children's Anxiety Scale" = c("spence children's anxiety scale","spence child anxiety scale","scas"),
      "Mood and Feelings Questionnaire" = c("mood and feelings questionnaire","mfq"),
      "Generalised Anxiety Disorder Scale" = c("generalised anxiety disorder","generalized anxiety disorder","gad"),
      "Multidimensional Anxiety Scale" = c("multidimensional anxiety scale","masc"),
      "State-Trait Anxiety Inventory" = c("state-trait anxiety inventory","speilberger state-trait anxiety inventory","spielberger state-trait anxiety inventory","stai"),
      "Major Depression Inventory" = c("major depression inventory","mdi"),
      "Warwick-Edinburgh Mental Wellbeing Scale" = c("warwick-edinburgh mental wellbeing scale","wemwbs"),
      "Kessler Psychological Distress Scale" = c("kessler psychological distress scale","six-item short form of the kessler psychological distress scale","k6","k10")
    )
    for (instrument_name in names(instrument_patterns)) {
      for (pattern in instrument_patterns[[instrument_name]]) {
        if (grepl(pattern, lower_measure, fixed = TRUE)) return(instrument_name)
      }
    }
    clean_measure <- measure
    clean_measure <- gsub("\\s*>\\s*\\d+.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*(Short\\s+Form|Parent\\s+Report|Youth\\s+Self-Report|Child\\s+Report|Teacher\\s+Report|Self-Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*:\\s*(Self-Report|Teacher\\s+Report|Parent\\s+Report).*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*(General\\s+Anxiety\\s+Subscale|Panic\\s+Subscale|Separation\\s+Anxiety\\s+Subscale|Social\\s+Phobia\\s+Subscale|Depression\\s+Subscale|Anxiety\\s+Subscale).*$", "", clean_measure)
    clean_measure <- gsub("\\s*Short\\s+Version.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-\\s*Revised.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-2.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-Youth.*$", "", clean_measure)
    clean_measure <- gsub("\\s*-II.*$", "", clean_measure)
    clean_measure <- gsub("\\s*for\\s+Children.*$", "", clean_measure)
    clean_measure <- gsub("-9$", "", clean_measure)
    clean_measure <- gsub("\\s*\\(.*\\).*$", "", clean_measure)
    clean_measure <- gsub("\\s+", " ", trimws(clean_measure))
    clean_measure
  }
  
  normalized <- sapply(clean_measures, normalize_instrument_name, USE.NAMES = FALSE)
  result <- character(length(all_outcome_measures))
  clean_idx <- 1
  for (i in seq_along(all_outcome_measures)) {
    if (is.na(all_outcome_measures[i]) || trimws(all_outcome_measures[i]) == "") {
      result[i] <- "Other/Unclear"
    } else {
      result[i] <- normalized[clean_idx]
      clean_idx <- clean_idx + 1
    }
  }
  result
}

# Intervention processing
process_intervention_roots <- function(all_interventions) {
  normalize_text <- function(x) {
    x <- gsub("[\u2012\u2013\u2014\u2015\u2212]", "-", x)
    x <- gsub("[–—−]", "-", x)
    trimws(gsub("\\s+", " ", x))
  }
  clean_interventions <- all_interventions[!is.na(all_interventions) & trimws(all_interventions) != ""]
  if (length(clean_interventions) == 0) return(rep("Other/Unclear", length(all_interventions)))
  
  extract_root <- function(intervention) {
    root <- normalize_text(intervention)
    root <- gsub("\\([^)]*\\)", "", root)
    normalize_text(root)
  }
  custom_group_root <- function(root) {
    if (grepl("^\\.b", root, ignore.case = TRUE)) return(".b Interventions")
    if (grepl("^Adolescents? Coping with Depression", root, ignore.case = TRUE) |
        grepl("^Adolescent Coping with Depression", root, ignore.case = TRUE)) return("Adolescent Coping with Depression Interventions")
    if (grepl("^Resourceful Adolescent Program", root, ignore.case = TRUE)) return("Resourceful Adolescent Program Interventions")
    if (grepl("^Penn Prevention Program", root, ignore.case = TRUE) |
        grepl("^Penn Resiliency Program", root, ignore.case = TRUE) |
        grepl("^Reversed Penn Group", root, ignore.case = TRUE) |
        grepl("^Normal Penn group", root, ignore.case = TRUE)) return("Penn Program Interventions")
    if (grepl("^Social Support Intervention", root, ignore.case = TRUE)) return("Social Support Intervention Interventions")
    if (grepl("^Aussie Optimism Program", root, ignore.case = TRUE)) return("Aussie Optimism Program Interventions")
    if (grepl("^Cognitive-Behavior", root, ignore.case = TRUE)) return("Cognitive-Behavioral Program Interventions")
    if (grepl("^EMOTION", root, ignore.case = TRUE) |
        grepl("Coping Kids Managing Anxiety and Depression", root, ignore.case = TRUE)) return("EMOTION / Coping Kids Interventions")
    if (grepl("^MoodGYM", root, ignore.case = TRUE)) return("MoodGYM Interventions")
    if (grepl("^Universal Intervention", root, ignore.case = TRUE) |
        grepl("^Universal plus Indicated Intervention", root, ignore.case = TRUE)) return("Universal Intervention Interventions")
    if (grepl("^Interpersonal Psychotherapy", root, ignore.case = TRUE)) return("Interpersonal Psychotherapy Interventions")
    if (grepl("^Smart, Positive, Active, Realistic, X-factor thoughts", root, ignore.case = TRUE)) return("Smart, Positive, Active, Realistic, X-factor thoughts Interventions")
    root
  }
  roots <- sapply(clean_interventions, extract_root, USE.NAMES = FALSE)
  grouped_roots <- sapply(roots, custom_group_root, USE.NAMES = FALSE)
  result <- character(length(all_interventions))
  clean_idx <- 1
  for (i in seq_along(all_interventions)) {
    if (is.na(all_interventions[i]) || trimws(all_interventions[i]) == "") {
      result[i] <- "Other/Unclear"
    } else {
      result[i] <- grouped_roots[clean_idx]
      clean_idx <- clean_idx + 1
    }
  }
  result
}

# Apply ALL data processing to merged_all_domains
merged_all_domains <- merged_all_domains %>%
  mutate(urbanicity_clean = sapply(urbanicity, clean_urbanicity)) %>%
  mutate(processed_grades = sapply(grade_level, process_grade_levels)) %>%
  mutate(processed_school_types = sapply(school_type, process_school_types))

# Apply outcome measure processing
processed_outcomes_roots <- process_outcome_measures_roots(merged_all_domains$outcome_measure)
merged_all_domains <- merged_all_domains %>%
  mutate(processed_outcome_measure_roots = processed_outcomes_roots)

# Apply intervention processing
merged_all_domains$processed_intervention_roots <- process_intervention_roots(merged_all_domains$intervention)

# Use ALL data by default (as in your original)
merged <- merged_all_domains

# Export single CSV for the app to load

readr::write_csv(merged, here("data","app_data.csv"))
message("Wrote to: ", here("data","app_data.csv"))
# Central sample-filter entry point for every analysis that uses Group.
# Edit `sample_filter_config` below when the selection rule changes.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("00.sample_filter.R requires the readxl package.", call. = FALSE)
}

sample_filter_config <- list(
  version = "v1",
  group_file = "01.rawdata/Group/Group_info_final.xlsx",
  genus_file = "01.rawdata/Tax_kraken/Final.Bacteria.G.xls",
  exclude_whole_experimental_unit = TRUE,
  ralstonia = list(
    enabled = TRUE,
    genus = "Ralstonia",
    healthy_stem_gt = 0.20,
    diseased_stem_lt = 0.20
  ),
  # Keep sample-specific exceptions here and document the reason explicitly.
  manual_exclusion_ids = c("CS53"),
  manual_exclusion_reason = "extreme healthy-stem Bray-Curtis beta-diversity outlier"
)

required_group_columns <- c(
  "ID", "LinuxID", "TreatID", "GroupID", "Treatment", "City",
  "SampleOrder", "State", "Source", "Rep_Group"
)
Group_raw <- as.data.frame(readxl::read_xlsx(sample_filter_config$group_file))
missing_group_columns <- setdiff(required_group_columns, names(Group_raw))
if (length(missing_group_columns) > 0L) {
  stop("Group metadata is missing required columns: ",
       paste(missing_group_columns, collapse = ", "), call. = FALSE)
}
Group_raw <- Group_raw[, required_group_columns, drop = FALSE]
if (anyNA(Group_raw$ID) || anyDuplicated(Group_raw$ID)) {
  stop("Group metadata contains missing or duplicated sample IDs.", call. = FALSE)
}
if (anyNA(Group_raw$LinuxID) || anyDuplicated(Group_raw$LinuxID)) {
  stop("Group metadata contains missing or duplicated Linux IDs.", call. = FALSE)
}

Group_raw$Rep1 <- paste(substr(Group_raw$GroupID, 1, 2),
                         substr(Group_raw$GroupID, 5, nchar(Group_raw$GroupID)), sep = "")
Group_raw$Rep2 <- paste(substr(Group_raw$Treatment, 1, 2),
                         substr(Group_raw$Treatment, 5, nchar(Group_raw$Treatment)), sep = "")
Group_raw$Rep3 <- sub("-D", "", sub("-H", "", Group_raw$TreatID))

direct_exclusions <- data.frame(
  sample_id = character(), exclusion_reason = character(), stringsAsFactors = FALSE
)

if (isTRUE(sample_filter_config$ralstonia$enabled)) {
  genus_table <- read.delim(sample_filter_config$genus_file,
                            check.names = FALSE, stringsAsFactors = FALSE)
  if (!"ID" %in% names(genus_table) ||
      !sample_filter_config$ralstonia$genus %in% genus_table$ID) {
    stop("The genus table does not contain the configured Ralstonia row.", call. = FALSE)
  }
  missing_genus_columns <- setdiff(Group_raw$LinuxID, names(genus_table))
  if (length(missing_genus_columns) > 0L) {
    stop("The genus table is missing Group Linux IDs: ",
         paste(missing_genus_columns, collapse = ", "), call. = FALSE)
  }
  genus_values <- as.matrix(genus_table[, Group_raw$LinuxID, drop = FALSE])
  storage.mode(genus_values) <- "double"
  genus_totals <- colSums(genus_values)
  if (any(genus_totals <= 0)) {
    stop("The genus table contains samples with non-positive total abundance.", call. = FALSE)
  }
  ralstonia_values <- as.numeric(genus_table[
    genus_table$ID == sample_filter_config$ralstonia$genus,
    Group_raw$LinuxID,
    drop = TRUE
  ])
  Group_raw$Ralstonia_relative_abundance <- ralstonia_values / genus_totals

  healthy_stem_ids <- Group_raw$ID[
    Group_raw$State == "Healthy" & Group_raw$Source == "Stem" &
      Group_raw$Ralstonia_relative_abundance > sample_filter_config$ralstonia$healthy_stem_gt
  ]
  diseased_stem_ids <- Group_raw$ID[
    Group_raw$State == "Diseased" & Group_raw$Source == "Stem" &
      Group_raw$Ralstonia_relative_abundance < sample_filter_config$ralstonia$diseased_stem_lt
  ]
  direct_exclusions <- rbind(
    direct_exclusions,
    data.frame(
      sample_id = healthy_stem_ids,
      exclusion_reason = sprintf("healthy stem: %s relative abundance > %.2f",
                                 sample_filter_config$ralstonia$genus,
                                 sample_filter_config$ralstonia$healthy_stem_gt),
      stringsAsFactors = FALSE
    ),
    data.frame(
      sample_id = diseased_stem_ids,
      exclusion_reason = sprintf("diseased stem: %s relative abundance < %.2f",
                                 sample_filter_config$ralstonia$genus,
                                 sample_filter_config$ralstonia$diseased_stem_lt),
      stringsAsFactors = FALSE
    )
  )
} else {
  Group_raw$Ralstonia_relative_abundance <- NA_real_
}

if (length(sample_filter_config$manual_exclusion_ids) > 0L) {
  direct_exclusions <- rbind(
    direct_exclusions,
    data.frame(
      sample_id = sample_filter_config$manual_exclusion_ids,
      exclusion_reason = sample_filter_config$manual_exclusion_reason,
      stringsAsFactors = FALSE
    )
  )
}
unknown_direct_ids <- setdiff(direct_exclusions$sample_id, Group_raw$ID)
if (length(unknown_direct_ids) > 0L) {
  stop("Configured exclusion IDs are absent from Group metadata: ",
       paste(unknown_direct_ids, collapse = ", "), call. = FALSE)
}
direct_exclusions <- stats::aggregate(
  exclusion_reason ~ sample_id, data = direct_exclusions,
  FUN = function(x) paste(unique(x), collapse = "; ")
)
affected_units <- unique(Group_raw$Rep3[match(direct_exclusions$sample_id, Group_raw$ID)])

sample_filter_audit <- Group_raw
sample_filter_audit$filter_version <- sample_filter_config$version
sample_filter_audit$direct_exclusion_reason <- direct_exclusions$exclusion_reason[
  match(sample_filter_audit$ID, direct_exclusions$sample_id)
]
sample_filter_audit$excluded <- if (isTRUE(sample_filter_config$exclude_whole_experimental_unit)) {
  sample_filter_audit$Rep3 %in% affected_units
} else {
  sample_filter_audit$ID %in% direct_exclusions$sample_id
}
sample_filter_audit$exclusion_reason <- sample_filter_audit$direct_exclusion_reason
sample_filter_audit$exclusion_reason[
  sample_filter_audit$excluded & is.na(sample_filter_audit$exclusion_reason)
] <- "same Rep3 experimental unit as a directly excluded sample"
sample_filter_audit$exclusion_reason[!sample_filter_audit$excluded] <- "retained"

Group <- sample_filter_audit[!sample_filter_audit$excluded, names(Group_raw), drop = FALSE]
Group_h <- Group[Group$State != "Diseased", , drop = FALSE]
Group_d <- Group[Group$State != "Healthy", , drop = FALSE]
Group_rh <- Group[Group$Source == "Rhizosphere", , drop = FALSE]
Group_ck <- Group[Group$Source == "CK", , drop = FALSE]
Group_nck <- Group[Group$State != "None", , drop = FALSE]
Group_st <- Group[Group$Source == "Stem", , drop = FALSE]
Group_hrh <- Group_h[Group_h$Source == "Rhizosphere", , drop = FALSE]
Group_hst <- Group_h[Group_h$Source == "Stem", , drop = FALSE]
Group_drh <- Group_d[Group_d$Source == "Rhizosphere", , drop = FALSE]
Group_dst <- Group_d[Group_d$Source == "Stem", , drop = FALSE]
order <- Group$ID
order_rh <- Group_rh$ID
order_ck <- Group_ck$ID
order_st <- Group_st$ID

audit_file <- "02.processed/00.sample_filter/sample_filter_audit.tsv"
dir.create(dirname(audit_file), recursive = TRUE, showWarnings = FALSE)
utils::write.table(sample_filter_audit, file = audit_file, sep = "\t",
                   quote = FALSE, row.names = FALSE)
message("Sample filter ", sample_filter_config$version, ": ", nrow(Group_raw),
        " starting; ", nrow(direct_exclusions), " directly flagged; ",
        sum(sample_filter_audit$excluded), " excluded; ", nrow(Group),
        " retained. Audit: ", audit_file)

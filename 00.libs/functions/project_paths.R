# Project-local output guards for this analysis directory.
#
# These helpers intentionally stop before replacing an existing artifact.  If a
# result needs to be regenerated, archive or remove the old version explicitly
# and rerun the relevant script.

save_pdf <- function(filename, plot = ggplot2::last_plot(), width, height, ...) {
  if (!is.character(filename) || length(filename) != 1L || !grepl("\\.pdf$", filename, ignore.case = TRUE)) {
    stop("save_pdf() requires one PDF filename.", call. = FALSE)
  }
  if (file.exists(filename)) {
    stop(sprintf("Refusing to overwrite existing figure: %s", filename), call. = FALSE)
  }

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    bg = "white",
    ...
  )
}

save_rdata <- function(..., file, compress = TRUE) {
  if (!is.character(file) || length(file) != 1L || !grepl("\\.rdata$", file, ignore.case = TRUE)) {
    stop("save_rdata() requires one .RData or .Rdata filename.", call. = FALSE)
  }
  if (file.exists(file)) {
    stop(sprintf("Refusing to overwrite existing R data: %s", file), call. = FALSE)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  base::save(..., file = file, compress = compress)
}

save_tsv <- function(x, file, ...) {
  if (file.exists(file)) {
    stop(sprintf("Refusing to overwrite existing table: %s", file), call. = FALSE)
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(x, file = file, sep = "\t", quote = FALSE, row.names = FALSE, ...)
}

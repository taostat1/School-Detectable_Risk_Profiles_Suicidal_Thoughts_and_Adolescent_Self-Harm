# Reproduce the article's family-specific adjustments from aggregate unrounded p values.
# Rscript --vanilla code/revision/apply_BH.R [input.csv] [output.csv]
bh_results <- function(d) {
  stopifnot(all(is.finite(d$p_raw)), all(d$p_raw >= 0 & d$p_raw <= 1))
  d$model_term <- NULL
  d$p_BH <- NA_real_
  d$family_size <- NA_integer_
  key <- interaction(d$table, d$solution, d$family, drop = TRUE)
  for (ix in split(seq_len(nrow(d)), key)) {
    distinct <- ix[is.na(d$same_as[ix])]
    stopifnot(length(distinct) > 0L, !anyDuplicated(d$row[distinct]))
    d$p_BH[distinct] <- p.adjust(d$p_raw[distinct], method = "BH")
    d$family_size[ix] <- length(distinct)
    repeats <- setdiff(ix, distinct)
    if (length(repeats)) {
      original <- distinct[match(d$same_as[repeats], d$row[distinct])]
      stopifnot(!anyNA(original), all(abs(d$p_raw[repeats] - d$p_raw[original]) < 1e-12))
      d$p_BH[repeats] <- d$p_BH[original]
    }
  }
  stopifnot(!anyNA(d$p_BH), all(d$p_BH + 1e-13 >= d$p_raw))
  d$significant <- d$p_BH < 0.05
  d
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  folder <- dirname(normalizePath(script))
  input <- if (length(args) >= 1L) args[1L] else file.path(folder, "BH_inputs.csv")
  output <- if (length(args) >= 2L) args[2L] else file.path(dirname(dirname(folder)), "output", "BH_results.csv")
  data <- read.csv(input, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  options(digits = 17)
  write.csv(bh_results(data), output, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

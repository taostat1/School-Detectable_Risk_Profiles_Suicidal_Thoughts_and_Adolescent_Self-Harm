# Read the restricted study archive and reconstruct questionnaire clusters in memory.
read_study <- function(project, integration_script) {
  archive_file <- function(candidates) {
    found <- candidates[file.exists(candidates)]
    if (!length(found)) stop("Missing restricted input: ", basename(candidates[1L]))
    normalizePath(found[1L], winslash = "/")
  }
  read_archive <- function(path) {
    e <- new.env(parent = emptyenv())
    load(path, envir = e)
    stopifnot(is.data.frame(e$df_common))
    e$df_common
  }
  domains4 <- read_archive(archive_file(file.path(project,
    c("github_upload/output/LPA_analysis_data.RData", "output/LPA_analysis_data.RData"))))
  three_dirs <- file.path(project, c("敏感性分析/output", "output"))
  class3_files <- unique(unlist(lapply(three_dirs, list.files,
    pattern = "_3class\\.RData$", full.names = TRUE)))
  if (length(class3_files) != 1L) stop("Expected one restricted three-profile LPA archive.")
  domains3 <- read_archive(class3_files)
  keep <- setdiff(names(domains4), "injury_class")
  stopifnot(nrow(domains4) == 508L, identical(domains4[keep], domains3[keep]))

  raw_names <- c("psychological_assessment_dimension_scores.csv" = "心理测评数据-维度结果.csv",
    "psychological_assessment_raw_responses.csv" = "心理测评数据-原始答题结果.csv",
    "physical_fitness_data.xlsx" = "体测数据_2.xlsx", "emotion_sonar_data.csv" = "情绪声呐数据.csv",
    "baoshan_data_anonymized.xls" = "宝山数据_匿名化.xls")
  paths <- vapply(names(raw_names), function(n)
    archive_file(file.path(project, "data", c(n, raw_names[[n]]))), character(1))
  # Use the published STEP1 transformations. No participant-level files are written.
  step1 <- readLines(integration_script, encoding = "UTF-8", warn = FALSE)
  for (n in names(paths)) {
    step1 <- gsub(paste0('"data/', n, '"'), encodeString(paths[[n]], quote = '"'),
                  step1, fixed = TRUE)
  }
  old <- new.env(parent = globalenv())
  for (expr in parse(text = step1, encoding = "UTF-8")) {
    if (is.call(expr) && as.character(expr[[1L]])[1L] %in% c("rm", "write.csv", "save")) next
    invisible(capture.output(eval(expr, old)))
  }
  items <- old$col_names_puce[-c(11, 29)]
  answer_string <- function(df) do.call(paste, c(lapply(df[items], as.character), sep = ":"))
  clean_key <- function(v) trimws(as.character(v))
  eligible <- old$df_all[which(old$df_all$grade %in% 15 & old$df_all$age %in% 14:17), ]
  response <- answer_string(eligible)
  stopifnot(identical(response, answer_string(domains4)))

  # Read literal keys as text. Where an answer pattern is shared, validate the pair of keys.
  primary <- read.csv(paths[["psychological_assessment_raw_responses.csv"]],
                      colClasses = "character", check.names = FALSE)
  names(primary) <- trimws(names(primary))
  keep <- trimws(primary$SCALE_NAME) == "学生生活、学习、健康状况调查表(普测版)" &
          trimws(primary$IS_LIE) == "false"
  primary <- primary[which(keep), , drop = FALSE]
  stopifnot(nrow(primary) == nrow(old$df1_1), all(primary$SELECTED_DATA == old$df1_1$SELECTED_DATA))
  ids <- data.frame(person = clean_key(primary$USER_ID), assessment = clean_key(primary$GLOABL_ID),
                    response = answer_string(old$df1_1))
  additional <- readxl::read_xls(paths[["baoshan_data_anonymized.xls"]],
                                sheet = "华师大学生测评结果数据", col_types = "text")
  keep <- trimws(additional$IS_LIE) != "true" &
          trimws(additional$SCALE_NAME) == "学生生活、学习、健康状况调查表(普测版)"
  additional <- additional[which(keep), ]
  if (nrow(additional)) {
    parse_answers <- function(s) {
      z <- jsonlite::fromJSON(gsub("'", '"', gsub("\\\\", "", s)))
      ix <- match(match(items, old$col_names_puce), as.integer(z$TopicNumber))
      paste(z$SelectData[ix], collapse = ":")
    }
    ids <- rbind(ids, data.frame(person = clean_key(additional$USER_ID),
      assessment = clean_key(additional$GLOABL_ID),
      response = vapply(additional$SELECTED_DATA, parse_answers, character(1), USE.NAMES = FALSE)))
  }
  matched <- unique(ids[ids$response %in% response, ])
  by_answer <- split(matched$person, matched$response)
  lookup <- vapply(by_answer, function(v) if (length(unique(v)) == 1L) v[1L] else NA_character_,
                   character(1))
  person <- unname(lookup[response])
  missing <- which(is.na(person))
  if (length(missing)) {
    pair <- function(a, b) paste(a, b, sep = "\u001f")
    uid <- clean_key(eligible$USER_ID[missing])
    assessment <- clean_key(eligible$GLOBAL_ID[missing])
    valid <- pair(uid, assessment) %in% pair(matched$person, matched$assessment)
    person[missing[valid]] <- uid[valid]
  }
  stopifnot(!anyNA(person), all(nzchar(person)))
  list(four = domains4, three = domains3, cluster = match(person, unique(person)),
       questions = old$col_names_puce)
}

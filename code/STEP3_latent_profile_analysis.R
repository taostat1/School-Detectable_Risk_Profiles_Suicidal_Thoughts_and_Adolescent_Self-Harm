rm(list = ls()) # clear everything
library(mclust)
library(tidyLPA)
library(dplyr)
library(openxlsx)
load("output/cleaned_data.RData")

# ============================================================
# BLRT函数（Bootstrap Likelihood Ratio Test）
# ============================================================
bootstrap_LRT <- function(data, G0, G1, modelName = "EEI", nboot = 100) {
  null_mod <- Mclust(data, G = G0, modelNames = modelName)
  alt_mod  <- Mclust(data, G = G1, modelNames = modelName)
  LRT_obs <- -2 * (null_mod$loglik - alt_mod$loglik)
  
  LRT_boot <- numeric(nboot)
  for (i in 1:nboot) {
    boot_data <- sim(NULL, modelName = modelName,
                     parameters = null_mod$parameters, n = nrow(data))
    null_boot <- Mclust(boot_data, G = G0, modelNames = modelName, verbose = FALSE)
    alt_boot  <- Mclust(boot_data, G = G1, modelNames = modelName, verbose = FALSE)
    if (is.null(alt_boot$loglik) || is.null(null_boot$loglik) ||
        alt_boot$G < G1 || null_boot$G < G0) {
      LRT_boot[i] <- 0
    } else {
      lrt_val <- -2 * (null_boot$loglik - alt_boot$loglik)
      if (is.na(lrt_val) || lrt_val < 0) {
        LRT_boot[i] <- 0
      } else {
        LRT_boot[i] <- lrt_val
      }
    }
  }
  p_value <- mean(LRT_boot >= LRT_obs)
  return(p_value)
}

# 计算BLRT p值（G=2vs1, 3vs2, 4vs3, 5vs4, 6vs5）
compute_BLRT_all <- function(data, maxG = 5, modelName = "EEI", nboot = 100) {
  blrt_p <- rep(NA, maxG)  # G=1时无BLRT
  for (k in 2:maxG) {
    cat("  BLRT: G=", k-1, " vs G=", k, "... ")
    blrt_p[k] <- bootstrap_LRT(data, G0 = k-1, G1 = k,
                                modelName = modelName, nboot = nboot)
    cat("p = ", format(blrt_p[k], digits = 4), "\n")
  }
  return(blrt_p)
}

# 构建fit table（英文名，所有数字保留两位小数）
build_fit_table <- function(cluster_obj, data, maxG = 5, modelName = "EEI",
                            nboot = 100, label = "") {
  fit_raw <- get_fit(cluster_obj)
  
  # BLRT
  cat("Computing BLRT for", label, "...\n")
  blrt_p <- compute_BLRT_all(data, maxG = maxG, modelName = modelName, nboot = nboot)
  
  # 类别比例（保留两位小数，用 ": " 分隔）
  prop_list <- lapply(1:maxG, function(k) {
    model_name <- paste0("model_1_class_", k)
    if (!model_name %in% names(cluster_obj)) return(NA)
    classifications <- cluster_obj[[model_name]]$model$classification
    class_counts <- table(classifications)
    class_props <- class_counts / sum(class_counts)
    paste(sprintf("%.2f", class_props), collapse = ": ")
  })
  
  fit_table <- data.frame(
    Classes             = fit_raw$Classes,
    AIC                 = round(fit_raw$AIC, 2),
    BIC                 = round(fit_raw$BIC, 2),
    aBIC                = round(fit_raw$SABIC, 2),
    Entropy             = round(fit_raw$Entropy, 2),
    BLRT_p              = round(blrt_p[fit_raw$Classes], 2),
    Potential_Class_Proportion = unlist(prop_list),
    stringsAsFactors    = FALSE
  )
  
  # 英文列名
  colnames(fit_table) <- c(
    "Classes", "AIC", "BIC", "aBIC", "Entropy",
    "BLRT(p)", "Potential Class Proportion"
  )
  
  return(fit_table)
}

# 向xlsx写入单个带caption的sheet
write_lpa_sheet <- function(wb, sheet_name, df, caption) {
  addWorksheet(wb, sheet_name)
  
  # 标题样式
  title_style <- createStyle(
    textDecoration = "bold",
    fontSize       = 12,
    halign         = "left"
  )
  # 表头样式
  header_style <- createStyle(
    textDecoration = "bold",
    border         = "Bottom"
  )
  # 数值两位小数
  num_style <- createStyle(numFmt = "0.00")
  
  n_col <- ncol(df)
  
  # 1) 写入caption并合并整行
  writeData(wb, sheet_name, caption, startRow = 1, startCol = 1)
  mergeCells(wb, sheet_name, cols = 1:n_col, rows = 1)
  addStyle(wb, sheet_name, title_style, rows = 1, cols = 1:n_col, gridExpand = TRUE)
  
  # 2) 直接写入数据框（带列名），表头自然落在第2行，数据从第3行开始
  writeData(wb, sheet_name, df, startRow = 2, startCol = 1, colNames = TRUE)
  addStyle(wb, sheet_name, header_style, rows = 2, cols = 1:n_col, gridExpand = TRUE)
  
  # 3) 数值列强制两位小数（Classes除外；Potential Class Proportion是字符串）
  num_cols <- which(names(df) %in% c("AIC", "BIC", "aBIC", "Entropy", "BLRT(p)"))
  if (length(num_cols) > 0) {
    addStyle(wb, sheet_name, num_style,
             rows = 3:(2 + nrow(df)), cols = num_cols,
             gridExpand = TRUE, stack = TRUE)
  }
  
  # 4) 设置列宽，避免 BLRT(p) 显示为 ###
  col_widths <- c(10, 11, 11, 11, 11, 11, 30)
  setColWidths(wb, sheet_name, cols = 1:n_col, widths = col_widths)
}

# ============================================================
# 1. School Weariness (厌学倾向)
# ============================================================
school_select <- select(df_common, '我不喜欢上学', '我觉得学习很累很烦', '我希望可以不用学习')
school_select <- school_select %>% mutate(across(everything(), as.numeric))
school_cluster <- estimate_profiles(school_select, 1:5)

school_fit_table <- build_fit_table(
  school_cluster, as.matrix(school_select),
  maxG = 5, modelName = "EEI", nboot = 100, label = "School Weariness"
)

cla_schoolresult <- school_cluster$model_1_class_4$dff
df_common$Class <- cla_schoolresult$Class

class_school_means <- cla_schoolresult %>%
  group_by(Class) %>%
  summarise(
    mean_dislike = mean(`我不喜欢上学`, na.rm = TRUE),
    mean_tired   = mean(`我觉得学习很累很烦`, na.rm = TRUE),
    mean_skip    = mean(`我希望可以不用学习`, na.rm = TRUE),
    mean_composite = mean(c(`我不喜欢上学`, `我觉得学习很累很烦`, `我希望可以不用学习`), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_composite) %>%
  mutate(school_class = row_number())

df_common <- df_common %>%
  left_join(class_school_means %>% select(Class, school_class), by = "Class") %>%
  select(-Class)

cat("\nSchool Weariness - 4-class proportions:\n")
print(table(school_cluster$model_1_class_4$model$classification) / nrow(school_select))

# ============================================================
# 2. Internet Dependency (网络依赖)
# ============================================================
internet_select <- select(df_common,
  "我只要有一段时间没有上网、看手机，就会莫名地情绪低落",
  "长时间网游，使我的身体健康状况越来越不如以前了",
  "由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间")
internet_select <- internet_select %>% mutate(across(everything(), as.numeric))
internet_cluster <- estimate_profiles(internet_select, 1:5)

internet_fit_table <- build_fit_table(
  internet_cluster, as.matrix(internet_select),
  maxG = 5, modelName = "EEI", nboot = 100, label = "Internet Dependency"
)

cla_internetresult <- internet_cluster$model_1_class_4$dff
df_common$Class <- cla_internetresult$Class

class_internet_means <- internet_select %>%
  mutate(Class = cla_internetresult$Class) %>%
  group_by(Class) %>%
  summarise(
    mean_mood   = mean(`我只要有一段时间没有上网、看手机，就会莫名地情绪低落`, na.rm = TRUE),
    mean_health = mean(`长时间网游，使我的身体健康状况越来越不如以前了`, na.rm = TRUE),
    mean_social = mean(`由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间`, na.rm = TRUE),
    mean_composite = mean(c(`我只要有一段时间没有上网、看手机，就会莫名地情绪低落`,
                      `长时间网游，使我的身体健康状况越来越不如以前了`,
                      `由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间`), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_composite) %>%
  mutate(internet_class = row_number())

df_common <- df_common %>%
  left_join(class_internet_means %>% select(Class, internet_class), by = "Class") %>%
  select(-Class)

cat("\nInternet Dependency - 4-class proportions:\n")
print(table(internet_cluster$model_1_class_4$model$classification) / nrow(internet_select))

# ============================================================
# 3. Bullying Victimization (被欺凌倾向)
# ============================================================
injury_select <- select(df_common,
  "别人给我起难听的外号，骂我，或取笑、讽刺我",
  "别人强迫向我要钱，或者拿走、损坏我的东西",
  "某些同学采用打、踢、推、撞等方式欺负我")
injury_select <- injury_select %>% mutate(across(everything(), as.numeric))
injury_cluster <- estimate_profiles(injury_select, 1:5)

injury_fit_table <- build_fit_table(
  injury_cluster, as.matrix(injury_select),
  maxG = 5, modelName = "EEI", nboot = 100, label = "Bullying Victimization"
)

cla_injuryresult <- injury_cluster$model_1_class_4$dff
df_common$Class <- cla_injuryresult$Class

class_injury_means <- injury_select %>%
  mutate(Class = cla_injuryresult$Class) %>%
  group_by(Class) %>%
  summarise(
    mean_verbal    = mean(`别人给我起难听的外号，骂我，或取笑、讽刺我`, na.rm = TRUE),
    mean_property  = mean(`别人强迫向我要钱，或者拿走、损坏我的东西`, na.rm = TRUE),
    mean_physical  = mean(`某些同学采用打、踢、推、撞等方式欺负我`, na.rm = TRUE),
    mean_composite = mean(c(`别人给我起难听的外号，骂我，或取笑、讽刺我`,
                    `别人强迫向我要钱，或者拿走、损坏我的东西`,
                    `某些同学采用打、踢、推、撞等方式欺负我`), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_composite) %>%
  mutate(injury_class = row_number())

df_common <- df_common %>%
  left_join(class_injury_means %>% select(Class, injury_class), by = "Class") %>%
  select(-Class)

cat("\nBullying Victimization - 4-class proportions:\n")
print(table(injury_cluster$model_1_class_4$model$classification) / nrow(injury_select))

# ============================================================
# 4. 合并输出为xlsx（3个sheet，每个sheet带caption）
# ============================================================
wb <- createWorkbook()

write_lpa_sheet(
  wb, "Table S1", school_fit_table,
  "Table S1. Classification Indicator Table for Academic Disengagement"
)
write_lpa_sheet(
  wb, "Table S2", internet_fit_table,
  "Table S2. Classification Indicator Table for Internet Dependency"
)
write_lpa_sheet(
  wb, "Table S3", injury_fit_table,
  "Table S3. Classification Indicator Table for Bullying Victimization"
)

out_file <- "output/LPA_fit_indices.xlsx"
tryCatch({
  saveWorkbook(wb, out_file, overwrite = TRUE)
  cat("\nLPA fit indices saved to", out_file, "\n")
}, error = function(e) {
  temp_file <- "output/LPA_fit_indices_temp.xlsx"
  saveWorkbook(wb, temp_file, overwrite = TRUE)
  cat("\nWarning: could not overwrite", out_file, "(file may be open in Excel).\n")
  cat("Saved to", temp_file, "instead. Close Excel and rename/replace manually.\n")
})
cat("  Table S1: Academic Disengagement\n")
cat("  Table S2: Internet Dependency\n")
cat("  Table S3: Bullying Victimization\n")

# ============================================================
# 5. 保存fit table表格
# ============================================================
write.csv(school_fit_table, "output/school_LPA_fit_table.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(internet_fit_table, "output/internet_LPA_fit_table.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(injury_fit_table, "output/bullying_LPA_fit_table.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ============================================================
# 6. 保存数据供后续分析使用
# ============================================================
write.csv(df_common, "output/figure_data.csv", row.names = FALSE, fileEncoding = "UTF-8")
save(list = c("df_common"), file = "output/LPA_analysis_data.RData")

cat("\n========== STEP3 Complete ==========\n")
cat("df_common rows:", nrow(df_common), "\n")
cat("school_class distribution:\n")
print(table(df_common$school_class))
cat("internet_class distribution:\n")
print(table(df_common$internet_class))
cat("injury_class distribution:\n")
print(table(df_common$injury_class))

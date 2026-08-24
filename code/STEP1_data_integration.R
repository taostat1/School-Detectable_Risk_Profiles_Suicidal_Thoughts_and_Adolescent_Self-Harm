rm(list = ls())
library(tidyverse)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)

# ============================================================
# PART 1: 原始心理测评数据处理
# ============================================================

# --- 1.1 维度结果 ---
df <- read.csv("data/psychological_assessment_dimension_scores.csv")
df_unique <- df %>% distinct()
names(df_unique) <- gsub("`", "", names(df_unique))
df_unique <- df_unique[, c("X.GLOABL_ID.", "X.DIM_NAME.", "X.DIM_SCORE.")]

df_unique <- df %>%
  group_by(X.GLOABL_ID., X.DIM_NAME.) %>%
  summarise(X.DIM_SCORE. = mean(X.DIM_SCORE.), .groups = "drop")

df_new <- df_unique %>%
  pivot_wider(names_from = X.DIM_NAME., values_from = X.DIM_SCORE.)
colnames(df_new)[1] <- "GLOBAL_ID"

# 18个维度列名（固定）
dim_cols <- c("人格因素","人际敏感","人际讨好","创伤事件",
              "厌学倾向","坚持性","家庭关系","心理健康","情绪状况","效度量表",
              "效能感","睡眠状况","网络依赖","自我伤害","自我伤害倾向","诱因",
              "不被关注","敌意")

# --- 1.2 原始答题结果 ---
df1 <- read.csv("data/psychological_assessment_raw_responses.csv")
df1 <- df1[df1$IS_LIE == " false",]
colnames(df1)[1] <- "GLOBAL_ID"

df1_puce <- df1[df1$SCALE_NAME == " 学生生活、学习、健康状况调查表(普测版)",]
df1_chengzhang <- df1[df1$SCALE_NAME == " 学生生活、学习、健康状况调查表(成长版)",]

# 普测版题目列名（36题）
col_names_puce <- c(
  "下列哪一个图案更符合你现在的心情？","我有能力学习任何东西","只要开始做一件事情，我就会完成它",
  "当我有开心的事情发生时，我总希望与家人谈论分享","需要做的事，我大部分都能做","我觉得自己能够应付出现的问题",
  "别人一句不经意的话，常常会让我纠结很久","一旦我计划了要做某事，我就会按计划进行","我很在意别人对我的评价",
  "相对于别人的感受和需要，我觉得自己的感受不重要","你目前的身份是下面哪一种？","我从未有过失眠的体验",
  "我从没有过自杀的想法和准备","别人给我起难听的外号，骂我，或取笑、讽刺我","我在半夜醒来，难以再入睡",
  "我感到不开心","我不喜欢上学","我会做一些不情愿做的事，尤其是为了别人","我跟朋友闹矛盾后，内心会久久难以平复",
  "我觉得我的问题和困难都怨我自己","我喜欢我们家里的家庭活动","再困难我也想活着",
  "我只要有一段时间没有上网、看手机，就会莫名地情绪低落","别人强迫向我要钱，或者拿走、损坏我的东西",
  "某些同学采用打、踢、推、撞等方式欺负我","长时间网游，使我的身体健康状况越来越不如以前了",
  "晚上入睡时，我需要很长时间才能睡着","当别人与自己观点、想法不一致时，即使内心不认同，我也倾向于赞同对方",
  "中国的首都是哪一座城市？","我觉得学习很累很烦","我从没做过故意弄伤自己的行为",
  "我做事提前制定计划，有条理","我希望可以不用学习","我的问题快把我压垮了",
  "由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间","遇到困难时我更倾向于求助家人"
)

# 成长版题目列名（36题）
col_names_chengzhang <- c(
  "下列哪一个图案更符合你现在的心情？","一些不想要的念头和感受一直困扰着我","只要开始做一件事情，我就会完成它",
  "晚上入睡时，我需要很长时间才能睡着","我跟朋友闹矛盾后，内心会久久难以平复","我在半夜醒来，难以再入睡",
  "我被一些麻烦事困住了","我觉得我的问题和困难都怨我自己","我认为有些人的存在没有价值",
  "身边人都不在意我","你目前的身份是下面哪一种","当一件事没做好，我会埋怨合作者",
  "别人给我起难听的外号，骂我，或取笑、讽刺我","无论我做什么都得不到回应","我从没有过自杀的想法和准备",
  "他人的言行常引起我的不满","我很难入睡或睡得不安稳","我从没有过伤害自己的想法",
  "我生病都没人照顾关心我","我睡得很浅，容易被惊醒","我常常觉得别人做得不够好",
  "一旦我计划了要做某事，我就会按计划进行","我从没做过故意弄伤自己的行为","最近我跟老师关系紧张",
  "我很在意别人对我的评价","我的问题快把我压垮了","中国的首都是哪一座城市",
  "别人强迫向我要钱，或者拿走、损坏我的东西","再困难我也想活着","偶尔听到别人谈论我，我会想自己是不是做错了什么",
  "我会一直坚持做作业直到完成为止","别人一句不经意的话，常常会让我纠结很久",
  "某些同学采用打、踢、推、撞等方式欺负我","没人在意我开不开心","我是一个勤奋的人"
)

# 55个答题列名（全部唯一题目 = 16共有 + 20普测版独有 + 19成长版独有）
answer_cols <- union(col_names_puce, col_names_chengzhang)

# 解析普测版JSON
cleaned_puce <- gsub("\\\\", "", df1_puce$SELECTED_DATA)
cleaned_puce <- gsub("'", "\"", cleaned_puce)
json_puce <- lapply(cleaned_puce, fromJSON, simplifyVector = FALSE)
new_df_puce <- as.data.frame(matrix(NA, nrow = nrow(df1_puce), ncol = length(col_names_puce)))
names(new_df_puce) <- col_names_puce
for (i in seq_along(json_puce)) {
  for (j in seq_along(json_puce[[i]])) {
    new_df_puce[i, col_names_puce[json_puce[[i]][[j]]$TopicNumber]] <- json_puce[[i]][[j]]$SelectData
  }
}
df1_1 <- cbind(df1_puce, new_df_puce)

# 解析成长版JSON
cleaned_chengzhang <- gsub("\\\\", "", df1_chengzhang$SELECTED_DATA)
cleaned_chengzhang <- gsub("'", "\"", cleaned_chengzhang)
json_chengzhang <- lapply(cleaned_chengzhang, fromJSON, simplifyVector = FALSE)
new_df_chengzhang <- as.data.frame(matrix(NA, nrow = nrow(df1_chengzhang), ncol = length(col_names_chengzhang)))
names(new_df_chengzhang) <- col_names_chengzhang
for (i in seq_along(json_chengzhang)) {
  for (j in seq_along(json_chengzhang[[i]])) {
    new_df_chengzhang[i, col_names_chengzhang[json_chengzhang[[i]][[j]]$TopicNumber]] <- json_chengzhang[[i]][[j]]$SelectData
  }
}
df1_2 <- cbind(df1_chengzhang, new_df_chengzhang)

# 合并普测版+成长版
df1_ <- bind_rows(df1_1, df1_2, .id = NULL)
# 选取关键列: GLOBAL_ID, USER_ID + 所有55个答题列
df1 <- df1_[, c("GLOBAL_ID", "USER_ID", answer_cols)]

# 维度+答题合并
A_with_USER_ID <- left_join(df_new, df1, by = "GLOBAL_ID")

# ============================================================
# PART 2: 体测+声呐数据合并
# ============================================================

# 原始体测数据（保留原有STEP1逻辑，含ding_userid数值转换）
df2 <- read_excel("data/physical_fitness_data.xlsx", col_types = "text")
df2$age <- as.numeric(df2$age)
df2$sex <- as.numeric(df2$sex)
df2$scorelevel <- as.factor(df2$scorelevel)
df2 <- df2[, c("ding_userid", "age", "sex", "scorelevel", "njname")]
df2$ding_userid <- as.numeric(df2$ding_userid)
df2$ding_userid <- as.character(df2$ding_userid)

# 未经数值转换的原始体测数据（用于宝山匹配，保留18位ID精度）
df2_raw <- read_excel("data/physical_fitness_data.xlsx", col_types = "text")
df2_raw <- df2_raw[, c("ding_userid", "age", "sex", "scorelevel", "njname")]
df2_raw_dedup <- df2_raw %>% distinct(ding_userid, .keep_all = TRUE)
df2_raw_dedup$age <- as.numeric(df2_raw_dedup$age)
df2_raw_dedup$sex <- as.numeric(df2_raw_dedup$sex)

A_with_USER_ID$USER_ID <- gsub(" ", "", A_with_USER_ID$USER_ID)
B_with_USER_ID <- merge(A_with_USER_ID, df2, by.x = "USER_ID", by.y = "ding_userid", all.x = TRUE)

df3 <- read.csv("data/emotion_sonar_data.csv", fileEncoding = "GBK")
df3 <- df3 %>% mutate(sex = case_when(sex == "男" ~ 1, sex == "女" ~ 0, TRUE ~ NA_real_))
df3 <- df3[, c("ding_userid", "grade_name", "sex")]
df3_dedup <- df3 %>% distinct(ding_userid, .keep_all = TRUE)

C_with_USER_ID <- merge(B_with_USER_ID, df3_dedup, by.x = "USER_ID", by.y = "ding_userid", all.x = TRUE)

# ============================================================
# PART 3: 原有数据 grade/sex/age 计算
# ============================================================

df_orig <- C_with_USER_ID
df_orig$grade <- ifelse(is.na(df_orig$njname), df_orig$grade_name, df_orig$njname)

df_orig <- df_orig %>% mutate(grade = case_when(
  grade == "八年级2021级" ~ 13,
  grade == "三年级2021级" ~ 14,
  grade == "一年级2023级" ~ 15,
  grade == "六年级2023级" ~ 11,
  grade == "二年级2022级" ~ 16,
  grade == "2023级" ~ NA_real_,
  grade == "高一" ~ 15,
  grade == "高二" ~ 16,
  grade == "高三" ~ 17,
  grade == "七年级2023级" ~ 12,
  grade == "七年级2022级" ~ 12,
  TRUE ~ as.numeric(grade)
))

df_orig <- df_orig %>% mutate(sex = coalesce(sex.x, sex.y))
df_orig <- df_orig[, colSums(is.na(df_orig)) < nrow(df_orig)]
df_orig <- df_orig %>% filter(!is.na(sex) & !is.na(age))
df_orig <- df_orig %>% mutate(scorelevel = case_when(
  scorelevel == "及格" ~ 2, scorelevel == "良好" ~ 3,
  scorelevel == "优秀" ~ 4, scorelevel == "不及格" ~ 1, TRUE ~ NA_real_
))

cat("=== 原有数据处理完成 ===\n")
cat("行数:", nrow(df_orig), "\n")
cat("grade分布:\n")
print(table(df_orig$grade))

# ============================================================
# PART 4: 宝山华师大测评数据合并（方案A新增）
# ============================================================

cat("\n=== 处理宝山华师大数据 ===\n")
xls_path <- "data/baoshan_data_anonymized.xls"

# --- 4.1 读取宝山华师大测评结果，筛选非说谎 ---
baoshan_result <- read_xls(xls_path, sheet = "华师大学生测评结果数据")
baoshan_result <- baoshan_result[baoshan_result$IS_LIE != "true", ]
cat("宝山非说谎:", nrow(baoshan_result), "人\n")

# --- 4.2 篮选有原始体测匹配的（方案A：仅保留有sex/age的） ---
# 使用未经数值转换的ding_userid（保留18位ID精度），确保最大匹配率
baoshan_matched_users <- as.character(baoshan_result$USER_ID)
baoshan_matched_users <- baoshan_matched_users[baoshan_matched_users %in% df2_raw_dedup$ding_userid]
cat("有体测匹配(方案A):", length(baoshan_matched_users), "人\n")

baoshan_result_matched <- baoshan_result[as.character(baoshan_result$USER_ID) %in% baoshan_matched_users, ]

# --- 4.3 宝山维度数据不可用（GLOABL_ID零交集+每人仅1个异常维度） ---
#      后续分析基于36题答题而非18维度分数，因此跳过维度数据，维度列填NA

# --- 4.4 解析宝山答题数据（全部普测版） ---
cleaned_baoshan <- gsub("\\\\", "", baoshan_result_matched$SELECTED_DATA)
cleaned_baoshan <- gsub("'", "\"", cleaned_baoshan)
json_baoshan <- lapply(cleaned_baoshan, fromJSON, simplifyVector = FALSE)

new_df_baoshan <- as.data.frame(matrix(NA, nrow = nrow(baoshan_result_matched), ncol = length(col_names_puce)))
names(new_df_baoshan) <- col_names_puce
for (i in seq_along(json_baoshan)) {
  for (j in seq_along(json_baoshan[[i]])) {
    new_df_baoshan[i, col_names_puce[json_baoshan[[i]][[j]]$TopicNumber]] <- json_baoshan[[i]][[j]]$SelectData
  }
}

baoshan_answers <- cbind(baoshan_result_matched, new_df_baoshan)
# 宝山数据列名标准化：GLOABL_ID → GLOBAL_ID
if ("GLOABL_ID" %in% names(baoshan_answers)) {
  names(baoshan_answers)[names(baoshan_answers) == "GLOABL_ID"] <- "GLOBAL_ID"
}
# 宝山数据只有普测版36题，需先选取已有列，再补成长版独有列
baoshan_existing_cols <- intersect(c("GLOBAL_ID", "USER_ID", answer_cols), names(baoshan_answers))
baoshan_answers <- baoshan_answers[, baoshan_existing_cols]
for (col in setdiff(answer_cols, names(baoshan_answers))) {
  baoshan_answers[[col]] <- NA_character_
}

# --- 4.5 宝山答题数据构建（无需维度，维度列填NA） ---
baoshan_A <- baoshan_answers
# 为宝山数据添加18个维度列（全填NA，后续分析不用维度分数）
for (dim_col in dim_cols) {
  if (!(dim_col %in% names(baoshan_A))) {
    baoshan_A[[dim_col]] <- NA_real_
  }
}

# --- 4.6 宝山体测数据关联（通过ding_userid=USER_ID，使用原始非截断ID） ---
baoshan_B <- merge(baoshan_A, df2_raw_dedup, by.x = "USER_ID", by.y = "ding_userid", all.x = FALSE)
cat("宝山合并体测后:", nrow(baoshan_B), "行\n")

# --- 4.7 宝山grade映射 ---
baoshan_B$grade <- ifelse(is.na(baoshan_B$njname), NA, baoshan_B$njname)
baoshan_B <- baoshan_B %>% mutate(grade = case_when(
  grade == "一年级2023级" ~ 15,
  grade == "六年级2023级" ~ 11,
  grade == "2023级" ~ NA_real_,
  grade == "八年级2021级" ~ 13,
  grade == "三年级2021级" ~ 14,
  grade == "二年级2022级" ~ 16,
  grade == "高一" ~ 15,
  grade == "高二" ~ 16,
  grade == "高三" ~ 17,
  grade == "七年级2023级" ~ 12,
  grade == "七年级2022级" ~ 12,
  TRUE ~ NA_real_
))

# 宝山没有声呐数据，sex直接来自体测
baoshan_B$sex <- baoshan_B$sex  # 体测sex字段已为数值

# 篮选有sex/age/grade的宝山学生（方案A核心：仅保留有效grade）
baoshan_B <- baoshan_B %>% filter(!is.na(sex) & !is.na(age) & !is.na(grade))

# scorelevel编码
baoshan_B <- baoshan_B %>% mutate(scorelevel = case_when(
  scorelevel == "及格" ~ 2, scorelevel == "良好" ~ 3,
  scorelevel == "优秀" ~ 4, scorelevel == "不及格" ~ 1, TRUE ~ NA_real_
))

cat("宝山筛选后:", nrow(baoshan_B), "行\n")
cat("宝山grade分布:\n")
print(table(baoshan_B$grade))

# ============================================================
# PART 5: 合并原有数据与宝山数据
# ============================================================

# 选取核心列：维度列 + 答题列 + 人口学列
core_cols <- c("GLOBAL_ID", "USER_ID", dim_cols, answer_cols, "age", "sex", "scorelevel", "grade")

df_orig_core <- df_orig[, intersect(core_cols, names(df_orig))]
baoshan_core <- baoshan_B[, intersect(core_cols, names(baoshan_B))]

# 确保两边列完全一致
final_cols <- intersect(names(df_orig_core), names(baoshan_core))
df_orig_final <- df_orig_core[, final_cols]
baoshan_final <- baoshan_core[, final_cols]

# 合并
df_all <- bind_rows(df_orig_final, baoshan_final)

cat("\n=== 合并后数据 ===\n")
cat("总行数:", nrow(df_all), "\n")
cat("grade分布:\n")
print(table(df_all$grade))
cat("其中宝山新增:", nrow(baoshan_final), "行\n")

# ============================================================
# PART 6: 分版输出
# ============================================================

df_common <- df_all[df_all$grade %in% c(15), ]
df_grown <- df_all[df_all$grade %in% c(11, 13, 14, 16), ]

# 普通版列选择（无USER_ID/GLOBAL_ID，原有逻辑）
common_select_cols <- c(
  dim_cols,
  intersect(answer_cols, names(df_common)),
  "age", "sex", "scorelevel"
)
common_select_cols <- intersect(common_select_cols, names(df_common))
df_common <- df_common[, common_select_cols]

# 成长版列选择（保留USER_ID/GLOBAL_ID）
grown_select_cols <- c("USER_ID", "GLOBAL_ID",
  dim_cols,
  intersect(answer_cols, names(df_grown)),
  "age", "sex", "scorelevel"
)
grown_select_cols <- intersect(grown_select_cols, names(df_grown))
df_grown <- df_grown[, grown_select_cols]

# 清理普通版：去除全NA列和零方差列
df_common <- df_common %>% select(where(~ !all(is.na(.x))))
df_common <- as.data.frame(df_common)
df_common <- df_common[, colSums(is.na(df_common)) == 0]
df_common <- df_common[, sapply(df_common, function(x) sd(x, na.rm = TRUE) > 0)]
df_common <- df_common[df_common$age >= 14 & df_common$age <= 17, ] # 清理年龄不符合高一学生要求的

df_grown <- as.data.frame(df_grown)

write.csv(df_common, "output/combined_questionnaire_standard.csv")
write.csv(df_grown, "output/combined_questionnaire_growth.csv")
save(list = c("df_common", "df_grown"), file = "output/cleaned_data.RData")

common_columns <- intersect(colnames(df_common), colnames(df_grown))
df_combined <- merge(df_common[, common_columns], df_grown[, common_columns], by = common_columns, all = TRUE)
write.csv(df_combined, "combined_questionnaire_combined.csv")

# ============================================================
# PART 7: 最终统计报告
# ============================================================

cat("\n========== 最终统计 ==========\n")
cat("普通版总行数:", nrow(df_common), "\n")
cat("普通版14-17岁:", sum(df_common$age >= 14 & df_common$age <= 17), "人\n")
cat("  其中原有:", sum(df_common[df_common$age >= 14 & df_common$age <= 17, "age"] >= 14), "人\n")

# 区分原有和宝山新增
cat("成长版总行数:", nrow(df_grown), "\n")
cat("宝山新增总数:", nrow(baoshan_final), "\n")
cat("  普通版新增:", sum(baoshan_final$grade == 15), "人\n")
cat("  成长版新增:", sum(baoshan_final$grade %in% c(11, 13, 14, 16)), "人\n")

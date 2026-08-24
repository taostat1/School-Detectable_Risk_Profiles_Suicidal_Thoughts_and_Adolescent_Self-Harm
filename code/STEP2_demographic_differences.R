rm(list = ls()) # clear everything

# renv::install(c("dplyr", "flextable", "stats", "janitor"))
library(dplyr)
library(flextable)
library(stats)
library(janitor)

load("output/cleaned_data.RData")

mean_age <- mean(df_common$age, na.rm = TRUE)
cat("年龄均值 =", mean_age, "\n")
sd_age <- sd(df_common$age, na.rm = TRUE)
cat("标准差 =", sd_age, "\n")
# 数据预处理
df_common <- df_common %>%
  # 自杀意念：原变量“我从没有过自杀的想法和准备”，反向编码（>3=有，<=3=无）
  mutate(
    自杀意念 = ifelse(`我从没有过自杀的想法和准备` > 3, "有", "无"),
  # 自伤行为：原变量“我从没做过故意弄伤自己的行为”，反向编码（>3=有，<=3=无）
    自伤行为 = ifelse(`我从没做过故意弄伤自己的行为` > 3, "有", "无"),
  # 身体素质：scorelevel>2 =好，<2=差
    身体素质 = ifelse(scorelevel >= 3, "好", "差"),
  # 性别标签化（1=男，0=女）
    性别 = factor(sex, levels = c(1,0), labels = c("男", "女"))
  ) %>%
  # 保留分析变量
  select(性别, 身体素质, 自杀意念, 自伤行为)
print(names(df_common)) 

df_common <- df_common %>%
  mutate(
    两者都有 = ifelse(自杀意念 == "有" & 自伤行为 == "有", "有", "无")
  )

# 计算人数(%) + 卡方检验
get_stats <- function(group_var, outcome_var) {
  # 生成交叉表
  tab <- df_common %>%
    tabyl({{group_var}}, {{outcome_var}}) %>%
    adorn_totals(where = "row") %>%
    adorn_percentages(denominator = "row") %>%
    adorn_pct_formatting(digits = 2) %>%
    adorn_ns(position = "front")
  
  count_pct_res <- tab[-nrow(tab), "有"]
  
  # 卡方检验
  chisq_test <- chisq.test(table(df_common[[deparse(substitute(group_var))]], 
                                 df_common[[deparse(substitute(outcome_var))]]),correct=FALSE)
  chisq_val <- round(chisq_test$statistic, 3)
  p_val <- chisq_test$p.value
  sig_mark <- ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", ""))
  chisq_res <- paste0("χ²=", chisq_val, sig_mark)
  
  return(list(
    count_pct = count_pct_res,
    chisq = chisq_res,
    p=p_val
  ))
}

# 计算各组统计量
sex_suicide <- get_stats(性别, 自杀意念)
sex_selfharm <- get_stats(性别, 自伤行为)
physical_suicide <- get_stats(身体素质, 自杀意念)
physical_selfharm <- get_stats(身体素质, 自伤行为)
both_sex <- get_stats(性别, 两者都有)
both_physical <- get_stats(身体素质, 两者都有)

# 整理表格数据
table_result <- data.frame(
  变量 = c("性别", "男", "女", "", "身体素质", "好", "差", ""),
  自杀意念 = c("", 
           sex_suicide$count_pct[1], 
           sex_suicide$count_pct[2], 
           paste(sex_suicide$chisq, sex_suicide$p, sep = ", "),
           "", 
           physical_suicide$count_pct[2], 
           physical_suicide$count_pct[1], 
           paste(physical_suicide$chisq, physical_suicide$p, sep = ", ")),
  自伤行为 = c("", 
           sex_selfharm$count_pct[1], 
           sex_selfharm$count_pct[2], 
           paste(sex_selfharm$chisq, sex_selfharm$p, sep = ", "),
           "", 
           physical_selfharm$count_pct[2], 
           physical_selfharm$count_pct[1], 
           paste(physical_selfharm$chisq, physical_selfharm$p, sep = ", ")),
  两者都有 = c("", 
           both_sex$count_pct[1], 
           both_sex$count_pct[2], 
           paste(both_sex$chisq, both_sex$p, sep = ", "),
           "", 
           both_physical$count_pct[2], 
           both_physical$count_pct[1], 
           paste(both_physical$chisq, both_physical$p, sep = ", "))
)

# 写入CSV文件
write.csv(table_result, 
          file = "output/demographic_differences_results.csv", 
          row.names = FALSE,  
          fileEncoding = "UTF-8") 


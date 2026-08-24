rm(list = ls()) 
library(dplyr)
library(broom)
library(ggplot2)
library(lavaan)
set.seed(2026) 

############################################
## 1. 数据导入与预处理
############################################
load("output/LPA_analysis_data.RData")
df_pls <- df_common %>%
  rename(
    school_class = school_class,
    internet_class = internet_class,
    injury_class = injury_class,
    suicide_idea = `我从没有过自杀的想法和准备`,  # 中介变量M
    self_harm = `我从没做过故意弄伤自己的行为`,    # 因变量Y
    help_family = `当我有开心的事情发生时，我总希望与家人谈论分享`,  # 调节变量Z
    sex = sex  # 控制变量：性别
  ) %>%
  # 保留模型变量
  select(school_class, internet_class, injury_class, sex, suicide_idea, self_harm, help_family) %>%
  # 类型转换+缺失值删除
  mutate(
    across(c(suicide_idea, self_harm, help_family, sex), ~as.numeric(as.character(.x))),
    across(c(school_class, internet_class, injury_class), as.numeric)
  ) %>%
  na.omit() %>%
  # 转为因子
  mutate(
    school_class  = factor(school_class),
    internet_class = factor(internet_class),
    injury_class   = factor(injury_class),
    sex  = sex,
    suicide_idea = as.numeric(suicide_idea),
    self_harm    = as.numeric(self_harm)
  )

df_pls <- df_pls %>%
  mutate(
    school_class  = relevel(school_class,  ref = levels(school_class)[3]),
    internet_class = relevel(internet_class, ref = levels(internet_class)[1]),
    injury_class   = relevel(injury_class,   ref = levels(injury_class)[1])
  )

# ========== 调节变量help_family均值中心化（方杰等2023） ==========
df_pls$help_family_cent <- scale(df_pls$help_family, center = TRUE, scale = FALSE)
# 计算调节变量的关键水平：均值(M)、均值+1SD、均值-1SD
Z_m <- mean(df_pls$help_family_cent) # 中心化后均值为0
Z_sd <- sd(df_pls$help_family)
Z_plus1sd <- Z_m + Z_sd
Z_min1sd <- Z_m - Z_sd

cat("调节变量中心化后均值：", Z_m, "\nZ+1SD：", Z_plus1sd, "\nZ-1SD：", Z_min1sd, "\n")

# 生成四分类自变量的虚拟变量
dummies <- model.matrix(
  ~ school_class + internet_class + injury_class,
  data = df_pls
)
dummies <- dummies[, colnames(dummies) != "(Intercept)"] 
dat_sem <- cbind(df_pls, as.data.frame(dummies)) 

# ==========基于中心化调节变量生成交互项 ==========
dat_sem$school2_help   <- dat_sem$school_class2   * dat_sem$help_family_cent
dat_sem$school1_help   <- dat_sem$school_class1   * dat_sem$help_family_cent
dat_sem$school4_help   <- dat_sem$school_class4   * dat_sem$help_family_cent
dat_sem$internet2_help <- dat_sem$internet_class2 * dat_sem$help_family_cent
dat_sem$internet3_help <- dat_sem$internet_class3 * dat_sem$help_family_cent
dat_sem$internet4_help <- dat_sem$internet_class4 * dat_sem$help_family_cent
dat_sem$injury2_help   <- dat_sem$injury_class2   * dat_sem$help_family_cent
dat_sem$injury3_help   <- dat_sem$injury_class3   * dat_sem$help_family_cent
dat_sem$injury4_help   <- dat_sem$injury_class4   * dat_sem$help_family_cent
dat_sem$suicide_help   <- dat_sem$suicide_idea    * dat_sem$help_family_cent

############################################
## 2. 构建两阶段被调节的中介模型
############################################
mod_moderated_med <- ' 
  # 路径1：X→M（suicide_idea）
  suicide_idea ~ a_s2*school_class2 + a_s1*school_class1 + a_s4*school_class4
               + a_i2*internet_class2 + a_i3*internet_class3 + a_i4*internet_class4
               + a_in2*injury_class2 + a_in3*injury_class3 + a_in4*injury_class4
               + a_sh2*school2_help + a_sh1*school1_help + a_sh4*school4_help
               + a_ih2*internet2_help + a_ih3*internet3_help + a_ih4*internet4_help
               + a_inh2*injury2_help + a_inh3*injury3_help  + a_inh4*injury4_help
               + a_z*help_family_cent
               + s_sex*sex

  # 路径2：X→Y + M→Y（self_harm）
  self_harm ~ c_s2*school_class2 + c_s1*school_class1 + c_s4*school_class4
            + c_i2*internet_class2 + c_i3*internet_class3 + c_i4*internet_class4
            + c_in2*injury_class2 + c_in3*injury_class3 + c_in4*injury_class4
            + b*suicide_idea
            + b_z*suicide_help
            + c_sh2*school2_help + c_sh1*school1_help + c_sh4*school4_help
            + c_ih2*internet2_help + c_ih3*internet3_help + c_ih4*internet4_help
            + c_inh2*injury2_help + c_inh3*injury3_help  + c_inh4*injury4_help
            + c_z*help_family_cent
            + y_sex*sex

  # 相对中介效应 Z=均值(0)
  ind_s2_m := (a_s2 + a_sh2*0) * (b + b_z*0)
  ind_s1_m := (a_s1 + a_sh1*0) * (b + b_z*0)
  ind_s4_m := (a_s4 + a_sh4*0) * (b + b_z*0)
  # Z=+1SD
  ind_s2_p1sd := (a_s2 + a_sh2*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_s1_p1sd := (a_s1 + a_sh1*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_s4_p1sd := (a_s4 + a_sh4*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  # Z=-1SD
  ind_s2_m1sd := (a_s2 + a_sh2*@Z_min1sd) * (b + b_z*@Z_min1sd)
  ind_s1_m1sd := (a_s1 + a_sh1*@Z_min1sd) * (b + b_z*@Z_min1sd)
  ind_s4_m1sd := (a_s4 + a_sh4*@Z_min1sd) * (b + b_z*@Z_min1sd)

  # internet_class 相对中介效应
  ind_i2_m := (a_i2 + a_ih2*0) * (b + b_z*0)
  ind_i3_m := (a_i3 + a_ih3*0) * (b + b_z*0)
  ind_i4_m := (a_i4 + a_ih4*0) * (b + b_z*0)
  ind_i2_p1sd := (a_i2 + a_ih2*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_i3_p1sd := (a_i3 + a_ih3*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_i4_p1sd := (a_i4 + a_ih4*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_i2_m1sd := (a_i2 + a_ih2*@Z_min1sd) * (b + b_z*@Z_min1sd)
  ind_i3_m1sd := (a_i3 + a_ih3*@Z_min1sd) * (b + b_z*@Z_min1sd)
  ind_i4_m1sd := (a_i4 + a_ih4*@Z_min1sd) * (b + b_z*@Z_min1sd)

  # injury_class 相对中介效应
  ind_in2_m := (a_in2 + a_inh2*0) * (b + b_z*0)
  ind_in3_m := (a_in3 + a_inh3*0) * (b + b_z*0)
  ind_in4_m := (a_in4 + a_inh4*0) * (b + b_z*0)
  ind_in2_p1sd := (a_in2 + a_inh2*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_in3_p1sd := (a_in3 + a_inh3*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_in4_p1sd := (a_in4 + a_inh4*@Z_plus1sd) * (b + b_z*@Z_plus1sd)
  ind_in2_m1sd := (a_in2 + a_inh2*@Z_min1sd) * (b + b_z*@Z_min1sd)
  ind_in3_m1sd := (a_in3 + a_inh3*@Z_min1sd) * (b + b_z*@Z_min1sd)
  ind_in4_m1sd := (a_in4 + a_inh4*@Z_min1sd) * (b + b_z*@Z_min1sd)

  # 调节效应差值
  delta_s2 := ind_s2_p1sd - ind_s2_m1sd
  delta_s1 := ind_s1_p1sd - ind_s1_m1sd
  delta_s4 := ind_s4_p1sd - ind_s4_m1sd
  delta_i2 := ind_i2_p1sd - ind_i2_m1sd
  delta_i3 := ind_i3_p1sd - ind_i3_m1sd
  delta_i4 := ind_i4_p1sd - ind_i4_m1sd
  delta_in2 := ind_in2_p1sd - ind_in2_m1sd
  delta_in3 := ind_in3_p1sd - ind_in3_m1sd
  delta_in4 := ind_in4_p1sd - ind_in4_m1sd

  # 总间接效应（Z=均值）
  total_ind_school := ind_s2_m + ind_s1_m + ind_s4_m
  total_ind_internet := ind_i2_m + ind_i3_m + ind_i4_m
  total_ind_injury := ind_in2_m + ind_in3_m + ind_in4_m

  # 总间接效应（Z=+1SD）
  total_ind_school_p1sd := ind_s2_p1sd + ind_s1_p1sd + ind_s4_p1sd
  total_ind_internet_p1sd := ind_i2_p1sd + ind_i3_p1sd + ind_i4_p1sd
  total_ind_injury_p1sd := ind_in2_p1sd + ind_in3_p1sd + ind_in4_p1sd

  # 总间接效应（Z=-1SD）
  total_ind_school_m1sd := ind_s2_m1sd + ind_s1_m1sd + ind_s4_m1sd
  total_ind_internet_m1sd := ind_i2_m1sd + ind_i3_m1sd + ind_i4_m1sd
  total_ind_injury_m1sd := ind_in2_m1sd + ind_in3_m1sd + ind_in4_m1sd

  # 总中介效应调节（sum of delta = total indirect moderation）
  delta_school_total := delta_s2 + delta_s1 + delta_s4
  delta_internet_total := delta_i2 + delta_i3 + delta_i4
  delta_injury_total := delta_in2 + delta_in3 + delta_in4

  # 总直接效应调节系数之和（sum of c-prime x Z interactions）
  c_mod_school_total := c_sh2 + c_sh1 + c_sh4
  c_mod_internet_total := c_ih2 + c_ih3 + c_ih4
  c_mod_injury_total := c_inh2 + c_inh3 + c_inh4
'

# 替换模型中的Z水平数值
mod_moderated_med <- gsub("@Z_plus1sd", Z_plus1sd, mod_moderated_med)
mod_moderated_med <- gsub("@Z_min1sd", Z_min1sd, mod_moderated_med)

############################################
## 3. 模型拟合 — 先用ML标准SE拟合base model
############################################
fit_ml <- sem(
  model       = mod_moderated_med,
  data        = dat_sem,
  se          = "standard",   # ML标准SE，用于Wald检验
  fixed.x     = FALSE
)

cat("\n=== ML baseline model summary ===\n")
summary(fit_ml, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

############################################
## 4. 手动Bootstrap + 进度条（10000次，BC置信区间）
############################################
nboot <- 10000
N <- nrow(dat_sem)

# 提取需要跟踪的参数标签
param_labels <- c(
  "a_s2","a_s1","a_s4","a_i2","a_i3","a_i4",
  "a_in2","a_in3","a_in4",
  "a_sh2","a_sh1","a_sh4","a_ih2","a_ih3","a_ih4",
  "a_inh2","a_inh3","a_inh4",
  "a_z","s_sex",
  "b","b_z",
  "c_s2","c_s1","c_s4","c_i2","c_i3","c_i4",
  "c_in2","c_in3","c_in4",
  "c_sh2","c_sh1","c_sh4","c_ih2","c_ih3","c_ih4",
  "c_inh2","c_inh3","c_inh4",
  "c_z","y_sex",
  "ind_s2_m","ind_s1_m","ind_s4_m",
  "ind_s2_p1sd","ind_s1_p1sd","ind_s4_p1sd",
  "ind_s2_m1sd","ind_s1_m1sd","ind_s4_m1sd",
  "ind_i2_m","ind_i3_m","ind_i4_m",
  "ind_i2_p1sd","ind_i3_p1sd","ind_i4_p1sd",
  "ind_i2_m1sd","ind_i3_m1sd","ind_i4_m1sd",
  "ind_in2_m","ind_in3_m","ind_in4_m",
  "ind_in2_p1sd","ind_in3_p1sd","ind_in4_p1sd",
  "ind_in2_m1sd","ind_in3_m1sd","ind_in4_m1sd",
  "delta_s2","delta_s1","delta_s4",
  "delta_i2","delta_i3","delta_i4",
  "delta_in2","delta_in3","delta_in4",
  "total_ind_school","total_ind_internet","total_ind_injury",
  "total_ind_school_p1sd","total_ind_internet_p1sd","total_ind_injury_p1sd",
  "total_ind_school_m1sd","total_ind_internet_m1sd","total_ind_injury_m1sd",
  "delta_school_total","delta_internet_total","delta_injury_total",
  "c_mod_school_total","c_mod_internet_total","c_mod_injury_total"
)

# Bootstrap存储矩阵
boot_mat <- matrix(NA, nrow = nboot, ncol = length(param_labels))
colnames(boot_mat) <- param_labels

cat("\n=== Starting bootstrap (nboot = ", nboot, ") ===\n")

# 进度条
pb <- txtProgressBar(min = 0, max = nboot, style = 3)
converged <- 0

for (i in 1:nboot) {
  # 随机抽样（有放回）
  boot_idx <- sample(1:N, N, replace = TRUE)
  boot_dat <- dat_sem[boot_idx, ]
  
  tryCatch({
    fit_b <- sem(
      model   = mod_moderated_med,
      data    = boot_dat,
      se      = "none",
      fixed.x = FALSE
    )
    # 提取参数估计
    pe <- parameterEstimates(fit_b)
    pe_labeled <- pe[pe$label %in% param_labels, ]
    vals <- setNames(pe_labeled$est, pe_labeled$label)
    # 检查所有标签都有值
    if (length(vals) == length(param_labels) && !any(is.na(vals))) {
      boot_mat[i, ] <- vals[param_labels]
      converged <- converged + 1
    }
  }, error = function(e) NULL)
  
  # 每100次更新进度条
  if (i %% 100 == 0) setTxtProgressBar(pb, i)
  # 每500次输出到控制台（防止idle timeout）
  if (i %% 500 == 0) cat(sprintf("Bootstrap iter %d/%d (converged: %d)\n", i, nboot, converged))
}

setTxtProgressBar(pb, nboot)
close(pb)

cat("\n=== Bootstrap converged: ", converged, "/", nboot, " (", 
    round(converged/nboot*100, 1), "%) ===\n")

############################################
## 5. 计算BC(bias-corrected) 95%置信区间
############################################
# 去除未收敛的行
boot_valid <- boot_mat[!apply(boot_mat, 1, function(r) any(is.na(r))), ]

# ML估计作为observed值
pe_ml <- parameterEstimates(fit_ml)
pe_ml_labeled <- pe_ml[pe_ml$label %in% param_labels, ]
obs_est <- setNames(pe_ml_labeled$est, pe_ml_labeled$label)

# BC CI计算函数
calc_bc_ci <- function(boot_vals, obs_val, alpha = 0.05) {
  n <- length(boot_vals)
  # 偏度修正因子z0
  prop_below <- sum(boot_vals < obs_val) / n
  z0 <- qnorm(prop_below)
  # BC百分位
  z_alpha_lower <- qnorm(alpha / 2)
  z_alpha_upper <- qnorm(1 - alpha / 2)
  p_lower <- pnorm(z0 + (z0 + z_alpha_lower))
  p_upper <- pnorm(z0 + (z0 + z_alpha_upper))
  ci_lower <- quantile(boot_vals, p_lower, type = 6)
  ci_upper <- quantile(boot_vals, p_upper, type = 6)
  return(c(ci_lower = as.numeric(ci_lower), ci_upper = as.numeric(ci_upper)))
}

# 对每个参数计算BC CI
bc_ci_mat <- matrix(NA, nrow = length(param_labels), ncol = 2,
                    dimnames = list(param_labels, c("ci.lower", "ci.upper")))

for (p in param_labels) {
  bvals <- boot_valid[, p]
  bc_ci_mat[p, ] <- calc_bc_ci(bvals, obs_est[p])
}

############################################
## 6. 组合输出结果表
############################################
results_df <- data.frame(
  label    = param_labels,
  est      = obs_est[param_labels],
  se_boot  = apply(boot_valid, 2, sd),
  ci_lower = bc_ci_mat[, "ci.lower"],
  ci_upper = bc_ci_mat[, "ci.upper"]
)

# 添加显著性判断（BC CI不含0）
results_df$significant <- ifelse(
  results_df$ci_lower > 0 | results_df$ci_upper < 0, "*", ""
)

# 按类别分组输出
cat("\n=== Bootstrap BC 95% CI Results ===\n\n")

## 6.1 调节效应（X×Z交互项）
cat("--- X×Z moderation effects (a paths) ---\n")
a_mod_labels <- c("a_sh2","a_sh1","a_sh4",
                   "a_ih2","a_ih3","a_ih4",
                   "a_inh2","a_inh3","a_inh4")
print(results_df[results_df$label %in% a_mod_labels, ])

## 6.2 M×Z调节效应（b_z）
cat("\n--- M×Z moderation (b_z path) ---\n")
print(results_df[results_df$label == "b_z", ])

## 6.3 直接效应×Z交互项（c路径）
cat("\n--- Direct-effect × Z moderation (c paths) ---\n")
c_mod_labels <- c("c_sh2","c_sh1","c_sh4",
                   "c_ih2","c_ih3","c_ih4",
                   "c_inh2","c_inh3","c_inh4")
print(results_df[results_df$label %in% c_mod_labels, ])

## 6.4 相对中介效应（不同Z水平）
cat("\n--- Conditional indirect effects ---\n")
ind_labels <- grep("^ind_", param_labels, value = TRUE)
print(results_df[results_df$label %in% ind_labels, ])

## 6.5 调节效应差值（ΔInd）
cat("\n--- Moderation of indirect effects (ΔInd) ---\n")
delta_labels <- grep("^delta_", param_labels, value = TRUE)
print(results_df[results_df$label %in% delta_labels, ])

## 6.6 总间接效应
cat("\n--- Total indirect effects (at Z=mean, +1SD, -1SD) ---\n")
total_labels <- grep("^total_", param_labels, value = TRUE)
print(results_df[results_df$label %in% total_labels, ])

## 6.7 总中介效应调节（delta_total）
cat("\n--- Total indirect-effect moderation (delta_school/internet/injury_total) ---\n")
delta_total_labels <- c("delta_school_total", "delta_internet_total", "delta_injury_total")
print(results_df[results_df$label %in% delta_total_labels, ])

## 6.8 总直接效应调节系数之和（c_mod_total）
cat("\n--- Total direct-effect moderation coefficient sum (c_mod_school/internet/injury_total) ---\n")
c_mod_total_labels <- c("c_mod_school_total", "c_mod_internet_total", "c_mod_injury_total")
print(results_df[results_df$label %in% c_mod_total_labels, ])

############################################
## 7. 联合Wald检验（基于ML baseline model）
############################################
cat("\n=== Joint Wald tests (ML baseline) ===\n\n")

# school_class X→M路径调节项（第一阶段中介路径调节）
wald_school_a <- lavTestWald(
  fit_ml,
  constraints = 'a_sh1 == 0; a_sh2 == 0; a_sh4 == 0'
)
cat("=== school_class X→M moderation (Wald) ===\n")
print(wald_school_a)

# school_class X→Y路径调节项（直接效应 c' 路径调节）
wald_school_c <- lavTestWald(
  fit_ml,
  constraints = 'c_sh1 == 0; c_sh2 == 0; c_sh4 == 0'
)
cat("\n=== school_class X→Y moderation (Wald) ===\n")
print(wald_school_c)

# internet_class X→M路径调节项（第一阶段中介路径调节）
wald_internet_a <- lavTestWald(
  fit_ml,
  constraints = 'a_ih2 == 0; a_ih3 == 0; a_ih4 == 0'
)
cat("\n=== internet_class X→M moderation (Wald) ===\n")
print(wald_internet_a)

# internet_class X→Y路径调节项（直接效应 c' 路径调节）
wald_internet_c <- lavTestWald(
  fit_ml,
  constraints = 'c_ih2 == 0; c_ih3 == 0; c_ih4 == 0'
)
cat("\n=== internet_class X→Y moderation (Wald) ===\n")
print(wald_internet_c)

# injury_class X→M路径调节项（第一阶段中介路径调节）
wald_injury_a <- lavTestWald(
  fit_ml,
  constraints = 'a_inh2 == 0; a_inh3 == 0; a_inh4 == 0'
)
cat("\n=== injury_class X→M moderation (Wald) ===\n")
print(wald_injury_a)

# injury_class X→Y路径调节项（直接效应 c' 路径调节）
wald_injury_c <- lavTestWald(
  fit_ml,
  constraints = 'c_inh2 == 0; c_inh3 == 0; c_inh4 == 0'
)
cat("\n=== injury_class X→Y moderation (Wald) ===\n")
print(wald_injury_c)

# M×Z调节项（第二阶段中介路径调节）
wald_mz <- lavTestWald(fit_ml, constraints = 'b_z == 0')
cat("\n=== M×Z (suicide_help) Wald test ===\n")
print(wald_mz)

# ============================================================
## 7.2 总效应调节 Wald 检验（无中介 M 的模型）
## 总效应 = c，在不含中介变量的模型中 X→Y 的效应
# ============================================================
mod_total_effect <- '
  self_harm ~ c_s2*school_class2 + c_s1*school_class1 + c_s4*school_class4
            + c_i2*internet_class2 + c_i3*internet_class3 + c_i4*internet_class4
            + c_in2*injury_class2 + c_in3*injury_class3 + c_in4*injury_class4
            + c_sh2*school2_help + c_sh1*school1_help + c_sh4*school4_help
            + c_ih2*internet2_help + c_ih3*internet3_help + c_ih4*internet4_help
            + c_inh2*injury2_help + c_inh3*injury3_help + c_inh4*injury4_help
            + c_z*help_family_cent
            + y_sex*sex
'

fit_total_effect <- sem(
  model   = mod_total_effect,
  data    = dat_sem,
  se      = "standard",
  fixed.x = FALSE
)

cat("\n=== Total-effect model (X→Y without mediator) summary ===\n")
summary(fit_total_effect, standardized = TRUE, fit.measures = TRUE)

# school_class 总效应调节（X→Y without M）
wald_school_total <- lavTestWald(
  fit_total_effect,
  constraints = 'c_sh1 == 0; c_sh2 == 0; c_sh4 == 0'
)
cat("\n=== school_class total-effect moderation (X→Y without M) ===\n")
print(wald_school_total)

# internet_class 总效应调节（X→Y without M）
wald_internet_total <- lavTestWald(
  fit_total_effect,
  constraints = 'c_ih2 == 0; c_ih3 == 0; c_ih4 == 0'
)
cat("\n=== internet_class total-effect moderation (X→Y without M) ===\n")
print(wald_internet_total)

# injury_class 总效应调节（X→Y without M）
wald_injury_total <- lavTestWald(
  fit_total_effect,
  constraints = 'c_inh2 == 0; c_inh3 == 0; c_inh4 == 0'
)
cat("\n=== injury_class total-effect moderation (X→Y without M) ===\n")
print(wald_injury_total)


############################################
## 8. 方杰(2023)方法论推荐：中介效应差异检验（delta BC CI）
## 判断标准：delta的BC 95% CI不含0 → 中介效应被Z调节
############################################
cat("\n=== 方杰(2023) Moderated-mediation difference test (delta BC CIs) ===\n")
cat("准则：若 delta 的 BC 95% CI 不包含 0，则中介效应被家庭关系调节\n\n")

delta_labels <- grep("^delta_", param_labels, value = TRUE)
delta_df <- results_df[results_df$label %in% delta_labels, ]
delta_df$judgment <- ifelse(delta_df$ci_lower > 0 | delta_df$ci_upper < 0,
                            "Moderated (CI不含0)", "Not moderated (CI含0)")
print(delta_df[, c("label", "est", "ci_lower", "ci_upper", "judgment")])

# 分domain汇总
cat("\n--- School (厌学) ---\n")
school_delta <- delta_df[grepl("^delta_s[124]$", delta_df$label), ]
cat(sprintf("  %s: est=%.3f, BC CI [%.3f, %.3f] → %s\n",
            school_delta$label, school_delta$est,
            school_delta$ci_lower, school_delta$ci_upper, school_delta$judgment))

cat("\n--- Internet (网络依赖) ---\n")
inet_delta <- delta_df[grepl("^delta_i[234]$", delta_df$label), ]
cat(sprintf("  %s: est=%.3f, BC CI [%.3f, %.3f] → %s\n",
            inet_delta$label, inet_delta$est,
            inet_delta$ci_lower, inet_delta$ci_upper, inet_delta$judgment))

cat("\n--- Injury (欺凌受害) ---\n")
inj_delta <- delta_df[grepl("^delta_in[234]$", delta_df$label), ]
cat(sprintf("  %s: est=%.3f, BC CI [%.3f, %.3f] → %s\n",
            inj_delta$label, inj_delta$est,
            inj_delta$ci_lower, inj_delta$ci_upper, inj_delta$judgment))

# 方杰(2023)也要求检验直接效应是否被调节：c'×Z交互项BC CI
cat("\n=== 方杰(2023) Moderated direct-effect test (c'×Z BC CIs) ===\n")
cat("准则：若 c'×Z 交互项的 BC 95% CI 不包含 0，则直接效应被Z调节\n\n")

c_mod_labels <- c("c_sh2","c_sh1","c_sh4",
                   "c_ih2","c_ih3","c_ih4",
                   "c_inh2","c_inh3","c_inh4")
c_mod_df <- results_df[results_df$label %in% c_mod_labels, ]
c_mod_df$judgment <- ifelse(c_mod_df$ci_lower > 0 | c_mod_df$ci_upper < 0,
                            "Moderated (CI不含0)", "Not moderated (CI含0)")
print(c_mod_df[, c("label", "est", "ci_lower", "ci_upper", "judgment")])

# ============================================================
## 8.2 检验汇总：Wald全局检验 vs delta BC CI具体性检验（互补）
# ============================================================
cat("\n=== 检验策略汇总：Wald全局检验 vs 方杰delta BC CI具体性检验 ===\n\n")

# 构建对比表
comparison_df <- data.frame(
  domain = c(rep("School(厌学)", 3), rep("Internet(网络)", 3), rep("Injury(欺凌)", 3)),
  test_type = c(
    "Wald全局(a路径)", "Wald全局(c'路径)", "Wald全局(总效应)",
    "Wald全局(a路径)", "Wald全局(c'路径)", "Wald全局(总效应)",
    "Wald全局(a路径)", "Wald全局(c'路径)", "Wald全局(总效应)"
  ),
  wald_chi2 = c(
    wald_school_a$stat, wald_school_c$stat, wald_school_total$stat,
    wald_internet_a$stat, wald_internet_c$stat, wald_internet_total$stat,
    wald_injury_a$stat, wald_injury_c$stat, wald_injury_total$stat
  ),
  wald_p = c(
    wald_school_a$p.value, wald_school_c$p.value, wald_school_total$p.value,
    wald_internet_a$p.value, wald_internet_c$p.value, wald_internet_total$p.value,
    wald_injury_a$p.value, wald_injury_c$p.value, wald_injury_total$p.value
  )
)

# 添加delta BC CI判断（a路径被调节的细节）
school_mod_count <- sum(school_delta$judgment == "Moderated (CI不含0)")
inet_mod_count <- sum(inet_delta$judgment == "Moderated (CI不含0)")
inj_mod_count <- sum(inj_delta$judgment == "Moderated (CI不含0)")

cat("Wald全局检验（所有交互项同时=0）:\n")
for (r in 1:nrow(comparison_df)) {
  sig <- ifelse(comparison_df$wald_p[r] < .001, "***",
                ifelse(comparison_df$wald_p[r] < .01, "**",
                       ifelse(comparison_df$wald_p[r] < .05, "*", "ns")))
  cat(sprintf("  %s %s: χ²(3)=%.2f, p=%.3f %s\n",
              comparison_df$domain[r], comparison_df$test_type[r],
              comparison_df$wald_chi2[r], comparison_df$wald_p[r], sig))
}

cat("\n方杰delta BC CI具体性检验（每个相对中介效应是否被调节）:\n")
cat(sprintf("  School: %d/3个delta CI不含0 → %s\n", school_mod_count,
            ifelse(school_mod_count >= 1, "中介效应被调节", "中介效应不被调节")))
cat(sprintf("  Internet: %d/3个delta CI不含0 → %s\n", inet_mod_count,
            ifelse(inet_mod_count >= 1, "中介效应被调节", "中介效应不被调节")))
cat(sprintf("  Injury: %d/3个delta CI不含0 → %s\n", inj_mod_count,
            ifelse(inj_mod_count >= 1, "中介效应被调节", "中介效应不被调节")))

# b_z检验（第二阶段调节）
b_z_df <- results_df[results_df$label == "b_z", ]
cat(sprintf("\n  b_z (M×Z): est=%.3f, BC CI [%.3f, %.3f] → %s\n",
            b_z_df$est, b_z_df$ci_lower, b_z_df$ci_upper,
            ifelse(b_z_df$ci_lower > 0 | b_z_df$ci_upper < 0,
                   "Stage-2被调节", "Stage-2不被调节")))

# 总中介效应调节（delta_domain_total）
cat("\n=== 总中介效应调节 BC CI检验 ===\n")
for (d in c("delta_school_total", "delta_internet_total", "delta_injury_total")) {
  r <- results_df[results_df$label == d, ]
  if (nrow(r) > 0) {
    judg <- ifelse(r$ci_lower > 0 | r$ci_upper < 0, "总中介效应被调节(CI不含0)", "总中介效应不被调节(CI含0)")
    cat(sprintf("  %s: est=%.3f, BC CI [%.3f, %.3f] → %s\n", d, r$est, r$ci_lower, r$ci_upper, judg))
  }
}

# 总直接效应调节系数之和（c_mod_domain_total）
cat("\n=== 总直接效应调节系数之和 BC CI检验 ===\n")
for (d in c("c_mod_school_total", "c_mod_internet_total", "c_mod_injury_total")) {
  r <- results_df[results_df$label == d, ]
  if (nrow(r) > 0) {
    judg <- ifelse(r$ci_lower > 0 | r$ci_upper < 0, "总直接效应被调节(CI不含0)", "总直接效应不被调节(CI含0)")
    cat(sprintf("  %s: est=%.3f, BC CI [%.3f, %.3f] → %s\n", d, r$est, r$ci_lower, r$ci_upper, judg))
  }
}

# 保存delta BC CI汇总到CSV
delta_summary <- delta_df[, c("label", "est", "ci_lower", "ci_upper", "judgment")]
write.csv(delta_summary, "output/STEP5_delta_BC_CI_summary.csv", row.names = FALSE)
cat("\n=== Delta BC CI summary saved to STEP5_delta_BC_CI_summary.csv ===\n")

############################################
## 9. 保存所有Wald检验结果到统一CSV
############################################
wald_all <- data.frame(
  test = c(
    "School_XtoM_moderation",       # a路径调节（中介路径调节）
    "School_XtoY_moderation",       # c'路径调节（直接路径调节）
    "School_total_moderation",      # 总效应调节（a+c'联合）
    "Internet_XtoM_moderation",     # a路径调节
    "Internet_XtoY_moderation",     # c'路径调节
    "Internet_total_moderation",    # 总效应调节
    "Injury_XtoM_moderation",       # a路径调节
    "Injury_XtoY_moderation",       # c'路径调节
    "Injury_total_moderation",      # 总效应调节
    "MtoY_moderation_bz"            # b路径调节（第二阶段）
  ),
  chi2 = c(
    wald_school_a$stat,       wald_school_c$stat,       wald_school_total$stat,
    wald_internet_a$stat,     wald_internet_c$stat,     wald_internet_total$stat,
    wald_injury_a$stat,       wald_injury_c$stat,       wald_injury_total$stat,
    wald_mz$stat
  ),
  df = c(
    3, 3, 3,
    3, 3, 3,
    3, 3, 3,
    1
  ),
  pvalue = c(
    wald_school_a$p.value,    wald_school_c$p.value,    wald_school_total$p.value,
    wald_internet_a$p.value,  wald_internet_c$p.value,  wald_internet_total$p.value,
    wald_injury_a$p.value,    wald_injury_c$p.value,    wald_injury_total$p.value,
    wald_mz$p.value
  ),
  path_type = c(
    "stage-1 (a path: X→M by Z)", "direct (c\' path: X→Y by Z)", "total (c path: X→Y without M by Z)",
    "stage-1 (a path: X→M by Z)", "direct (c\' path: X→Y by Z)", "total (c path: X→Y without M by Z)",
    "stage-1 (a path: X→M by Z)", "direct (c\' path: X→Y by Z)", "total (c path: X→Y without M by Z)",
    "stage-2 (b path: M→Y by Z)"
  )
)
write.csv(wald_all, "output/STEP5_wald_tests_complete.csv", row.names = FALSE)
cat("\n=== All Wald tests saved to STEP5_wald_tests_complete.csv ===\n")
print(wald_all)

# 保存完整bootstrap结果
saveRDS(results_df, "output/STEP5_moderated_mediation_BC_results.rds")
write.csv(results_df, "output/STEP5_moderated_mediation_BC_results.csv", row.names = FALSE)

# 保存ML baseline fit
saveRDS(fit_ml, "output/STEP5_ml_baseline_fit.rds")

cat("\n=== STEP5 completed. Results saved to output/ ===\n")

rm(list = ls())
library(dplyr)
library(broom)
library(lavaan)

set.seed(2026)

############################################
## 1. 导入及处理数据
############################################
load("output/LPA_analysis_data.RData")

df_pls <- df_common %>%
  rename(
    school_class   = school_class,
    internet_class = internet_class,
    injury_class   = injury_class,
    suicide_idea   = `我从没有过自杀的想法和准备`,
    self_harm      = `我从没做过故意弄伤自己的行为`,
    sex            = sex
  ) %>%
  select(school_class, internet_class, injury_class, sex, suicide_idea, self_harm) %>%
  mutate(
    across(c(suicide_idea, self_harm, sex), ~as.numeric(as.character(.x))),
    across(c(school_class, internet_class, injury_class), as.factor)
  ) %>%
  na.omit()

cat("Analysis sample: N =", nrow(df_pls), "\n\n")

# ── 参照水平 ──
# 厌学：class3（school aversion / low exhaustion）为参照
df_pls <- df_pls %>% mutate(school_class   = relevel(school_class,   ref = levels(school_class)[3]))
# 网络：class1（low impairment）为参照
df_pls <- df_pls %>% mutate(internet_class  = relevel(internet_class, ref = levels(internet_class)[1]))
# 欺凌：class1（low exposure）为参照
df_pls <- df_pls %>% mutate(injury_class    = relevel(injury_class,   ref = levels(injury_class)[1]))

# ── 虚拟变量 ──
dummies <- model.matrix(~ school_class + internet_class + injury_class, data = df_pls)
dummies <- dummies[, colnames(dummies) != "(Intercept)"]
dat_sem <- cbind(df_pls, as.data.frame(dummies))
cat("Dummy columns:", paste(colnames(dat_sem)[7:ncol(dat_sem)], collapse = ", "), "\n\n")

############################################
## 2. 整体效应检验（ANOVA，无需bootstrap）
############################################
cat("===== Overall total-effect tests =====\n")
fit_total_school   <- lm(self_harm ~ school_class + internet_class + injury_class + sex, data = df_pls)
fit_total_internet <- lm(self_harm ~ internet_class + school_class + injury_class + sex, data = df_pls)
fit_total_injury   <- lm(self_harm ~ injury_class + school_class + internet_class + sex, data = df_pls)
cat("\n-- School --\n"); print(anova(fit_total_school)["school_class", ])
cat("\n-- Internet --\n"); print(anova(fit_total_internet)["internet_class", ])
cat("\n-- Injury --\n"); print(anova(fit_total_injury)["injury_class", ])

cat("\n===== Overall direct-effect tests =====\n")
fit_direct_school   <- lm(self_harm ~ school_class + internet_class + injury_class + suicide_idea + sex, data = df_pls)
fit_direct_internet <- lm(self_harm ~ internet_class + school_class + injury_class + suicide_idea + sex, data = df_pls)
fit_direct_injury   <- lm(self_harm ~ injury_class + school_class + internet_class + suicide_idea + sex, data = df_pls)
cat("\n-- School --\n"); print(anova(fit_direct_school)["school_class", ])
cat("\n-- Internet --\n"); print(anova(fit_direct_internet)["internet_class", ])
cat("\n-- Injury --\n"); print(anova(fit_direct_injury)["injury_class", ])

############################################
## 3. 中介模型（lavaan）+ 手动 Bootstrap 10000次 + 进度条
############################################
med_model <- '
  # a路径：suicide_idea 回归
  suicide_idea ~ a_s2*school_class2 + a_s1*school_class1 + a_s4*school_class4
               + a_i2*internet_class2 + a_i3*internet_class3 + a_i4*internet_class4
               + a_in2*injury_class2 + a_in3*injury_class3 + a_in4*injury_class4
               + sex

  # b路径 + 直接效应：self_harm 回归
  self_harm ~ c_s2*school_class2 + c_s1*school_class1 + c_s4*school_class4
            + c_i2*internet_class2 + c_i3*internet_class3 + c_i4*internet_class4
            + c_in2*injury_class2 + c_in3*injury_class3 + c_in4*injury_class4
            + b*suicide_idea
            + sex

  # 相对中介效应
  ind_school2 := a_s2 * b
  ind_school1 := a_s1 * b
  ind_school4 := a_s4 * b

  ind_internet2 := a_i2 * b
  ind_internet3 := a_i3 * b
  ind_internet4 := a_i4 * b

  ind_injury2 := a_in2 * b
  ind_injury3 := a_in3 * b
  ind_injury4 := a_in4 * b

  # 整体中介效应
  ind_school_total   := ind_school2 + ind_school1 + ind_school4
  ind_internet_total := ind_internet2 + ind_internet3 + ind_internet4
  ind_injury_total   := ind_injury2 + ind_injury3 + ind_injury4

  # 整体直接效应（direct path sum）
  direct_school_total   := c_s2 + c_s1 + c_s4
  direct_internet_total := c_i2 + c_i3 + c_i4
  direct_injury_total   := c_in2 + c_in3 + c_in4

  # 整体总效应（direct + indirect）
  total_school_total   := direct_school_total + ind_school_total
  total_internet_total := direct_internet_total + ind_internet_total
  total_injury_total   := direct_injury_total + ind_injury_total

  # 相对总效应 (direct + indirect = c-prime + ab)
  total_school2 := c_s2 + ind_school2
  total_school1 := c_s1 + ind_school1
  total_school4 := c_s4 + ind_school4

  total_internet2 := c_i2 + ind_internet2
  total_internet3 := c_i3 + ind_internet3
  total_internet4 := c_i4 + ind_internet4

  total_injury2 := c_in2 + ind_injury2
  total_injury3 := c_in3 + ind_injury3
  total_injury4 := c_in4 + ind_injury4

  # 中介效应占比 (ab/c)
  prop_school2 := ind_school2 / total_school2
  prop_school1 := ind_school1 / total_school1
  prop_school4 := ind_school4 / total_school4

  prop_internet2 := ind_internet2 / total_internet2
  prop_internet3 := ind_internet3 / total_internet3
  prop_internet4 := ind_internet4 / total_internet4

  prop_injury2 := ind_injury2 / total_injury2
  prop_injury3 := ind_injury3 / total_injury3
  prop_injury4 := ind_injury4 / total_injury4
'

# ── 3a. 先拟合ML模型（获取点估计 + Wald检验）──
cat("\n===== Fitting mediation model (ML, standard SEs for Wald) =====\n")
fit_base <- sem(med_model, data = dat_sem, se = "standard", fixed.x = FALSE)
cat("Base model fitted.\n")

# ── 3b. Wald 联合检验 ──
cat("\n===== Wald joint tests =====\n")
wald_school <- lavTestWald(fit_base, constraints = '
  ind_school2 == 0
  ind_school1 == 0
  ind_school4 == 0
')
cat("\n-- School indirect effects --\n"); print(wald_school)

wald_internet <- lavTestWald(fit_base, constraints = '
  ind_internet2 == 0
  ind_internet3 == 0
  ind_internet4 == 0
')
cat("\n-- Internet indirect effects --\n"); print(wald_internet)

wald_injury <- lavTestWald(fit_base, constraints = '
  ind_injury2 == 0
  ind_injury3 == 0
  ind_injury4 == 0
')
cat("\n-- Injury indirect effects --\n"); print(wald_injury)

# ── 3b2. 直接效应联合Wald检验 ──
cat("\n===== Wald joint tests for direct effects =====\n")

wald_school_direct <- lavTestWald(fit_base, constraints = '
  c_s2 == 0
  c_s1 == 0
  c_s4 == 0
')
cat("\n-- School direct effects --\n"); print(wald_school_direct)

wald_internet_direct <- lavTestWald(fit_base, constraints = '
  c_i2 == 0
  c_i3 == 0
  c_i4 == 0
')
cat("\n-- Internet direct effects --\n"); print(wald_internet_direct)

wald_injury_direct <- lavTestWald(fit_base, constraints = '
  c_in2 == 0
  c_in3 == 0
  c_in4 == 0
')
cat("\n-- Injury direct effects --\n"); print(wald_injury_direct)

# ── 3c. 手动 Bootstrap 10000次 + 进度条 ──
n_boot <- 10000
cat("\n===== Manual Bootstrap (n =", n_boot, ") with progress bar =====\n")

pe_base <- parameterEstimates(fit_base)
# 构建参数唯一标识（用label优先，否则用lhs~op~rhs）
param_uid <- ifelse(pe_base$label != "",
                    pe_base$label,
                    paste(pe_base$lhs, pe_base$op, pe_base$rhs))
n_params <- length(param_uid)

boot_est <- matrix(NA, nrow = n_boot, ncol = n_params)
colnames(boot_est) <- param_uid

pb <- txtProgressBar(min = 0, max = n_boot, style = 3, char = "=", width = 50)

for (i in 1:n_boot) {
  boot_idx <- sample(1:nrow(dat_sem), nrow(dat_sem), replace = TRUE)
  boot_data <- dat_sem[boot_idx, ]

  tryCatch({
    fit_b <- sem(med_model, data = boot_data, se = "none", fixed.x = FALSE,
                 control = list(verbose = FALSE))
    pe_b <- parameterEstimates(fit_b)
    boot_est[i, ] <- pe_b$est
  }, error = function(e) {
    # 非收敛样本留NA
  })

  if (i %% 100 == 0 || i == n_boot) setTxtProgressBar(pb, i)
  if (i %% 500 == 0) cat(sprintf("Bootstrap iter %d/%d\n", i, n_boot))
}
close(pb)

cat("\nBootstrap completed.\n")
converged <- rowSums(is.na(boot_est)) == 0
cat("Convergence rate:", sum(converged), "/", n_boot,
    "(", round(sum(converged)/n_boot*100, 1), "%)\n\n")

boot_est <- boot_est[converged, ]

# ── 3d. Bias-corrected (BC) 95% 置信区间 ──
cat("===== Bias-corrected 95% Bootstrap CIs =====\n")
point_est <- pe_base$est
alpha <- 0.05

bc_cis <- matrix(NA, nrow = n_params, ncol = 2)
colnames(bc_cis) <- c("ci.lower", "ci.upper")

for (j in 1:n_params) {
  b_j <- boot_est[, j]
  b_j <- b_j[!is.na(b_j)]
  if (length(b_j) < 10) continue  # 极少收敛
  z0 <- qnorm(sum(b_j < point_est[j]) / length(b_j))
  z_lo <- qnorm(alpha / 2)
  z_hi <- qnorm(1 - alpha / 2)
  p_lo <- pnorm(2 * z0 + z_lo)
  p_hi <- pnorm(2 * z0 + z_hi)
  bc_cis[j, 1] <- quantile(b_j, probs = p_lo, type = 1)
  bc_cis[j, 2] <- quantile(b_j, probs = p_hi, type = 1)
}

# ── 3e. 输出所有关键参数 ──
# 辅助函数：格式化输出
print_param <- function(lbl) {
  row_idx <- which(pe_base$label == lbl)
  if (length(row_idx) == 0) return()
  e <- point_est[row_idx]
  lo <- bc_cis[row_idx, 1]
  hi <- bc_cis[row_idx, 2]
  # 判断CI是否不含0（异号=含0=NS）
  sig <- ifelse(lo > 0 | hi < 0, "*", "NS")
  cat(sprintf("  %s: est = %.3f, BC 95%% CI [%.3f, %.3f]  %s\n", lbl, e, lo, hi, sig))
}

# 整体中介效应
cat("\n-- Omnibus indirect effects --\n")
for (l in c("ind_school_total", "ind_internet_total", "ind_injury_total")) print_param(l)

# 整体直接效应（c-prime路径之和）
cat("\n-- Omnibus direct effects --\n")
for (l in c("direct_school_total", "direct_internet_total", "direct_injury_total")) print_param(l)

# 整体总效应（direct + indirect）
cat("\n-- Omnibus total effects (direct + indirect) --\n")
for (l in c("total_school_total", "total_internet_total", "total_injury_total")) print_param(l)

# 相对中介效应
cat("\n-- Relative indirect effects --\n")
for (l in c("ind_school2", "ind_school1", "ind_school4",
            "ind_internet2", "ind_internet3", "ind_internet4",
            "ind_injury2", "ind_injury3", "ind_injury4")) print_param(l)

# 相对总效应
cat("\n-- Relative total effects --\n")
for (l in c("total_school2", "total_school1", "total_school4",
            "total_internet2", "total_internet3", "total_internet4",
            "total_injury2", "total_injury3", "total_injury4")) print_param(l)

# a路径
cat("\n-- a-path coefficients --\n")
for (l in c("a_s2", "a_s1", "a_s4", "a_i2", "a_i3", "a_i4",
            "a_in2", "a_in3", "a_in4")) print_param(l)

# b路径
cat("\n-- b-path coefficient --\n")
print_param("b")

# 直接效应 (c'路径)
cat("\n-- Direct effects (c' paths) --\n")
for (l in c("c_s2", "c_s1", "c_s4", "c_i2", "c_i3", "c_i4",
            "c_in2", "c_in3", "c_in4")) print_param(l)

# 中介效应占比
cat("\n-- Proportion mediated (ab/c) --\n")
for (l in c("prop_school2", "prop_school1", "prop_school4",
            "prop_internet2", "prop_internet3", "prop_internet4",
            "prop_injury2", "prop_injury3", "prop_injury4")) print_param(l)

cat("\n===== Done =====\n")

# ── 4. 保存BC CI结果到CSV ──
bc_results_df <- data.frame(
  label = param_uid,
  est = point_est,
  ci_lower = bc_cis[, 1],
  ci_upper = bc_cis[, 2],
  stringsAsFactors = FALSE
)
# 只保留有label的参数（去掉无label的残差等）
bc_results_df <- bc_results_df[bc_results_df$label != "" & !grepl("^var_|^cov_", bc_results_df$label), ]
# 判断显著性
bc_results_df$significant <- ifelse(bc_results_df$ci_lower > 0 | bc_results_df$ci_upper < 0, "*", "")
write.csv(bc_results_df, "output/STEP4_BC_results.csv", row.names = FALSE)
cat("STEP4 BC CI results saved to output/STEP4_BC_results.csv\n")

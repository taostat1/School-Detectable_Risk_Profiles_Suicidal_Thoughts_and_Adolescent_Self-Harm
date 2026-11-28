# Observed profile indicators, standardized outcome means and the six omnibus ANCOVAs.
figure1_ancova <- function(data, questions, age_c, sex, outcomes) {
  indicators <- list(school=questions[c(17,30,33)], internet=questions[c(23,26,35)],
                     injury=questions[c(14,24,25)])
  numeric_value <- function(x) as.numeric(as.character(x))
  profile_rows <- mean_rows <- test_rows <- list()
  for (domain in names(indicators)) {
    group <- factor(data[[paste0(domain,"_class")]], levels=1:4)
    stopifnot(!anyNA(group), all(table(group) > 0L))
    cols <- indicators[[domain]]
    for (j in seq_along(cols)) {
      scores <- numeric_value(data[[cols[j]]])
      stopifnot(all(scores %in% 1:5))
      profile_rows[[length(profile_rows)+1L]] <- data.frame(
        domain, group=1:4, n=as.integer(table(group)), indicator=j,
        est=as.numeric(tapply(scores, group, mean)))
    }
    # Each domain is fitted separately for each outcome. Group predictions
    # are standardized to the same age and sex distribution in all four groups.
    design <- model.matrix(~group+sex+age_c)
    ref <- data.frame(group=factor(1:4, levels=1:4), sex=mean(sex), age_c=0)
    pred <- model.matrix(~group+sex+age_c, ref)
    stopifnot(nrow(design)==508L, identical(colnames(design),colnames(pred)))
    for (outcome in names(outcomes)) {
      stopifnot(all(outcomes[[outcome]] %in% 1:5))
      fit <- cluster_fit(design,outcomes[[outcome]])
      test <- joint(fit, paste0("group",2:4))
      est <- drop(pred %*% fit$b)
      ci <- qt(.975,G-1)*sqrt(diag(pred %*% fit$V %*% t(pred)))
      mean_rows[[length(mean_rows)+1L]] <- data.frame(
        domain,outcome,group=1:4,n=as.integer(table(group)),est,lower=est-ci,upper=est+ci)
      test_rows[[length(test_rows)+1L]] <- data.frame(
        domain,outcome,chi2=unname(test["chi2"]),df=3L,p=unname(test["p"]))
    }
  }
  tests <- do.call(rbind, test_rows)
  tests$p_bh <- p.adjust(tests$p, method = "BH")
  list(n=N, covariates=c("age","sex"), profiles=do.call(rbind,profile_rows),
       figure1=do.call(rbind,mean_rows), tests=tests)
}

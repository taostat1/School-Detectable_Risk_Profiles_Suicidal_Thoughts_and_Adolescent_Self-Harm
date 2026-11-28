# Run from any directory: Rscript --vanilla run_models.R PRIVATE_STUDY_ROOT OUTPUT_DIRECTORY
# The public plotting scripts can also run directly from the supplied aggregate figure inputs.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
}
args <- commandArgs(TRUE)
if (length(args) != 2L) stop("Usage: run_models.R PRIVATE_STUDY_ROOT OUTPUT_DIRECTORY")
project <- normalizePath(args[1L], winslash="/")
dir.create(args[2L], recursive=TRUE, showWarnings=FALSE)
outdir <- normalizePath(args[2L], winslash="/")
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE)[1L])
here <- dirname(normalizePath(script, winslash="/"))
private_lib <- file.path(project, "renv", "library", .Platform$OS.type,
  paste0("R-", R.version$major, ".", strsplit(R.version$minor, ".", fixed=TRUE)[[1L]][1L]), R.version$platform)
if (dir.exists(private_lib)) .libPaths(c(private_lib, .libPaths()))
source(file.path(here, "read_study.R"), encoding="UTF-8")
source(file.path(here, "model_functions.R"), encoding="UTF-8")
source(file.path(here, "Figure1_ANCOVA.R"), encoding="UTF-8")
source(file.path(here, "apply_BH.R"), encoding="UTF-8")
study <- read_study(project, file.path(dirname(here), "STEP1_data_integration.R"))
d <- study$four
t3 <- study$three
cluster <- study$cluster
qn <- study$questions
N <- nrow(d)
G <- max(cluster)
num <- function(x) as.numeric(as.character(x))
age <- num(d$age)
age_c <- age - mean(age)
sex <- num(d$sex)
m <- num(d[[qn[13L]]])
y <- num(d[[qn[31L]]])
items <- as.data.frame(lapply(d[qn[c(4L,36L)]], num))
score <- rowMeans(items)
stopifnot(N == 508L, all(age %in% 14:17), all(sex %in% 0:1),
          all(unlist(items) %in% 1:5), all(m %in% 1:5), all(y %in% 1:5),
          !anyNA(cbind(d[c("school_class","internet_class","injury_class")], t3$injury_class)),
          all(t3$injury_class == c(1L,1L,2L,3L)[as.integer(as.character(d$injury_class))]))
write_summary <- function(x, filename) {
  jsonlite::write_json(x, file.path(outdir, filename), dataframe="rows",
                       auto_unbox=TRUE, digits=16, na="null", pretty=TRUE)
}
fig1 <- figure1_ancova(d, qn, age_c, sex, list(thoughts=m, self_harm=y))
write_summary(fig1, "Figure1_values.json")
bh_inputs <- read.csv(file.path(here, "BH_inputs.csv"), stringsAsFactors=FALSE)
# The catalogue supplies only the row definitions. Every probability below is refitted.
bh_inputs$p_raw <- NA_real_
t1 <- bh_inputs$table == "T1"
screens <- list(thoughts=m >= 4, self_harm=y >= 4, both=(m >= 4 & y >= 4))
sex_p <- vapply(screens, function(screen) chisq.test(table(sex, screen), correct=FALSE)$p.value, numeric(1))
bh_inputs$p_raw[t1] <- sex_p[bh_inputs$model_term[t1]]
t2 <- bh_inputs$table == "T2"
ancova_p <- setNames(fig1$tests$p, paste(fig1$tests$domain, fig1$tests$outcome, sep="."))
bh_inputs$p_raw[t2] <- ancova_p[bh_inputs$model_term[t2]]
B <- 10000L
set.seed(2026L)
blocks <- split(seq_len(N), factor(cluster, levels=seq_len(G)))
indices <- replicate(B, unlist(blocks[sample.int(G,G,replace=TRUE)], use.names=FALSE),
                     simplify=FALSE)

out <- list(n=N,draws=B,seed=2026,score_mean=mean(score),score_sd=sd(score))
for (which in c("three_moderated","three_unmoderated","four_unmoderated","four_moderated")) {
 k <- if(startsWith(which,"three")) 3L else 4L
 moderated <- endsWith(which, "_moderated")
 ds <- with_age(prepare_design(k,score),cbind(age=age_c))
 if(moderated){
   labs <- c("a_intercept",ds$names_a,"c_intercept",ds$names_c)
   labs[labs=="s_sex"] <- "a_sex";labs[labs=="y_sex"] <- "c_sex"
   ds$labels <- labs
 } else {
   ds$x <- ds$x[,c("intercept",paste0("x_",ds$ids),"sex","age"),drop=FALSE]
   ds$z <- cbind(ds$x,m=m)
   ds$labels <- c("a_intercept",paste0("a_",ds$ids),"a_sex","a_age","c_intercept",paste0("c_",ds$ids),"c_sex","c_age","b")
 }
 ob <- coefficients_at(ds,seq_len(N)); stopifnot(!anyNA(ob),length(ob)==length(ds$labels))
 raw <- rbind(ob,t(vapply(indices,coefficients_at,numeric(length(ob)),spec=ds)))
 colnames(raw) <- ds$labels
 raw <- raw[complete.cases(raw),,drop=FALSE]
 extra <- list()
 levels <- if(moderated) c(low=-sd(score),mean=0,high=sd(score)) else c(all=0)
 for(lev in names(levels)){
   w <- levels[[lev]]
   b <- raw[,"b"]+if(moderated) w*raw[,"b_z"] else 0
   if(moderated) extra[[paste0("slope_",lev)]] <- b
   suf <- if(moderated) paste0("_",lev) else ""
   for(id in ds$ids){
      h <- sub("([1-4])$","h\\1",id)
      a <- raw[,paste0("a_",id)]+if(moderated) w*raw[,paste0("a_",h)] else 0
      cc <- raw[,paste0("c_",id)]+if(moderated) w*raw[,paste0("c_",h)] else 0
      if(moderated){
        extra[[paste0("first_",id,suf)]] <- a
        extra[[paste0("direct_",id,suf)]] <- cc
      }
      extra[[paste0("ind_",id,suf)]] <- a*b
      extra[[paste0("total_",id,suf)]] <- cc+a*b
   }
   for(group in unique(ds$groups)){
      ids <- ds$ids[ds$groups==group]
      for(tp in c("ind","total")){
         keys <- paste0(tp,"_",ids,suf)
         extra[[paste0("sum_",tp,"_",group,suf)]] <- Reduce("+",extra[keys])
      }
   }
 }
 if(moderated) for(id in ds$ids) extra[[paste0("delta_",id)]] <- extra[[paste0("ind_",id,"_high")]]-extra[[paste0("ind_",id,"_low")]]
 band <- summary_bands(cbind(raw,do.call(cbind,extra)))
 a <- cluster_fit(ds$x,m);cc <- cluster_fit(ds$z,y)
 se <- c(sqrt(diag(a$V)),sqrt(diag(cc$V))); names(se)<-ds$labels
 p <- 2*pnorm(-abs(ob/se));names(p)<-ds$labels
 band$se_cluster <- unname(se[band$label])
 band$p_wald <- unname(p[band$label])
 S <- c("domain","path","df","chi2","p")
 if(moderated) wtab <- wald_paths(ds)[S] else {
  wtab <- do.call(rbind,lapply(c("a","c_prime"),function(path)do.call(rbind,lapply(unique(ds$groups),function(domain) {
     f <- if(path=="a") a else cc
     stat <- joint(f,paste0("x_",ds$ids[ds$groups==domain]))
     data.frame(domain,path,t(stat),row.names=NULL)[S]
  }))))
  wb <- joint(cc,"m"); wtab <- rbind(wtab,data.frame(domain="all",path="b",t(wb),row.names=NULL)[S])
 }
 wtab$p_bh <- ave(wtab$p, endsWith(wtab$path, "by_family"), FUN=function(p) p.adjust(p, method="BH"))
 ij <- indirect_joint(ds)[c("domain","df","chi2","p")]
 ij$p_bh <- p.adjust(ij$p, method="BH")
 tab <- if (moderated) "S5" else "S4"
 selected <- bh_inputs$table == tab & bh_inputs$solution == k
 probabilities <- c(setNames(band$p_wald, paste0("coef:", band$label)),
                    setNames(band$p_bc, paste0("bootstrap:", band$label)),
                    setNames(wtab$p, paste("path", wtab$domain, wtab$path, sep=":")),
                    setNames(ij$p, paste0("joint:", ij$domain)))
 bh_inputs$p_raw[selected] <- probabilities[bh_inputs$model_term[selected]]
 out[[which]] <- list(bands=band,tests=wtab,indirect_joint=ij,
   R2_M=1-sum(lm.fit(ds$x,m)$residuals^2)/sum((m-mean(m))^2),
   R2_Y=1-sum(lm.fit(ds$z,y)$residuals^2)/sum((y-mean(y))^2))
  message(which, " complete")
}
write_summary(out, "supplement_models.json")
options(digits=17)
write.csv(bh_results(bh_inputs), file.path(outdir, "BH_results.csv"), row.names=FALSE, na="", fileEncoding="UTF-8")

# Only the four-profile model supplies Figure 2. Export the plotted statistics.
primary <- out$four_moderated
plot_terms <- grepl("^([ac]_(s|i|in)h?[1-4]|ind_(s|i|in)[1-4]_(low|mean|high))$",
                    primary$bands$label)
plotted <- primary$bands[plot_terms | primary$bands$label %in% c("b","b_z"), ]
estimates <- setNames(lapply(seq_len(nrow(plotted)), function(i) {
  fields <- if (plotted$label[i] %in% c("b","b_z")) c("est","lo","hi") else "est"
  as.list(plotted[i, fields, drop=FALSE])
}), plotted$label)
write_summary(list(n=N, profiles=4L, family_items=c(4L,36L), covariates=c("age","sex"),
  test="Student-clustered HC1 Wald; p is unadjusted, p_bh is adjusted within seven main paths or seven interactions.", source="supplement_models.json, four_moderated",
  tests=primary$tests, estimates=estimates), "Figure2_data.json")

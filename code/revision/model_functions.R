# Regression, student-cluster HC1 Wald tests, and bias-corrected bootstrap intervals.
# The fitted equations share questionnaire rows and student resampling blocks.

prepare_design <- function(solution, score) {
  ids <- c("s2","s1","s4","i2","i3","i4","in2","in3","in4")
  groups <- c("school","school","school","internet","internet","internet",rep("injury",3))
  lev <- c(2,1,4,2,3,4,2,3,4)
  frame <- d
  if (solution == 3L) {
    frame <- t3
    ids <- ids[-7]
    groups <- groups[-7]
    lev <- c(lev[1:6],2,3)
  }
  dummy <- vapply(seq_along(ids), function(j)
    as.numeric(frame[[paste0(groups[j], "_class")]] == lev[j]), numeric(nrow(frame)))
  colnames(dummy) <- paste0("x_", ids)
  w <- score - mean(score)
  prod <- dummy * w
  colnames(prod) <- paste0("h_", ids)
  x <- cbind(intercept = 1, dummy, prod, w = w, sex = sex)
  z <- cbind(x, m = m, mw = m * w)
  names_a <- c(paste0("a_", ids), paste0("a_", sub("([1-4])$", "h\\1", ids)), "a_z", "s_sex")
  names_c <- c(paste0("c_", ids), paste0("c_", sub("([1-4])$", "h\\1", ids)), "c_z", "y_sex", "b", "b_z")
  list(ids = ids, groups = groups, x = x, z = z, sd = sd(score), names_a = names_a, names_c = names_c)
}

bc <- function(draw, observed, p_at_zero = FALSE) {
  n <- length(draw)
  z0 <- qnorm(max(0.5/n, min(1-0.5/n, mean(draw < observed))))
  bounds <- quantile(draw, pnorm(2*z0 + qnorm(c(.025,.975))), names = FALSE, type = 6)
  p <- if (p_at_zero) 2 * pnorm(-abs(qnorm((sum(draw <= 0) + .5)/(n + 1)) - 2*z0)) else NA_real_
  c(estimate = observed, lo = bounds[1], hi = bounds[2], p = p)
}

with_age <- function(ds,added) {
  ds$x <- cbind(ds$x,added)
  ds$z <- cbind(ds$z,added)
  ds$names_a <- c(ds$names_a,paste0("a_",colnames(added)))
  ds$names_c <- c(ds$names_c,paste0("c_",colnames(added)))
  ds
}

cluster_fit<-function(x,v,w=rep(1,N)) {
  f<-lm.wfit(x,v,w)
  stopifnot(f$rank==ncol(x))
  bread<-solve(crossprod(x,x*w))
  scores<-rowsum(x*(f$residuals*w),cluster,reorder=FALSE)
  V<-bread%*%crossprod(scores)%*%bread*(G/(G-1))*((N-1)/(N-ncol(x)))
  stopifnot(all(diag(V)>0))
  list(b=f$coefficients,V=V)
}

joint<-function(f,ids) {
  v<-f$b[ids]; vv<-f$V[ids,ids,drop=FALSE]
  w<-drop(crossprod(v,solve(vv,v))); k<-length(v)
  c(chi2=w,df=k,p=pchisq(w,k,lower.tail=FALSE),p_F=pf(w/k,k,G-1,lower.tail=FALSE))
}

wald_paths <- function(ds) {
  a<-cluster_fit(ds$x,m);cfit<-cluster_fit(ds$z,y); rows<-list()
  for(inter in c(FALSE,TRUE)) {
    for(whichfit in c("a","c_prime")) for(domain in unique(ds$groups)){
      nm<-paste0(if(inter)"h_" else "x_",ds$ids[ds$groups==domain])
      st<-joint(if(whichfit=="a")a else cfit,nm)
      rows[[length(rows)+1L]]<-data.frame(domain,path=paste0(whichfit,if(inter)"_by_family" else ""),t(st),
        test_family=if(inter)"interaction" else "main")
    }
    st<-joint(cfit,if(inter)"mw" else "m")
    rows[[length(rows)+1L]]<-data.frame(domain="all",path=if(inter)"b_by_family" else "b",t(st),
      test_family=if(inter)"interaction" else "main")
  }
  do.call(rbind,rows)
}

coefficients_at <- function(spec,idx) {
  a <- lm.fit(spec$x[idx,,drop=FALSE],m[idx])
  cfit <- lm.fit(spec$z[idx,,drop=FALSE],y[idx])
  if (a$rank<ncol(spec$x) || cfit$rank<ncol(spec$z)) return(rep(NA_real_,length(spec$labels)))
  unname(c(a$coefficients,cfit$coefficients))
}

summary_bands <- function(v) {
  # Row 1 is the full-sample fit. The remaining rows must be complete for the
  # specification(s) being summarized; no independent-samples approximation.
  v <- v[complete.cases(v),,drop=FALSE]
  stopifnot(nrow(v)>1L)
  bounds <- t(apply(v,2,function(z) bc(unname(z[-1L]),unname(z[1L]),TRUE)))
  data.frame(label=colnames(v),est=bounds[,"estimate"],lo=bounds[,"lo"],hi=bounds[,"hi"],
             p_bc=bounds[,"p"],retained=nrow(v)-1L,row.names=NULL)
}

indirect_joint <- function(ds) {
  a <- cluster_fit(ds$x,m)
  cfit <- cluster_fit(ds$z,y)
  influence <- function(x,v,fit) {
    scores <- rowsum(x*drop(v-x%*%fit$b),cluster,reorder=FALSE)
    scores %*% solve(crossprod(x))*sqrt((G/(G-1))*((N-1)/(N-ncol(x))))
  }
  ia <- influence(ds$x,m,a); ic <- influence(ds$z,y,cfit)
  stopifnot(max(abs(crossprod(ia)-a$V))<1e-9,max(abs(crossprod(ic)-cfit$V))<1e-9)
  per_domain <- lapply(unique(ds$groups),function(domain) {
    names <- paste0("x_", ds$ids[ds$groups==domain])
    products <- a$b[names]*cfit$b["m"]
    ij <- ia[,names,drop=FALSE]*unname(cfit$b["m"]) + tcrossprod(ic[,"m"],a$b[names])
    w <- drop(crossprod(products,solve(crossprod(ij),products)))
    data.frame(domain=domain,df=length(names),chi2=w,p=pchisq(w,length(names),lower.tail=FALSE))
  })
  tab <- do.call(rbind,per_domain)
  tab
}

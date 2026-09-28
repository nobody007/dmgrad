suppressMessages({library(xgboost); library(ParBayesianOptimization); library(jsonlite)})
bank <- read.csv("bank.csv", sep=";", stringsAsFactors=TRUE)
bank$duration <- NULL                                   # 통화시간: 결과를 알고 난 뒤의 정보(누수)
y <- as.integer(bank$y == "yes")
X <- model.matrix(~ . - y, data = bank)[, -1]           # 범주형 → 더미
set.seed(2026)
te <- c(sample(which(y==1), round(.2*sum(y==1))), sample(which(y==0), round(.2*sum(y==0))))
Xtr <- X[-te,]; ytr <- y[-te]; Xte <- X[te,]; yte <- y[te]
dtr <- xgb.DMatrix(Xtr, label = ytr)
set.seed(1)
fid <- integer(length(ytr)); for (k in 0:1) { i <- which(ytr==k); fid[i] <- sample(rep(1:5, length.out=length(i))) }
folds <- lapply(1:5, function(k) which(fid==k))
scoringFunction <- function(max_depth, log_mcw, subsample, colsample_bytree, log_lambda) {
  p <- list(objective="binary:logistic", eval_metric="auc", eta=0.05, nthread=1,
            max_depth=max_depth, min_child_weight=10^log_mcw, subsample=subsample,
            colsample_bytree=colsample_bytree, lambda=10^log_lambda)
  cv <- xgb.cv(params=p, data=dtr, nrounds=2000, folds=folds, early_stopping_rounds=50, verbose=0)
  list(Score = max(cv$evaluation_log$test_auc_mean), nrounds = cv$early_stop$best_iteration)
}
bounds <- list(max_depth=c(2L,8L), log_mcw=c(0,log10(50)), subsample=c(0.5,1), colsample_bytree=c(0.5,1), log_lambda=c(-2,2))
out <- list()
for (s in 1:2) {
  set.seed(s); t0 <- Sys.time()
  opt <- bayesOpt(FUN=scoringFunction, bounds=bounds, initPoints=10, iters.n=30, iters.k=1, acq="ei", verbose=0)
  el <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  out[[paste0("bo",s)]] <- list(scores=opt$scoreSummary$Score, time=el, best=getBestPars(opt), bestScore=max(opt$scoreSummary$Score),
                                nrounds=opt$scoreSummary$nrounds[which.max(opt$scoreSummary$Score)])
  cat("bo",s,max(opt$scoreSummary$Score),el,"\n")
  set.seed(100+s); t0 <- Sys.time()
  rs <- sapply(1:40, function(i) scoringFunction(sample(2:8,1), runif(1,0,log10(50)), runif(1,.5,1), runif(1,.5,1), runif(1,-2,2))$Score)
  out[[paste0("rand",s)]] <- list(scores=rs, time=as.numeric(difftime(Sys.time(), t0, units="secs")))
  cat("rand",s,max(rs),"\n")
}
g <- expand.grid(max_depth=c(2,4,6), log_mcw=log10(c(1,7,50)), subsample=c(.5,.75,1), colsample_bytree=c(.5,.75,1), log_lambda=c(-2,0,2))
t0 <- Sys.time(); gs <- t(sapply(1:nrow(g), function(i) unlist(do.call(scoringFunction, as.list(g[i,])))))
out$grid <- list(scores=gs[,"Score"], time=as.numeric(difftime(Sys.time(), t0, units="secs")), best=g[which.max(gs[,"Score"]),], nrounds=gs[which.max(gs[,"Score"]),"nrounds"])
cat("grid",max(gs[,"Score"]),out$grid$time,"\n")
fit_test <- function(bp, n) { p <- list(objective="binary:logistic", eta=0.05, nthread=1, max_depth=bp$max_depth, min_child_weight=10^bp$log_mcw, subsample=bp$subsample, colsample_bytree=bp$colsample_bytree, lambda=10^bp$log_lambda)
  set.seed(7); m <- xgb.train(params=p, data=dtr, nrounds=n); pr <- predict(m, Xte)
  r <- rank(pr); (sum(r[yte==1]) - sum(yte==1)*(sum(yte==1)+1)/2)/(sum(yte==1)*sum(yte==0)) }
out$test <- list(bo=fit_test(out$bo1$best, out$bo1$nrounds), grid=fit_test(as.list(out$grid$best), out$grid$nrounds))
print(out$test)
write_json(out, "r_log.json", auto_unbox=TRUE, digits=6)

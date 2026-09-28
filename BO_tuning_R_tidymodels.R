suppressMessages({library(parsnip); library(workflows); library(recipes); library(rsample)
  library(tune); library(yardstick); library(dials); library(jsonlite)})
bank <- read.csv("bank.csv", sep = ";", stringsAsFactors = TRUE); bank$duration <- NULL
bank$y <- factor(bank$y, levels = c("yes", "no"))           # 첫 수준 = 사건
set.seed(2026); sp <- initial_split(bank, prop = 0.8, strata = y)
tr <- training(sp); set.seed(1); folds <- vfold_cv(tr, v = 5, strata = y)
spec <- boost_tree(trees = 2000, learn_rate = 0.05, stop_iter = 50,
                   tree_depth = tune(), min_n = tune(), sample_size = tune(),
                   mtry = tune(), loss_reduction = tune()) |>
  set_engine("xgboost", validation = 0.2, nthread = 1, counts = FALSE) |>
  set_mode("classification")
rec <- recipe(y ~ ., data = tr) |> step_dummy(all_nominal_predictors())
wf  <- workflow(rec, spec)
prm <- extract_parameter_set_dials(wf) |>
  update(tree_depth = tree_depth(c(2L, 8L)), min_n = min_n(c(2L, 50L)),
         sample_size = sample_prop(c(0.5, 1)), mtry = mtry_prop(c(0.5, 1)))
t0 <- Sys.time(); set.seed(3)
res <- tune_bayes(wf, resamples = folds, param_info = prm, initial = 10, iter = 30,
                  metrics = metric_set(roc_auc), control = control_bayes(no_improve = 30))
el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
print(show_best(res, metric = "roc_auc", n = 3))
best  <- select_best(res, metric = "roc_auc")
final <- last_fit(finalize_workflow(wf, best), sp, metrics = metric_set(roc_auc))
print(collect_metrics(final))
m <- collect_metrics(res); m <- m[order(m$.iter), ]
write_json(list(time = el, iter = m$.iter, auc = m$mean, best = best, test = collect_metrics(final)$.estimate), "r_tidy.json", auto_unbox = TRUE, digits = 6)

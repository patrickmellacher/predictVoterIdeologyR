#' @title Fast Soft-Vote Predictions for scikit-learn Random Forests
#' @description
#' Reproduces scikit-learn RandomForestClassifier predictions using soft voting:
#' per-tree leaf class distributions are normalized and averaged across trees;
#' final label = argmax of averaged probabilities. Includes vectorized scaling
#' using StandardScaler params and optional parallelization across trees.
#'
#' @keywords models random-forest prediction soft-vote sklearn json

suppressPackageStartupMessages({
  #' @import jsonlite
  NULL
})
#' @import parallel
NULL

# ---------- robustes Laden ----------
#' @keywords internal
load_forest_json <- function(path) {
  if (!file.exists(path)) stop("Datei nicht gefunden: ", path)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

# ---------- Vorverarbeitung eines einzelnen Trees -> flache Strukturen ----------
#' @keywords internal
preprocess_tree <- function(tree, n_classes) {
  left  <- as.integer(unlist(tree$children_left,  use.names = FALSE))
  right <- as.integer(unlist(tree$children_right, use.names = FALSE))
  feat  <- as.integer(unlist(tree$feature,        use.names = FALSE))
  thr   <- as.numeric(unlist(tree$threshold,      use.names = FALSE))
  
  # value: Liste (n_nodes) von Listen (n_classes) -> Matrix n_nodes x n_classes
  values <- t(vapply(
    tree$value,
    function(v) as.numeric(unlist(v, use.names = FALSE)),
    FUN.VALUE = numeric(n_classes)
  ))
  storage.mode(values) <- "double"
  
  list(left = left, right = right, feat = feat, thr = thr, values = values)
}

# ---------- Vorverarbeitung des gesamten Forests ----------
#' @keywords internal
preprocess_forest <- function(forest_raw) {
  feature_names <- unlist(forest_raw$feature_names, use.names = FALSE)
  classes      <- unlist(forest_raw$classes_,      use.names = FALSE)
  n_classes    <- length(classes)
  
  trees_pp <- lapply(forest_raw$trees, preprocess_tree, n_classes = n_classes)
  
  # Scaler in die Reihenfolge von feature_names bringen
  s_feat  <- unlist(forest_raw$scaler_feature_names, use.names = FALSE)
  s_mean  <- as.numeric(unlist(forest_raw$scaler_mean_,  use.names = FALSE))
  s_scale <- as.numeric(unlist(forest_raw$scaler_scale_, use.names = FALSE))
  idx     <- match(feature_names, s_feat)
  if (any(is.na(idx))) {
    stop("Scaler-Infos fehlen für: ", paste(feature_names[is.na(idx)], collapse = ", "))
  }
  
  list(
    feature_names = feature_names,
    classes       = classes,
    n_classes     = n_classes,
    trees         = trees_pp,
    scaler_mean   = s_mean[idx],
    scaler_scale  = s_scale[idx]
  )
}

# ---------- Data scaling (vektorisiert, ohne Schleife) ----------
#' @keywords internal
scale_dataframe <- function(df_input, feature_names, mu, sc) {
  X <- as.matrix(df_input[, feature_names, drop = FALSE])
  storage.mode(X) <- "double"
  # (x - mu) / sc
  X <- sweep(X, 2L, mu, FUN = "-")
  sc[is.na(sc)] <- 1
  sc[sc == 0]   <- 1
  X <- sweep(X, 2L, sc, FUN = "/")
  X
}

# ---------- Traversiere EINEN Baum für ALLE Zeilen -> Probabilitäten (n x k) ----------
#' @keywords internal
predict_one_tree_probs <- function(tree_pp, X_scaled, n_classes) {
  n <- nrow(X_scaled)
  res <- matrix(0, nrow = n, ncol = n_classes)
  
  left <- tree_pp$left; right <- tree_pp$right
  feat <- tree_pp$feat; thr <- tree_pp$thr
  vals <- tree_pp$values
  
  for (i in 1:n) {
    node <- 0L
    repeat {
      f <- feat[node + 1L]
      # Leaf in sklearn: feat == -2
      if (!is.na(f) && f == -2L) {
        counts <- vals[node + 1L, ]
        s <- sum(counts)
        if (s > 0) res[i, ] <- counts / s else res[i, ] <- rep(1 / n_classes, n_classes)
        break
      }
      l <- left[node + 1L]; r <- right[node + 1L]; t <- thr[node + 1L]
      # Fallback: -1/-1 oder fehlendes feat
      if ((l == -1L && r == -1L) || is.na(f)) {
        counts <- vals[node + 1L, ]
        s <- sum(counts)
        if (s > 0) res[i, ] <- counts / s else res[i, ] <- rep(1 / n_classes, n_classes)
        break
      }
      # Featurewert
      x <- X_scaled[i, f + 1L]
      node <- if (!is.na(x) && x <= t) l else r
      if (node < 0L) {  # Sicherheit
        counts <- vals[node + 1L, ]
        s <- sum(counts)
        if (s > 0) res[i, ] <- counts / s else res[i, ] <- rep(1 / n_classes, n_classes)
        break
      }
    }
  }
  
  res
}

# ---------- Soft Vote über ALLE Trees (optional parallel) ----------
#' Fast soft-vote prediction from a preprocessed forest
#'
#' @param forest_pp Preprocessed forest as created by \code{preprocess_forest()}.
#' @param df_input Data frame containing at least the model's \code{feature_names}.
#' @param n_workers Number of parallel workers (default: all cores minus one).
#' @return Numeric vector of predicted class labels (same coding as \code{forest_pp$classes}).
#' @export
predict_forest_soft_fast <- function(forest_pp, df_input, n_workers = max(1L, parallel::detectCores() - 1L)) {
  missing <- setdiff(forest_pp$feature_names, colnames(df_input))
  if (length(missing)) {
    stop("Fehlende Spalten: ", paste(missing, collapse = ", "))
  }
  
  X_scaled <- scale_dataframe(
    df_input,
    forest_pp$feature_names,
    forest_pp$scaler_mean,
    forest_pp$scaler_scale
  )
  
  n <- nrow(X_scaled); k <- forest_pp$n_classes
  prob_sum <- matrix(0, nrow = n, ncol = k)
  
  if (n_workers > 1L && length(forest_pp$trees) > 1L) {
    cl <- parallel::makeCluster(n_workers)
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
    parallel::clusterExport(cl, varlist = c("predict_one_tree_probs"), envir = environment())
    parallel::clusterExport(cl, varlist = c("X_scaled", "forest_pp"), envir = environment())
    parts <- parallel::parLapply(cl, forest_pp$trees, function(tpp) {
      predict_one_tree_probs(tpp, X_scaled, forest_pp$n_classes)
    })
    for (m in parts) prob_sum <- prob_sum + m
  } else {
    for (tpp in forest_pp$trees) {
      prob_sum <- prob_sum + predict_one_tree_probs(tpp, X_scaled, forest_pp$n_classes)
    }
  }
  
  forest_pp$classes[max.col(prob_sum, ties.method = "first")]
}

# ---------- Ein einzelnes JSON-Modell anwenden (Fast-Path) ----------
#' Predict with a forest JSON (fast soft vote)
#'
#' @param df_input Data frame with required feature columns.
#' @param json_path Path to the forest JSON export.
#' @param n_workers Parallel workers (default: all cores minus one).
#' @return Numeric vector of predicted labels.
#' @export
predict_with_json_fast <- function(df_input, json_path, n_workers = max(1L, parallel::detectCores() - 1L)) {
  forest_raw <- load_forest_json(json_path)
  forest_pp  <- preprocess_forest(forest_raw)
  predict_forest_soft_fast(forest_pp, df_input, n_workers = n_workers)
}

# ---------- Drei Modelle laden & drei Spalten anhängen ----------
#' Add three ideology predictions to a data frame (paths provided)
#'
#' @param df Data frame with all features required by each model.
#' @param path_lrgen Path to forest_lrgen_export.json
#' @param path_lrecon Path to forest_lrecon_export.json
#' @param path_galtan Path to forest_galtan_export.json
#' @param n_workers Parallel workers (default: all cores minus one).
#' @return The input data frame with three new columns:
#'         prediction_lrgen, prediction_lrecon, prediction_galtan.
#' @export
add_predictions_three_fast <- function(df,
                                       path_lrgen,
                                       path_lrecon,
                                       path_galtan,
                                       n_workers = max(1L, parallel::detectCores() - 1L)) {
  out <- df
  out$prediction_lrgen  <- predict_with_json_fast(out, path_lrgen,  n_workers = n_workers)
  out$prediction_lrecon <- predict_with_json_fast(out, path_lrecon, n_workers = n_workers)
  out$prediction_galtan <- predict_with_json_fast(out, path_galtan, n_workers = n_workers)
  out
}

# ---------- Komfort-Wrapper: nutzt Modelle aus inst/model ----------------------
#' Predict three ideology labels using packaged models from inst/model
#'
#' Looks for \code{forest_*_export.json} under \code{inst/model/} of this package
#' and appends three prediction columns.
#'
#' @param df Data frame with the required feature columns.
#' @param n_workers Parallel workers (default: all cores minus one).
#' @return The input data frame with:
#'         prediction_lrgen, prediction_lrecon, prediction_galtan.
#' @export
predict_voter_ideology <- function(df, n_workers = max(1L, parallel::detectCores() - 1L)) {
  path_lrgen  <- system.file("model", "forest_lrgen_export.json",  package = "predictVoterIdeologyR")
  path_lrecon <- system.file("model", "forest_lrecon_export.json", package = "predictVoterIdeologyR")
  path_galtan <- system.file("model", "forest_galtan_export.json", package = "predictVoterIdeologyR")
  
  if (path_lrgen  == "" || !file.exists(path_lrgen))   stop("Modell nicht gefunden: forest_lrgen_export.json")
  if (path_lrecon == "" || !file.exists(path_lrecon))  stop("Modell nicht gefunden: forest_lrecon_export.json")
  if (path_galtan == "" || !file.exists(path_galtan))  stop("Modell nicht gefunden: forest_galtan_export.json")
  
  add_predictions_three_fast(
    df,
    path_lrgen  = path_lrgen,
    path_lrecon = path_lrecon,
    path_galtan = path_galtan,
    n_workers   = n_workers
  )
}
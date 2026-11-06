#' iTAPIO fitting, prediction and feature importance
#'
#' Fit frozen PCA-tree ensembles on complete numeric training data and predict
#' cluster assignments for new samples or visit prefixes from new patients.
#' The fitters take a fixed number of clusters (default 4). Models retain
#' training labels in cluster and frozen trees for prediction.
#'
#' @name inductive
#' @param DATA Numeric training matrix or data frame, without missing values.
#' @param user_id Patient identifiers, one per longitudinal row. Training needs
#'   equal visit counts, in chronological order within each patient.
#' @param k Number of training clusters, at least two.
#' @param n_features Number of features sampled per tree; NULL or NA selects
#'   max(1, floor(sqrt(ncol(DATA)))).
#' @param n_trees Positive integer number of trees.
#' @param levels Hierarchy levels, smaller than the number of training units.
#' @param method Hierarchical linkage method.
#' @param scale Scale features in each training PCA; reused during prediction.
#' @param replace Sample features with replacement.
#' @param pca_selection PCA selection mode. TAPIO supports first/random_weighted;
#'   longTAPIO also supports random_weighted_95. All fitters default to
#'   random_weighted.
#' @param object A fitted inductiveTAPIO or inductiveLongTAPIO model.
#' @param newdata New numeric data with matching feature names and order.
#' @param visits Number of leading visits per patient used for prediction, from
#'   one to the training visit count. Each new patient needs at least this many.
#' @param res Fitted inductive model for importance calculation.
#' @param normalize Normalize importance by its largest value.
#' @param ... Additional arguments passed to importance methods, or unused by prediction methods.
#' @return Fitters return models of class inductiveTAPIO or inductiveLongTAPIO,
#'   with cluster, train_clusters, trees, affinity and distance. Predictions return
#'   cluster, cluster_scores, margin, confidence (an alias for margin) and affinity;
#'   longitudinal predictions also include sorted patients and visits. Scores and
#'   margins are affinity measures, not probabilities. TAPIO importance returns a
#'   cluster-by-feature matrix; longitudinal importance returns a list with global
#'   and, when complete new trajectories are supplied, patient,
#'   cluster_progressive and incremental importance.
#' @seealso [discretize_longitudinal()]
#' @importFrom stats as.dist cutree dist prcomp
NULL

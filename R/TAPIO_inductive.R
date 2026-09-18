# ======================================================================
# INDUCTIVE TAPIO
#
# Strict inductive extension of the original static TAPIO method.
#
# Main API:
#
#   fit <- TAPIO_inductive(
#       DATA_train,
#       k = 4
#   )
#
#   pred <- predict(
#       fit,
#       newdata = DATA_test
#   )
#
#
# Training:
#   1. Randomly sample features for each tree
#   2. Fit PCA on TRAINING data only
#   3. Select one PC proportional to its eigenvalue
#   4. Perform hierarchical clustering of the 1-D PCA scores
#   5. Store hierarchy cluster labels and score centroids
#   6. Construct TAPIO training affinity
#   7. Obtain final reference clusters
#
#
# Prediction:
#   1. Apply frozen feature subset
#   2. Apply frozen PCA transformation
#   3. At each hierarchy level, assign the new observation to
#      the closest stored training centroid
#   4. Construct new-to-training TAPIO affinity
#   5. Assign the new observation to the final training cluster
#      with maximum mean affinity
#
# ======================================================================


# ======================================================================
# INTERNAL: ASSOCIATION MATRIX
# ======================================================================

.TAPIO_association <- function(cl) {

    outer(
        cl,
        cl,
        FUN = function(a, b) {
            as.numeric(a == b)
        }
    )
}


# ======================================================================
# MAIN FITTING FUNCTION
# ======================================================================

#' Fit inductive TAPIO
#'
#' Fits a strictly inductive TAPIO model on a reference dataset.
#' Previously unseen observations can subsequently be assigned to
#' the learned clusters without refitting PCA or hierarchical
#' clustering.
#'
#' @param DATA Numeric matrix or data.frame. Rows correspond to
#'   observations and columns to features.
#' @param k Number of final clusters.
#' @param n_features Number of features sampled for each tree.
#'   If NULL, floor(sqrt(ncol(DATA))) is used.
#' @param n_trees Number of ensemble trees.
#' @param levels Number of hierarchy levels used to construct the
#'   TAPIO affinity.
#' @param method Hierarchical clustering method. Default "ward.D2".
#' @param scale Logical. Should variables be scaled during PCA?
#' @param replace Logical. Sample features with replacement?
#'
#' @return Object of class "inductiveTAPIO".
#'
#' @export
TAPIO_inductive <- function(
    DATA,
    k = 4,
    n_features = NULL,
    n_trees = 500,
    levels = 10,
    method = "ward.D2",
    scale = FALSE,
    replace = TRUE
) {

    # ==============================================================
    # BASIC INPUT CHECKS
    # ==============================================================

    DATA <- as.matrix(DATA)


    if(!is.numeric(DATA)) {

        stop(
            "DATA must contain numeric features."
        )
    }


    if(anyNA(DATA)) {

        stop(
            paste0(
                "Missing feature values are currently not supported ",
                "by inductive TAPIO."
            )
        )
    }


    n <- nrow(DATA)

    p <- ncol(DATA)


    if(n < 2) {

        stop(
            "DATA must contain at least two observations."
        )
    }


    if(p < 1) {

        stop(
            "DATA must contain at least one feature."
        )
    }


    if(k < 2 || k > n) {

        stop(
            "k must be between 2 and nrow(DATA)."
        )
    }


    if(levels < 1) {

        stop(
            "levels must be at least 1."
        )
    }


    if((levels + 1) > n) {

        stop(
            "levels + 1 cannot exceed nrow(DATA)."
        )
    }


    if(n_trees < 1) {

        stop(
            "n_trees must be at least 1."
        )
    }


    # ==============================================================
    # DEFAULT NUMBER OF FEATURES
    # ==============================================================

    if(is.null(n_features)) {

        n_features <- floor(
            sqrt(p)
        )
    }


    n_features <- as.integer(
        n_features
    )


    if(n_features < 1) {

        stop(
            "n_features must be at least 1."
        )
    }


    if(
        !replace &&
        n_features > p
    ) {

        stop(
            paste0(
                "n_features cannot exceed ncol(DATA) when ",
                "replace = FALSE."
            )
        )
    }


    # ==============================================================
    # FEATURE NAMES
    # ==============================================================

    feature_names <- colnames(
        DATA
    )


    # ==============================================================
    # STORAGE
    # ==============================================================

    AFF <- matrix(
        0,
        nrow = n,
        ncol = n
    )


    TREES <- vector(
        "list",
        n_trees
    )


    # ==============================================================
    # TAPIO ENSEMBLE
    # ==============================================================

    for(tree_id in seq_len(
        n_trees
    )) {

        # ==========================================================
        # RANDOM FEATURE SUBSET
        # ==========================================================

        ids <- sample(
            seq_len(p),
            size = n_features,
            replace = replace
        )


        DATA_s <- DATA[
            ,
            ids,
            drop = FALSE
        ]


        # ==========================================================
        # PCA
        #
        # Fitted only on the reference/training observations.
        # ==========================================================

        pca <- stats::prcomp(
            DATA_s,
            center = TRUE,
            scale. = scale
        )


        # ==========================================================
        # RANDOM EIGENVALUE-WEIGHTED PC
        #
        #       lambda_j
        # P(j) = --------
        #       sum lambda
        #
        # ==========================================================

        eigenvalues <- pca$sdev^2


        probabilities <- eigenvalues /
            sum(
                eigenvalues
            )


        selected_pc <- sample(
            seq_along(
                probabilities
            ),
            size = 1,
            prob = probabilities
        )


        # ==========================================================
        # SELECTED 1-D PCA REPRESENTATION
        # ==========================================================

        scores <- pca$x[
            ,
            selected_pc
        ]


        # ==========================================================
        # HIERARCHICAL CLUSTERING
        # ==========================================================

        hc <- fastcluster::hclust(
            stats::dist(
                scores
            ),
            method = method
        )


        # ==========================================================
        # STORAGE FOR HIERARCHY
        # ==========================================================

        level_clusters <- vector(
            "list",
            levels
        )


        level_centroids <- vector(
            "list",
            levels
        )


        # ==========================================================
        # MULTI-LEVEL PARTITIONS
        # ==========================================================

        for(level_id in seq_len(
            levels
        )) {

            n_level_clusters <- level_id + 1


            cl <- stats::cutree(
                hc,
                k = n_level_clusters
            )


            level_clusters[[level_id]] <- cl


            # ======================================================
            # CENTROID OF EACH CLUSTER IN SELECTED PC SPACE
            # ======================================================

            cluster_ids <- sort(
                unique(
                    cl
                )
            )


            centroids <- numeric(
                length(
                    cluster_ids
                )
            )


            names(
                centroids
            ) <- as.character(
                cluster_ids
            )


            for(j in seq_along(
                cluster_ids
            )) {

                cluster_id <- cluster_ids[j]


                members <- which(
                    cl == cluster_id
                )


                centroids[j] <- mean(
                    scores[
                        members
                    ]
                )
            }


            level_centroids[[level_id]] <- centroids


            # ======================================================
            # ADD CO-MEMBERSHIP TO TAPIO AFFINITY
            # ======================================================

            AFF <- AFF +
                .TAPIO_association(
                    cl
                )
        }


        # ==========================================================
        # STORE FROZEN TREE
        # ==========================================================

        TREES[[tree_id]] <- list(

            feature_ids =
                ids,

            pca_center =
                pca$center,

            pca_scale =
                pca$scale,

            pca_rotation =
                pca$rotation[
                    ,
                    selected_pc
                ],

            selected_pc =
                selected_pc,

            selected_eigenvalue =
                eigenvalues[
                    selected_pc
                ],

            train_scores =
                scores,

            level_clusters =
                level_clusters,

            level_centroids =
                level_centroids
        )
    }


    # ==============================================================
    # NORMALIZED TAPIO AFFINITY
    #
    # Each tree contributes `levels` binary co-memberships.
    #
    # Therefore AFF is naturally in [0,1].
    # ==============================================================

    AFF <- AFF /
        (
            n_trees *
            levels
        )


    # ==============================================================
    # TAPIO DISTANCE
    # ==============================================================

    DIST <- 1 - AFF


    diag(
        DIST
    ) <- 0


    # ==============================================================
    # FINAL REFERENCE CLUSTERING
    # ==============================================================

    final_hc <- fastcluster::hclust(
        stats::as.dist(
            DIST
        ),
        method = method
    )


    final_clusters <- stats::cutree(
        final_hc,
        k = k
    )


    # ==============================================================
    # MODEL OBJECT
    # ==============================================================

    model <- list(

        call =
            match.call(),

        trees =
            TREES,

        cluster =
            final_clusters,

        train_clusters =
            final_clusters,

        affinity =
            AFF,

        distance =
            DIST,

        final_hclust =
            final_hc,

        k =
            k,

        levels =
            levels,

        n_trees =
            n_trees,

        n_features =
            n_features,

        n_features_total =
            p,

        feature_names =
            feature_names,

        method =
            method,

        scale =
            scale,

        replace =
            replace,

        pca_selection =
            "random_weighted"
    )


    class(
        model
    ) <- "inductiveTAPIO"


    return(
        model
    )
}


# ======================================================================
# INTERNAL:
# PROJECT NEW OBSERVATIONS THROUGH ONE FROZEN TREE
# ======================================================================

.TAPIO_project_tree <- function(
    tree,
    newdata
) {

    # ==============================================================
    # SAME FEATURE SUBSET AS DURING TRAINING
    # ==============================================================

    X <- newdata[
        ,
        tree$feature_ids,
        drop = FALSE
    ]


    # ==============================================================
    # FROZEN TRAINING PCA CENTER
    # ==============================================================

    X <- sweep(
        X,
        MARGIN = 2,
        STATS = tree$pca_center,
        FUN = "-"
    )


    # ==============================================================
    # FROZEN TRAINING PCA SCALE
    # ==============================================================

    if(
        !is.null(
            tree$pca_scale
        ) &&
        !identical(
            tree$pca_scale,
            FALSE
        )
    ) {

        X <- sweep(
            X,
            MARGIN = 2,
            STATS = tree$pca_scale,
            FUN = "/"
        )
    }


    # ==============================================================
    # FROZEN PCA PROJECTION
    #
    # One scalar per new observation.
    # ==============================================================

    scores <- drop(
        X %*%
            tree$pca_rotation
    )


    return(
        scores
    )
}


# ======================================================================
# INTERNAL:
# ONE TREE -> NEW-TO-TRAIN AFFINITY
# ======================================================================

.TAPIO_tree_affinity <- function(
    tree,
    new_scores
) {

    n_new <- length(
        new_scores
    )


    n_train <- length(
        tree$train_scores
    )


    A <- matrix(
        0,
        nrow = n_new,
        ncol = n_train
    )


    # ==============================================================
    # EACH STORED HIERARCHY LEVEL
    # ==============================================================

    for(level_id in seq_along(
        tree$level_clusters
    )) {

        train_level_cluster <-
            tree$level_clusters[
                [level_id]
            ]


        centroids <-
            tree$level_centroids[
                [level_id]
            ]


        # ==========================================================
        # ASSIGN EACH NEW OBSERVATION TO NEAREST STORED CENTROID
        # ==========================================================

        predicted_level_cluster <- integer(
            n_new
        )


        for(i in seq_len(
            n_new
        )) {

            distances <- (
                new_scores[i] -
                centroids
            )^2


            predicted_level_cluster[i] <-
                as.integer(
                    names(
                        centroids
                    )[
                        which.min(
                            distances
                        )
                    ]
                )
        }


        # ==========================================================
        # NEW -> TRAIN CO-MEMBERSHIP
        # ==========================================================

        A <- A +
            outer(
                predicted_level_cluster,
                train_level_cluster,
                FUN = function(a, b) {
                    as.numeric(a == b)
                }
            )
    }


    return(
        A
    )
}


# ======================================================================
# INTERNAL:
# ENSEMBLE NEW-TO-TRAIN AFFINITY
# ======================================================================

.TAPIO_predict_affinity <- function(
    object,
    newdata
) {

    n_new <- nrow(
        newdata
    )


    n_train <- length(
        object$train_clusters
    )


    AFF_NEW <- matrix(
        0,
        nrow = n_new,
        ncol = n_train
    )


    # ==============================================================
    # ALL TREES
    # ==============================================================

    for(tree_id in seq_len(
        object$n_trees
    )) {

        tree <- object$trees[
            [tree_id]
        ]


        # ==========================================================
        # FROZEN PCA PROJECTION
        # ==========================================================

        new_scores <- .TAPIO_project_tree(
            tree,
            newdata
        )


        # ==========================================================
        # TREE AFFINITY
        # ==========================================================

        AFF_NEW <- AFF_NEW +
            .TAPIO_tree_affinity(
                tree,
                new_scores
            )
    }


    # ==============================================================
    # NORMALIZE
    # ==============================================================

    AFF_NEW <- AFF_NEW /
        (
            object$n_trees *
            object$levels
        )


    return(
        AFF_NEW
    )
}


# ======================================================================
# PREDICT METHOD
# ======================================================================

#' Predict clusters for new observations using inductive TAPIO
#'
#' Projects previously unseen observations through the frozen TAPIO
#' ensemble and assigns them to clusters learned from the reference
#' dataset.
#'
#' @param object Fitted object returned by \code{TAPIO_inductive}.
#' @param newdata Numeric matrix or data.frame containing new
#'   observations.
#' @param ... Additional arguments, currently ignored.
#'
#' @return A prediction object containing cluster assignments,
#'   cluster affinity scores, assignment margins, and new-to-training
#'   affinities.
#'
#' @export
predict.inductiveTAPIO <- function(
    object,
    newdata,
    ...
) {

    # ==============================================================
    # MODEL CHECK
    # ==============================================================

    if(
        !inherits(
            object,
            "inductiveTAPIO"
        )
    ) {

        stop(
            "object must be an inductiveTAPIO model."
        )
    }


    # ==============================================================
    # NEW DATA CHECKS
    # ==============================================================

    newdata <- as.matrix(
        newdata
    )


    if(!is.numeric(newdata)) {

        stop(
            "newdata must contain numeric features."
        )
    }


    if(anyNA(newdata)) {

        stop(
            paste0(
                "Missing feature values are currently not supported ",
                "during inductive TAPIO prediction."
            )
        )
    }


    if(
        ncol(newdata) !=
        object$n_features_total
    ) {

        stop(
            paste0(
                "newdata contains ",
                ncol(newdata),
                " features, whereas the fitted model expects ",
                object$n_features_total,
                "."
            )
        )
    }


    # ==============================================================
    # OPTIONAL FEATURE-NAME CHECK
    # ==============================================================

    if(
        !is.null(
            object$feature_names
        ) &&
        !is.null(
            colnames(
                newdata
            )
        )
    ) {

        if(
            !identical(
                object$feature_names,
                colnames(
                    newdata
                )
            )
        ) {

            stop(
                paste0(
                    "Feature names/order in newdata do not match ",
                    "the training data."
                )
            )
        }
    }


    # ==============================================================
    # NEW -> TRAIN TAPIO AFFINITY
    # ==============================================================

    Anew <- .TAPIO_predict_affinity(
        object,
        newdata
    )


    n_new <- nrow(
        newdata
    )


    # ==============================================================
    # AFFINITY TO EACH FINAL REFERENCE CLUSTER
    #
    #                     1
    # s_k(x*) = ---------------------- sum A(x*, x_i)
    #             number of i in C_k   i in C_k
    #
    # ==============================================================

    cluster_scores <- matrix(
        NA_real_,
        nrow = n_new,
        ncol = object$k
    )


    colnames(
        cluster_scores
    ) <- paste0(
        "cluster_",
        seq_len(
            object$k
        )
    )


    for(k_id in seq_len(
        object$k
    )) {

        members <- which(
            object$train_clusters ==
            k_id
        )


        cluster_scores[
            ,
            k_id
        ] <- rowMeans(
            Anew[
                ,
                members,
                drop = FALSE
            ]
        )
    }


    # ==============================================================
    # FINAL CLUSTER
    # ==============================================================

    predicted_cluster <- max.col(
        cluster_scores,
        ties.method = "first"
    )


    # Preserve row names where possible.

    if(
        !is.null(
            rownames(
                newdata
            )
        )
    ) {

        names(
            predicted_cluster
        ) <- rownames(
            newdata
        )


        rownames(
            cluster_scores
        ) <- rownames(
            newdata
        )


        rownames(
            Anew
        ) <- rownames(
            newdata
        )
    }


    # ==============================================================
    # ASSIGNMENT MARGIN
    #
    # largest cluster affinity - second largest cluster affinity
    #
    # NOTE:
    # This is a relative assignment score, NOT a probability.
    # ==============================================================

    margin <- apply(
        cluster_scores,
        1,
        function(x) {

            sx <- sort(
                x,
                decreasing = TRUE
            )


            if(length(sx) < 2) {

                return(
                    NA_real_
                )
            }


            sx[1] - sx[2]
        }
    )


    # ==============================================================
    # RESULT
    # ==============================================================

    result <- list(

        cluster =
            predicted_cluster,

        scores =
            cluster_scores,

        margin =
            margin,

        affinity =
            Anew
    )


    class(
        result
    ) <- "predict.inductiveTAPIO"


    return(
        result
    )
}


# ======================================================================
# PRINT FITTED MODEL
# ======================================================================

#' @export
print.inductiveTAPIO <- function(
    x,
    ...
) {

    cat(
        "Inductive TAPIO\n"
    )

    cat(
        "---------------\n"
    )

    cat(
        "Training samples :",
        length(
            x$cluster
        ),
        "\n"
    )

    cat(
        "Features         :",
        x$n_features_total,
        "\n"
    )

    cat(
        "Features/tree    :",
        x$n_features,
        "\n"
    )

    cat(
        "Trees            :",
        x$n_trees,
        "\n"
    )

    cat(
        "Hierarchy levels :",
        x$levels,
        "\n"
    )

    cat(
        "Final clusters   :",
        x$k,
        "\n"
    )

    cat(
        "PCA selection    : random weighted\n"
    )

    cat(
        "Clustering       :",
        x$method,
        "\n"
    )


    invisible(
        x
    )
}


# ======================================================================
# PRINT PREDICTION
# ======================================================================

#' @export
print.predict.inductiveTAPIO <- function(
    x,
    ...
) {

    cat(
        "Inductive TAPIO prediction\n"
    )

    cat(
        "---------------------------\n"
    )

    cat(
        "New observations:",
        length(
            x$cluster
        ),
        "\n\n"
    )


    result <- data.frame(

        cluster =
            as.integer(
                x$cluster
            ),

        margin =
            as.numeric(
                x$margin
            )
    )


    if(
        !is.null(
            names(
                x$cluster
            )
        )
    ) {

        rownames(
            result
        ) <- names(
            x$cluster
        )
    }


    print(
        result
    )


    invisible(
        x
    )
}
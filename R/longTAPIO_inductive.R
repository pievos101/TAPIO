# ======================================================================
# INDUCTIVE longTAPIO
#
# Strict inductive and progressive/prefix extension of longTAPIO
#
# Main API:
#
#   fit <- longTAPIO_inductive(
#       DATA,
#       user_id,
#       k = 4
#   )
#
#   predict(
#       fit,
#       newdata,
#       user_id = new_user_id,
#       visits = 2
#   )
#
#
# Training:
#   * feature subsets sampled on training/reference data
#   * PCA fitted on training/reference data
#   * PCA component sampled proportional to explained variance
#   * hierarchical trajectory clusters learned on training data
#   * trajectory prototypes learned on training data
#   * final TAPIO clusters learned on training data
#
# Prediction:
#   * all learned quantities remain frozen
#   * a new patient may have only the first h visits available
#   * new trajectory prefixes are compared with corresponding
#     prefixes of stored training trajectory prototypes
#
# Current assumptions:
#   * aligned visits
#   * equal number of visits for training patients
#   * prediction uses the first h visits
#   * numeric input matrix/data.frame
#
# ======================================================================


# ======================================================================
# INTERNAL: ASSOCIATION MATRIX
# ======================================================================

.longTAPIO_association <- function(cl) {

    outer(
        cl,
        cl,
        FUN = function(a, b) {
            as.numeric(a == b)
        }
    )
}


# ======================================================================
# INTERNAL: PREPARE TRAINING LONGITUDINAL DATA
# ======================================================================

.longTAPIO_prepare_data <- function(
    DATA,
    user_id
) {

    DATA <- as.matrix(DATA)


    if(nrow(DATA) != length(user_id)) {

        stop(
            "length(user_id) must equal nrow(DATA)."
        )
    }


    if(anyNA(user_id)) {

        stop(
            "user_id must not contain missing values."
        )
    }


    if(anyNA(DATA)) {

        stop(
            paste0(
                "Missing feature values are currently not supported ",
                "by inductive longTAPIO."
            )
        )
    }


    patients <- sort(
        unique(user_id)
    )


    visit_counts <- table(
        user_id
    )


    if(
        length(
            unique(
                visit_counts
            )
        ) != 1
    ) {

        stop(
            paste0(
                "inductive longTAPIO currently requires the same ",
                "number of aligned visits for every training patient."
            )
        )
    }


    n_visits <- as.integer(
        visit_counts[1]
    )


    # --------------------------------------------------------------
    # Put complete trajectories next to each other:
    #
    # patient 1: visit 1 ... T
    # patient 2: visit 1 ... T
    # ...
    #
    # Within-patient order is assumed to already represent
    # chronological visit order.
    # --------------------------------------------------------------

    row_order <- unlist(
        lapply(
            patients,
            function(pid) {
                which(user_id == pid)
            }
        ),
        use.names = FALSE
    )


    DATA <- DATA[
        row_order,
        ,
        drop = FALSE
    ]


    user_id <- user_id[
        row_order
    ]


    return(
        list(
            DATA = DATA,
            user_id = user_id,
            patients = patients,
            n_visits = n_visits
        )
    )
}


# ======================================================================
# INTERNAL: EXTRACT FIRST h VISITS OF NEW PATIENTS
# ======================================================================

.longTAPIO_extract_prefix <- function(
    DATA,
    user_id,
    visits
) {

    DATA <- as.matrix(DATA)


    if(nrow(DATA) != length(user_id)) {

        stop(
            "length(user_id) must equal nrow(newdata)."
        )
    }


    if(anyNA(user_id)) {

        stop(
            "user_id must not contain missing values."
        )
    }


    if(anyNA(DATA)) {

        stop(
            paste0(
                "Missing feature values are currently not supported ",
                "during prediction."
            )
        )
    }


    patients <- sort(
        unique(user_id)
    )


    selected_rows <- integer(0)


    for(pid in patients) {

        ids <- which(
            user_id == pid
        )


        if(length(ids) < visits) {

            stop(
                paste0(
                    "Patient ",
                    pid,
                    " has only ",
                    length(ids),
                    " visits, but visits = ",
                    visits,
                    " was requested."
                )
            )
        }


        # ----------------------------------------------------------
        # First h visits.
        #
        # The order in DATA is assumed to be chronological within
        # each patient.
        # ----------------------------------------------------------

        selected_rows <- c(
            selected_rows,
            ids[
                seq_len(visits)
            ]
        )
    }


    X <- DATA[
        selected_rows,
        ,
        drop = FALSE
    ]


    # --------------------------------------------------------------
    # Temporary consecutive patient IDs
    # --------------------------------------------------------------

    new_user_id <- rep(
        seq_along(patients),
        each = visits
    )


    return(
        list(
            DATA = X,
            user_id = new_user_id,
            patients = patients
        )
    )
}


# ======================================================================
# MAIN FITTING FUNCTION
# ======================================================================

#' Fit inductive longitudinal TAPIO
#'
#' Fits a strictly inductive longitudinal TAPIO model on a reference
#' cohort. The fitted model can subsequently assign previously unseen
#' patients to the learned trajectory clusters using either complete
#' trajectories or prefixes containing only the first few visits.
#'
#' @param DATA Numeric matrix or data.frame. Rows correspond to visits
#'   and columns to features.
#' @param user_id Patient identifier for every row of DATA.
#' @param k Number of final trajectory clusters.
#' @param n_features Number of features sampled for each ensemble tree.
#'   If NULL, floor(sqrt(ncol(DATA))) is used.
#' @param n_trees Number of ensemble trees.
#' @param levels Number of hierarchy levels used for construction of
#'   the TAPIO affinity.
#' @param method Hierarchical clustering method. Default is "ward.D2".
#' @param scale Logical. Should variables be scaled during PCA?
#' @param replace Logical. Sample features with replacement?
#'
#' @return Object of class "inductiveLongTAPIO".
#'
#' @export
longTAPIO_inductive <- function(
    DATA,
    user_id,
    k = 4,
    n_features = NULL,
    n_trees = 500,
    levels = 4,
    method = "ward.D2",
    scale = TRUE,
    replace = TRUE
) {

    # ==============================================================
    # BASIC CHECKS
    # ==============================================================

    DATA <- as.matrix(DATA)


    if(!is.numeric(DATA)) {

        stop(
            "DATA must contain numeric features."
        )
    }


    if(nrow(DATA) != length(user_id)) {

        stop(
            "length(user_id) must equal nrow(DATA)."
        )
    }


    if(ncol(DATA) < 1) {

        stop(
            "DATA must contain at least one feature."
        )
    }


    if(k < 2) {

        stop(
            "k must be at least 2."
        )
    }


    if(levels < 1) {

        stop(
            "levels must be at least 1."
        )
    }


    if(n_trees < 1) {

        stop(
            "n_trees must be at least 1."
        )
    }


    # ==============================================================
    # PREPARE TRAINING DATA
    # ==============================================================

    tmp <- .longTAPIO_prepare_data(
        DATA,
        user_id
    )


    DATA <- tmp$DATA

    user_id <- tmp$user_id

    patients <- tmp$patients

    n_visits <- tmp$n_visits


    n_patients <- length(
        patients
    )


    p <- ncol(
        DATA
    )


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


    if(!replace && n_features > p) {

        stop(
            paste0(
                "n_features cannot exceed ncol(DATA) when ",
                "replace = FALSE."
            )
        )
    }


    if(k > n_patients) {

        stop(
            "k cannot exceed the number of training patients."
        )
    }


    if((levels + 1) > n_patients) {

        stop(
            paste0(
                "levels + 1 cannot exceed the number ",
                "of training patients."
            )
        )
    }


    # ==============================================================
    # STORE FEATURE NAMES
    # ==============================================================

    feature_names <- colnames(
        DATA
    )


    # ==============================================================
    # AFFINITY MATRIX
    # ==============================================================

    AFF <- matrix(
        0,
        nrow = n_patients,
        ncol = n_patients
    )


    # ==============================================================
    # TREE STORAGE
    # ==============================================================

    TREES <- vector(
        "list",
        n_trees
    )


    # ==============================================================
    # ENSEMBLE
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
        # IMPORTANT:
        # This PCA is fitted exclusively on reference/training data.
        # ==========================================================

        pca <- stats::prcomp(
            DATA_s,
            center = TRUE,
            scale. = scale
        )


        # ==========================================================
        # RANDOM EIGENVALUE-WEIGHTED COMPONENT SELECTION
        #
        # P(PC = j) =
        #
        #       lambda_j
        # ---------------------
        # sum_r lambda_r
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
        # PCA SCORE OF EVERY TRAINING VISIT
        # ==========================================================

        scores <- pca$x[
            ,
            selected_pc
        ]


        # ==========================================================
        # PATIENT TRAJECTORIES
        #
        #               visit
        #             1 ... T
        #
        # patient 1   z ... z
        # patient 2   z ... z
        # ...
        #
        # ==========================================================

        trajectories <- matrix(
            scores,
            nrow = n_patients,
            ncol = n_visits,
            byrow = TRUE
        )


        rownames(
            trajectories
        ) <- as.character(
            patients
        )


        # ==========================================================
        # HIERARCHICAL CLUSTERING OF TRAJECTORIES
        # ==========================================================

        trajectory_distance <- stats::dist(
            trajectories
        )


        hc <- fastcluster::hclust(
            trajectory_distance,
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
        # CUT DENDROGRAM AT MULTIPLE LEVELS
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
            # TRAJECTORY CENTROID OF EACH HIERARCHY CLUSTER
            # ======================================================

            cluster_ids <- sort(
                unique(
                    cl
                )
            )


            centroids <- matrix(
                NA_real_,
                nrow = length(cluster_ids),
                ncol = n_visits
            )


            rownames(
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


                centroids[
                    j,
                ] <- colMeans(
                    trajectories[
                        members,
                        ,
                        drop = FALSE
                    ]
                )
            }


            level_centroids[[level_id]] <- centroids


            # ======================================================
            # ADD LEVEL CO-MEMBERSHIP
            # ======================================================

            AFF <- AFF +
                .longTAPIO_association(
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

            train_trajectories =
                trajectories,

            level_clusters =
                level_clusters,

            level_centroids =
                level_centroids
        )
    }


    # ==============================================================
    # NORMALIZED TAPIO AFFINITY
    # ==============================================================

    AFF <- AFF /
        (
            n_trees *
            levels
        )


    # ==============================================================
    # DISTANCE
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


    names(
        final_clusters
    ) <- as.character(
        patients
    )


    # ==============================================================
    # CONSTRUCT MODEL
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

        patients =
            patients,

        train_patients =
            patients,

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

        n_visits =
            n_visits,

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
    ) <- "inductiveLongTAPIO"


    return(
        model
    )
}


# ======================================================================
# INTERNAL:
# PROJECT NEW PREFIX THROUGH ONE FROZEN TREE
# ======================================================================

.longTAPIO_project_prefix <- function(
    tree,
    DATA,
    n_patients,
    visits
) {

    # ==============================================================
    # EXACT SAME FEATURE POSITIONS AS TRAINING
    # ==============================================================

    X <- DATA[
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
    # ==============================================================

    scores <- drop(
        X %*%
            tree$pca_rotation
    )


    # ==============================================================
    # RECONSTRUCT PREFIX TRAJECTORIES
    # ==============================================================

    trajectories <- matrix(
        scores,
        nrow = n_patients,
        ncol = visits,
        byrow = TRUE
    )


    return(
        trajectories
    )
}


# ======================================================================
# INTERNAL:
# ONE TREE PREFIX AFFINITY
# ======================================================================

.longTAPIO_tree_prefix_affinity <- function(
    tree,
    new_trajectories,
    visits
) {

    n_new <- nrow(
        new_trajectories
    )


    n_train <- nrow(
        tree$train_trajectories
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
            tree$level_clusters[[level_id]]


        full_centroids <-
            tree$level_centroids[[level_id]]


        # ==========================================================
        # PREFIX OF STORED COMPLETE TRAJECTORY PROTOTYPES
        # ==========================================================

        prefix_centroids <- full_centroids[
            ,
            seq_len(visits),
            drop = FALSE
        ]


        # ==========================================================
        # ASSIGN NEW PATIENT TO CLOSEST PROTOTYPE
        # ==========================================================

        predicted_level_cluster <- integer(
            n_new
        )


        for(i in seq_len(
            n_new
        )) {

            z <- new_trajectories[
                i,
                ,
                drop = TRUE
            ]


            # ------------------------------------------------------
            # Squared Euclidean distance
            # ------------------------------------------------------

            distances <- rowSums(
                (
                    prefix_centroids -
                    matrix(
                        z,
                        nrow = nrow(
                            prefix_centroids
                        ),
                        ncol = visits,
                        byrow = TRUE
                    )
                )^2
            )


            predicted_level_cluster[i] <-
                as.integer(
                    rownames(
                        prefix_centroids
                    )[
                        which.min(
                            distances
                        )
                    ]
                )
        }


        # ==========================================================
        # NEW -> TRAIN CO-MEMBERSHIP AT THIS LEVEL
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
# COMPLETE NEW -> TRAIN PREFIX AFFINITY
# ======================================================================

.longTAPIO_predict_affinity <- function(
    object,
    DATA,
    visits
) {

    n_new <- nrow(
        DATA
    ) / visits


    if(
        n_new != floor(
            n_new
        )
    ) {

        stop(
            "The number of prefix rows is inconsistent with visits."
        )
    }


    n_new <- as.integer(
        n_new
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
    # ENSEMBLE
    # ==============================================================

    for(tree_id in seq_len(
        object$n_trees
    )) {

        tree <- object$trees[
            [tree_id]
        ]


        # ==========================================================
        # PROJECT PREFIX
        # ==========================================================

        new_trajectories <-
            .longTAPIO_project_prefix(
                tree = tree,
                DATA = DATA,
                n_patients = n_new,
                visits = visits
            )


        # ==========================================================
        # TREE AFFINITY
        # ==========================================================

        AFF_NEW <- AFF_NEW +
            .longTAPIO_tree_prefix_affinity(
                tree = tree,
                new_trajectories = new_trajectories,
                visits = visits
            )
    }


    # ==============================================================
    # NORMALIZATION
    # ==============================================================

    AFF_NEW <- AFF_NEW /
        (
            object$n_trees *
            object$levels
        )


    colnames(
        AFF_NEW
    ) <- as.character(
        object$train_patients
    )


    return(
        AFF_NEW
    )
}


# ======================================================================
# PREDICT METHOD
# ======================================================================

#' Predict clusters for new patients using inductive longTAPIO
#'
#' Assigns previously unseen patients to clusters learned by
#' \code{longTAPIO_inductive}. Prediction can use either the complete
#' trajectory or only the first few available visits.
#'
#' @param object Fitted object returned by \code{longTAPIO_inductive}.
#' @param newdata Numeric matrix or data.frame containing new visits.
#' @param user_id Patient identifier for every row of newdata.
#' @param visits Number of first visits to use for prediction.
#'   By default, all visits used during model fitting are required.
#' @param ... Additional arguments, currently ignored.
#'
#' @return A list containing predicted clusters, cluster affinity scores,
#'   prediction margins, and new-to-training affinity.
#'
#' @export
predict.inductiveLongTAPIO <- function(
    object,
    newdata,
    user_id,
    visits = object$n_visits,
    ...
) {

    # ==============================================================
    # CHECK MODEL
    # ==============================================================

    if(
        !inherits(
            object,
            "inductiveLongTAPIO"
        )
    ) {

        stop(
            "object must be an inductiveLongTAPIO model."
        )
    }


    # ==============================================================
    # CHECK VISITS
    # ==============================================================

    if(
        length(visits) != 1 ||
        is.na(visits) ||
        visits < 1 ||
        visits > object$n_visits
    ) {

        stop(
            paste0(
                "visits must be an integer between 1 and ",
                object$n_visits,
                "."
            )
        )
    }


    visits <- as.integer(
        visits
    )


    # ==============================================================
    # CHECK NEW DATA
    # ==============================================================

    newdata <- as.matrix(
        newdata
    )


    if(!is.numeric(newdata)) {

        stop(
            "newdata must contain numeric features."
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
    # EXTRACT FIRST h VISITS
    # ==============================================================

    prefix <- .longTAPIO_extract_prefix(
        DATA = newdata,
        user_id = user_id,
        visits = visits
    )


    n_new <- length(
        prefix$patients
    )


    # ==============================================================
    # NEW -> TRAIN AFFINITY
    # ==============================================================

    Anew <- .longTAPIO_predict_affinity(
        object = object,
        DATA = prefix$DATA,
        visits = visits
    )


    # ==============================================================
    # AFFINITY TO EACH FINAL TRAINING CLUSTER
    #
    # s_k(x*) =
    #
    #       1
    #   --------- sum_i A(x*,x_i)
    #    | C_k |   i in C_k
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


        if(length(members) == 0) {

            cluster_scores[
                ,
                k_id
            ] <- NA_real_

        } else {

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
    }


    # ==============================================================
    # FINAL CLUSTER ASSIGNMENT
    # ==============================================================

    predicted_cluster <- max.col(
        cluster_scores,
        ties.method = "first"
    )


    names(
        predicted_cluster
    ) <- as.character(
        prefix$patients
    )


    rownames(
        cluster_scores
    ) <- as.character(
        prefix$patients
    )


    rownames(
        Anew
    ) <- as.character(
        prefix$patients
    )


    # ==============================================================
    # PREDICTION MARGIN
    #
    # best cluster score - second best cluster score
    #
    # This is NOT a calibrated probability.
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
    # RETURN
    # ==============================================================

    result <- list(

        cluster =
            predicted_cluster,

        scores =
            cluster_scores,

        margin =
            margin,

        affinity =
            Anew,

        patients =
            prefix$patients,

        visits =
            visits
    )


    class(
        result
    ) <- "predict.inductiveLongTAPIO"


    return(
        result
    )
}


# ======================================================================
# PRINT FITTED MODEL
# ======================================================================

#' @export
print.inductiveLongTAPIO <- function(
    x,
    ...
) {

    cat(
        "Inductive longitudinal TAPIO\n"
    )

    cat(
        "----------------------------\n"
    )

    cat(
        "Training patients :",
        length(
            x$train_patients
        ),
        "\n"
    )

    cat(
        "Visits/patient    :",
        x$n_visits,
        "\n"
    )

    cat(
        "Features          :",
        x$n_features_total,
        "\n"
    )

    cat(
        "Features/tree     :",
        x$n_features,
        "\n"
    )

    cat(
        "Trees             :",
        x$n_trees,
        "\n"
    )

    cat(
        "Hierarchy levels  :",
        x$levels,
        "\n"
    )

    cat(
        "Final clusters    :",
        x$k,
        "\n"
    )

    cat(
        "PCA selection     : random weighted\n"
    )

    cat(
        "Clustering method :",
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
print.predict.inductiveLongTAPIO <- function(
    x,
    ...
) {

    cat(
        "Inductive longTAPIO prediction\n"
    )

    cat(
        "--------------------------------\n"
    )

    cat(
        "Patients          :",
        length(
            x$cluster
        ),
        "\n"
    )

    cat(
        "Observed visits   :",
        x$visits,
        "\n\n"
    )


    result <- data.frame(

        patient =
            x$patients,

        cluster =
            as.integer(
                x$cluster
            ),

        margin =
            as.numeric(
                x$margin
            )
    )


    print(
        result,
        row.names = FALSE
    )


    invisible(
        x
    )
}
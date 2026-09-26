# =============================================================================
# PBC2 - INDUCTIVE / INTERPRETABILITY DEMONSTRATION FOR longTAPIO
#
# PURPOSE
#
# Demonstrate properties that are specifically useful for an inductive,
# interpretable longitudinal clustering method:
#
# 1. Progressive prediction of unseen patients
# 2. Prediction confidence / margin
# 3. Relationship between confidence and final assignment
# 4. Patient-specific feature importance
# 5. Convergence of early explanations toward final explanations
# 6. Representative stable and switching patients
#
#
# FIGURE 1:
#   Prediction confidence across visits
#
# FIGURE 2:
#   Confidence for correct vs changing early assignments
#
# FIGURE 3:
#   Explanation convergence across visits
#
# FIGURE 4:
#   Confidence versus explanation convergence
#
# FIGURE 5:
#   Patient-specific importance evolution
#
# FIGURE 6:
#   Patient-level prediction trajectory + confidence
#
# =============================================================================


# =============================================================================
# PACKAGES
# =============================================================================

library(DynForest)
library(ggplot2)
library(aricode)
library(survival)
library(survminer)


# =============================================================================
# CHECK FUNCTIONS
# =============================================================================

if(!exists("longTAPIO_inductive")) {

    stop(
        "Please source longTAPIO_inductive() first."
    )
}


if(!exists("importance_longTAPIO_inductive")) {

    stop(
        "Please source importance_longTAPIO_inductive() first."
    )
}


# =============================================================================
# SETTINGS
# =============================================================================

SEED <- 42

K <- 2

TRAIN_FRACTION <- 0.70

MAX_VISITS <- 5

HORIZONS <- seq_len(
    MAX_VISITS
)


N_TREES <- 500

LEVELS <- 4

PCA_SELECTION <- "random_weighted"


FEATURES <- c(
    "serBilir",
    "serChol",
    "albumin",
    "alkaline",
    "SGOT",
    "platelets",
    "prothrombin"
)


LOG_FEATURES <- c(
    "serBilir",
    "serChol",
    "alkaline",
    "SGOT",
    "platelets"
)


# =============================================================================
# LOAD PBC2
# =============================================================================

data(
    pbc2,
    package = "DynForest"
)


PBC <- pbc2


cat("\n")
cat("==============================================================================================================\n")
cat("PBC2 - INDUCTIVE longTAPIO INTERPRETABILITY ANALYSIS\n")
cat("==============================================================================================================\n")


cat(
    "\nPatients: ",
    length(unique(PBC$id)),
    "\n",
    sep = ""
)


cat(
    "Rows    : ",
    nrow(PBC),
    "\n",
    sep = ""
)


# =============================================================================
# DEATH ENDPOINT
# =============================================================================

PBC$death_event <- ifelse(
    PBC$event == 2,
    1L,
    0L
)


# =============================================================================
# PATIENT-LEVEL SURVIVAL
# =============================================================================

extract_patient_survival <- function(dat) {

    ids <- sort(
        unique(dat$id)
    )


    OUT <- vector(
        "list",
        length(ids)
    )


    for(i in seq_along(ids)) {

        d_i <- dat[
            dat$id == ids[i],
            ,
            drop = FALSE
        ]


        survival_values <- unique(
            d_i$years[
                !is.na(d_i$years)
            ]
        )


        death_values <- unique(
            d_i$death_event[
                !is.na(d_i$death_event)
            ]
        )


        original_values <- unique(
            d_i$event[
                !is.na(d_i$event)
            ]
        )


        if(length(survival_values) != 1) {

            stop(
                "Non-unique survival time."
            )
        }


        if(length(death_values) != 1) {

            stop(
                "Non-unique death endpoint."
            )
        }


        OUT[[i]] <- data.frame(

            id =
                ids[i],

            survival_time =
                as.numeric(
                    survival_values[1]
                ),

            event =
                as.integer(
                    death_values[1]
                ),

            original_event =
                as.integer(
                    original_values[1]
                ),

            stringsAsFactors = FALSE
        )
    }


    OUT <- do.call(
        rbind,
        OUT
    )


    rownames(OUT) <- NULL


    return(
        OUT
    )
}


SURVIVAL_DATA <- extract_patient_survival(
    PBC
)


# =============================================================================
# PREPARE LONGITUDINAL COHORT
# =============================================================================

prepare_pbc2 <- function(
    dat,
    survival_data,
    features,
    max_visits
) {

    dat <- dat[
        order(
            dat$id,
            dat$time
        ),
        ,
        drop = FALSE
    ]


    complete_rows <- complete.cases(
        dat[
            ,
            features,
            drop = FALSE
        ]
    )


    dat_complete <- dat[
        complete_rows,
        ,
        drop = FALSE
    ]


    visit_counts <- table(
        dat_complete$id
    )


    eligible_ids <- names(
        visit_counts[
            visit_counts >= max_visits
        ]
    )


    cat(
        "\nPatients with >= ",
        max_visits,
        " complete visits: ",
        length(eligible_ids),
        "\n",
        sep = ""
    )


    dat_complete <- dat_complete[
        as.character(dat_complete$id) %in% eligible_ids,
        ,
        drop = FALSE
    ]


    patient_list <- split(
        dat_complete,
        dat_complete$id
    )


    patient_list <- lapply(
        patient_list,
        function(d) {

            d <- d[
                order(
                    d$time
                ),
                ,
                drop = FALSE
            ]


            d <- d[
                seq_len(max_visits),
                ,
                drop = FALSE
            ]


            d$visit_index <- seq_len(
                max_visits
            )


            return(
                d
            )
        }
    )


    dat_final <- do.call(
        rbind,
        patient_list
    )


    rownames(dat_final) <- NULL


    patients <- sort(
        unique(
            dat_final$id
        )
    )


    dat_final$patient_index <- match(
        dat_final$id,
        patients
    )


    dat_final <- dat_final[
        order(
            dat_final$patient_index,
            dat_final$visit_index
        ),
        ,
        drop = FALSE
    ]


    survival_final <- survival_data[
        match(
            patients,
            survival_data$id
        ),
        ,
        drop = FALSE
    ]


    survival_final$patient_index <- seq_len(
        nrow(survival_final)
    )


    landmark_time <- tapply(
        dat_final$time,
        dat_final$patient_index,
        max
    )


    survival_final$landmark_time <- as.numeric(
        landmark_time[
            as.character(
                survival_final$patient_index
            )
        ]
    )


    survival_final$residual_survival <-
        survival_final$survival_time -
        survival_final$landmark_time


    valid <- (
        is.finite(
            survival_final$residual_survival
        ) &
        survival_final$residual_survival >= 0
    )


    if(any(!valid)) {

        keep_ids <- survival_final$id[
            valid
        ]


        dat_final <- dat_final[
            dat_final$id %in% keep_ids,
            ,
            drop = FALSE
        ]


        survival_final <- survival_final[
            valid,
            ,
            drop = FALSE
        ]


        patients <- sort(
            unique(
                dat_final$id
            )
        )


        dat_final$patient_index <- match(
            dat_final$id,
            patients
        )


        dat_final <- dat_final[
            order(
                dat_final$patient_index,
                dat_final$visit_index
            ),
            ,
            drop = FALSE
        ]


        survival_final <- survival_final[
            match(
                patients,
                survival_final$id
            ),
            ,
            drop = FALSE
        ]


        survival_final$patient_index <- seq_len(
            nrow(survival_final)
        )
    }


    return(
        list(
            longitudinal = dat_final,
            survival = survival_final
        )
    )
}


PREP <- prepare_pbc2(

    dat = PBC,

    survival_data = SURVIVAL_DATA,

    features = FEATURES,

    max_visits = MAX_VISITS
)


PBC_LONG <- PREP$longitudinal

PATIENT_DATA <- PREP$survival


cat("\nAnalysis cohort:\n")

cat(
    "Patients : ",
    nrow(PATIENT_DATA),
    "\n",
    sep = ""
)


cat(
    "Deaths   : ",
    sum(PATIENT_DATA$event),
    "\n",
    sep = ""
)


# =============================================================================
# CREATE PATIENT x VISIT x FEATURE ARRAY
# =============================================================================

pbc2_to_array <- function(
    dat,
    features,
    max_visits
) {

    ids <- sort(
        unique(
            dat$patient_index
        )
    )


    N <- length(ids)

    P <- length(features)


    X <- array(
        NA_real_,
        dim = c(
            N,
            max_visits,
            P
        ),
        dimnames = list(
            patient =
                as.character(ids),
            visit =
                paste0(
                    "Visit_",
                    seq_len(max_visits)
                ),
            feature =
                features
        )
    )


    for(i in seq_along(ids)) {

        d_i <- dat[
            dat$patient_index == ids[i],
            ,
            drop = FALSE
        ]


        d_i <- d_i[
            order(
                d_i$visit_index
            ),
            ,
            drop = FALSE
        ]


        X[i, , ] <- as.matrix(
            d_i[
                ,
                features,
                drop = FALSE
            ]
        )
    }


    return(
        X
    )
}


X_RAW <- pbc2_to_array(

    dat = PBC_LONG,

    features = FEATURES,

    max_visits = MAX_VISITS
)


X <- X_RAW


# =============================================================================
# LOG TRANSFORMATION
# =============================================================================

for(feature_name in LOG_FEATURES) {

    j <- match(
        feature_name,
        FEATURES
    )


    X[, , j] <- log1p(
        X[, , j]
    )
}


# =============================================================================
# ARRAY -> longTAPIO FORMAT
# =============================================================================

array_to_longTAPIO <- function(X) {

    N <- dim(X)[1]

    V <- dim(X)[2]

    P <- dim(X)[3]


    DATA <- matrix(
        NA_real_,
        nrow = N * V,
        ncol = P
    )


    user_id <- rep(
        seq_len(N),
        each = V
    )


    for(i in seq_len(N)) {

        rows_i <- (
            (i - 1) * V + 1
        ):(
            i * V
        )


        DATA[rows_i, ] <- X[i, , ]
    }


    DATA <- as.data.frame(
        DATA
    )


    names(DATA) <- FEATURES


    return(
        list(
            DATA = DATA,
            user_id = user_id
        )
    )
}


# =============================================================================
# TRAIN / TEST SPLIT
# =============================================================================

make_split <- function(
    event,
    train_fraction,
    seed
) {

    set.seed(
        seed
    )


    train_idx <- integer(0)


    for(status in sort(unique(event))) {

        ids <- which(
            event == status
        )


        n_train <- floor(
            length(ids) *
            train_fraction
        )


        train_idx <- c(
            train_idx,
            sample(
                ids,
                size = n_train,
                replace = FALSE
            )
        )
    }


    train_idx <- sort(
        train_idx
    )


    test_idx <- setdiff(
        seq_along(event),
        train_idx
    )


    return(
        list(
            train = train_idx,
            test = test_idx
        )
    )
}


SPLIT <- make_split(

    event = PATIENT_DATA$event,

    train_fraction = TRAIN_FRACTION,

    seed = SEED
)


TRAIN_IDX <- SPLIT$train

TEST_IDX <- SPLIT$test


X_TRAIN <- X[
    TRAIN_IDX,
    ,
    ,
    drop = FALSE
]


X_TEST <- X[
    TEST_IDX,
    ,
    ,
    drop = FALSE
]


SURV_TRAIN <- PATIENT_DATA[
    TRAIN_IDX,
    ,
    drop = FALSE
]


SURV_TEST <- PATIENT_DATA[
    TEST_IDX,
    ,
    drop = FALSE
]


TRAIN_PREP <- array_to_longTAPIO(
    X_TRAIN
)


TEST_PREP <- array_to_longTAPIO(
    X_TEST
)


cat("\nTraining patients : ", nrow(SURV_TRAIN), "\n", sep = "")
cat("Test patients     : ", nrow(SURV_TEST), "\n", sep = "")
cat("Training deaths   : ", sum(SURV_TRAIN$event), "\n", sep = "")
cat("Test deaths       : ", sum(SURV_TEST$event), "\n", sep = "")


# =============================================================================
# FIT longTAPIO
# =============================================================================

N_FEATURES_TREE <- max(
    2,
    ceiling(
        sqrt(
            length(FEATURES)
        )
    )
)


N_FEATURES_TREE <- min(
    N_FEATURES_TREE,
    length(FEATURES)
)


set.seed(
    1002
)


MODEL <- longTAPIO_inductive(

    DATA = TRAIN_PREP$DATA,

    user_id = TRAIN_PREP$user_id,

    k = K,

    n_features = N_FEATURES_TREE,

    n_trees = N_TREES,

    levels = LEVELS,

    method = "ward.D2",

    scale = TRUE,

    replace = FALSE,

    pca_selection = PCA_SELECTION
)


TRAIN_CLUSTER <- as.integer(
    MODEL$train_clusters
)


cat("\nTraining clusters:\n")

print(
    table(
        TRAIN_CLUSTER
    )
)


# =============================================================================
# RISK LABELS - TRAINING SURVIVAL ONLY
# =============================================================================

TRAIN_SURVIVAL <- data.frame(

    time =
        SURV_TRAIN$residual_survival,

    event =
        SURV_TRAIN$event,

    cluster =
        TRAIN_CLUSTER
)


TRAIN_EVENT_RATE <- aggregate(

    event ~ cluster,

    data = TRAIN_SURVIVAL,

    FUN = mean
)


HIGH_RISK_CLUSTER <- TRAIN_EVENT_RATE$cluster[
    which.max(
        TRAIN_EVENT_RATE$event
    )
]


LOW_RISK_CLUSTER <- TRAIN_EVENT_RATE$cluster[
    which.min(
        TRAIN_EVENT_RATE$event
    )
]


risk_label <- function(cluster) {

    ifelse(
        cluster == HIGH_RISK_CLUSTER,
        "Higher-risk trajectory",
        "Lower-risk trajectory"
    )
}


# =============================================================================
# INDUCTIVE PREDICTION + FEATURE IMPORTANCE
# =============================================================================

IMP <- importance_longTAPIO_inductive(

    res = MODEL,

    newdata = TEST_PREP$DATA,

    user_id = TEST_PREP$user_id,

    normalize = TRUE
)


PREDICTED_CLUSTER <- as.matrix(
    IMP$predicted_cluster[
        ,
        HORIZONS,
        drop = FALSE
    ]
)


FINAL_CLUSTER <- as.integer(
    PREDICTED_CLUSTER[
        ,
        MAX_VISITS
    ]
)


# =============================================================================
# OBTAIN PREDICTION MARGINS DIRECTLY FROM predict()
#
# We call predict() at every prefix because the margin is an important
# inductive uncertainty/confidence quantity.
# =============================================================================

MARGIN_MATRIX <- matrix(

    NA_real_,

    nrow = length(TEST_IDX),

    ncol = MAX_VISITS,

    dimnames = list(
        NULL,
        paste0(
            "Visit_",
            HORIZONS
        )
    )
)


PREDICT_MATRIX_CHECK <- matrix(

    NA_integer_,

    nrow = length(TEST_IDX),

    ncol = MAX_VISITS
)


for(H in HORIZONS) {

    PRED_H <- predict(

        MODEL,

        newdata = TEST_PREP$DATA,

        user_id = TEST_PREP$user_id,

        visits = H
    )


    MARGIN_MATRIX[, H] <- as.numeric(
        PRED_H$margin
    )


    PREDICT_MATRIX_CHECK[, H] <- as.integer(
        PRED_H$cluster
    )
}


# =============================================================================
# VERIFY PREDICTIONS
# =============================================================================

if(
    !all(
        PREDICT_MATRIX_CHECK ==
            PREDICTED_CLUSTER
    )
) {

    warning(
        "Predictions from predict() and importance() differ."
    )
}


# =============================================================================
# LONG PATIENT-PREDICTION DATA
# =============================================================================

PREDICTION_LONG <- data.frame()


for(H in HORIZONS) {

    tmp <- data.frame(

        Patient =
            seq_along(
                FINAL_CLUSTER
            ),

        PatientID =
            SURV_TEST$id,

        Visit =
            H,

        Cluster =
            as.integer(
                PREDICTED_CLUSTER[, H]
            ),

        Final_cluster =
            FINAL_CLUSTER,

        Margin =
            MARGIN_MATRIX[, H],

        Correct_final =
            as.integer(
                PREDICTED_CLUSTER[, H] ==
                    FINAL_CLUSTER
            ),

        Risk_group =
            risk_label(
                FINAL_CLUSTER
            ),

        stringsAsFactors = FALSE
    )


    PREDICTION_LONG <- rbind(
        PREDICTION_LONG,
        tmp
    )
}


PREDICTION_LONG$Risk_group <- factor(

    PREDICTION_LONG$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# ANALYSIS 1:
# CONFIDENCE ACROSS VISITS
# =============================================================================

CONFIDENCE_SUMMARY <- aggregate(

    Margin ~ Visit,

    data = PREDICTION_LONG,

    FUN = function(x) {

        c(
            Mean = mean(
                x,
                na.rm = TRUE
            ),

            SE = sd(
                x,
                na.rm = TRUE
            ) /
                sqrt(
                    sum(
                        is.finite(x)
                    )
                )
        )
    }
)


CONFIDENCE_VISIT <- data.frame(

    Visit =
        CONFIDENCE_SUMMARY$Visit,

    Mean_margin =
        CONFIDENCE_SUMMARY$Margin[, "Mean"],

    SE =
        CONFIDENCE_SUMMARY$Margin[, "SE"]
)


cat("\n")
cat("==============================================================================================================\n")
cat("INDUCTIVE CONFIDENCE ACROSS VISITS\n")
cat("==============================================================================================================\n\n")


print(
    CONFIDENCE_VISIT,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 1:
# CONFIDENCE ACROSS VISITS
# =============================================================================

PLOT_CONFIDENCE <- ggplot(

    CONFIDENCE_VISIT,

    aes(
        x = Visit,
        y = Mean_margin
    )
) +

    geom_ribbon(

        aes(
            ymin =
                Mean_margin - SE,

            ymax =
                Mean_margin + SE
        ),

        alpha = 0.18
    ) +

    geom_line(
        linewidth = 1.3
    ) +

    geom_point(
        size = 3.5
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Inductive prediction confidence increases with longitudinal information",

        subtitle =
            "Mean cluster-assignment margin in unseen patients",

        x =
            "Number of observed visits",

        y =
            "Prediction margin"
    ) +

    theme_classic(
        base_size = 14
    )


print(
    PLOT_CONFIDENCE
)


# =============================================================================
# ANALYSIS 2:
# DOES CONFIDENCE IDENTIFY RELIABLE EARLY ASSIGNMENTS?
#
# At visits 1-4:
#
# Correct_final = 1
#     current assignment already equals eventual visit-5 assignment
#
# Correct_final = 0
#     assignment will change before visit 5
#
# =============================================================================

EARLY_PREDICTION <- PREDICTION_LONG[
    PREDICTION_LONG$Visit < MAX_VISITS,
    ,
    drop = FALSE
]


EARLY_PREDICTION$Assignment_status <- ifelse(

    EARLY_PREDICTION$Correct_final == 1,

    "Already final",

    "Changes later"
)


EARLY_PREDICTION$Assignment_status <- factor(

    EARLY_PREDICTION$Assignment_status,

    levels = c(
        "Changes later",
        "Already final"
    )
)


CONFIDENCE_CORRECTNESS <- aggregate(

    Margin ~ Visit + Assignment_status,

    data = EARLY_PREDICTION,

    FUN = mean
)


cat("\n")
cat("==============================================================================================================\n")
cat("CONFIDENCE AND EARLY ASSIGNMENT RELIABILITY\n")
cat("==============================================================================================================\n\n")


print(
    CONFIDENCE_CORRECTNESS,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 2:
# CONFIDENCE OF STABLE/CORRECT VS CHANGING ASSIGNMENTS
# =============================================================================

PLOT_CONFIDENCE_CORRECTNESS <- ggplot(

    EARLY_PREDICTION,

    aes(
        x = factor(Visit),
        y = Margin,
        fill = Assignment_status
    )
) +

    geom_boxplot(

        width = 0.68,

        outlier.shape = NA
    ) +

    geom_jitter(

        aes(
            shape = Assignment_status
        ),

        width = 0.12,

        alpha = 0.65,

        size = 2
    ) +

    labs(

        title =
            "Prediction confidence identifies uncertain early assignments",

        subtitle =
            "Assignments are classified by whether they agree with the final five-visit cluster",

        x =
            "Number of observed visits",

        y =
            "Prediction margin",

        fill =
            "Early assignment",

        shape =
            "Early assignment"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        legend.position =
            "bottom",

        plot.title =
            element_text(
                face = "bold"
            )
    )


print(
    PLOT_CONFIDENCE_CORRECTNESS
)


# =============================================================================
# ANALYSIS 3:
# PATIENT-SPECIFIC IMPORTANCE
# =============================================================================

N_TEST <- length(
    FINAL_CLUSTER
)


N_FEATURES <- length(
    FEATURES
)


# =============================================================================
# EXPLANATION CONVERGENCE
#
# For every patient and every visit:
#
# compare current feature-importance vector with visit-5 importance vector.
#
# We use cosine similarity.
#
# 1.0 = identical direction of importance profile
# 0   = unrelated importance profile
#
# =============================================================================

cosine_similarity <- function(
    x,
    y
) {

    x <- as.numeric(x)

    y <- as.numeric(y)


    good <- (
        is.finite(x) &
        is.finite(y)
    )


    x <- x[good]

    y <- y[good]


    if(length(x) == 0) {

        return(
            NA_real_
        )
    }


    nx <- sqrt(
        sum(
            x^2
        )
    )


    ny <- sqrt(
        sum(
            y^2
        )
    )


    if(
        nx == 0 ||
        ny == 0
    ) {

        return(
            NA_real_
        )
    }


    sum(
        x * y
    ) /
        (
            nx *
            ny
        )
}


EXPLANATION_CONVERGENCE <- matrix(

    NA_real_,

    nrow = N_TEST,

    ncol = MAX_VISITS
)


for(i in seq_len(N_TEST)) {

    final_importance <- IMP$patient[
        i,
        ,
        MAX_VISITS
    ]


    for(H in HORIZONS) {

        current_importance <- IMP$patient[
            i,
            ,
            H
        ]


        EXPLANATION_CONVERGENCE[i, H] <-
            cosine_similarity(
                current_importance,
                final_importance
            )
    }
}


# =============================================================================
# LONG EXPLANATION DATA
# =============================================================================

EXPLANATION_LONG <- data.frame()


for(H in HORIZONS) {

    tmp <- data.frame(

        Patient =
            seq_len(N_TEST),

        PatientID =
            SURV_TEST$id,

        Visit =
            H,

        Explanation_similarity =
            EXPLANATION_CONVERGENCE[, H],

        Margin =
            MARGIN_MATRIX[, H],

        Current_cluster =
            PREDICTED_CLUSTER[, H],

        Final_cluster =
            FINAL_CLUSTER,

        Correct_final =
            as.integer(
                PREDICTED_CLUSTER[, H] ==
                    FINAL_CLUSTER
            ),

        Risk_group =
            risk_label(
                FINAL_CLUSTER
            ),

        stringsAsFactors = FALSE
    )


    EXPLANATION_LONG <- rbind(
        EXPLANATION_LONG,
        tmp
    )
}


EXPLANATION_LONG$Risk_group <- factor(

    EXPLANATION_LONG$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# SUMMARY
# =============================================================================

EXPLANATION_SUMMARY_RAW <- aggregate(

    Explanation_similarity ~ Visit,

    data = EXPLANATION_LONG,

    FUN = function(x) {

        c(
            Mean = mean(
                x,
                na.rm = TRUE
            ),

            SE = sd(
                x,
                na.rm = TRUE
            ) /
                sqrt(
                    sum(
                        is.finite(x)
                    )
                )
        )
    }
)


EXPLANATION_SUMMARY <- data.frame(

    Visit =
        EXPLANATION_SUMMARY_RAW$Visit,

    Mean_similarity =
        EXPLANATION_SUMMARY_RAW$Explanation_similarity[, "Mean"],

    SE =
        EXPLANATION_SUMMARY_RAW$Explanation_similarity[, "SE"]
)


cat("\n")
cat("==============================================================================================================\n")
cat("EXPLANATION CONVERGENCE\n")
cat("==============================================================================================================\n\n")


print(
    EXPLANATION_SUMMARY,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 3:
# EXPLANATION CONVERGENCE
# =============================================================================

PLOT_EXPLANATION_CONVERGENCE <- ggplot(

    EXPLANATION_SUMMARY,

    aes(
        x = Visit,
        y = Mean_similarity
    )
) +

    geom_ribbon(

        aes(
            ymin =
                Mean_similarity - SE,

            ymax =
                Mean_similarity + SE
        ),

        alpha = 0.18
    ) +

    geom_line(
        linewidth = 1.3
    ) +

    geom_point(
        size = 3.5
    ) +

    geom_text(

        aes(
            label = sprintf(
                "%.2f",
                Mean_similarity
            )
        ),

        vjust = -1,

        size = 4
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    scale_y_continuous(

        limits = c(
            0,
            1.08
        ),

        breaks = seq(
            0,
            1,
            0.2
        )
    ) +

    labs(

        title =
            "Patient-specific explanations converge as observations accumulate",

        subtitle =
            "Cosine similarity with each patient's final five-visit feature-importance profile",

        x =
            "Number of observed visits",

        y =
            "Explanation similarity"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            )
    )


print(
    PLOT_EXPLANATION_CONVERGENCE
)


# =============================================================================
# ANALYSIS 4:
# CONFIDENCE VERSUS EXPLANATION CONVERGENCE
#
# If useful, patients with confident assignments should often also have
# relatively mature/stable explanations.
#
# Exclude visit 5 because similarity is exactly 1 by construction.
# =============================================================================

CONF_EXPL <- EXPLANATION_LONG[
    EXPLANATION_LONG$Visit < MAX_VISITS,
    ,
    drop = FALSE
]


CONF_EXPL <- CONF_EXPL[
    is.finite(CONF_EXPL$Margin) &
    is.finite(CONF_EXPL$Explanation_similarity),
    ,
    drop = FALSE
]


COR_CONF_EXPL <- cor(

    CONF_EXPL$Margin,

    CONF_EXPL$Explanation_similarity,

    method = "spearman"
)


cat("\n")
cat("==============================================================================================================\n")
cat("CONFIDENCE VS EXPLANATION CONVERGENCE\n")
cat("==============================================================================================================\n\n")


cat(
    sprintf(
        "Spearman rho = %.3f\n",
        COR_CONF_EXPL
    )
)


# =============================================================================
# FIGURE 4:
# CONFIDENCE VS EXPLANATION CONVERGENCE
# =============================================================================

PLOT_CONF_EXPL <- ggplot(

    CONF_EXPL,

    aes(
        x = Margin,
        y = Explanation_similarity
    )
) +

    geom_point(

        aes(
            shape = factor(Visit)
        ),

        size = 2.7,

        alpha = 0.75
    ) +

    geom_smooth(

        method = "lm",

        se = TRUE,

        linewidth = 1
    ) +

    labs(

        title =
            "Prediction confidence and explanation maturity",

        subtitle =
            paste0(
                "Visits 1-4; Spearman rho = ",
                sprintf(
                    "%.2f",
                    COR_CONF_EXPL
                )
            ),

        x =
            "Prediction margin",

        y =
            "Similarity with final explanation",

        shape =
            "Visit"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            ),

        legend.position =
            "bottom"
    )


print(
    PLOT_CONF_EXPL
)


# =============================================================================
# ANALYSIS 5:
# TIME TO STABLE ASSIGNMENT
# =============================================================================

STABLE_VISIT <- integer(
    N_TEST
)


for(i in seq_len(N_TEST)) {

    final_i <- FINAL_CLUSTER[i]


    stable_i <- MAX_VISITS


    for(H in HORIZONS) {

        if(
            all(
                PREDICTED_CLUSTER[
                    i,
                    H:MAX_VISITS
                ] == final_i
            )
        ) {

            stable_i <- H

            break
        }
    }


    STABLE_VISIT[i] <- stable_i
}


PATIENT_SUMMARY <- data.frame(

    Patient =
        seq_len(N_TEST),

    PatientID =
        SURV_TEST$id,

    Final_cluster =
        FINAL_CLUSTER,

    Risk_group =
        risk_label(
            FINAL_CLUSTER
        ),

    Stable_from_visit =
        STABLE_VISIT,

    Final_margin =
        MARGIN_MATRIX[
            ,
            MAX_VISITS
        ],

    Death =
        SURV_TEST$event,

    stringsAsFactors = FALSE
)


PATIENT_SUMMARY$Risk_group <- factor(

    PATIENT_SUMMARY$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


cat("\n")
cat("==============================================================================================================\n")
cat("TIME TO STABLE ASSIGNMENT\n")
cat("==============================================================================================================\n\n")


print(
    table(
        PATIENT_SUMMARY$Stable_from_visit
    )
)


# =============================================================================
# SELECT REPRESENTATIVE PATIENTS
#
# Patient A:
# earliest stable patient with high final confidence
#
# Patient B:
# patient whose assignment changes relatively late
#
# =============================================================================

EARLY_STABLE <- which(
    STABLE_VISIT == min(
        STABLE_VISIT
    )
)


EXAMPLE_STABLE <- EARLY_STABLE[
    which.max(
        MARGIN_MATRIX[
            EARLY_STABLE,
            MAX_VISITS
        ]
    )
]


LATE_STABLE <- which(
    STABLE_VISIT == max(
        STABLE_VISIT
    )
)


EXAMPLE_SWITCHING <- LATE_STABLE[
    which.max(
        MARGIN_MATRIX[
            LATE_STABLE,
            MAX_VISITS
        ]
    )
]


EXAMPLE_PATIENTS <- unique(
    c(
        EXAMPLE_STABLE,
        EXAMPLE_SWITCHING
    )
)


EXAMPLE_LABELS <- rep(
    NA_character_,
    N_TEST
)


EXAMPLE_LABELS[
    EXAMPLE_STABLE
] <- "Early-stable patient"


EXAMPLE_LABELS[
    EXAMPLE_SWITCHING
] <- "Late-switching patient"


cat("\nRepresentative patients:\n")

cat(
    "Early-stable patient ID : ",
    SURV_TEST$id[
        EXAMPLE_STABLE
    ],
    "\n",
    sep = ""
)


cat(
    "Late-switching patient ID: ",
    SURV_TEST$id[
        EXAMPLE_SWITCHING
    ],
    "\n",
    sep = ""
)


# =============================================================================
# PATIENT-SPECIFIC IMPORTANCE DATA
# =============================================================================

EXAMPLE_IMPORTANCE <- data.frame()


for(i in EXAMPLE_PATIENTS) {

    for(H in HORIZONS) {

        for(j in seq_along(FEATURES)) {

            tmp <- data.frame(

                Patient =
                    i,

                PatientID =
                    SURV_TEST$id[i],

                Patient_type =
                    EXAMPLE_LABELS[i],

                Visit =
                    H,

                Feature =
                    FEATURES[j],

                Importance =
                    as.numeric(
                        IMP$patient[
                            i,
                            j,
                            H
                        ]
                    ),

                Cluster =
                    PREDICTED_CLUSTER[
                        i,
                        H
                    ],

                Margin =
                    MARGIN_MATRIX[
                        i,
                        H
                    ],

                stringsAsFactors = FALSE
            )


            EXAMPLE_IMPORTANCE <- rbind(
                EXAMPLE_IMPORTANCE,
                tmp
            )
        }
    }
}


EXAMPLE_IMPORTANCE$Feature <- factor(

    EXAMPLE_IMPORTANCE$Feature,

    levels = FEATURES
)


# =============================================================================
# FIGURE 5:
# PATIENT-SPECIFIC EXPLANATION EVOLUTION
# =============================================================================

PLOT_PATIENT_IMPORTANCE <- ggplot(

    EXAMPLE_IMPORTANCE,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.1
    ) +

    geom_point(
        size = 2.3
    ) +

    facet_wrap(

        ~ Patient_type,

        ncol = 1
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Patient-specific explanations evolve with longitudinal evidence",

        subtitle =
            "Example unseen patients with early-stable and late-changing cluster assignments",

        x =
            "Number of observed visits",

        y =
            "Normalized feature importance",

        colour =
            "Biomarker"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        strip.text =
            element_text(
                face = "bold"
            ),

        legend.position =
            "right"
    )


print(
    PLOT_PATIENT_IMPORTANCE
)


# =============================================================================
# FIGURE 6:
# PATIENT-SPECIFIC PREDICTION TRAJECTORY
# =============================================================================

EXAMPLE_PREDICTIONS <- PREDICTION_LONG[
    PREDICTION_LONG$Patient %in% EXAMPLE_PATIENTS,
    ,
    drop = FALSE
]


EXAMPLE_PREDICTIONS$Patient_type <- EXAMPLE_LABELS[
    EXAMPLE_PREDICTIONS$Patient
]


EXAMPLE_PREDICTIONS$Cluster_label <- paste0(
    "Cluster ",
    EXAMPLE_PREDICTIONS$Cluster
)


PLOT_PATIENT_CONFIDENCE <- ggplot(

    EXAMPLE_PREDICTIONS,

    aes(
        x = Visit,
        y = Margin,
        group = 1
    )
) +

    geom_line(
        linewidth = 1.2
    ) +

    geom_point(

        aes(
            shape = Cluster_label
        ),

        size = 4
    ) +

    geom_text(

        aes(
            label = paste0(
                "C",
                Cluster
            )
        ),

        vjust = -1,

        size = 4
    ) +

    facet_wrap(

        ~ Patient_type,

        ncol = 1,

        scales = "free_y"
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Inductive patient trajectories reveal assignment uncertainty",

        subtitle =
            "Cluster identity and prediction margin as new visits become available",

        x =
            "Number of observed visits",

        y =
            "Prediction margin",

        shape =
            "Assigned cluster"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        strip.text =
            element_text(
                face = "bold"
            ),

        legend.position =
            "bottom"
    )


print(
    PLOT_PATIENT_CONFIDENCE
)


# =============================================================================
# ANALYSIS 6:
# EXPLANATION CONVERGENCE FOR CORRECT VS CHANGING ASSIGNMENTS
#
# This asks whether an assignment that will eventually change also has
# a less mature feature-importance explanation.
# =============================================================================

EXPLANATION_EARLY <- EXPLANATION_LONG[
    EXPLANATION_LONG$Visit < MAX_VISITS,
    ,
    drop = FALSE
]


EXPLANATION_EARLY$Assignment_status <- ifelse(

    EXPLANATION_EARLY$Correct_final == 1,

    "Already final",

    "Changes later"
)


EXPLANATION_EARLY$Assignment_status <- factor(

    EXPLANATION_EARLY$Assignment_status,

    levels = c(
        "Changes later",
        "Already final"
    )
)


EXPLANATION_BY_STATUS <- aggregate(

    Explanation_similarity ~ Visit + Assignment_status,

    data = EXPLANATION_EARLY,

    FUN = mean
)


cat("\n")
cat("==============================================================================================================\n")
cat("EXPLANATION MATURITY AND ASSIGNMENT STATUS\n")
cat("==============================================================================================================\n\n")


print(
    EXPLANATION_BY_STATUS,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 7:
# EXPLANATION MATURITY: CORRECT VS CHANGING
# =============================================================================

PLOT_EXPLANATION_STATUS <- ggplot(

    EXPLANATION_EARLY,

    aes(
        x = factor(Visit),
        y = Explanation_similarity,
        fill = Assignment_status
    )
) +

    geom_boxplot(

        width = 0.68,

        outlier.shape = NA
    ) +

    geom_jitter(

        width = 0.12,

        alpha = 0.55,

        size = 1.8
    ) +

    labs(

        title =
            "Stable early assignments have more mature explanations",

        subtitle =
            "Similarity to each patient's final five-visit feature-importance profile",

        x =
            "Number of observed visits",

        y =
            "Explanation similarity",

        fill =
            "Early assignment"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold"
            ),

        legend.position =
            "bottom"
    )


print(
    PLOT_EXPLANATION_STATUS
)


# =============================================================================
# SAVE TABLES
# =============================================================================

write.csv(

    PREDICTION_LONG,

    "PBC2_Inductive_Prediction_History.csv",

    row.names = FALSE
)


write.csv(

    CONFIDENCE_VISIT,

    "PBC2_Inductive_Confidence_By_Visit.csv",

    row.names = FALSE
)


write.csv(

    EXPLANATION_LONG,

    "PBC2_Explanation_Convergence_Patient.csv",

    row.names = FALSE
)


write.csv(

    EXPLANATION_SUMMARY,

    "PBC2_Explanation_Convergence_Summary.csv",

    row.names = FALSE
)


write.csv(

    PATIENT_SUMMARY,

    "PBC2_Inductive_Patient_Summary.csv",

    row.names = FALSE
)


write.csv(

    EXAMPLE_IMPORTANCE,

    "PBC2_Representative_Patient_Importance.csv",

    row.names = FALSE
)


# =============================================================================
# SAVE FIGURES
# =============================================================================

ggsave(

    "PBC2_Inductive_Figure1_Confidence_By_Visit.pdf",

    PLOT_CONFIDENCE,

    width = 7,

    height = 5
)


ggsave(

    "PBC2_Inductive_Figure2_Confidence_Reliability.pdf",

    PLOT_CONFIDENCE_CORRECTNESS,

    width = 7.5,

    height = 5.5
)


ggsave(

    "PBC2_Inductive_Figure3_Explanation_Convergence.pdf",

    PLOT_EXPLANATION_CONVERGENCE,

    width = 7,

    height = 5
)


ggsave(

    "PBC2_Inductive_Figure4_Confidence_Explanation.pdf",

    PLOT_CONF_EXPL,

    width = 7,

    height = 5.5
)


ggsave(

    "PBC2_Inductive_Figure5_Patient_Importance.pdf",

    PLOT_PATIENT_IMPORTANCE,

    width = 9,

    height = 7
)


ggsave(

    "PBC2_Inductive_Figure6_Patient_Confidence.pdf",

    PLOT_PATIENT_CONFIDENCE,

    width = 8,

    height = 7
)


ggsave(

    "PBC2_Inductive_Figure7_Explanation_Status.pdf",

    PLOT_EXPLANATION_STATUS,

    width = 7.5,

    height = 5.5
)


# =============================================================================
# FINAL CONSOLE SUMMARY
# =============================================================================

cat("\n\n")
cat("==============================================================================================================\n")
cat("INDUCTIVE longTAPIO SUMMARY\n")
cat("==============================================================================================================\n\n")


cat("FINAL TEST CLUSTERS\n\n")

print(
    table(
        FINAL_CLUSTER
    )
)


cat("\nMEAN PREDICTION MARGIN BY VISIT\n\n")

print(
    CONFIDENCE_VISIT,
    digits = 3,
    row.names = FALSE
)


cat("\nMEAN EXPLANATION CONVERGENCE BY VISIT\n\n")

print(
    EXPLANATION_SUMMARY,
    digits = 3,
    row.names = FALSE
)


cat("\nCONFIDENCE VS EXPLANATION CONVERGENCE\n\n")

cat(
    sprintf(
        "Spearman rho = %.3f\n",
        COR_CONF_EXPL
    )
)


cat("\nTIME TO STABLE ASSIGNMENT\n\n")

print(
    table(
        PATIENT_SUMMARY$Stable_from_visit
    )
)


cat("\nREPRESENTATIVE PATIENTS\n\n")

cat(
    "Early-stable patient ID  : ",
    SURV_TEST$id[
        EXAMPLE_STABLE
    ],
    "\n",
    sep = ""
)


cat(
    "Late-switching patient ID : ",
    SURV_TEST$id[
        EXAMPLE_SWITCHING
    ],
    "\n",
    sep = ""
)


cat("\n")
cat("==============================================================================================================\n")
cat("DONE\n")
cat("==============================================================================================================\n")
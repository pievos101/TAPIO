# =============================================================================
# PBC2 - K = 2 INDUCTIVE longTAPIO
#
# COMPLETE FINAL ANALYSIS
#
# FIGURE 1:
#   Progressive assignment ARI across visits
#
# FIGURE 2:
#   Kaplan-Meier curves of TRAINING clusters using survminer
#
# FIGURE 3:
#   Overall feature-importance evolution
#
# FIGURE 4:
#   Cluster-specific feature-importance evolution
#
# FIGURE 5:
#   Actual biomarker trajectories by cluster
#   -> shows DIRECTION: higher/lower biomarker values in each cluster
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(DynForest)
library(survival)
library(survminer)
library(ggplot2)
library(aricode)


# =============================================================================
# 1. CHECK longTAPIO FUNCTIONS
# =============================================================================

if(!exists("longTAPIO_inductive")) {
    stop("Please source longTAPIO_inductive() first.")
}


if(!exists("importance_longTAPIO_inductive")) {
    stop("Please source importance_longTAPIO_inductive() first.")
}


# =============================================================================
# 2. SETTINGS
# =============================================================================

SEED <- 42

K <- 2

TRAIN_FRACTION <- 0.70

MAX_VISITS <- 5

HORIZONS <- 1:MAX_VISITS

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
# 3. LOAD PBC2
# =============================================================================

data(
    pbc2,
    package = "DynForest"
)


PBC <- pbc2


cat("\n")
cat("==============================================================================================================\n")
cat("PBC2 - K=2 INDUCTIVE longTAPIO\n")
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


cat("\nOriginal event coding:\n")

print(
    table(PBC$event)
)


# =============================================================================
# 4. DEATH ENDPOINT
# =============================================================================
#
# PBC2:
#
# 0 = censored
# 1 = transplantation
# 2 = death
#
# We perform death-specific survival analysis.
# Transplantation is treated as censoring.
#
# =============================================================================

PBC$death_event <- ifelse(
    PBC$event == 2,
    1L,
    0L
)


# =============================================================================
# 5. PATIENT-LEVEL SURVIVAL DATA
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
            stop("Non-unique survival time.")
        }


        if(length(death_values) != 1) {
            stop("Non-unique death endpoint.")
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
# 6. PREPARE LONGITUDINAL DATA
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
                order(d$time),
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
# 7. CREATE PATIENT x VISIT x FEATURE ARRAY
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


# =============================================================================
# IMPORTANT:
#
# X_RAW contains ORIGINAL biomarker values.
#
# We retain this because Figure 5 should show actual biomarker direction,
# not transformed/scaled values.
#
# =============================================================================


X <- X_RAW


# =============================================================================
# 8. LOG TRANSFORM MODEL INPUT
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
# 9. ARRAY -> longTAPIO FORMAT
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
# 10. TRAIN / TEST SPLIT
# =============================================================================

make_split <- function(
    event,
    train_fraction,
    seed
) {

    set.seed(seed)


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


X_RAW_TRAIN <- X_RAW[
    TRAIN_IDX,
    ,
    ,
    drop = FALSE
]


X_RAW_TEST <- X_RAW[
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


cat("\n")

cat(
    "Training patients : ",
    nrow(SURV_TRAIN),
    "\n",
    sep = ""
)

cat(
    "Test patients     : ",
    nrow(SURV_TEST),
    "\n",
    sep = ""
)

cat(
    "Training deaths   : ",
    sum(SURV_TRAIN$event),
    "\n",
    sep = ""
)

cat(
    "Test deaths       : ",
    sum(SURV_TEST$event),
    "\n",
    sep = ""
)


# =============================================================================
# 11. FIT longTAPIO
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


cat("\nTraining cluster sizes:\n")

print(
    table(
        TRAIN_CLUSTER
    )
)


# =============================================================================
# 12. DEFINE RISK LABELS FROM TRAINING SURVIVAL
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


TRAIN_SURVIVAL$risk_group <- factor(

    risk_label(
        TRAIN_CLUSTER
    ),

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# 13. KAPLAN-MEIER
# =============================================================================

KM_TRAIN <- survival::survfit(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TRAIN_SURVIVAL
)


LOGRANK_TRAIN <- survival::survdiff(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TRAIN_SURVIVAL
)


TRAIN_CHISQ <- as.numeric(
    LOGRANK_TRAIN$chisq
)


TRAIN_P <- pchisq(

    TRAIN_CHISQ,

    df = 1,

    lower.tail = FALSE
)


# =============================================================================
# FIGURE 1
# KAPLAN-MEIER
# =============================================================================

PLOT_KM <- survminer::ggsurvplot(

    fit =
        KM_TRAIN,

    data =
        TRAIN_SURVIVAL,

    pval =
        TRUE,

    pval.method =
        TRUE,

    conf.int =
        TRUE,

    risk.table =
        TRUE,

    risk.table.height =
        0.25,

    risk.table.y.text =
        FALSE,

    risk.table.col =
        "strata",

    censor =
        TRUE,

    censor.shape =
        124,

    censor.size =
        3,

    xlab =
        "Years after fifth-visit landmark",

    ylab =
        "Survival probability",

    break.time.by =
        1,

    surv.scale =
        "percent",

    legend.title =
        "Trajectory cluster",

    legend.labs =
        c(
            "Lower-risk trajectory",
            "Higher-risk trajectory"
        ),

    legend =
        "bottom",

    size =
        1.2,

    ggtheme =
        theme_classic(
            base_size = 14
        ),

    title =
        "Survival of training-derived trajectory clusters"
)


print(
    PLOT_KM
)


# =============================================================================
# 14. PROGRESSIVE TEST ASSIGNMENT + IMPORTANCE
# =============================================================================

IMP <- importance_longTAPIO_inductive(

    res = MODEL,

    newdata = TEST_PREP$DATA,

    user_id = TEST_PREP$user_id,

    normalize = TRUE
)


FINAL_CLUSTER <- as.integer(
    IMP$predicted_cluster[, MAX_VISITS]
)


# =============================================================================
# 15. PROGRESSIVE ARI
# =============================================================================

ARI_VALUES <- numeric(
    MAX_VISITS
)


NMI_VALUES <- numeric(
    MAX_VISITS
)


AGREEMENT_VALUES <- numeric(
    MAX_VISITS
)


for(H in HORIZONS) {

    PRED_H <- as.integer(
        IMP$predicted_cluster[, H]
    )


    ARI_VALUES[H] <- aricode::ARI(
        FINAL_CLUSTER,
        PRED_H
    )


    NMI_VALUES[H] <- aricode::NMI(
        FINAL_CLUSTER,
        PRED_H
    )


    AGREEMENT_VALUES[H] <- mean(
        FINAL_CLUSTER ==
            PRED_H
    )
}


PERFORMANCE <- data.frame(

    Visit =
        HORIZONS,

    ARI =
        ARI_VALUES,

    NMI =
        NMI_VALUES,

    Agreement =
        AGREEMENT_VALUES
)


# =============================================================================
# FIGURE 2
# PROGRESSIVE ARI
# =============================================================================

PLOT_ARI <- ggplot(

    PERFORMANCE,

    aes(
        x = Visit,
        y = ARI
    )
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
                ARI
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
        )
    ) +

    labs(

        title =
            "Progressive trajectory-cluster assignment",

        subtitle =
            "Agreement with final five-visit assignment",

        x =
            "Number of observed visits",

        y =
            "Adjusted Rand Index"
    ) +

    theme_classic(
        base_size = 14
    )


print(
    PLOT_ARI
)


# =============================================================================
# 16. PATIENT-LEVEL FEATURE IMPORTANCE
# =============================================================================

IMPORTANCE_LIST <- list()

counter <- 1L


for(H in HORIZONS) {

    for(j in seq_along(FEATURES)) {

        IMPORTANCE_LIST[[counter]] <- data.frame(

            Patient =
                seq_along(
                    FINAL_CLUSTER
                ),

            Visit =
                H,

            Feature =
                FEATURES[j],

            Importance =
                as.numeric(
                    IMP$patient[, j, H]
                ),

            Final_cluster =
                FINAL_CLUSTER,

            Risk_group =
                risk_label(
                    FINAL_CLUSTER
                ),

            stringsAsFactors = FALSE
        )


        counter <- counter + 1L
    }
}


IMPORTANCE_PATIENT <- do.call(
    rbind,
    IMPORTANCE_LIST
)


IMPORTANCE_PATIENT$Risk_group <- factor(

    IMPORTANCE_PATIENT$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# 17. OVERALL IMPORTANCE
# =============================================================================

OVERALL_IMPORTANCE <- aggregate(

    Importance ~ Visit + Feature,

    data = IMPORTANCE_PATIENT,

    FUN = mean
)


# =============================================================================
# FIGURE 3
# OVERALL IMPORTANCE EVOLUTION
# =============================================================================

PLOT_IMPORTANCE <- ggplot(

    OVERALL_IMPORTANCE,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.15
    ) +

    geom_point(
        size = 2.7
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Evolution of biomarker importance",

        x =
            "Number of observed visits",

        y =
            "Normalized importance",

        colour =
            "Biomarker"
    ) +

    theme_classic(
        base_size = 14
    )


print(
    PLOT_IMPORTANCE
)


# =============================================================================
# 18. CLUSTER-SPECIFIC IMPORTANCE
# =============================================================================

CLUSTER_IMPORTANCE <- aggregate(

    Importance ~ Visit + Feature + Final_cluster + Risk_group,

    data = IMPORTANCE_PATIENT,

    FUN = mean
)


# =============================================================================
# FIGURE 4
# CLUSTER-SPECIFIC FEATURE IMPORTANCE
# =============================================================================

PLOT_CLUSTER_IMPORTANCE <- ggplot(

    CLUSTER_IMPORTANCE,

    aes(
        x = Visit,
        y = Importance,
        colour = Feature,
        group = Feature
    )
) +

    geom_line(
        linewidth = 1.15
    ) +

    geom_point(
        size = 2.5
    ) +

    facet_wrap(

        ~ Risk_group,

        ncol = 1
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Cluster-specific evolution of biomarker importance",

        subtitle =
            "Patients grouped by final five-visit assignment",

        x =
            "Number of observed visits",

        y =
            "Normalized feature importance",

        colour =
            "Biomarker"
    ) +

    theme_classic(
        base_size = 14
    ) +

    theme(

        strip.text =
            element_text(
                face = "bold"
            )
    )


print(
    PLOT_CLUSTER_IMPORTANCE
)


# =============================================================================
# 19. NEW ANALYSIS
#
# ACTUAL BIOMARKER VALUES BY CLUSTER
#
# This answers:
#
# "Which biomarkers are HIGHER or LOWER in each cluster?"
#
# IMPORTANT:
#
# We use RAW biomarker values here, not importance scores.
#
# To make biomarkers comparable in ONE figure, each biomarker is
# standardized using TRAINING-set mean and SD.
#
# Thus:
#
#   z > 0 = above training population mean
#   z < 0 = below training population mean
#
# =============================================================================


# =============================================================================
# 20. TRAINING-BASED STANDARDIZATION
# =============================================================================

P <- length(
    FEATURES
)


TRAIN_MEAN <- numeric(
    P
)


TRAIN_SD <- numeric(
    P
)


names(TRAIN_MEAN) <- FEATURES

names(TRAIN_SD) <- FEATURES


for(j in seq_len(P)) {

    TRAIN_MEAN[j] <- mean(
        X_RAW_TRAIN[, , j],
        na.rm = TRUE
    )


    TRAIN_SD[j] <- sd(
        X_RAW_TRAIN[, , j],
        na.rm = TRUE
    )


    if(
        !is.finite(TRAIN_SD[j]) ||
        TRAIN_SD[j] == 0
    ) {

        TRAIN_SD[j] <- 1
    }
}


# =============================================================================
# 21. STANDARDIZE RAW TEST BIOMARKERS
# =============================================================================

X_Z_TEST <- X_RAW_TEST


for(j in seq_len(P)) {

    X_Z_TEST[, , j] <- (
        X_RAW_TEST[, , j] -
        TRAIN_MEAN[j]
    ) /
        TRAIN_SD[j]
}


# =============================================================================
# 22. LONG DATA FRAME OF STANDARDIZED BIOMARKER VALUES
# =============================================================================

BIOMARKER_LONG_LIST <- list()

counter <- 1L


for(i in seq_len(dim(X_Z_TEST)[1])) {

    for(H in HORIZONS) {

        for(j in seq_along(FEATURES)) {

            BIOMARKER_LONG_LIST[[counter]] <- data.frame(

                Patient =
                    i,

                PatientID =
                    SURV_TEST$id[i],

                Visit =
                    H,

                Feature =
                    FEATURES[j],

                Raw_value =
                    X_RAW_TEST[i, H, j],

                Z_value =
                    X_Z_TEST[i, H, j],

                Final_cluster =
                    FINAL_CLUSTER[i],

                Risk_group =
                    risk_label(
                        FINAL_CLUSTER[i]
                    ),

                stringsAsFactors = FALSE
            )


            counter <- counter + 1L
        }
    }
}


BIOMARKER_LONG <- do.call(
    rbind,
    BIOMARKER_LONG_LIST
)


BIOMARKER_LONG$Feature <- factor(

    BIOMARKER_LONG$Feature,

    levels = FEATURES
)


BIOMARKER_LONG$Risk_group <- factor(

    BIOMARKER_LONG$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


# =============================================================================
# 23. MEAN BIOMARKER TRAJECTORY BY CLUSTER
# =============================================================================

BIOMARKER_CLUSTER_MEAN <- aggregate(

    Z_value ~ Visit + Feature + Final_cluster + Risk_group,

    data = BIOMARKER_LONG,

    FUN = mean
)


BIOMARKER_CLUSTER_MEAN$Feature <- factor(

    BIOMARKER_CLUSTER_MEAN$Feature,

    levels = FEATURES
)


BIOMARKER_CLUSTER_MEAN$Risk_group <- factor(

    BIOMARKER_CLUSTER_MEAN$Risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


cat("\n")
cat("==============================================================================================================\n")
cat("STANDARDIZED BIOMARKER VALUES BY CLUSTER\n")
cat("==============================================================================================================\n\n")


print(
    BIOMARKER_CLUSTER_MEAN,
    digits = 3,
    row.names = FALSE
)


# =============================================================================
# FIGURE 5
#
# THE NEW IMPORTANT FIGURE:
#
# ACTUAL BIOMARKER TRAJECTORIES BY CLUSTER
#
# Each panel = one biomarker
#
# Two lines:
#
#   lower-risk trajectory
#   higher-risk trajectory
#
# y-axis = standardized biomarker value
#
# 0 = overall training mean
#
# =============================================================================

PLOT_BIOMARKER_TRAJECTORIES <- ggplot(

    BIOMARKER_CLUSTER_MEAN,

    aes(
        x = Visit,
        y = Z_value,
        colour = Risk_group,
        group = Risk_group
    )
) +

    geom_hline(

        yintercept = 0,

        linetype = "dashed",

        linewidth = 0.5
    ) +

    geom_line(
        linewidth = 1.25
    ) +

    geom_point(
        size = 2.8
    ) +

    facet_wrap(

        ~ Feature,

        ncol = 3#,

        #scales = "free_y"
    ) +

    scale_x_continuous(
        breaks = HORIZONS
    ) +

    labs(

        title =
            "Biomarker trajectories characterizing the two clusters",

        subtitle =
            "Values standardized relative to the training population",

        x =
            "Visit",

        y =
            "Standardized biomarker value (z-score)",

        colour =
            "Trajectory cluster"
    ) +

    theme_classic(
        base_size = 13
    ) +

    theme(

        plot.title =
            element_text(
                face = "bold",
                size = 16
            ),

        plot.subtitle =
            element_text(
                size = 11
            ),

        strip.text =
            element_text(
                face = "bold",
                size = 11
            ),

        legend.position =
            "bottom",

        legend.title =
            element_text(
                face = "bold"
            )
    )


print(
    PLOT_BIOMARKER_TRAJECTORIES
)


# =============================================================================
# 24. OPTIONAL:
#
# FINAL VISIT BIOMARKER PROFILE
#
# This is useful because it gives a compact direct comparison of the
# two clusters at visit 5.
#
# =============================================================================

FINAL_PROFILE <- BIOMARKER_CLUSTER_MEAN[
    BIOMARKER_CLUSTER_MEAN$Visit == MAX_VISITS,
    ,
    drop = FALSE
]


PLOT_FINAL_PROFILE <- ggplot(

    FINAL_PROFILE,

    aes(
        x = Feature,
        y = Z_value,
        fill = Risk_group
    )
) +

    geom_col(

        position =
            position_dodge(
                width = 0.8
            ),

        width =
            0.7
    ) +

    geom_hline(

        yintercept = 0,

        linetype = "dashed"
    ) +

    coord_flip() +

    labs(

        title =
            "Biomarker profile after five visits",

        subtitle =
            "Standardized biomarker levels in the two trajectory clusters",

        x =
            NULL,

        y =
            "Standardized biomarker value (z-score)",

        fill =
            "Trajectory cluster"
    ) +

    theme_classic(
        base_size = 13
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
    PLOT_FINAL_PROFILE
)


# =============================================================================
# 25. SAVE TABLES
# =============================================================================

write.csv(

    PERFORMANCE,

    "PBC2_K2_progressive_ARI.csv",

    row.names = FALSE
)


write.csv(

    OVERALL_IMPORTANCE,

    "PBC2_K2_overall_importance.csv",

    row.names = FALSE
)


write.csv(

    CLUSTER_IMPORTANCE,

    "PBC2_K2_cluster_specific_importance.csv",

    row.names = FALSE
)


write.csv(

    BIOMARKER_CLUSTER_MEAN,

    "PBC2_K2_cluster_biomarker_trajectories.csv",

    row.names = FALSE
)


write.csv(

    FINAL_PROFILE,

    "PBC2_K2_final_biomarker_profile.csv",

    row.names = FALSE
)


# =============================================================================
# 26. SAVE FIGURES
# =============================================================================


# -----------------------------------------------------------------------------
# KM including risk table
# -----------------------------------------------------------------------------

pdf(
    "PBC2_Figure1_KaplanMeier.pdf",
    width = 7.5,
    height = 7
)

print(
    PLOT_KM
)

dev.off()


# -----------------------------------------------------------------------------
# ARI
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure2_Progressive_ARI.pdf",

    PLOT_ARI,

    width = 6.5,

    height = 5
)


# -----------------------------------------------------------------------------
# Overall importance
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure3_Overall_Importance.pdf",

    PLOT_IMPORTANCE,

    width = 8,

    height = 5.5
)


# -----------------------------------------------------------------------------
# Cluster-specific importance
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure4_ClusterSpecific_Importance.pdf",

    PLOT_CLUSTER_IMPORTANCE,

    width = 8,

    height = 7
)


# -----------------------------------------------------------------------------
# Biomarker trajectories
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Figure5_Biomarker_Trajectories.pdf",

    PLOT_BIOMARKER_TRAJECTORIES,

    width = 10,

    height = 8
)


# -----------------------------------------------------------------------------
# Final biomarker profile
# -----------------------------------------------------------------------------

ggsave(

    "PBC2_Supplement_Final_Biomarker_Profile.pdf",

    PLOT_FINAL_PROFILE,

    width = 8,

    height = 5
)


# =============================================================================
# 27. FINAL SUMMARY
# =============================================================================

cat("\n\n")
cat("==============================================================================================================\n")
cat("FINAL PBC2 RESULTS\n")
cat("==============================================================================================================\n\n")


cat("TRAINING CLUSTERS\n\n")

print(
    table(
        TRAIN_CLUSTER
    )
)


cat("\nTRAINING SURVIVAL\n")

cat(
    sprintf(
        "Log-rank chi-square = %.3f\n",
        TRAIN_CHISQ
    )
)

cat(
    sprintf(
        "p = %.8g\n",
        TRAIN_P
    )
)


cat("\nPROGRESSIVE ASSIGNMENT\n\n")

print(
    PERFORMANCE,
    digits = 3,
    row.names = FALSE
)


cat("\nFINAL TEST CLUSTERS\n\n")

print(
    table(
        FINAL_CLUSTER
    )
)


cat("\nFINAL BIOMARKER PROFILE\n\n")

print(
    FINAL_PROFILE,
    digits = 3,
    row.names = FALSE
)


cat("\n")
cat("==============================================================================================================\n")
cat("DONE\n")
cat("==============================================================================================================\n")
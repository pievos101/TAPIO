# =============================================================================
# NICE KAPLAN-MEIER PLOT
# TRAINING DATA - BOTH K=2 CLUSTERS
# =============================================================================

library(survival)
library(survminer)


# -----------------------------------------------------------------------------
# Training survival data
# -----------------------------------------------------------------------------

TRAIN_SURVIVAL <- data.frame(

    time =
        SURV_TRAIN$residual_survival,

    event =
        SURV_TRAIN$event,

    cluster =
        TRAIN_CLUSTER
)


# -----------------------------------------------------------------------------
# Give arbitrary cluster IDs meaningful descriptive labels
#
# IMPORTANT:
# Risk labels are derived AFTER unsupervised clustering.
# They do not influence longTAPIO fitting.
# -----------------------------------------------------------------------------

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


TRAIN_SURVIVAL$risk_group <- ifelse(

    TRAIN_SURVIVAL$cluster == HIGH_RISK_CLUSTER,

    "Higher-risk trajectory",

    "Lower-risk trajectory"
)


TRAIN_SURVIVAL$risk_group <- factor(

    TRAIN_SURVIVAL$risk_group,

    levels = c(
        "Lower-risk trajectory",
        "Higher-risk trajectory"
    )
)


cat("\nTraining survival groups:\n")

print(
    table(
        TRAIN_SURVIVAL$risk_group
    )
)


cat("\nDeaths by survival group:\n")

print(
    with(
        TRAIN_SURVIVAL,
        table(
            risk_group,
            event
        )
    )
)


# =============================================================================
# KAPLAN-MEIER MODEL
# =============================================================================

KM_TRAIN <- survival::survfit(

    survival::Surv(
        time,
        event
    ) ~ risk_group,

    data = TRAIN_SURVIVAL
)


print(
    KM_TRAIN
)


# =============================================================================
# LOG-RANK TEST
# =============================================================================

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


TRAIN_DF <- length(
    LOGRANK_TRAIN$n
) - 1L


TRAIN_P <- pchisq(

    TRAIN_CHISQ,

    df = TRAIN_DF,

    lower.tail = FALSE
)


cat("\n")
cat("TRAINING SURVIVAL DIFFERENCE\n")
cat("----------------------------------------\n")

cat(
    sprintf(
        "Log-rank chi-square = %.3f\n",
        TRAIN_CHISQ
    )
)

cat(
    sprintf(
        "df                  = %d\n",
        TRAIN_DF
    )
)

cat(
    sprintf(
        "p                   = %.8g\n",
        TRAIN_P
    )
)


# =============================================================================
# PUBLICATION-STYLE KAPLAN-MEIER FIGURE
# =============================================================================

KM_PLOT <- survminer::ggsurvplot(

    fit =
        KM_TRAIN,

    data =
        TRAIN_SURVIVAL,

    # -------------------------------------------------------------------------
    # Statistical information
    # -------------------------------------------------------------------------

    pval =
        TRUE,

    pval.method =
        TRUE,

    conf.int =
        TRUE,

    # -------------------------------------------------------------------------
    # Risk table
    # -------------------------------------------------------------------------

    risk.table =
        TRUE,

    risk.table.height =
        0.25,

    risk.table.y.text =
        FALSE,

    risk.table.col =
        "strata",

    # -------------------------------------------------------------------------
    # Censoring
    # -------------------------------------------------------------------------

    censor =
        TRUE,

    censor.shape =
        124,

    censor.size =
        3,

    # -------------------------------------------------------------------------
    # Axes
    # -------------------------------------------------------------------------

    xlab =
        "Years after fifth-visit landmark",

    ylab =
        "Survival probability",

    break.time.by =
        1,

    surv.scale =
        "percent",

    # -------------------------------------------------------------------------
    # Legend
    # -------------------------------------------------------------------------

    legend.title =
        "Trajectory cluster",

    legend.labs =
        c(
            "Lower-risk trajectory",
            "Higher-risk trajectory"
        ),

    legend =
        "bottom",

    # -------------------------------------------------------------------------
    # Appearance
    # -------------------------------------------------------------------------

    size =
        1.2,

    ggtheme =
        ggplot2::theme_classic(
            base_size = 14
        ),

    font.title =
        c(
            16,
            "bold"
        ),

    font.x =
        c(
            13,
            "bold"
        ),

    font.y =
        c(
            13,
            "bold"
        ),

    font.tickslab =
        c(
            11
        ),

    title =
        "Survival of training-derived trajectory clusters"
)


# =============================================================================
# ADDITIONAL CLEAN-UP
# =============================================================================

KM_PLOT$plot <- KM_PLOT$plot +

    ggplot2::theme(

        plot.title =
            ggplot2::element_text(
                face = "bold",
                hjust = 0
            ),

        legend.position =
            "bottom",

        legend.title =
            ggplot2::element_text(
                face = "bold"
            ),

        panel.grid =
            ggplot2::element_blank()
    )


KM_PLOT$table <- KM_PLOT$table +

    ggplot2::theme_classic(
        base_size = 11
    ) +

    ggplot2::theme(

        legend.position =
            "none",

        axis.title.x =
            ggplot2::element_blank(),

        panel.grid =
            ggplot2::element_blank()
    )


# =============================================================================
# DISPLAY
# =============================================================================

print(
    KM_PLOT
)


# =============================================================================
# SAVE COMPLETE KM FIGURE INCLUDING RISK TABLE
# =============================================================================

pdf(
    "PBC2_K2_Training_KaplanMeier.pdf",
    width = 7.5,
    height = 7
)

print(
    KM_PLOT
)

dev.off()


png(
    "PBC2_K2_Training_KaplanMeier.png",
    width = 2200,
    height = 2000,
    res = 300
)

print(
    KM_PLOT
)

dev.off()
# =============================================================================
# longTAPIO - PUBLICATION READY SIMULATION PLOTS
# Native ggplot2
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)


# =============================================================================
# COLORS
# =============================================================================

COL_METHOD <- c(
    "Transductive" = "#3B73B9",
    "Inductive"    = "#D65A4A"
)

COL_ETA <- c(
    "3"  = "#2A9D8F",
    "5"  = "#E9A03B",
    "10" = "#C44E73"
)


# =============================================================================
# THEME
# =============================================================================

theme_paper <- function() {

    theme_bw(base_size = 13) +

        theme(
            plot.title = element_text(
                face = "bold",
                size = 14
            ),

            axis.title = element_text(
                face = "bold",
                size = 12
            ),

            axis.text = element_text(
                size = 11,
                colour = "black"
            ),

            legend.position = "top",

            legend.title = element_text(
                face = "bold"
            ),

            panel.grid.minor = element_blank(),

            panel.grid.major.x = element_blank(),

            panel.grid.major.y = element_line(
                colour = "grey90",
                linewidth = 0.3
            ),

            strip.background = element_rect(
                fill = "grey95"
            ),

            strip.text = element_text(
                face = "bold"
            )
        )
}


# =============================================================================
# HELPER
#
# Tries to find run-level full-trajectory results.
#
# Add your actual raw object name here if it is different.
# =============================================================================

get_raw_full_results <- function() {

    possible_names <- c(
        "RUN_RESULTS",
        "RAW_RESULTS",
        "ALL_RESULTS",
        "RESULTS_ALL",
        "FULL_RUN_RESULTS",
        "FULL_RESULTS_RAW"
    )

    for (nm in possible_names) {

        if (exists(
            nm,
            envir = .GlobalEnv
        )) {

            X <- get(
                nm,
                envir = .GlobalEnv
            )

            needed <- c(
                "Eta",
                "Sigma",
                "ARI_transductive",
                "ARI_inductive"
            )

            if (
                is.data.frame(X) &&
                all(needed %in% names(X))
            ) {

                message(
                    "Using run-level results: ",
                    nm
                )

                return(X)
            }
        }
    }

    message(
        "No run-level full-results object found. ",
        "Using FULL_RESULTS."
    )

    FULL_RESULTS
}


# =============================================================================
# FIGURE 1
#
# NESTED/GROUPED BOXPLOT
# TRANSDUCTIVE VS INDUCTIVE
#
# x      = eta
# group  = method
# facet  = sigma
#
# If raw runs are available this gives true boxplots.
# =============================================================================

plot_method_boxplots <- function() {

    X <- get_raw_full_results()


    D <- X %>%

        select(
            Eta,
            Sigma,
            ARI_transductive,
            ARI_inductive
        ) %>%

        pivot_longer(
            cols = c(
                ARI_transductive,
                ARI_inductive
            ),
            names_to = "Method",
            values_to = "ARI"
        ) %>%

        mutate(
            Method = ifelse(
                Method == "ARI_transductive",
                "Transductive",
                "Inductive"
            ),

            Method = factor(
                Method,
                levels = c(
                    "Transductive",
                    "Inductive"
                )
            ),

            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            ),

            Sigma = factor(
                Sigma,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Eta,
            y = ARI,
            fill = Method
        )
    ) +

        geom_boxplot(
            position = position_dodge(
                width = 0.75
            ),
            width = 0.62,
            outlier.size = 1.2,
            outlier.alpha = 0.45,
            linewidth = 0.55
        ) +

        scale_fill_manual(
            values = COL_METHOD
        ) +

        facet_wrap(
            ~ Sigma,
            nrow = 1,
            labeller = labeller(
                Sigma = function(x) {
                    paste0(
                        "sigma = ",
                        x
                    )
                }
            )
        ) +

        labs(
            title = "Clustering performance across simulation conditions",
            x = "Observation noise (eta)",
            y = "Adjusted Rand index",
            fill = "Method"
        ) +

        theme_paper()
}


# =============================================================================
# FIGURE 2
#
# SIMPLER NESTED BOXPLOT
#
# No facets.
# Pool sigma settings.
#
# This is cleaner for the MAIN PAPER.
# =============================================================================

plot_method_boxplots_simple <- function() {

    X <- get_raw_full_results()


    D <- X %>%

        select(
            Eta,
            Sigma,
            ARI_transductive,
            ARI_inductive
        ) %>%

        pivot_longer(
            cols = c(
                ARI_transductive,
                ARI_inductive
            ),
            names_to = "Method",
            values_to = "ARI"
        ) %>%

        mutate(
            Method = ifelse(
                Method == "ARI_transductive",
                "Transductive",
                "Inductive"
            ),

            Method = factor(
                Method,
                levels = c(
                    "Transductive",
                    "Inductive"
                )
            ),

            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Eta,
            y = ARI,
            fill = Method
        )
    ) +

        geom_boxplot(
            position = position_dodge(
                width = 0.75
            ),
            width = 0.62,
            outlier.size = 1.2,
            outlier.alpha = 0.4,
            linewidth = 0.55
        ) +

        scale_fill_manual(
            values = COL_METHOD
        ) +

        labs(
            title = "Inductive and transductive clustering performance",
            x = "Observation noise (eta)",
            y = "Adjusted Rand index",
            fill = "Method"
        ) +

        theme_paper()
}


# =============================================================================
# FIGURE 3
#
# PROGRESSIVE ARI
#
# Average over sigma.
# Three curves only.
# =============================================================================

plot_progressive_ari <- function(PROGRESSIVE_RESULTS) {

    D <- PROGRESSIVE_RESULTS %>%

        group_by(
            Visit,
            Eta
        ) %>%

        summarise(
            ARI = mean(
                ARI_truth,
                na.rm = TRUE
            ),
            .groups = "drop"
        ) %>%

        mutate(
            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Visit,
            y = ARI,
            colour = Eta,
            group = Eta
        )
    ) +

        geom_line(
            linewidth = 1.2
        ) +

        geom_point(
            size = 2.8
        ) +

        scale_colour_manual(
            values = COL_ETA
        ) +

        scale_x_continuous(
            breaks = 1:10
        ) +

        labs(
            title = "Progressive cluster recovery",
            x = "Observed visits",
            y = "Adjusted Rand index",
            colour = "Noise (eta)"
        ) +

        theme_paper()
}


# =============================================================================
# FIGURE 4
#
# PROGRESSIVE STABILITY
# =============================================================================

plot_progressive_stability <- function(PROGRESSIVE_RESULTS) {

    D <- PROGRESSIVE_RESULTS %>%

        group_by(
            Visit,
            Eta
        ) %>%

        summarise(
            Agreement = mean(
                Agreement_final,
                na.rm = TRUE
            ),
            .groups = "drop"
        ) %>%

        mutate(
            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Visit,
            y = Agreement,
            colour = Eta,
            group = Eta
        )
    ) +

        geom_line(
            linewidth = 1.2
        ) +

        geom_point(
            size = 2.8
        ) +

        scale_colour_manual(
            values = COL_ETA
        ) +

        scale_x_continuous(
            breaks = 1:10
        ) +

        scale_y_continuous(
            labels = function(x) {
                paste0(
                    round(x * 100),
                    "%"
                )
            }
        ) +

        labs(
            title = "Progressive assignment stability",
            x = "Observed visits",
            y = "Agreement with final assignment",
            colour = "Noise (eta)"
        ) +

        theme_paper()
}


# =============================================================================
# FIGURE 5
#
# ASSIGNMENT MARGIN
# =============================================================================

plot_progressive_margin <- function(PROGRESSIVE_RESULTS) {

    D <- PROGRESSIVE_RESULTS %>%

        group_by(
            Visit,
            Eta
        ) %>%

        summarise(
            Margin = mean(
                Mean_margin,
                na.rm = TRUE
            ),
            .groups = "drop"
        ) %>%

        mutate(
            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Visit,
            y = Margin,
            colour = Eta,
            group = Eta
        )
    ) +

        geom_line(
            linewidth = 1.2
        ) +

        geom_point(
            size = 2.8
        ) +

        scale_colour_manual(
            values = COL_ETA
        ) +

        scale_x_continuous(
            breaks = 1:10
        ) +

        labs(
            title = "Assignment margin across follow-up",
            x = "Observed visits",
            y = "Mean assignment margin",
            colour = "Noise (eta)"
        ) +

        theme_paper()
}


# =============================================================================
# FIGURE 6
#
# NESTED BOXPLOT OF PAIRED DIFFERENCES
#
# Particularly useful because Delta ARI directly addresses:
#
#   inductive - transductive
#
# zero = identical performance
# positive = inductive better
# negative = transductive better
#
# Requires raw simulation runs to be a genuine boxplot.
# =============================================================================

plot_delta_boxplots <- function() {

    X <- get_raw_full_results()


    if (!"Delta_ARI" %in% names(X)) {

        X$Delta_ARI <-
            X$ARI_inductive -
            X$ARI_transductive
    }


    D <- X %>%

        mutate(
            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            ),

            Sigma = factor(
                Sigma,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Eta,
            y = Delta_ARI,
            fill = Sigma
        )
    ) +

        geom_hline(
            yintercept = 0,
            linetype = 2,
            colour = "grey35",
            linewidth = 0.7
        ) +

        geom_boxplot(
            position = position_dodge(
                width = 0.8
            ),
            width = 0.68,
            outlier.size = 1.2,
            outlier.alpha = 0.4,
            linewidth = 0.55
        ) +

        scale_fill_manual(
            values = c(
                "3"  = "#8EC9C0",
                "5"  = "#F3C969",
                "10" = "#D88AA1"
            )
        ) +

        labs(
            title = "Paired difference in clustering performance",
            x = "Observation noise (eta)",
            y = "ARI difference (inductive - transductive)",
            fill = "Heterogeneity (sigma)"
        ) +

        theme_paper()
}


# =============================================================================
# FIGURE 7
#
# NMI METHOD BOXPLOT
#
# Useful as supplementary confirmation that conclusions are not ARI-specific.
# =============================================================================

plot_nmi_boxplots <- function() {

    X <- get_raw_full_results()


    if (
        !all(
            c(
                "NMI_transductive",
                "NMI_inductive"
            ) %in% names(X)
        )
    ) {

        stop(
            "Run-level NMI columns were not found."
        )
    }


    D <- X %>%

        select(
            Eta,
            Sigma,
            NMI_transductive,
            NMI_inductive
        ) %>%

        pivot_longer(
            cols = c(
                NMI_transductive,
                NMI_inductive
            ),
            names_to = "Method",
            values_to = "NMI"
        ) %>%

        mutate(
            Method = ifelse(
                Method == "NMI_transductive",
                "Transductive",
                "Inductive"
            ),

            Method = factor(
                Method,
                levels = c(
                    "Transductive",
                    "Inductive"
                )
            ),

            Eta = factor(
                Eta,
                levels = c(3, 5, 10)
            )
        )


    ggplot(
        D,
        aes(
            x = Eta,
            y = NMI,
            fill = Method
        )
    ) +

        geom_boxplot(
            position = position_dodge(
                width = 0.75
            ),
            width = 0.62,
            outlier.size = 1.2,
            outlier.alpha = 0.4,
            linewidth = 0.55
        ) +

        scale_fill_manual(
            values = COL_METHOD
        ) +

        labs(
            title = "Normalized mutual information",
            x = "Observation noise (eta)",
            y = "NMI",
            fill = "Method"
        ) +

        theme_paper()
}


# =============================================================================
# CREATE ALL FIGURES
# =============================================================================

p1 <- plot_method_boxplots()

p2 <- plot_method_boxplots_simple()

p3 <- plot_progressive_ari(
    PROGRESSIVE_RESULTS
)

p4 <- plot_progressive_stability(
    PROGRESSIVE_RESULTS
)

p5 <- plot_progressive_margin(
    PROGRESSIVE_RESULTS
)

p6 <- plot_delta_boxplots()


# NMI only if raw NMI exists
X_check <- get_raw_full_results()

if (
    all(
        c(
            "NMI_transductive",
            "NMI_inductive"
        ) %in% names(X_check)
    )
) {

    p7 <- plot_nmi_boxplots()

} else {

    p7 <- NULL
}


# =============================================================================
# DISPLAY
# =============================================================================

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)

if (!is.null(p7)) {
    print(p7)
}


# =============================================================================
# SAVE
# =============================================================================

ggsave(
    "Fig1_nested_ARI.pdf",
    p1,
    width = 8,
    height = 4.5
)

ggsave(
    "Fig2_ARI_simple.pdf",
    p2,
    width = 6.5,
    height = 4.5
)

ggsave(
    "Fig3_progressive_ARI.pdf",
    p3,
    width = 6.5,
    height = 4.5
)

ggsave(
    "Fig4_progressive_stability.pdf",
    p4,
    width = 6.5,
    height = 4.5
)

ggsave(
    "Fig5_assignment_margin.pdf",
    p5,
    width = 6.5,
    height = 4.5
)

ggsave(
    "Fig6_delta_ARI_boxplot.pdf",
    p6,
    width = 6.5,
    height = 4.5
)

if (!is.null(p7)) {

    ggsave(
        "Fig7_NMI_boxplot.pdf",
        p7,
        width = 6.5,
        height = 4.5
    )
}
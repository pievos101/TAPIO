# Run from the repository root. Reproduce README figures from the committed PBC2
# analysis results; this does not rerun or alter the original clinical analysis.
library(ggplot2)
source_dir <- "experiments_inductive/PBC2_application"
output_dir <- "man/figures"
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
assignment <- read.csv(file.path(source_dir, "PBC2_K2_progressive_ARI.csv"))
features <- read.csv(file.path(source_dir, "PBC2_K2_overall_importance.csv"))
stopifnot(identical(assignment$Visit, 1:5),
          all(is.finite(assignment$ARI)),
          all(is.finite(features$Importance)),
          all(table(features$Feature)==5L))

p_ari <- ggplot(assignment, aes(Visit, ARI)) +
    geom_line(linewidth=1.1, colour="#255F85") +
    geom_point(size=3, colour="#255F85") +
    geom_text(aes(label=sprintf("%.2f", ARI)), vjust=-0.9, size=4) +
    scale_x_continuous(breaks=1:5) +
    scale_y_continuous(limits=c(0,1.1), breaks=seq(0,1,0.2)) +
    labs(title="PBC2: visit-wise assignment to training clusters",
         subtitle="Unseen patients: agreement with their final five-visit assignment",
         x="Number of observed visits", y="Adjusted Rand Index") +
    theme_classic(base_size=13)

p_features <- ggplot(features, aes(Visit, Importance, colour=Feature)) +
    geom_line(linewidth=1) + geom_point(size=2) +
    scale_x_continuous(breaks=1:5) +
    scale_colour_brewer(palette="Dark2") +
    labs(title="PBC2: visit-wise biomarker importance",
         subtitle="Mean patient-level importance in the held-out cohort",
         x="Number of observed visits", y="Normalized importance", colour="Biomarker") +
    theme_classic(base_size=13) +
    theme(legend.position="bottom")

ggsave(file.path(output_dir,"pbc2-visit-ari.png"), p_ari,
       width=8, height=4.3, dpi=160, bg="white")
ggsave(file.path(output_dir,"pbc2-visit-importance.png"), p_features,
       width=8, height=4.8, dpi=160, bg="white")

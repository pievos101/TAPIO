# Run from the repository root. Optional first argument: fresh application output
# directory. Default uses the committed output snapshot for the README.
library(ggplot2)
args <- commandArgs(TRUE)
source_dir <- if (length(args)) args[1] else "experiments_inductive/PBC2_application/readme_results"
output_dir <- "man/figures"
dir.create(output_dir, recursive=TRUE, showWarnings=FALSE)
assignment <- read.csv(file.path(source_dir, "PBC2_K2_progressive_assignment.csv"))
features <- read.csv(file.path(source_dir, "PBC2_K2_cluster_specific_importance.csv"))
training <- read.csv(file.path(source_dir, "PBC2_training_risk_groups.csv"))
test <- read.csv(file.path(source_dir, "PBC2_test_risk_groups.csv"))
risk_levels <- c("Lower-risk trajectory", "Higher-risk trajectory")
features$Risk_group <- factor(features$Risk_group, levels=risk_levels)
stopifnot(identical(assignment$Visit, 1:5), all(is.finite(assignment$ARI)),
          all(is.finite(features$Importance)), all(table(features$Feature)==10L))

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
    facet_wrap(~Risk_group) +
    scale_x_continuous(breaks=1:5) +
    scale_colour_brewer(palette="Dark2") +
    labs(title="PBC2: visit-wise importance in both risk clusters",
         subtitle="Mean held-out patient importance, grouped by final cluster",
         x="Number of observed visits", y="Normalized importance", colour="Biomarker") +
    theme_classic(base_size=13) + theme(legend.position="bottom")

km_data <- function(d, cohort) {
    d$risk_group <- factor(d$risk_group, levels=risk_levels)
    stopifnot(length(unique(d$risk_group))==2L)
    fit <- survival::survfit(survival::Surv(time,event) ~ risk_group, data=d)
    s <- summary(fit, censored=TRUE)
    out <- data.frame(time=s$time, survival=s$surv, lower=s$lower, upper=s$upper,
                      censored=s$n.censor, Risk_group=sub("^risk_group=", "", as.character(s$strata)))
    start <- data.frame(time=0, survival=1, lower=1, upper=1, censored=0, Risk_group=risk_levels)
    out <- rbind(start,out)
    out$Cohort <- paste0(cohort," (n=",nrow(d),")")
    out
}
km <- rbind(km_data(training,"Training"), km_data(test,"Held-out"))
km$Risk_group <- factor(km$Risk_group, levels=risk_levels)
km$Cohort <- factor(km$Cohort, levels=c(paste0("Training (n=",nrow(training),")"),
                                    paste0("Held-out (n=",nrow(test),")")))
p_km <- ggplot(km, aes(time, survival, colour=Risk_group, fill=Risk_group)) +
    geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.12, colour=NA, na.rm=TRUE) +
    geom_step(linewidth=0.9) +
    geom_point(data=km[km$censored>0,], shape=3, size=1.6, show.legend=FALSE) +
    facet_wrap(~Cohort) +
    scale_colour_manual(values=c("#255F85","#BC3D3D")) +
    scale_fill_manual(values=c("#255F85","#BC3D3D")) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
    labs(title="PBC2: Kaplan–Meier curves for both trajectory clusters",
         subtitle="Risk labels determined using training survival only; shaded 95% intervals",
         x="Years after the fifth-visit landmark", y="Survival probability", colour=NULL, fill=NULL) +
    theme_classic(base_size=12) + theme(legend.position="bottom")

ggsave(file.path(output_dir,"pbc2-visit-ari.png"), p_ari, width=8, height=4.3, dpi=160, bg="white")
ggsave(file.path(output_dir,"pbc2-visit-importance.png"), p_features, width=10, height=5, dpi=160, bg="white")
ggsave(file.path(output_dir,"pbc2-kaplan-meier.png"), p_km, width=10, height=5, dpi=160, bg="white")

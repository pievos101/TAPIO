#library(survival)
library(survminer)

# Your grouping variable
groups <- factor(res$cl)
names(groups) <- rownames(survival)

# Fit Kaplan–Meier curves
km_fit <- survfit(Surv(Survival, Death) ~ groups, data = survival)

# Plot with survminer
ggsurvplot(
  km_fit,
  data = survival,
  risk.table = TRUE,        # Add risk table
  pval = TRUE,              # Show log-rank p-value
  conf.int = TRUE,          # Confidence bands
  legend.title = "Cluster",
  legend.labs  = levels(groups),
  palette = "Dark2",        # Optional color palette
  ggtheme = theme_minimal() + theme(text = element_text(size=17))
)

#############################################

# Your grouping variable
groups <- factor(CL)
names(groups) <- rownames(survival)

# Fit Kaplan–Meier curves
km_fit <- survfit(Surv(Survival, Death) ~ groups, data = survival)

# Plot with survminer
ggsurvplot(
  km_fit,
  data = survival,
  risk.table = TRUE,        # Add risk table
  pval = TRUE,              # Show log-rank p-value
  conf.int = TRUE,          # Confidence bands
  legend.title = "Group",
  legend.labs  = levels(groups),
  palette = "Dark2",        # Optional color palette
  ggtheme = theme_minimal() #+ theme(text = element_text(size=17))
)

########################################
library(ggplot2)

# Convert matrix to data frame
df_imp <- as.data.frame(IMP)
df_imp$Cluster <- rownames(IMP)

# Reshape to long format (no dplyr)
df_long <- reshape(
  df_imp,
  direction = "long",
  varying = colnames(IMP),
  v.name = "Importance",
  timevar = "Biomarker",
  times = colnames(IMP)
)

# Plot heatmap
ggplot(df_long, aes(x = Biomarker, y = Cluster, fill = Importance)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(Importance, 2)), size = 4) +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = mean(range(df_long$Importance)),
    name = "Importance"
  ) +
  theme_minimal(base_size = 17) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "",
    x = "Features",
    y = "Cluster"
  )
# This loads ann and data
load("AIFI-Metadata.Rda")
load("AIFI-Olink-NPX_log2_Protein.Rda")

# Convert data into the right format 
data2 = t(data)

id = sort(rep(1:6, 10))
time = rep(1:10,6)

data3 = cbind(id, time, data2)

# remove NaNs
na_ids = which(apply(data3, 2, function(x){any(is.na(x))}))
data4 = data3[,-na_ids]

Longdat2_wide = as.data.frame(data4)

# Call TAPIO 
DD = as.matrix(Longdat2_wide[,3:ncol(Longdat2_wide)])

res = longTAPIO_trajectories(DD,
                         k = 3, max.k=5,
                         user_id = Longdat2_wide$id, 
                         levels=3, verbose = 1, 
                         n_trees=10000, method="ward.D2",
                         n_features=NaN, do.leveling=TRUE)

foundClusIDs = res$cl

imp = importance(res)

rownames(imp) = paste("cluster",1:nrow(imp), sep=" ")
colnames(imp) = colnames(DD)

library(reshape)

imp_melt = melt(imp)
colnames(imp_melt) = c("Cluster","Gene","Importance")

library(ggplot2)

##### GGPLOT ################################
# get top N manually
topN <- 10
top_list <- list()

df <- imp_melt   # your data: Gene, Cluster, Importance
clusters <- unique(df$Cluster)

for (cl in clusters) {
    sub <- df[df$Cluster == cl, ]
    ord <- sub[order(-sub$Importance), ]
    top_list[[cl]] <- ord[1:topN, ]
}

df_top <- do.call(rbind, top_list)

# -----------------------------
# 1. CREATE GLOBAL GENE ORDER
# -----------------------------
gene_order <- names(sort(tapply(df_top$Importance, df_top$Gene, max), decreasing = TRUE))

df_top$Gene <- factor(df_top$Gene, levels = gene_order)

# -----------------------------
# 2. PLOT (FIXED X-AXIS ORDER)
# -----------------------------
library(ggplot2)

ggplot(df_top, aes(x = Gene, y = Importance, fill = Cluster)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ Cluster, scales = "free_y") +
    theme_bw() +
    theme(
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1,
            size = 6     # <- smaller labels
        )
    ) +
    labs(title = paste("Top", topN, "Genes by Importance"),
         x = "Gene",
         y = "Importance")

################################################################


# Reverse the gene order
df_top$Gene <- factor(df_top$Gene, levels = rev(gene_order))

ggplot(df_top, aes(x = Gene, y = Importance, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +  # side-by-side bars
  coord_flip() +                                     # rotate axes
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),           # gene names
    axis.text.x = element_text(size = 8),
    legend.position = "top"
  ) +
  labs(
    title = paste("Top", topN, "Genes by Importance"),
    y = "Importance",
    x = "Gene"
  )

## DENDROGRAM 


# Create a distance matrix and hclust
hc = hclust(as.dist(res$DIST), method="ward.D2")

# Cluster assignments (your detected clusters)
clusters <- res$cl       # e.g. 1 2 1 3 2 3
clusters <- factor(clusters)

# Set colors for the clusters
cluster_colors <- c("1" = "red", "2" = "blue", "3" = "forestgreen")

library(dendextend)

# Create dendrogram
dend <- as.dendrogram(hc)

# Make it pretty
dend <- dend %>% 
    set("labels_cex", 0.8) %>%        # smaller label size
    set("branches_lwd", 2) %>%        # thicker lines
    set("branches_k_color", k = 3, palette = c("red","blue","forestgreen"))

# Plot (labels will remain black)
plot(dend, main = "Hierarchical Clustering Dendrogram\nBranches Colored by Cluster")


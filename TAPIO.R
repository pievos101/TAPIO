library(HCfused)
library(aricode)
library(NbClust)
library(fastcluster)


TAPIO <- function(DATA, k, n_features=NaN, n_trees=1000, 
						do.pca=TRUE, do.leveling=TRUE, levels=20){

	if(is.na(n_features)){

		n_features = ceiling(sqrt(ncol(DATA)))

	}

	PART = vector("list", n_trees)
	IMP  = vector("list", n_trees)

	for (xx in 1:n_trees){

		ids    = sample(1:ncol(DATA), n_features, replace=FALSE)
		ids_no = (1:ncol(DATA))[-ids]
		DATA_s = DATA[,ids]
		#print(DATA_s)
		# PCA
		if(do.pca){
			res.pca = prcomp(DATA_s, scale=FALSE)
			var.cor = t(apply(res.pca$rotation, 1, var_cor_func, res.pca$sdev))
			var = .get_pca_var_results(var.cor)
			IMP[[xx]] = var$contrib[,1]
			#IMP[[xx]][ids_no] = NaN 
			DATA_s = res.pca$x[,1] # first PCA
		}

		# LEVELING
		if(do.leveling){
			LEVELS = vector("list", levels)
			hc = fastcluster::hclust(dist(DATA_s), method="ward.D2")
			for(yy in 1:length(LEVELS)){
				cl = cutree(hc, yy+1)
				LEVELS[[yy]] = HCfused::association(cl)
			}
			PART[[xx]] = Reduce("+",LEVELS)
		
		# NO LEVELING
		}else{
			hc = fastcluster::hclust(dist(DATA_s), method="ward.D2")
			cl = cutree(hc, 2)
			PART[[xx]] = HCfused::association(cl)
		}
	}

	AFF  = Reduce("+", PART)
	DIST = 1 - AFF/max(AFF)

	# Final clustering
	hc = fastcluster::hclust(as.dist(DIST), method="ward.D2")
	cl = cutree(hc, k)


	# Get the Importances
	Importance = matrix(NaN, n_trees, ncol(DATA))

	for (xx in 1:n_trees){

		imp = IMP[[xx]]
		ids = match(names(imp), colnames(DATA))
		Importance[xx,ids] = imp

	}

	#Importance = colMeans(Importance)

	return(list(cl=cl, PART=PART, feature_importance=Importance))

}


# compute all the results for variables : coord, cor, cos2, contrib
# var.coord : coordinates of variables on the principal component
.get_pca_var_results <- function(var.coord){
  
  var.cor <- var.coord # correlation
  var.cos2 <- var.cor^2 # variable qualities 
  
  # variable contributions (in percent)
  # var.cos2*100/total Cos2 of the component
  comp.cos2 <- apply(var.cos2, 2, sum)
  contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
  var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
  
  colnames(var.coord) <- colnames(var.cor) <- colnames(var.cos2) <-
    colnames(var.contrib) <- paste0("Dim.", 1:ncol(var.coord)) 
  
  # Variable coord, cor, cos2 and contrib
  list(coord = var.coord, cor = var.cor, cos2 = var.cos2, contrib = var.contrib)
}


# Correlation of variables with the principal component
var_cor_func <- function(var.loadings, comp.sdev){var.loadings*comp.sdev}

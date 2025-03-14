# function to generate cluster centers for synthetic datasets
get_cluster_centers <- function(cluster_number){
  # centrality test
  if (cluster_number==1){
    cluster_center_1 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_2 <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_3 <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_4 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  } 
  # discriminating power test
  if (cluster_number==2){
    cluster_center_1 <- c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
    cluster_center_2 <- c(0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0)
    cluster_center_3 <- c(0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0)
    cluster_center_4 <- c(1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0)
  } 
  # cluster specific test
  if (cluster_number==3){
    cluster_center_1 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_2 <- c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_3 <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_4 <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  } 
  # repeated features test
  if (cluster_number==4){
    cluster_center_1 <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_2 <- c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0)
    cluster_center_3 <- c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
    cluster_center_4 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  } 
  # alternative test for redundant features
  if (cluster_number==5){
    cluster_center_1 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_2 <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_3 <- c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    cluster_center_4 <- c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  } 
  cluster_centers <- rbind(cluster_center_1, cluster_center_2, cluster_center_3, cluster_center_4)
  return(cluster_centers)
}

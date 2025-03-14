
# get benchmark datasets
library(mlbench)
library(FCPS)

get_dataset <- function(DATASET){

  #’Atom’, ’Chainlink, ’EngyTime’, ’GolfBall’, ’Hepta’, ’Lsun3D’,
  #’Target’ ’Tetra’ ’TwoDiamonds’ ’WingNut
  if(DATASET=="Atom"){ #yes
    res = ClusterChallenge(DATASET,300)
    train = res$Atom
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 

  if(DATASET=="Target"){ #yes
    res = ClusterChallenge(DATASET,300)
    train = res$Target
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  
  if(DATASET=="WingNut"){ #yes
    res = ClusterChallenge(DATASET,300)
    train = res$WingNut
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="TwoDiamonds"){ # ARI = 1
    res = ClusterChallenge(DATASET,300)
    train = res$TwoDiamonds
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="Tetra"){ #no
    res = ClusterChallenge(DATASET,300)
    train = res$Tetra
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="Lsun3D"){ #yes!
    res = ClusterChallenge(DATASET,300)
    train = res$Lsun3D
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="Hepta"){ # too big?
    res = ClusterChallenge(DATASET,300)
    train = res$Hepta
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="GolfBall"){
    res = ClusterChallenge(DATASET,300)
    train = res$GolfBall
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="EngyTime"){# only two features
    res = ClusterChallenge(DATASET,300)
    train = res$EngyTime
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="Chainlink"){ # yes
    res = ClusterChallenge(DATASET,300)
    train = res$Chainlink
    target = res$Cls 
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  } 
  

  if(DATASET=="ZOO"){
    data(Zoo)
    train = Zoo[,-ncol(Zoo)]
    target = Zoo[,ncol(Zoo)]
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }
  
  if(DATASET=="VEHICLE"){
    data(Vehicle)
    train = Vehicle[,-ncol(Vehicle)]
    target = Vehicle[,ncol(Vehicle)]
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }

  if(DATASET=="LETTER"){
    data(LetterRecognition)
    train = LetterRecognition[,-1]
    target = LetterRecognition[,1]
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }
  

  if(DATASET=="DNA"){
    data(DNA)
    train = DNA[,-ncol(DNA)]
    target = DNA[,ncol(DNA)]
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }

  if(DATASET=="NEWSGROUPS"){
    library(RTextTools)
    library(tm)
    ## load data
    data <- read.csv("http://ssc.wisc.edu/~ahanna/20_newsgroups.csv", stringsAsFactors = FALSE)
    data <- data[-1]
    ## make tbl_df for nicer behavior on output
    #data <- tbl_df(data)
    dtm  = create_matrix(data, language="english", weighting=tm::weightTfIdf)
    target = as.numeric(as.factor(data$target))
    train  = matrix(dtm[,], nrow(dtm), ncol(dtm))
    colnames(train) = 1:ncol(train)
    ids = sample(1:length(target), 500)
    train = train[ids,]
    target = target[ids]
    ids = apply(train,2,var)
    ids = which(ids==0)
    print(length(ids))
    train = train[,-ids]
    k = length(unique(target))
  }
  
  if(DATASET=="SOYBEAN"){
    data(Soybean)
    train = Soybean[,-1]
    train = matrix(as.numeric(unlist(train)), nrow(train), ncol(train))
    colnames(train) = 1:ncol(train)
    target = Soybean[,1]
    target = as.numeric(as.factor(target))
    k = length(unique(target))
    # remove NaNs
    ids = apply(train,1,is.na)
    ids2 = apply(ids,2,any)
    train = train[!ids2,]
    target = target[!ids2]
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }
  
  if(DATASET=="STATLOG"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "uci", "statlog")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
  }
  if(DATASET=="YEAST"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "uci", "yeast")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
  }
  if(DATASET=="WDBC"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "uci", "wdbc")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
  }
  if(DATASET=="SONAR"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "uci", "sonar")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
  }
  if(DATASET=="ECOLI"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "uci", "ecoli")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
  }
  if(DATASET=="IONOSPHERE"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "uci", "ionosphere")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
    train = train[,-2]
  }
  if(DATASET=="SMILE"){
    base_name <- file.path("~", "GitHub", "clustering-data-v1", "wut", "smile")
    train    <- as.matrix(read.table(paste0(base_name, ".data.gz")))
    target  <- scan(paste0(base_name, ".labels0.gz"), integer())
    k = length(unique(target))
  }
  if(DATASET=="PARKINSON"){# yes - but bad signal
    my_url="https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data"
    dataset2=read.csv(my_url, header=TRUE, sep=",")
    train = dataset2[,-c(1,18)]
    target = dataset2[,18]
    target = target + 1
    k = length(unique(target))
  }
  if(DATASET=="HEART"){
    library(kmed)
    data(heart)
    train = heart[, -ncol(heart)]
    train = matrix(as.numeric(unlist(train)), dim(train)[1], dim(train)[2])
    colnames(train) = paste("V", 1:ncol(train), sep="")
    target = heart[,ncol(heart)]
    target[target!=0] = 1
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }
  if(DATASET=="IRIS"){
    data(iris)
    train = iris[,-ncol(iris)]
    train = as.matrix(train)
    #train = train[51:150,]
    train = matrix(as.numeric(train), dim(train)[1], dim(train)[2])
    colnames(train) = paste("V", 1:ncol(train), sep="")
    target = iris[,ncol(iris)]
    #target = target[51:150]
    target = as.numeric(target)
    k = length(unique(target))
  }
  if(DATASET=="WINE"){
    library(HDclassif)
    data(wine)
    train = wine[,-1]
    train = as.matrix(train)
    train = matrix(as.numeric(train), dim(train)[1], dim(train)[2])
    colnames(train) = paste("V", 1:ncol(train), sep="")
    target = wine[,1]
    k = length(unique(target))
  }
  if(DATASET=="GLASS"){
    library(mlbench)
    data(Glass)
    train = Glass[,-10]
    train = as.matrix(train)
    train = matrix(as.numeric(train), dim(train)[1], dim(train)[2])
    colnames(train) = paste("V", 1:ncol(train), sep="")
    target = Glass[,10]
    target = as.numeric(target)
    ids = which(target>=5)
    target[ids] =  4
    k = length(unique(target))
  }
  if(DATASET=="BREAST"){
    train = read.csv("BreastTissue.csv", header=TRUE)
    target = train[,2]
    train = train[,-c(1,2)]
    train = as.matrix(train)
    train = matrix(as.numeric(train), dim(train)[1], dim(train)[2])
    colnames(train) = paste("V", 1:ncol(train), sep="")
    target = as.numeric(as.factor(target))
    k = length(unique(target))
  }
  if(DATASET=="SYNTHETIC"){
    a = rnorm(50, 0, 0.5)
    b = rnorm(50, 2, 0.5)
    c = rnorm(50,-2, 0.5)
    X = c(b,a,a)
    Y = c(a,c,a)
    Z1 = c(a,a,a)
    Z2 = c(a,a,a)
    Z3 = c(a,a,a)
    train = cbind(X, Y, Z1, Z2, Z3)
    target = c(rep(1,50), rep(2,50), rep(3,50))
    k=3
  }
  return(list(train = train, target = target, k = k))
}
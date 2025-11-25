library(TAPIO)
library(clusterMLD)
library(MASS)
library(aricode)
library(reshape)

Longdat2 = simLongData(ranTimes = FALSE, 
                            n_i = 10, 
                            eta = 10)

    Longdat2_wide <- reshape(
    Longdat2,
    idvar = c("subject", "time", "cluster"),  # columns that identify each row
    timevar = "outcome",                      # the variable that will become columns
    direction = "wide"
    )


colnames(Longdat2_wide) = c("subject","time","cluster",paste("y",1:5,sep=""))


library(lcmm)

# Fit single-class model first
model1 <- multlcmm(
  fixed = y1 + y2 + y3 + y4 + y5 ~ time,
  random = ~ time,
  subject = "subject",
  ng = 1,
  data = as.data.frame(Longdat2_wide)
)

# Use model1 as starting values for 4-class model
model2 <- multlcmm(
  fixed = y1 + y2 + y3 + y4 + y5 ~ time,
  random = ~ time,
  mixture = ~ time,
  subject = "subject",
  ng = 4,
  idiag = TRUE,
  data = as.data.frame(Longdat2_wide),
  B = model1
)
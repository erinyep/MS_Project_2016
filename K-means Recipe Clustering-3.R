####Weight with Tf IDF##########


testdtm_tfxidf <- weightTfIdf(testdtm)

noGround_tfxidf <-weightTfIdf(noGroundDtm)

##########################################
##K Means Clustering
m <- as.matrix(testdtm_tfxidf)
noGclust <- as.matrix(noGround_tfxidf)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)

set.seed(1234)
m_norm <- norm_eucl(m)

noG_norm <- norm_eucl(noGclust)

cl26 <- kmeans(m_norm, 26)
noG26 <- kmeans(noG_norm, 26, iter.max=30)
cl15 <- kmeans(m_norm, 15)

cl5 <- kmeans(m_norm, 5)

###add the cluster back into the ish. 
dataclust <- datatrue


dataclust$cluster <- cl26$cluster
dataclust$noGclust  <- noG26$cluster
write.csv(dataclust, file = "datawithclustersnog2.csv")

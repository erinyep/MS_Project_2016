library(tm)

####super snazzy code that I found online to help tag the id to the document#####
#### Will be important for our overall project#########
myReader1 <- readTabular(mapping=list(content="ingredients", id="recipe"))
myReaderCuisine <- readTabular(mapping=list(content="ingredients", id="cuisine"))
### create the corpus with document IDs

datatrue <- data
datatrue$ingredients <- gsub(",",", ",datatrue$ingredients) 
datatrue$ingredients <- gsub("_"," ",datatrue$ingredients)

testcorpus1 <- Corpus(DataframeSource(datatrue), readerControl=list(reader=myReader1))

testrcorp <- tm_map(testcorpus1, removePunctuation)


testrcorp.noGround <-testrcorp
testrcorp.noGround <- tm_map(testrcorp.noGround, removeWords, "ground")

#####Build DTMS

testdtm <- DocumentTermMatrix(testrcorp)

noGroundDtm<- DocumentTermMatrix(testrcorp.noGround)

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
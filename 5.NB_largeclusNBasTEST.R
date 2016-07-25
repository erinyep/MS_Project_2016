library('e1071');
library('SparseM');
library('tm');

#SO I CREATED A NEW MODEL ON ALL THE DATA INSTEAD OF USING THE TRAINING DATA SET (70% OF THE DATA).
  #COULD TRY TO DO THE SAME THING WITH THE MODEL FROM THE OTHER NAIVE BAYES.  

setwd("~/Bellarmine/Text Mining")
load("largeclusNB.Rda")
Sample_data1 <- cluster8
Sample_data1$ingredients <- gsub(",", " ", Sample_data1$ingredients)
Sample_data1$ingredients <- gsub('"', " ", Sample_data1$ingredients)
Sample_data1$ingredients <- gsub("_", " ", Sample_data1$ingredients)

load("dataClusNB.Rda")
Sample_data <- dataClusNB
Sample_data$ingredients <- gsub(",", " ", Sample_data$ingredients)
Sample_data$ingredients <- gsub('"', " ", Sample_data$ingredients)
Sample_data$ingredients <- gsub("_", " ", Sample_data$ingredients)
Sample_data$cluster <- droplevels(as.factor(Sample_data$cluster))

#Split Train/Test Sets
set.seed(40207)  #For replication purposes
traindata <- Sample_data
testdata <- Sample_data1[,c(1,2,3)]


#This creates corpus and keeps the ID. Other R file has both with/without
myReader <- readTabular(mapping=list(content="ingredients", id="recipe"))
traincorp <- Corpus(DataframeSource(traindata), readerControl=list(reader=myReader))
testcorp <- Corpus(DataframeSource(testdata), readerControl=list(reader=myReader))

# PERFORMING THE VARIOUS TRANSFORMATION on "traincorpus" and "testcorpus" DATASETS #SUCH AS TRIM WHITESPACE, REMOVE PUNCTUATION, REMOVE STOPWORDS.
traincorp <- tm_map(traincorp,stripWhitespace);
traincorp <- tm_map(traincorp,tolower);
traincorp <- tm_map(traincorp, removeWords,stopwords("english"));
traincorp<- tm_map(traincorp,removePunctuation);
traincorp <- tm_map(traincorp, PlainTextDocument);
testcorp <- tm_map(testcorp,stripWhitespace);
testcorp <- tm_map(testcorp,tolower);
testcorp <- tm_map(testcorp, removeWords,stopwords("english"));
testcorp<- tm_map(testcorp,removePunctuation);
testcorp <- tm_map(testcorp, PlainTextDocument);

# CREATE TERM DOCUMENT MATRIX
trainmatrix <- t(TermDocumentMatrix(traincorp));
testmatrix <- t(TermDocumentMatrix(testcorp));

trainmatrixDTM <- t(DocumentTermMatrix(traincorp))
testmatrixDTM <- t(DocumentTermMatrix(testcorp))

inspctTDM <- inspect(trainmatrix)
inspctDTM <- inspect(trainmatrixDTM)

testtdm_tfxidf <- weightTfIdf(testmatrix)
traintdm_tfxidf <- weightTfIdf(trainmatrix)

testdtm_tfxidf <- weightTfIdf(testmatrixDTM)
traindtm_tfxidf <- weightTfIdf(trainmatrixDTM)


trainmatrixDTM <- (DocumentTermMatrix(traincorp))
testmatrixDTM <-  (DocumentTermMatrix(testcorp))

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c(0, 1))
  y
}

sms_train <- apply(trainmatrixDTM, 2, convert_count)
sms_test <- apply(testmatrixDTM, 2, convert_count)

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Journal_group CLASS VECTOR
model2 <- naiveBayes(sms_train,as.factor(traindata$cluster));

# PREDICTION
resultsRECLUS2 <- predict(model2, newdata=sms_test)

Recluster <- data.frame(Sample_data1$recipe)
Recluster$NewCluster <- resultsRECLUS2
Recluster$OriginalCluster <- Sample_data1$cluster
Recluster$ingredients <- Sample_data1$ingredients
write.csv(Recluster, "NB_recluster.csv")

comptable <- table(resultsRECLUS2, Sample_data1$cluster)
comptabledf <- as.data.frame(comptable)
ClusClassification <- reshape(comptabledf, idvar ="resultsRECLUS2", timevar =  "Var2", direction = "wide")

rownames(ClusClassification) <- paste("Cluster",as.factor(ClusClassification$resultsRECLUS2))
ClusClassification <- ClusClassification[,-1]

names(ClusClassification) <- c("test_data_Cluster1", "test_data_Cluster2", "test_data_Cluster3",
                               "test_data_Cluster4", "test_data_Cluster5")
Freq.Matrix <- as.matrix(ClusClassification)


#saving confusion matrix output. 
save(Freq.Matrix,  file = "Freq_largeclus.Rda")

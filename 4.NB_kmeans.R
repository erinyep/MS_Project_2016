library('e1071');
library('SparseM');
library('tm');

setwd("~/Bellarmine/Text Mining")
load("dataClusNB.Rda")
Sample_data <- dataClusNB
Sample_data$ingredients <- gsub(",", " ", Sample_data$ingredients)
Sample_data$ingredients <- gsub('"', " ", Sample_data$ingredients)
Sample_data$ingredients <- gsub("_", " ", Sample_data$ingredients)
Sample_data$ingredients <- gsub("-", " ", Sample_data$ingredients)

#Drop levels removes all clusters that have zero predictions, caused an issue when trying to create a confusion matrix. 
Sample_data$cluster <- droplevels(as.factor(Sample_data$cluster))
#Split Train/Test Sets
set.seed(1234)  #For replication purposes
rand <- rbinom(nrow(Sample_data), 1, .7)
Sample_data$partition <- "test"
Sample_data$partition[rand==1] <- "train"
Sample_data$partition <- as.factor(Sample_data$partition)
traindata <- Sample_data[Sample_data$partition=="train",]
testdata <- Sample_data[Sample_data$partition=="test",]

#This creates corpus and keeps the ID. 
myReader <- readTabular(mapping=list(content="ingredients", id="recipe"))
traincorp <- Corpus(DataframeSource(traindata), readerControl=list(reader=myReader))
testcorp <- Corpus(DataframeSource(testdata), readerControl=list(reader=myReader))

# Transforming "traincorpus" and "testcorpus".
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

#Create Document Term Matrix
trainmatrixDTM <- (DocumentTermMatrix(traincorp))
testmatrixDTM <-  (DocumentTermMatrix(testcorp))

#Changing all proportions greater than zero to a 1, also converting numeric to factors. 
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c(0, 1))
}

sms_train <- apply(trainmatrixDTM, 2, convert_count)
sms_test <- apply(testmatrixDTM, 2, convert_count)

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Journal_group CLASS VECTOR
model <- naiveBayes(sms_train,as.factor(traindata$cluster));

# PREDICTION
resultsRECLUS <- predict(model, newdata=sms_test)
#Take Prediction Results and make them into a Confusion Matrix, IN ONLY 6 EASY STEPS!
comptable <- table(resultsRECLUS, testdata$cluster)
comptabledf <- data.frame(comptable)
ConfusionMatrix <- reshape(comptabledf, idvar ="resultsRECLUS", timevar =  "Var2", direction = "wide")
rownames(ConfusionMatrix) <- ConfusionMatrix[,1]
ConfMatx <- ConfusionMatrix[,-1]
ConfMatx1 <- as.matrix(ConfMatx)

#Misclassification Rate. 
cat("The Misclassification Rate is", (1-(sum(diag(ConfMatx1))/sum(ConfMatx1))))

#Viewing Confusion Matrix
View(ConfMatx1)

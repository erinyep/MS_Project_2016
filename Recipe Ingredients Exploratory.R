#Pull in recipe Data from Rdata
#load("C:/Users/Erin/Desktop/R Stuff/RecipeData-1.Rda")
yummly.data <- data[,1:2] #don't want cuisines

#Clean up ingredients list
yummly.data[1:3,] #Look to compare
yummly.data.t <- yummly.data #Create test group

##Add underscores to each individual recipe with spaces in it
#Leave a different version without underscores to test in separate col
yummly.data.t$concat.ingredients <- gsub(" ","_",yummly.data.t$ingredients)
yummly.data.t$concat.ingredients <- gsub(",_",",",yummly.data.t$concat.ingredients)
yummly.data.t[1:3,] #Looks good


#Look at frequency of all unique ingredients
library(tm)
myReader1 <- readTabular(mapping=list(content="ingredients", id="recipe"))

### create the corpus with document IDs
corpus.ingredients <- Corpus(DataframeSource(data), readerControl=list(reader=myReader1))

#corpus.ingredients <- 
  #Corpus(VectorSource(gsub(","," ",yummly.data.t$ingredients)))
dtm <- DocumentTermMatrix(corpus.ingredients)#Create dtm
#We then take the column sums of a matrix, 
#which will give us a named vector.
frequency.dtm <- colSums(as.matrix(dtm))
#And now we can sort this vector to see the most frequently used words:
frequency <- sort(frequency.dtm, decreasing=TRUE)
head(frequency)
freq.df <- as.data.frame(frequency)
freq.df$ingredient <- row.names(freq.df)
freq.df <- as.data.frame(freq.df, row.names = 1:length(freq.df$frequency))

#Do frequencies for the underscored ingredients
corpus.ingredients.concat <- 
  Corpus(VectorSource(gsub(","," ",yummly.data.t$concat.ingredients)))
dtm.concat <- DocumentTermMatrix(corpus.ingredients.concat)#Create dtm
#We then take the column sums of a matrix, 
#which will give us a named vector.
frequency.concat.dtm <- colSums(as.matrix(dtm.concat))
#And now we can sort this vector to see the most frequently used words:
frequency.concat <- sort(frequency.concat.dtm, decreasing=TRUE)
head(frequency.concat)
freq.concat.df <- as.data.frame(frequency.concat)
freq.concat.df$ingredient.concat <- row.names(freq.concat.df)
freq.concat.df <- 
  as.data.frame(freq.concat.df, row.names = 1:length(freq.concat.df$frequency))

#Look at ingredients for concat list and non-concat list
write.csv(freq.concat.df,"ingredients-concat-freq.csv", row.names = TRUE)
write.csv(freq.df,"freq-df.csv", row.names = TRUE)#Non-concat ingredients have errors

#Lets try doing corpus clean commangs on non-concat ingredients
# Clean corpus
corpus.ingredients2 <- tm_map(corpus.ingredients, content_transformer(tolower))
corpus.ingredients2 <- tm_map(corpus.ingredients, removeNumbers)
corpus.ingredients2 <- tm_map(corpus.ingredients, removeWords, stopwords("english"))
corpus.ingredients2 <- tm_map(corpus.ingredients, removePunctuation)
corpus.ingredients2 <- tm_map(corpus.ingredients, stripWhitespace)

corpus.ingredients3 <- Corpus(DataframeSource(data), readerControl=list(reader=myReader1))
corpus.ingredients3 <- x<-gsub('s"', "", corpus.ingredients3)

#remove other end s
corpus.ingredients3<-gsub("s "," ",corpus.ingredients3)

#remove punctuation
corpus.ingredients3<-gsub("[[:punct:]]", "", corpus.ingredients3)

#check the work
x
corpus.ingredients3 <- tm_map(corpus.ingredients3, stripWhitespace)


#Do we need stemming?
#corpus.ingredients2 <- tm_map(corpus.ingredients, stemDocument, language = "english")
#corpus.ingredients2 <- tm_map(corpus.ingredients, stemDocument)

#Lets create a dtm of cleaned, non-concat ingredients
dtm2 <- DocumentTermMatrix(corpus.ingredients3)#Create dtm
#We then take the column sums of a matrix, 
#which will give us a named vector.
frequency.dtm2 <- colSums(as.matrix(dtm2))
#And now we can sort this vector to see the most frequently used words:
frequency2 <- sort(frequency.dtm2, decreasing=TRUE)
head(frequency2)
freq.df.2 <- as.data.frame(frequency2)
freq.df.2$ingredient <- row.names(freq.df.2)
freq.df.2 <- as.data.frame(freq.df.2, row.names = 1:length(freq.df.2$frequency))
#this doesn't look promising but there are less ingredients
write.csv(freq.df.2,"freq-df2.csv", row.names = TRUE)#Non-concat ingredients have errors

#What does the TermDocumentMatrix for this corpus look like?
tdm <- TermDocumentMatrix(corpus.ingredients2)
#We then take the column sums of a matrix, 
#which will give us a named vector.
frequency.tdm <- colSums(as.matrix(tdm))
#And now we can sort this vector, but why?
#Not sure I understand how "TDM" works
frequency.tdm <- sort(frequency.tdm, decreasing=TRUE)
head(frequency.tdm)
freq.df.tdm <- as.data.frame(frequency.tdm)
freq.df.tdm$ingredient <- row.names(freq.df.tdm)
freq.df.tdm <- as.data.frame(freq.df.tdm, row.names = 1:length(freq.df.tdm$frequency))
#not sure if we want or need frequencies for TDM, but TDM itself
#might be useful
comptable <- table(freq.df.tdm)
comptabledf <- data.frame(comptable)
ConfusionMatrix <- reshape(comptabledf, idvar ="frequency.tdm", timevar =  "ingredient", direction = "wide")

rownames(ConfusionMatrix) <- ConfusionMatrix[,1]
ConfMatx <- ConfusionMatrix[,-1]
ConfMatx1 <- as.matrix(ConfMatx)



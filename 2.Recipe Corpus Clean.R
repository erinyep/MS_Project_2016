library(tm)
### The Beer Winners
# Run this code after uploading RecipeData.Rda
# Or if you ran Recipe Scraping-1

### Code to tag the ID to the document
myReader1 <- readTabular(mapping=list(content="ingredients", id="recipe"))

### Remove punctuation and adding spacing for preprocessing
data.clean <- data
data.clean$ingredients<-gsub('"', "", data.clean$ingredients)
data.clean$ingredients<-gsub(',', " ", data.clean$ingredients)
data.clean$ingredients<-gsub("_"," ",data.clean$ingredients)
data.clean$ingredients<-gsub("-","",data.clean$ingredients)

### Remove words that show up in ingredients list that are inefficient
data.clean$ingredients<-gsub("Kraft","",data.clean$ingredients)
data.clean$ingredients<-gsub("freshly","",data.clean$ingredients)
data.clean$ingredients<-gsub("fresh","",data.clean$ingredients)
data.clean$ingredients<-gsub("yolk","",data.clean$ingredients)
data.clean$ingredients<-gsub("ground","",data.clean$ingredients)
data.clean$ingredients<-gsub("dried","",data.clean$ingredients)
data.clean$ingredients<-gsub("extravirgin","",data.clean$ingredients)
data.clean$ingredients<-gsub("kosher","",data.clean$ingredients)
data.clean$ingredients<-gsub("unsalted","",data.clean$ingredients)
data.clean$ingredients<-gsub("allpurpose","",data.clean$ingredients)
data.clean$ingredients<-gsub("granulated","",data.clean$ingredients)
data.clean$ingredients<-gsub("chopped","",data.clean$ingredients)
data.clean$ingredients<-gsub("shredded","",data.clean$ingredients)
data.clean$ingredients<-gsub("light","",data.clean$ingredients)
data.clean$ingredients<-gsub("crushed","",data.clean$ingredients)
data.clean$ingredients<-gsub("yoghurt","yogurt",data.clean$ingredients)
data.clean$ingredients<-gsub("skinless","",data.clean$ingredients)
data.clean$ingredients<-gsub("nonfat","",data.clean$ingredients)
data.clean$ingredients<-gsub("lowfat","",data.clean$ingredients)
data.clean$ingredients<-gsub("large","",data.clean$ingredients)
data.clean$ingredients<-gsub("baby","",data.clean$ingredients)
data.clean$ingredients<-gsub("minced","",data.clean$ingredients)
data.clean$ingredients<-gsub("melted","",data.clean$ingredients)
data.clean$ingredients<-gsub("whole","",data.clean$ingredients)
data.clean$ingredients<-gsub("diced","",data.clean$ingredients)
data.clean$ingredients<-gsub("boneless","",data.clean$ingredients)
data.clean$ingredients<-gsub("frozen","",data.clean$ingredients)
data.clean$ingredients<-gsub("dark","",data.clean$ingredients)
data.clean$ingredients<-gsub("reduced","",data.clean$ingredients)
data.clean$ingredients<-gsub("canned","",data.clean$ingredients)
data.clean$ingredients<-gsub("sliced","",data.clean$ingredients)
data.clean$ingredients<-gsub("low","",data.clean$ingredients)
data.clean$ingredients<-gsub("sodium","",data.clean$ingredients)
data.clean$ingredients<-gsub("extra","",data.clean$ingredients)
data.clean$ingredients<-gsub("virgin","",data.clean$ingredients)


### Corpus clean up tasks
corpus <- Corpus(DataframeSource(data.clean),readerControl=list(reader=myReader1))
corpus <- tm_map(corpus, removePunctuation)

### Create dtm to review
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)

### View updated frequency and create dataframe
View(frequency)
freq.df <- as.data.frame(frequency)
freq.df$ingredient <- row.names(freq.df)
freq.df <- as.data.frame(freq.df, row.names = 1:length(freq.df$frequency))
View(freq.df)

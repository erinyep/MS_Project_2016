### The Beer Winners
# Run this code after uploading RecipeData.Rda
# Or if you ran Recipe Scraping-1

### Code to tag the ID to the document
### Will be important for our overall project
myReader1 <- readTabular(mapping=list(content="ingredients", id="recipe"))
myReaderCuisine <- readTabular(mapping=list(content="ingredients", id="cuisine"))

### create the corpus with document IDs
datadtm <- data
datadtm$ingredients<-gsub('"', "", datadtm$ingredients)
datadtm$ingredients<-gsub(',', " ", datadtm$ingredients)

### Corpus clean up tasks
ingredient_text <- paste(datadtm$ingredients, collapse=" ")
ingredient_source <- VectorSource(ingredient_text)
corpus <- Corpus(ingredient_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

### Create dtm to review
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
View(frequency)
head(frequency)
View(dtm2)

### Remove words that show up in ingredients list that are inefficient
datadtm$ingredients<-gsub("-","",datadtm$ingredients)
datadtm$ingredients<-gsub("_","",datadtm$ingredients)
datadtm$ingredients<-gsub("kraft","",datadtm$ingredients)
datadtm$ingredients<-gsub("freshly","",datadtm$ingredients)
datadtm$ingredients<-gsub("fresh","",datadtm$ingredients)
datadtm$ingredients<-gsub("yolk","",datadtm$ingredients)
datadtm$ingredients<-gsub("ground","",datadtm$ingredients)
datadtm$ingredients<-gsub("dried","",datadtm$ingredients)
datadtm$ingredients<-gsub("extravirgin","",datadtm$ingredients)
datadtm$ingredients<-gsub("kosher","",datadtm$ingredients)
datadtm$ingredients<-gsub("unsalted","",datadtm$ingredients)
datadtm$ingredients<-gsub("allpurpose","",datadtm$ingredients)
datadtm$ingredients<-gsub("granulated","",datadtm$ingredients)
datadtm$ingredients<-gsub("bonelessskinless","",datadtm$ingredients)
datadtm$ingredients<-gsub("chopped","",datadtm$ingredients)
datadtm$ingredients<-gsub("blackpepper","pepper",datadtm$ingredients)
datadtm$ingredients<-gsub("shredded","",datadtm$ingredients)
datadtm$ingredients<-gsub("light","",datadtm$ingredients)
datadtm$ingredients<-gsub("crushed","",datadtm$ingredients)
datadtm$ingredients<-gsub("yoghurt","yogurt",datadtm$ingredients)
datadtm$ingredients<-gsub("skinless","",datadtm$ingredients)
datadtm$ingredients<-gsub("nonfat","",datadtm$ingredients)
datadtm$ingredients<-gsub("lowfat","",datadtm$ingredients)
datadtm$ingredients<-gsub("large","",datadtm$ingredients)
datadtm$ingredients<-gsub("baby","",datadtm$ingredients)
datadtm$ingredients<-gsub("minced","",datadtm$ingredients)
datadtm$ingredients<-gsub("melted","",datadtm$ingredients)
datadtm$ingredients<-gsub("whole","",datadtm$ingredients)
datadtm$ingredients<-gsub("diced","",datadtm$ingredients)
datadtm$ingredients<-gsub("boneless","",datadtm$ingredients)
datadtm$ingredients<-gsub("frozen","",datadtm$ingredients)
datadtm$ingredients<-gsub("dark","",datadtm$ingredients)
datadtm$ingredients<-gsub("reducedsodium","",datadtm$ingredients)
datadtm$ingredients<-gsub("canned","",datadtm$ingredients)
datadtm$ingredients<-gsub("sliced","",datadtm$ingredients)
datadtm$ingredients<-gsub("lowsodium","",datadtm$ingredients)

#View updated frequency and create dataframe
View(frequency)
freq.df <- as.data.frame(frequency)
freq.df$ingredient <- row.names(freq.df)
freq.df <- as.data.frame(freq.df, row.names = 1:length(freq.df$frequency))

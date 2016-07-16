library(tidyr)
library(dplyr)
library(yummlyr)
library(stringr)
library(tm)
setwd("c:/temp")
getwd()

#required credentials for YummlyR API
save_yummly_credentials("934827d6", "4b8a14bc124c8f1d648a518da46f6879")

###Build a data frame of cuisines
cuisine <- get_metadata("cuisine")

###Build a list of cuisine types
cuisinename<- cuisine[,2]

###Build a list of 13 thousand recipes
recipes <- lapply(cuisinename, function (x) search_recipes(search_words="", allowed_cuisine=x, max_results = 500))

###recipes has 26 lists in it, and inside recipes[[i]]$matches$recipeName is the name of the recipe
###                                inside recipes[[i]]$matches$ingredients[j] are the 
###                                                            corresponding ingredients that we need     

###Build a column of recipe titles
fullrecipes<- as.data.frame(unlist(lapply(recipes, function(x) x$matches$recipeName)))
z<-1
for(i in 1:26){
  list1<-recipes[[i]]
  for(j in 1:500){
    list2<-list1$matches$recipeName[j]
    fullrecipes$recipe[z]<-list2
    z<-z+1
    print(z)
  }
}

###Build a column of ingredients
#fullrecipes$ingredients<- lapply(recipes, function(x) x$matches$ingredients)
### WRONG
### lists all ingredients used in the first 400 recipes in the first row, second 400 in row 2,
###  begins repeating at row 27
z<-1
for(i in 1:26){
  list1<-recipes[[i]]
  for(j in 1:500){
    list2<-list1$matches$ingredients[j]
    fullrecipes$ingredients[z]<-list2
    z<-z+1
    print(z)
  }
}

###Build a column of cuisine's
z<-1
for(i in 1:26){
  list1<-recipes[[i]]
  for(j in 1:500){
    list2<-list1$matches$attributes$cuisine[j]
    fullrecipes$cuisine[z]<-list2
    z<-z+1
    print(z)
  }
}

#drop the results of the apply and remove NA's
fullrecipes<-fullrecipes[,c(2,3,4)]
data <- fullrecipes[!is.na(fullrecipes$recipe),]


###Put ingredients into character form from lists
data$ingredients<-as.character(data$ingredients)
data$recipe<- as.character(data$recipe)
data$cuisine<- as.character(data$cuisine)

###Remove the c(...) from the beginning and end of the lists
data$ingredients<- substring(data$ingredients, 3)
data$ingredients<-gsub("\\)","",data$ingredients)
#remove spaces
data$ingredients <- gsub(" ","_",data$ingredients)
data$ingredients <- gsub(",_",",",data$ingredients)

#clean the Cuisine Column
for (i in 1:12982){
  if(substring(data$cuisine[i],1,1)=="c"){
    data$cuisine[i]<- substring(data$cuisine[i], 3)
    data$cuisine[i]<-gsub("\\)","",data$cuisine[i]) 
  }
}
data$cuisine<-gsub("[[:punct:]]", "", data$cuisine)
data$cuisine<-gsub('([A-z]+) .*', '\\1', data$cuisine)

#save data for sharing
save(data, file="RecipeData.Rda")

##########################################################
#Create a backup for redundancy
data1<-data
View(data1)
##########################################################

#Create a new data frame and remove punctuation
datatdm<-data
datatdm$ingredients<-gsub('"', "", datatdm$ingredients)
datatdm$ingredients<-gsub(',', " ", datatdm$ingredients)

#Create a document for word frequency
ingredient_text <- paste(datatdm$ingredients, collapse=" ")
ingredient_source <- VectorSource(ingredient_text)
corpus <- Corpus(ingredient_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
View(frequency)

#Make changes to ingredients based on visual overveiw of Frequency
datatdm$ingredients<-gsub("-","",datatdm$ingredients)
datatdm$ingredients<-gsub("_","",datatdm$ingredients)
datatdm$ingredients<-gsub("kraft","",datatdm$ingredients)
datatdm$ingredients<-gsub("freshly","",datatdm$ingredients)
datatdm$ingredients<-gsub("fresh","",datatdm$ingredients)
datatdm$ingredients<-gsub("yolk","",datatdm$ingredients)
datatdm$ingredients<-gsub("ground","",datatdm$ingredients)
datatdm$ingredients<-gsub("dried","",datatdm$ingredients)
datatdm$ingredients<-gsub("extravirgin","",datatdm$ingredients)
datatdm$ingredients<-gsub("kosher","",datatdm$ingredients)
datatdm$ingredients<-gsub("unsalted","",datatdm$ingredients)
datatdm$ingredients<-gsub("allpurpose","",datatdm$ingredients)
datatdm$ingredients<-gsub("granulated","",datatdm$ingredients)
datatdm$ingredients<-gsub("bonelessskinless","",datatdm$ingredients)
datatdm$ingredients<-gsub("chopped","",datatdm$ingredients)
datatdm$ingredients<-gsub("blackpepper","pepper",datatdm$ingredients)
datatdm$ingredients<-gsub("shredded","",datatdm$ingredients)
datatdm$ingredients<-gsub("light","",datatdm$ingredients)
datatdm$ingredients<-gsub("crushed","",datatdm$ingredients)
datatdm$ingredients<-gsub("yoghurt","yogurt",datatdm$ingredients)
datatdm$ingredients<-gsub("skinless","",datatdm$ingredients)
datatdm$ingredients<-gsub("nonfat","",datatdm$ingredients)
datatdm$ingredients<-gsub("lowfat","",datatdm$ingredients)
datatdm$ingredients<-gsub("large","",datatdm$ingredients)
datatdm$ingredients<-gsub("baby","",datatdm$ingredients)
datatdm$ingredients<-gsub("minced","",datatdm$ingredients)
datatdm$ingredients<-gsub("melted","",datatdm$ingredients)
datatdm$ingredients<-gsub("whole","",datatdm$ingredients)
datatdm$ingredients<-gsub("diced","",datatdm$ingredients)
datatdm$ingredients<-gsub("boneless","",datatdm$ingredients)
datatdm$ingredients<-gsub("frozen","",datatdm$ingredients)
datatdm$ingredients<-gsub("dark","",datatdm$ingredients)
datatdm$ingredients<-gsub("reducedsodium","",datatdm$ingredients)
datatdm$ingredients<-gsub("canned","",datatdm$ingredients)
datatdm$ingredients<-gsub("sliced","",datatdm$ingredients)
datatdm$ingredients<-gsub("lowsodium","",datatdm$ingredients)

#Check all changes to see if you are happy with the results
ingredient_text <- paste(datatdm$ingredients, collapse=" ")
ingredient_source <- VectorSource(ingredient_text)
corpus <- Corpus(ingredient_source)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
View(frequency)

############################################################
############################################################
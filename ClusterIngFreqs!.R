############Frequency of ingredients in Clusters
library(tm)
myReader1 <- readTabular(mapping=list(content="ingredients", id="recipe"))

#Create df for each cluster
df.clus1 <- dataClusNB[dataClusNB$cluster==1,]
df.clus2 <- dataClusNB[dataClusNB$cluster==2,]
df.clus3 <- dataClusNB[dataClusNB$cluster==3,]
df.clus4 <- dataClusNB[dataClusNB$cluster==4,]
df.clus5 <- dataClusNB[dataClusNB$cluster==5,]
df.clus6 <- dataClusNB[dataClusNB$cluster==6,]
df.clus7 <- dataClusNB[dataClusNB$cluster==7,]
df.clus9 <- dataClusNB[dataClusNB$cluster==9,]
df.clus10 <- dataClusNB[dataClusNB$cluster==10,]
df.clus11 <- dataClusNB[dataClusNB$cluster==11,]
df.clus12 <- dataClusNB[dataClusNB$cluster==12,]
df.clus14 <- dataClusNB[dataClusNB$cluster==14,]
df.clus15 <- dataClusNB[dataClusNB$cluster==15,]
df.clus16 <- dataClusNB[dataClusNB$cluster==16,]
df.clus17 <- dataClusNB[dataClusNB$cluster==17,]
df.clus18 <- dataClusNB[dataClusNB$cluster==18,]
df.clus19 <- dataClusNB[dataClusNB$cluster==19,]
df.clus20 <- dataClusNB[dataClusNB$cluster==20,]

corpus.clus1 <- Corpus(DataframeSource(df.clus1), readerControl=list(reader=myReader1))
corpus.clus2 <- Corpus(DataframeSource(df.clus2), readerControl=list(reader=myReader1))
corpus.clus3 <- Corpus(DataframeSource(df.clus3), readerControl=list(reader=myReader1))
corpus.clus4 <- Corpus(DataframeSource(df.clus4), readerControl=list(reader=myReader1))
corpus.clus5 <- Corpus(DataframeSource(df.clus5), readerControl=list(reader=myReader1))
corpus.clus6 <- Corpus(DataframeSource(df.clus6), readerControl=list(reader=myReader1))
corpus.clus7 <- Corpus(DataframeSource(df.clus7), readerControl=list(reader=myReader1))
corpus.clus9 <- Corpus(DataframeSource(df.clus9), readerControl=list(reader=myReader1))
corpus.clus10 <- Corpus(DataframeSource(df.clus10), readerControl=list(reader=myReader1))
corpus.clus11 <- Corpus(DataframeSource(df.clus11), readerControl=list(reader=myReader1))
corpus.clus12 <- Corpus(DataframeSource(df.clus12), readerControl=list(reader=myReader1))
corpus.clus14 <- Corpus(DataframeSource(df.clus14), readerControl=list(reader=myReader1))
corpus.clus15 <- Corpus(DataframeSource(df.clus15), readerControl=list(reader=myReader1))
corpus.clus16 <- Corpus(DataframeSource(df.clus16), readerControl=list(reader=myReader1))
corpus.clus17 <- Corpus(DataframeSource(df.clus17), readerControl=list(reader=myReader1))
corpus.clus18 <- Corpus(DataframeSource(df.clus18), readerControl=list(reader=myReader1))
corpus.clus19 <- Corpus(DataframeSource(df.clus19), readerControl=list(reader=myReader1))
corpus.clus20 <- Corpus(DataframeSource(df.clus20), readerControl=list(reader=myReader1))

#We then take the column sums of a matrix, 
#which will give us a named vector.
dtm1 <- DocumentTermMatrix(corpus.clus1)
frequency.dtm.clus1 <- colSums(as.matrix(dtm1))
dtm2 <- DocumentTermMatrix(corpus.clus2)
frequency.dtm.clus2 <- colSums(as.matrix(dtm2))
dtm3 <- DocumentTermMatrix(corpus.clus3)
frequency.dtm.clus3 <- colSums(as.matrix(dtm3))
dtm4 <- DocumentTermMatrix(corpus.clus4)
frequency.dtm.clus4 <- colSums(as.matrix(dtm4))
dtm5 <- DocumentTermMatrix(corpus.clus5)
frequency.dtm.clus5 <- colSums(as.matrix(dtm5))
dtm6 <- DocumentTermMatrix(corpus.clus6)
frequency.dtm.clus6 <- colSums(as.matrix(dtm6))
dtm7 <- DocumentTermMatrix(corpus.clus7)
frequency.dtm.clus7 <- colSums(as.matrix(dtm7))
dtm9 <- DocumentTermMatrix(corpus.clus9)
frequency.dtm.clus9 <- colSums(as.matrix(dtm9))
dtm10 <- DocumentTermMatrix(corpus.clus10)
frequency.dtm.clus10 <- colSums(as.matrix(dtm10))
dtm11 <- DocumentTermMatrix(corpus.clus11)
frequency.dtm.clus11 <- colSums(as.matrix(dtm11))
dtm12 <- DocumentTermMatrix(corpus.clus12)
frequency.dtm.clus12 <- colSums(as.matrix(dtm12))
dtm14 <- DocumentTermMatrix(corpus.clus14)
frequency.dtm.clus14 <- colSums(as.matrix(dtm14))
dtm15 <- DocumentTermMatrix(corpus.clus15)
frequency.dtm.clus15 <- colSums(as.matrix(dtm15))
dtm16 <- DocumentTermMatrix(corpus.clus16)
frequency.dtm.clus16 <- colSums(as.matrix(dtm16))
dtm17 <- DocumentTermMatrix(corpus.clus17)
frequency.dtm.clus17 <- colSums(as.matrix(dtm17))
dtm18 <- DocumentTermMatrix(corpus.clus18)
frequency.dtm.clus18 <- colSums(as.matrix(dtm18))
dtm19 <- DocumentTermMatrix(corpus.clus19)
frequency.dtm.clus19 <- colSums(as.matrix(dtm19))
dtm20 <- DocumentTermMatrix(corpus.clus20)
frequency.dtm.clus20 <- colSums(as.matrix(dtm20))

#And now we can sort this vector to see the most frequently used words:
frequency1 <- sort(frequency.dtm.clus1, decreasing=TRUE)
freq.df1 <- as.data.frame(frequency1)
freq.df1$ingredient <- row.names(freq.df1)
freq.df1 <- as.data.frame(freq.df1, row.names = 1:length(freq.df1$frequency))
####
frequency2 <- sort(frequency.dtm.clus2, decreasing=TRUE)
freq.df2 <- as.data.frame(frequency2)
freq.df2$ingredient <- row.names(freq.df2)
freq.df2 <- as.data.frame(freq.df2, row.names = 1:length(freq.df2$frequency))
####
frequency3 <- sort(frequency.dtm.clus3, decreasing=TRUE)
freq.df3 <- as.data.frame(frequency3)
freq.df3$ingredient <- row.names(freq.df3)
freq.df3 <- as.data.frame(freq.df3, row.names = 1:length(freq.df3$frequency))
####
frequency4 <- sort(frequency.dtm.clus4, decreasing=TRUE)
freq.df4 <- as.data.frame(frequency4)
freq.df4$ingredient <- row.names(freq.df4)
freq.df4 <- as.data.frame(freq.df4, row.names = 1:length(freq.df4$frequency))
####
frequency5 <- sort(frequency.dtm.clus5, decreasing=TRUE)
freq.df5 <- as.data.frame(frequency5)
freq.df5$ingredient <- row.names(freq.df5)
freq.df5 <- as.data.frame(freq.df5, row.names = 1:length(freq.df5$frequency))
####
frequency6 <- sort(frequency.dtm.clus6, decreasing=TRUE)
freq.df6 <- as.data.frame(frequency6)
freq.df6$ingredient <- row.names(freq.df6)
freq.df6 <- as.data.frame(freq.df6, row.names = 1:length(freq.df6$frequency))
####
frequency7 <- sort(frequency.dtm.clus7, decreasing=TRUE)
freq.df7 <- as.data.frame(frequency7)
freq.df7$ingredient <- row.names(freq.df7)
freq.df7 <- as.data.frame(freq.df7, row.names = 1:length(freq.df7$frequency))
####
frequency9 <- sort(frequency.dtm.clus9, decreasing=TRUE)
freq.df9 <- as.data.frame(frequency9)
freq.df9$ingredient <- row.names(freq.df9)
freq.df9 <- as.data.frame(freq.df9, row.names = 1:length(freq.df9$frequency))
####
frequency10 <- sort(frequency.dtm.clus10, decreasing=TRUE)
freq.df10 <- as.data.frame(frequency10)
freq.df10$ingredient <- row.names(freq.df10)
freq.df10 <- as.data.frame(freq.df10, row.names = 1:length(freq.df10$frequency))
####
frequency11 <- sort(frequency.dtm.clus11, decreasing=TRUE)
freq.df11 <- as.data.frame(frequency11)
freq.df11$ingredient <- row.names(freq.df11)
freq.df11 <- as.data.frame(freq.df11, row.names = 1:length(freq.df11$frequency))
####
frequency12 <- sort(frequency.dtm.clus12, decreasing=TRUE)
freq.df12 <- as.data.frame(frequency12)
freq.df12$ingredient <- row.names(freq.df12)
freq.df12 <- as.data.frame(freq.df12, row.names = 1:length(freq.df12$frequency))
####
frequency14 <- sort(frequency.dtm.clus14, decreasing=TRUE)
freq.df14 <- as.data.frame(frequency14)
freq.df14$ingredient <- row.names(freq.df14)
freq.df14 <- as.data.frame(freq.df14, row.names = 1:length(freq.df14$frequency))
####
frequency15 <- sort(frequency.dtm.clus15, decreasing=TRUE)
freq.df15 <- as.data.frame(frequency15)
freq.df15$ingredient <- row.names(freq.df15)
freq.df15 <- as.data.frame(freq.df15, row.names = 1:length(freq.df15$frequency))
####
frequency16 <- sort(frequency.dtm.clus16, decreasing=TRUE)
freq.df16 <- as.data.frame(frequency16)
freq.df16$ingredient <- row.names(freq.df16)
freq.df16 <- as.data.frame(freq.df16, row.names = 1:length(freq.df16$frequency))
####
frequency17 <- sort(frequency.dtm.clus17, decreasing=TRUE)
freq.df17 <- as.data.frame(frequency17)
freq.df17$ingredient <- row.names(freq.df17)
freq.df17 <- as.data.frame(freq.df17, row.names = 1:length(freq.df17$frequency))
####
frequency18 <- sort(frequency.dtm.clus18, decreasing=TRUE)
freq.df18 <- as.data.frame(frequency18)
freq.df18$ingredient <- row.names(freq.df18)
freq.df18 <- as.data.frame(freq.df18, row.names = 1:length(freq.df18$frequency))
####
frequency19 <- sort(frequency.dtm.clus19, decreasing=TRUE)
freq.df19 <- as.data.frame(frequency19)
freq.df19$ingredient <- row.names(freq.df19)
freq.df19 <- as.data.frame(freq.df19, row.names = 1:length(freq.df19$frequency))
####
frequency20 <- sort(frequency.dtm.clus20, decreasing=TRUE)
freq.df20 <- as.data.frame(frequency20)
freq.df20$ingredient <- row.names(freq.df20)
freq.df20 <- as.data.frame(freq.df20, row.names = 1:length(freq.df20$frequency))
####

### Create graphs
library(ggplot2)
ggplot(freq.df4[1:15,], aes(x=frequency4, y=reorder(ingredient, frequency4))) + 
  geom_point(size = 3) +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color='grey60', linetype='dashed'))

setwd("/Users/wangshuyan/Documents/yelp_dataset_challenge_academic_dataset")

#Summary of Variables
# X1 funny upvotes
# X2 useful upvotes
# X3 cool upvotes
# X4 user id
# X5 review id
# X6 stars
# X7 date
# X8 review text
# X9 type
# X10 business id
# X11 word count of review text
# X12 review text (Corpus format)
# X13 most likely topic number
# X14 number of top topics(threshold 0.01)

#Packages and Libraries
install.packages("rjson")
install.packages("ggplot2")
install.packages(c("RTextTools","topicmodels"))
install.packages("tm")
install.packages("hexbin")
install.packages("car")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("ggplot2")
install.packages("reshape")
install.packages("gplots")

library(rjson)
library(ggplot2)
library(RTextTools)
library(topicmodels)
library(tm)
library(hexbin)
library(car)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(reshape)
library(gplots)

#Read in external json data
path <- "/Users/wangshuyan/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
c <- file(path, "r")
l <- readLines(c, -1L)
json <- lapply(X=l, fromJSON)
class(json)

#Save raw data to r
save(json, file="jsondata")

main <- data.frame(matrix(unlist(json),nrow=1125458,byrow=T))
save(main, file = "maindata")

# Testing adding X11
# length(main$X1)
# main$X11 <- NULL
# length(main$X11)
# main$X11 <- rep(0, times=1125458)

#Copying the data
wkmain <- main

#Converting format
wkmain$X1 <- as.numeric(as.character(main$X1))
wkmain$X2 <- as.numeric(as.character(main$X2))
wkmain$X3 <- as.numeric(as.character(main$X3))
wkmain$X6 <- as.numeric(as.character(main$X6))
wkmain$X7 <- as.Date(main$X7, format="%Y-%m-%d")
wkmain$X8 <- as.character(wkmain$X8)

#Sanity check
main$X1[1:10]
wkmain$X1[1:10]
main$X2[1:10]
wkmain$X2[1:10]
main$X3[1:10]
wkmain$X3[1:10]
main$X6[1:10]
wkmain$X6[1:10]
main$X7[1:10]
wkmain$X7[1:10]
# main$X8[1:10]
# wkmain$X8[1:10]

save(wkmain, file="wkdata")
load("wkdata")

#New Variable X11, the word count of each text
wkmain$X11 <- sapply(gregexpr("\\S+", wkmain$X8), length)
summary(wkmain$X11)

save(wkmain, file="wkmain")
load("wkmain")

#Summary
summary(wkmain$X1)
summary(wkmain$X2)
summary(wkmain$X3)
summary(wkmain$X6)
summary(wkmain$X7)
sd(wkmain$X1)
sd(wkmain$X2)
sd(wkmain$X3)
sd(wkmain$X6)
sd(wkmain$X7)

class(wkmain$X7)

#Create LDA file to work with
#Keep the data of 2012, keep 10000 reviews (out of 203985) for computation efficiency, selecting randomly 
wk <- wkmain
summary(wk)
wk<-subset(wk,X7>'2011-12-31'&X7<'2013-01-01')
wk<-wk[sample(1:nrow(wk),10000,replace = FALSE),]
length(wk$X8)


# Create corpus from character formatted reviews
wk$X12 <- NULL
wk$X12 <- VCorpus(VectorSource(wk$X8))
class(wk$X12)

save(wk,file="wk")
load("wk")
summary(wk)
dim(wk)


# Create dataframe for review upvotes distribution and plot
wkreview <- data.frame(wk$X1,wk$X2,wk$X3)
wkreviewtemp <- as.data.frame(sapply(wkreview, function(x) table(factor(x, levels = c(0:33), exclude = NULL)))) #maximun number of upvotes is 33
wkreviewtemp$Upvote<-row.names(wkreviewtemp)
colnames(wkreviewtemp)<-c("Funny","Useful","Cool","Upvote")
wkreviewtemp
wkreviewtemp$Upvote<- factor(wkreviewtemp$Upvote, levels=unique(wkreviewtemp$Upvote)) #order x axis as integer instead of character
wkreviewfreq<-melt(wkreviewtemp,id.vars="Upvote")
ggplot(data=wkreviewfreq, aes(Upvote, value, fill=variable)) + geom_bar(stat="identity", position = position_dodge()) + scale_fill_hue(c=70, l=70) + xlab("Number of Upvotes") + ylab("Number of Documents") + labs(title="Number of Documents Sorted by Upvote Counts") + theme(legend.position = c(0.8,0.8))

# Partially maginified
wkreviewtemp <- as.data.frame(sapply(wkreview, function(x) table(factor(x, levels = c(9:29), exclude = NULL)))) #maximun number of upvotes is 33
wkreviewtemp$Upvote<-row.names(wkreviewtemp)
colnames(wkreviewtemp)<-c("Funny","Useful","Cool","Upvote")
wkreviewtemp
wkreviewtemp$Upvote<- factor(wkreviewtemp$Upvote, levels=unique(wkreviewtemp$Upvote)) #order x axis as integer instead of character
wkreviewfreq<-melt(wkreviewtemp,id.vars="Upvote")
ggplot(data=wkreviewfreq, aes(Upvote, value, fill=variable)) + geom_bar(stat="identity", position = position_dodge()) + scale_fill_hue(c=70, l=70) + xlab("Number of Upvotes") + ylab("Number of Documents") + labs(title="Number of Documents Sorted by Upvote Counts (Partially Magnified)") + theme(legend.position = c(0.8,0.8))
ggsave(file = "Number of Documents Sorted by Upvote Counts (Partially Magnified).png")

# Distribution of text length
table(wk$X11)
wk$X11.cut = cut(wk$X11, breaks=seq(0,1000,by=50),right=FALSE)
wktext<-(as.data.frame(table(wk$X11.cut)))
colnames(wktext)<-c("TextLength","Frequency")
wktext$TextLength<-seq(50,1000,by=50)

qplot(test)

ggplot(data=wktext, aes(TextLength, Frequency,fill="Number of Documents")) + geom_point(shape = 1) + scale_fill_hue(c=70, l=70) + xlab("Text Length of Review") + scale_fill_manual(values="#009E73") + ylab("Number of Documents") + labs(title="Distribution of Text Length") + theme(legend.position = c(0.8,0.8)) + geom_smooth()
ggsave(file = "Distribution of Text Length.png")















wkmatrix<-DocumentTermMatrix(wk$X12,control= list(removePunctuation = TRUE, stopwords = TRUE))
save(wkmatrix, file="wkmatrix")
load("wkmatrix")

dim(wkmatrix) #result:10000 34220
inspect(wkmatrix[1:10,1:10])

# Clear out all zero entry
# rowTotals <- apply(wkmatrix,1,sum)

lda <- LDA(wkmatrix,3)
lda100<- LDA(wkmatrix,100)
save(lda100,file="lda100")
load("lda100")


terms(lda100)
class(terms(lda100))
topics(lda100)
length(topics(lda100))
class(topics(lda100)) #interger
wk$X13<-NULL
wk$X13<-as.vector(topics(lda100))
summary(wk$X13)
terms(lda100)
wktopics<-data.frame(Y1<-terms(lda100), Y2<-as.vector(table(wk$X13)))
wktopics
wktopicsorder<-wktopics[order(Y2,decreasing=TRUE),]
wktopicsorder
pal2<-brewer.pal(8,"Dark2")
wordcloud(wktopics$Y1,wktopics$Y2,colors=brewer.pal(8,"Set3"))
ggsave(file = "Wordcloud.png")

table(wk$X13)
plot(sort(table(wk$X13)),col="light blue",xlab="Top Topics",ylab="Number of According Reviews",main="Most frequent topics of 10000 Reviews")

wkgamma <- as.matrix(lda100@gamma) 
wkgamma
class(wkgamma)
summary(wkgamma)
dim(wkgamma)
wk$X14<-rowSums(wkgamma>0.025)
table(wk$X14)


heatmap.2(wkgamma[1:100,], Rowv=FALSE, Colv=FALSE, 
          dendrogram="none", col=c("white","steelblue1","steelblue2","steelblue3","steelblue4","coral","coral1","coral2","coral3","coral4"), breaks=seq(0,0.5,by=0.05),
          key=T, keysize=1.5, density.info="none",       trace="none",xlab="Topics",ylab="Documents",RowSideColors=rainbow(100))





summary(wk$X11)
max(wk$X2[wk$X11<100])

# Use when the matrix is too sparse
# wkmatrixsp<-removeSparseTerms(wkmatrix,0.99)
# dim(wkmatrixsp)
# inspect(wkmatrix[1:20,1:10])

findFreqTerms(wkmatrix,1000)
findAssocs(wkmatrix,"pizza",0.2)
findAssocs(wkmatrix,"great",0.2)
findAssocs(wkmatrix,"great",0.2)
findAssocs(wkmatrix,"great",0.2)



wkmonth.funny<-c(
mean(wk$X1[wk$X7<'2012-1-31']),
mean(wk$X1[wk$X7>'2012-1-31'&wk$X7<'2012-2-28']),
mean(wk$X1[wk$X7<'2012-3-31'&wk$X7>'2012-2-28']),
mean(wk$X1[wk$X7>'2012-3-31'&wk$X7<'2012-4-30']),
mean(wk$X1[wk$X7<'2012-5-31'&wk$X7>'2012-4-30']),
mean(wk$X1[wk$X7>'2012-5-31'&wk$X7<'2012-6-30']),
mean(wk$X1[wk$X7<'2012-7-31'&wk$X7>'2012-6-30']),
mean(wk$X1[wk$X7>'2012-7-31'&wk$X7<'2012-8-30']),
mean(wk$X1[wk$X7<'2012-9-30'&wk$X7>'2012-8-31']),
mean(wk$X1[wk$X7>'2012-9-30'&wk$X7<'2012-10-30']),
mean(wk$X1[wk$X7<'2012-11-30'&wk$X7>'2012-10-30']),
mean(wk$X1[wk$X7>'2012-12-1']))
wkmonth.useful<-c(
mean(wk$X2[wk$X7<'2012-1-31']),
mean(wk$X2[wk$X7>'2012-1-31'&wk$X7<'2012-2-28']),
mean(wk$X2[wk$X7<'2012-3-31'&wk$X7>'2012-2-28']),
mean(wk$X2[wk$X7>'2012-3-31'&wk$X7<'2012-4-30']),
mean(wk$X2[wk$X7<'2012-5-31'&wk$X7>'2012-4-30']),
mean(wk$X2[wk$X7>'2012-5-31'&wk$X7<'2012-6-30']),
mean(wk$X2[wk$X7<'2012-7-31'&wk$X7>'2012-6-30']),
mean(wk$X2[wk$X7>'2012-7-31'&wk$X7<'2012-8-30']),
mean(wk$X2[wk$X7<'2012-9-30'&wk$X7>'2012-8-31']),
mean(wk$X2[wk$X7>'2012-9-30'&wk$X7<'2012-10-30']),
mean(wk$X2[wk$X7<'2012-11-30'&wk$X7>'2012-10-30']),
mean(wk$X2[wk$X7>'2012-12-1']))
wkmonth.cool<-c(
mean(wk$X3[wk$X7<'2012-1-31']),
mean(wk$X3[wk$X7>'2012-1-31'&wk$X7<'2012-2-28']),
mean(wk$X3[wk$X7<'2012-3-31'&wk$X7>'2012-2-28']),
mean(wk$X3[wk$X7>'2012-3-31'&wk$X7<'2012-4-30']),
mean(wk$X3[wk$X7<'2012-5-31'&wk$X7>'2012-4-30']),
mean(wk$X3[wk$X7>'2012-5-31'&wk$X7<'2012-6-30']),
mean(wk$X3[wk$X7<'2012-7-31'&wk$X7>'2012-6-30']),
mean(wk$X3[wk$X7>'2012-7-31'&wk$X7<'2012-8-30']),
mean(wk$X3[wk$X7<'2012-9-30'&wk$X7>'2012-8-31']),
mean(wk$X3[wk$X7>'2012-9-30'&wk$X7<'2012-10-30']),
mean(wk$X3[wk$X7<'2012-11-30'&wk$X7>'2012-10-30']),
mean(wk$X3[wk$X7>'2012-12-1']))
month<-month.name[c(1:12)]
wkupvotemonth<-data.frame(month)
wkupvotemonth$funny<-wkmonth.funny
wkupvotemonth$useful<-wkmonth.useful
wkupvotemonth$cool<-wkmonth.cool
summary(wkupvotemonth)

wkupvotemonth$month<- factor(wkupvotemonth$month, levels=unique(wkupvotemonth$month))
wkmonth<-melt(wkupvotemonth,id.vars="month")
ggplot(data= wkmonth, aes(month, value, fill=variable)) + geom_bar(stat="identity", position = position_dodge()) + scale_fill_hue(c=70, l=70) + xlab("Month") + ylab("Number of Upvotes") + labs(title="Average Number of Upvotes by Month") + theme(legend.position = c(0.12,0.91)) +facet_wrap(~variable, ncol=1)
ggsave(file = "Average Number of Upvotes by Month.png")







# regression of world count and upvotes
test1 <- lm(wk$X2 ~ wk$X11, data = wk) #significant positive
test2 <- lm(wk$X2 ~ lda100, data = wk) 
test3 <- lm(wk$X2 ~ wk$X14, data = wk) 
summary(test3)
plot(test3)
plot(jitter(wk$X14),jitter(wk$X2),type="p")#!!!
scatterplot(X2~X14,data=wkreg)#?
smoothScatter(wk$X14,wk$X2)

wkreg<-aggregate(X2 ~ X14, data=wk, FUN="mean")
scatterplot(X2~X14,data=wkreg,xlab="Number of Topic", ylab="Average Upvotes", main="Scatter Plot of Number of Topic and Upvotes")

wkreg1<-aggregate(X1 ~ X14, data=wk, FUN="mean")
wkreg1
scatterplot(X1~X14,data=wkreg1,xlab="Number of Topic", ylab="Average Upvotes", main="Scatter Plot of Number of Topic and Upvotes")



plot(hexbin(wk$X14,wk$X2), xlab="Number of Top Topics",ylab="Useful Upvotes",main="Number of Top Topics and Upvotes") #??

#Read in external user data
path_user <- "/Users/wangshuyan/Documents/yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json"
c_user <- file(path_user, "r")
l_user <- readLines(c, -1L)
user <- lapply(XU=l_user, fromJSON)
class(user)



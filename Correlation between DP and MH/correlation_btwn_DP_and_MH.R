library(tm)
library(ggplot2)
library(gridExtra)
library(gofastr)
library(tm)
# library(gofastr)
# library(Matrix)
library(quanteda)
# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# library(Rgraphviz)
# library(wordcloud)
# library(qdap)
# install.packages("tmcn.word2vec", type = "source", repos = "http://R-Forge.R-project.org")
# library(tmcn.word2vec)
# library(cluster)
# library(widyr)

# load data
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe.Rdata")

# how many values in DP column
1-sum(is.na(total$DP))/nrow(total) #0.173

# how many values in DA column
1-sum(is.na(total$DA))/nrow(total) #1

# how many values in DCOM column
1-sum(is.na(total$DCOM))/nrow(total) #0.999

# how many values in EDAT column
1-sum(is.na(total$EDAT))/nrow(total) #0.999

# how many values in MHDA column
1-sum(is.na(total$MHDA))/nrow(total) #0.999

# how many values in CRDT column
1-sum(is.na(total$CRDT))/nrow(total) #0.999

# add new column of just the year of EDAT
total$EDAT_Year <- format(as.Date(total$EDAT, "%Y-%m-%d"), "%Y")

# look at count of each year
barplot(table(total$EDAT_Year)) #only three years present because data was merged by three datasets uniquely identified by year

#-----------------------------------------------------------------
# text mining. creating a corpus and then a dtm. inspecting dtm since so large
myCorpus <- Corpus(VectorSource(total$MH))  
dtm <- DocumentTermMatrix(myCorpus)
inspect(dtm[1:10, 1:10])

# frequency of each term showing up
##freq <- colSums(as.matrix(dtm))
#freqr <- sum(wf$prop)
wf <- transform(wf, proportion = proportion/217173)
#wf$proportion <- with(wf, proportion/217173)
length(freq) # # of unique terms
ord <- order(freq, decreasing = T)
freq[head(ord, n = 1000)] # shows most counts

# finding most frequent terms (terms that showed up at least 1000 times)
c <- findFreqTerms(dtm, lowfreq = 5000)

# change so make a dtm where hyphens and underscores are accepted
testing2 <- q_dtm_stem(total$MH, docs = total$PMID, to = "tm", keep.hyphen = T)
testing <- q_dtm(total$MH, docs = total$PMID, to = "tm", keep.hyphen = T)
test_78 <- q_dtm(total$MH_78, docs = total$PMID, to = "tm", keep.hyphen = T)
test_80 <- q_dtm(total$MH_80, docs = total$PMID, to = "tm", keep.hyphen = T)
test_83 <- q_dtm(total$MH_83, docs = total$PMID, to = "tm", keep.hyphen = T)

# make frequency tables for all years
freq <- colSums(as.matrix(testing))
freq_78 <- colSums(as.matrix(test_78))
freq_80 <- colSums(as.matrix(test_80))
freq_83 <- colSums(as.matrix(test_83))

# add new columns that only contain MH for the particular year (1978)
MH_78 <- which(total$EDAT_Year == 1978)
total$MH_78 <- ""

total$MH_78[MH_78] <- total$MH[MH_78]

# add new columns that only contain MH for the particular year (1980)
MH_80 <- which(total$EDAT_Year == 1980)
total$MH_80 <- ""
total$MH_80[MH_80] <- total$MH[MH_80]

# add new columns that only contain MH for the particular year (1983)
MH_83 <- which(total$EDAT_Year == 1983)
total$MH_83 <- ""
total$MH_83[MH_83] <- total$MH[MH_83]

# find association between words
findAssocs(testing, "humans", 0.05)
findAssocs(testing, "surgery", 0.2)
findAssocs(dtm, "humans", 0.05)
findAssocs(dtm, "complications", 0.2)
findAssocs(dtm, "infant", 0.2)
findAssocs(dtm, c, 0.6)

# change class of EDAT_Year to numeric
total$EDAT_Year <- as.numeric(total$EDAT_Year)

# plotting graphs about word occurrence over entire sample
wf <- data.frame(term = names(freq), occurences = freq) # df of frequency of all words
p <- ggplot(subset(wf, occurences > 4000), aes(term, occurences))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#p <- p + facet_wrap(~EDAT_Year, ncol = 3)
p <- p + ggtitle("Top Word Occurrences Across All 3 Years")
p

# compare words among different years
y1978 <- total[which(total$EDAT_Year == 1978), ]
y1980 <- total[which(total$EDAT_Year == 1980), ]
y1983 <- total[which(total$EDAT_Year == 1983), ]

# create graphs for each year. 1978 first
myCorpus2 <- Corpus(VectorSource(y1978$MH))  
dtm2 <- DocumentTermMatrix(myCorpus2)
freq2 <- colSums(as.matrix(dtm2))

# plotting graphs about word occurrence in 1978
wf2 <- data.frame(term = names(freq_78), occurrences = freq_78) # df of frequency of all words
p2 <- ggplot(subset(wf2, freq_78 > 1000), aes(term, occurrences))
p2 <- p2 + geom_bar(stat = "identity")
p2 <- p2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2 <- p2 + ggtitle("Top Word Occurrences in 1978")
p2

# create graphs for each year. 1980 now
myCorpus3 <- Corpus(VectorSource(y1980$MH))  
dtm3 <- DocumentTermMatrix(myCorpus3)
freq3 <- colSums(as.matrix(testing))

# plotting graphs about word occurrence in 1980
wf3 <- data.frame(term = names(freq_80), occurrences = freq_80) # df of frequency of all words
p3 <- ggplot(subset(wf3, freq_80 > 500), aes(term, occurrences))
p3 <- p3 + geom_bar(stat = "identity")
p3 <- p3 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3 <- p3 + ggtitle("Top Word Occurrences in 1980")
p3

# create graphs for each year. 1983 now
myCorpus4 <- Corpus(VectorSource(y1983$MH))  
dtm4 <- DocumentTermMatrix(myCorpus4)
freq4 <- colSums(as.matrix(dtm4))

# plotting graphs about word occurrence in 1983
wf4 <- data.frame(term = names(freq_83), occurrences = freq_83) # df of frequency of all words
p4 <- ggplot(subset(wf4, freq_83 > 2000), aes(term, occurrences))
p4 <- p4 + geom_bar(stat = "identity")
p4 <- p4 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p4 <- p4 + ggtitle("Top Word Occurrences in 1983")
p4

# all 4 plots arranged side by side
require(gridExtra)
grid.arrange(p, p2, p3, p4)

# plot correlation plot
#plot(testing, c, corThreshold = 0.5) x and y length differs...problem

# plot wordcloud
set.seed(123)
wordcloud(names(freq), freq, min.freq = 500, colors = brewer.pal(6, "Dark2"), scale = c(5, 1.5))

# word distances - mathematical based
distance(total$MH, "surgery")
cor(total$MH, total$EDAT_Year)

# cluster dendogram
d <- dist((c), method = "euclidian")
fit <- hclust(d = d, method = "ward.D2")
summary(fit)
plot(fit, hang = -1)

# correlation of terms plot
freq.terms<-findFreqTerms(dtm, lowfreq=500)[1:25]
plot(dtm,term=freq.terms,corThreshold=0.1,weighting=T)

# save file which includes new columns
save(total, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe.Rdata")

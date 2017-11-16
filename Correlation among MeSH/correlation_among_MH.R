library(tm)
library(quanteda)
library(qdap)
library(graph)
library(igraph)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggfortify)
library(gofastr)
source("https://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(ggrepel)

load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe.Rdata")

#load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/infant vs gender/dtm_female_infant_MH.Rdata")

# creating corpus 
myCorpus <- Corpus(VectorSource(total$MH))  
dtm <- DocumentTermMatrix(myCorpus)
a <- freq_terms(dtm, 25)

# matrix of dtm
dtm2 <- as.matrix(dtm)

# finding most frequent terms
#frequency <- colSums(dtm2)

frequency <- dtm2[, order(colSums(-dtm2))]

dtm_female_infant_MH <- t(dtm_female_infant_MH)

# correlation of terms plot
freq.terms <- findFreqTerms(dtm)[1:25] # choose top 25 terms with frequency of 400
plot(dtm, term = freq.terms, corThreshold = 0.1, weighting = T) # choose terms with correlation of at least 0.1

assocs <- findAssocs(dtm, term = freq.terms, corlimit = 0.3)

# LABELS
# Recreate edges, using code from plot.DocumentTermMatrix
m <- dtm
corThreshold <- 0.1
m <- as.matrix(m[, freq.terms])
c <- cor(m)
c[c < corThreshold] <- 0
c[is.na(c)] <- 0
diag(c) <- 0
ig <- graph.adjacency(c, mode="undirected", weighted=TRUE)
g1 <- as_graphnel(ig)

# Make edge labels
ew <- as.character(unlist(edgeWeights(g1)))
ew <- ew[setdiff(seq(along=ew), Rgraphviz::removedEdges(g1))]
names(ew) <- edgeNames(g1)
eAttrs <- list()
elabs <- paste("        ", round(as.numeric(ew), 2)) # so it doesn't print on top of the edge
names(elabs) <- names(ew)
eAttrs$label <- elabs
fontsizes <- rep(7, length(elabs))
names(fontsizes) <- names(ew)
eAttrs$fontsize <- fontsizes

plot(dtm, term = freq.terms, corThreshold = 0.1, weighting = T, 
     edgeAttrs = eAttrs)

#------ PCA based off http://michael.hahsler.net/SMU/CSE7337/install/tm.R

# top 50 words
dtm2_new <- dtm2[, order(colSums(-dtm2))]
dtm2_new_sub <- dtm2_new[, 1:500]

# converting matrix into dtm
foo <- apply(dtm2_new_sub, 1, paste, collapse = " ")
tt <- Corpus(VectorSource(foo))
tt <- DocumentTermMatrix(tt)

# testing PCA
dtm_tf <- weightTfIdf(dtm)
# now do k-means
m <- as.matrix(dtm_tf)
#m <- dtm2_new
rownames(m) <- 1:nrow(m)
# normalize vectors so Euclidean distance makes sense
norm_eucl <- function(m) m/apply(m, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
m_norm <- norm_eucl(m)
m_norm <- m_norm[, order(colSums(-m_norm))]
m_norm_complete <- m_norm[complete.cases(m_norm), ]
# cluster into 10 clusters
c1 <- kmeans(m_norm_complete, 10)
c1
table(c1$cluster)

#dtm2_new_sub <- t(dtm2_new_sub)
#dtm2_new_sub <- norm_eucl(dtm2_new_sub)
m_norm <- t(m_norm_complete)
plot(prcomp(m_norm_complete)$x, col = c1$cluster)
m_norm_complete <- as.data.frame(m_norm_complete) ####
autoplot(prcomp(m_norm_complete), data = m_norm_complete, color = c1, label = TRUE)######

# based off of https://stackoverflow.com/questions/18105906/adding-labels-to-2d-scatter-plot-kmeans-clustering
pca_new <- prcomp((m_norm_complete), cor = F)
dat.loadings <- pca_new$x[ , 1:2]
c <- kmeans(dat.loadings, centers = 10)
pca1.1 <- pca_new$x[ , 1]
pca2.1 <- pca_new$x[ , 2]
mydf2 <- data.frame(ID = names(pca1.1), PCA1 = pca1.1, PCA2 = pca2.1, Cluster = factor(c$cluster))
ggplot(mydf2, aes(x = PCA1, y = PCA2, label = ID, color = Cluster)) + geom_point() + geom_text(size = 4, color = "black", vjust = -1) + geom_text_repel(aes(label = ID))

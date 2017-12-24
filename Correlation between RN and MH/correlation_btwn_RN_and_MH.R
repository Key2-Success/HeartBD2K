library(qdapRegex)
library(stringr)
library(tm)
library(ggplot2)
library(ggfortify)
library(ggrepel)
library(gofastr)

# to do: make 4 new variables on RN: A - named RN, B - 0 (general), C - no annotation (NAs), D - all annotations
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe.Rdata")
total <- d # testing
total$MH <- total$MH_new # testing
total_noNAinRN <- total[!is.na(total$RN), ]
total <- total_noNAinRN # testing

# create 4 new empty variables
total$named_RN <- ""
total$general_RN <- ""
total$no_RN <- ""
total$all_RN <- "" # remember to delete columns for testing

# no_RN
for(i in 1:nrow(total))
{
  if (is.na(total[i, 25]))
  {
    total[i, 28] = 1
  }
  else
  {
    total[i, 28] = 0
  }
}
total$no_RN <- as.factor(total$no_RN) # make binary


# all_RN
for(i in 1:nrow(total))
{
  if (total[i, 28] == 0)
  {
    total[i, 29] <- gsub("[\\(\\)]", "", regmatches(total[i, 25], gregexpr("\\(.*?\\)", total[i, 25])))
  }
  else
  {
    total[i, 29] = ""
  }
}
total$all_RN <- gsub('c"', "", total$all_RN) # removes c"
total$all_RN <- gsub('"', "", total$all_RN) # removes "


# # to-do: general_RN
# for(i in 1:nrow(total))
# {
#   if (total[i, 28] == 0)
#   {
#     if (gsub("0.*", "", total[i, 25]) == "")
#     {
#       total[i, 27] <- total[i, 25]
#       count <- str_count(total[i, 27], "0")
#       if (count > 1)
#       {
#         zero_loc <- gregexpr(pattern = "0", total[i, 27])
#         endpar_loc <- gregexpr(pattern = ")", total[i, 27]) # if distance between endpar_loc and zero_loc's second instance !=2...for loop. as well as if size of endpar_loc != zero_loc
#         if (lengths(endpar_loc) > lengths(zero_loc)) # if extra elements that don't have 0, substring up until that point
#         {
#           endpar_zero <- endpar_loc[[1]][[lengths(zero_loc)]]
#           total[i, 27] <- substr(total[i, 27], 1, endpar_zero)
#           total[i, 26] <- substr(total[i, 27], endpar_zero, nchar(total[i, 27])) # put those substring out into named_RN
#         }
#       }
#     }
#     else # no non-annotated ones
#     {
#       total[i, 26] <- total[i, 25] # put them into named_RN
#     }
#   }
#   else
#   {
#     total[i, 27] = ""
#   }
# }


# idea 2 for general and named RNs
total$RN_temp <- gsub("^[0] "," general_RN",total$RN) # replace leading 0s w/ general_RN
total$RN_temp <- gsub(" [0] "," general_RN",total$RN_temp) # replace other " 0 "
total$RN_temp <- gsub(" \\("," named_RN(",total$RN_temp) # replace rest w/ named_RN

total$named_RN <- regmatches(total$RN_temp,gregexpr("(?<=named_RN\\().*?(?=\\))", total$RN_temp, perl=TRUE))
total$general_RN <- regmatches(total$RN_temp,gregexpr("(?<=general_RN\\().*?(?=\\))", total$RN_temp, perl=TRUE))
total$RN_temp <- NULL

total$named_RN <- unlist(lapply(total$named_RN, function(x) ifelse(is.null(x), NA, paste0(x, collapse = ", "))))
total$general_RN <- unlist(lapply(total$general_RN, function(x) ifelse(is.null(x), NA, paste0(x, collapse = ", "))))

# 
# total$general_RN <- as.character(total$general_RN)
# total$named_RN <- as.character(total$named_RN)
# 
# total$general_RN[sapply(total$general_RN, is.null)] <- NA
# unlist(total$general_RN)

# # replace character(0) with empty string
# for (i in nrow(total))
# {
#   
#   if (total[i, "named_RN"] == "character(0)")
#   {
#     total[i, "named_RN"] <- ""
#   }
#   else
#   {
#     total[i, "named_RN"] <- rm_between(total[i, "named_RN", '"', '"', extract = TRUE])
#   }
#   if (total[i, "general_RN"] == "character(0)")
#   {
#     total[i, "general_RN"] <- ""
#   }
#   else
#   {
#     total[i, "general_RN"] <- rm_between(total[i, "general_RN", '"', '"', extract = TRUE])
#   }
# }
# 

save(total, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe_RN.Rdata") # save here for updated
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/all_merged_dataframe_RN.Rdata")
# create new column that combines all RN and all MH
total$RN_and_MH <- paste(total$MH, total$all_RN, sep = ", ")

# create column with higher frequency of RN
#total$RN_and_MH_more <- paste(total$RN_and_MH, total$all_RN, sep = ", ") # do for first time
#total$RN_and_MH_more <- paste(total$RN_and_MH_more, total$all_RN, sep = ", ")

# text mining on RN_and_MH

myCorp <- Corpus(VectorSource(total$RN_and_MH))  
dtm_rm <- q_dtm(total$RN_and_MH, keep.hyphen = TRUE) # used to be DocumentTermMatrix(myCorp)

# text mining only on RN
myCorp2 <- Corpus(VectorSource(total$all_RN))
dtm_rm2 <- DocumentTermMatrix(myCorp2)
#dtm_rm <- dtm_rm2 # for RN only

# testing PCA
dtm_tf2 <- weightTfIdf(dtm_rm)
# now do k-means
m2 <- as.matrix(dtm_tf2)
#m <- dtm2_new
rownames(m2) <- 1:nrow(m2)
# normalize vectors so Euclidean distance makes sense
norm_eucl <- function(m2) m2/apply(m2, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
m_norm2 <- norm_eucl(m2)
m_norm2 <- m_norm2[, order(colSums(-m_norm2))]

m_norm2 <- t(m_norm2)

# only use complete cases
#m_norm2_complete <- m_norm2[, complete.cases(m_norm2)]
m_norm2[is.na(m_norm2)] <- 0

# cluster into 10 clusters
c2 <- kmeans(m_norm2, 10)
c2
table(c2$cluster)

m_norm2 <- m_norm2[order(-rowSums(m_norm2)), ] # orders them 
m_norm2_sub <- m_norm2[1:100, ] # take only top 50
#dtm2_new_sub <- t(dtm2_new_sub)
#dtm2_new_sub <- norm_eucl(dtm2_new_sub)
#plot(prcomp(m_norm2)$x, col = c2$cluster)
#m_norm2_sub_new <- m_norm2_sub[-c(1:5), ] # removes top 5 words
#autoplot(kmeans(m_norm2_sub_new, 10), data = m_norm2_sub_new, label = TRUE)

# based off of https://stackoverflow.com/questions/18105906/adding-labels-to-2d-scatter-plot-kmeans-clustering
#m_norm2_complete <- t(m_norm2_complete) # transpose so PCA plot isn't on document ID
pca <- prcomp((m_norm2_sub))
dat.loadings <- pca$x[ , 1:2]
c <- kmeans(dat.loadings, centers = 10)
#centers <- as.data.frame(c$centers)
pca1 <- pca$x[ , 1]
pca2 <- pca$x[ , 2]
mydf <- data.frame(ID = names(pca1), PCA1 = pca1, PCA2 = pca2, Cluster = factor(c$cluster))


# create new variable that allows boundary words
mydf$ID_boundary <- paste0("\\b", mydf$ID)
mydf$ID_boundary <- paste0(mydf$ID_boundary, "\\b")

# create labels for RN vs MH terms
mydf$MH_or_RN <- 0
mydf$MH_or_RN <- as.numeric(mydf$MH_or_RN)
mydf$ID <- as.character(mydf$ID)
for (i in 1:100)
{
  # if (sum(grepl(pattern = mydf$ID[i], x = total$all_RN, ignore.case = TRUE)) > 0)
  # {
  #   mydf$MH_or_RN[i] <- "RN"
  # }
  
  mydf$MH_or_RN[i] <- sum(grepl(mydf$ID_boundary[i], total$all_RN, ignore.case=T))
  if (mydf$MH_or_RN[i] > 0)
  {
    mydf$MH_or_RN[i] <- "RN"
  }
  else
  {
    mydf$MH_or_RN[i] <- "MH"
  }
}
mydf$MH_or_RN <- as.factor(mydf$MH_or_RN)

# create new dataframes just for RN and MH for heat map
RN_only_df <- mydf[mydf$MH_or_RN == "RN", c(1:3)]
MH_only_df <- mydf[mydf$MH_or_RN == "MH", c(1:3)]
#RN_only_df <- as.matrix(RN_only_df)
#MH_only_df <- as.matrix(MH_only_df)

#autoplot(mtx)

#which(mydf_new$ID == "kappa")
#mydf_new <- mydf_new#[-c(62), ]
# heparin, immunoglobulin, warfarin, chains, kappa

# merge the PCA points of RN and MH
#my_df_RN <- mydf[1:50, -c(4)]
#my_df_MH <- mydf[1:50, -c(4)]
#my_df_RN_and_MH <- rbind(my_df_RN, my_df_MH)
#points <- my_df_RN_and_MH[, -c(1)]
#k <- kmeans(points, centers = 10)
#my_df_RN_and_MH <- cbind(my_df_RN_and_MH, Cluster = factor(k$cluster))
#save(my_df_RN_and_MH, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/my_df_RN_and_MH_top50.Rdata")

which(mydf == "character0")
mydf <- mydf[-c(), ]
# valve, heart, surgery, heparin, pathology, diagnosis, immunoglobulin, diagnostic, imaging, character0

ggplot(mydf, aes(x = PCA1, y = PCA2, label = ID, fill = MH_or_RN)) + geom_point(aes(color = MH_or_RN)) + geom_label_repel(aes(label = ID)) #+ geom_text(aes(color = mydf$MH_or_RN)) #
#in ggplot(color = Cluster instead of fill = Cluster for white one)

#autoplot(my_df_RN_and_MH)

# heat map
#m_norm2_sub2 <- m_norm2_sub[1:20, 1:20]
#autoplot(m_norm2_sub2)

# heat map (based off https://stackoverflow.com/questions/44731442/create-heat-map-from-pca-coordinates-in-r/44766868#44766868)
# Define a distance function based on euclidean norm
# calculated between PCA values of the i-th and j-th items
dst <- Vectorize(function(i,j,dtset) sqrt(sum((dtset[i,2:3]-dtset[j,2:3])^2)), vectorize.args=c("i","j"))

# Here is the distance between echocardiography and infarction
dst(1,2,mydf)
# [1] 0.7615773
# This value is given by
sqrt(sum((mydf[1,2:3] - mydf[2,2:3])^2))

# Calculate the distance matrix
nr <- nrow(mydf)
mtx <- outer(1:nr, 1:nr, "dst", dtset = mydf)
colnames(mtx) <- rownames(mtx) <- mydf[,1]

# Plot the heatmap using ggplot2
library(reshape2)
library(ggplot2)
mtx.long <- melt(mtx)

mtx.long <- mtx.long[mtx.long$Var1 %in% RN_only_df$ID, ]
mtx.long$Var1 <- as.factor(mtx.long$Var1) # will need to change to factor for heat map
mtx.long$Var2 <- as.factor(mtx.long$Var2) # will need to change to factor for heat map

ggplot(mtx.long, aes(x = Var1, y = Var2, fill = value)) + geom_tile()+xlab("RN terms")+ylab("MH terms") + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# corrogram
library(corrgram)
corrgram(mtx, upper.panel = panel.pie)

# correlation matrix
library(corrplot)

# standardize so values are between -1 and 1
mtx_normalized <- mtx
for(i in 1:100)
{
  for (j in 1:100)
  {
    mtx_normalized[i, j] <- (mtx[i, j] - min(mtx))/(max(mtx) - min(mtx))
    mtx_normalized[i, j] <- mtx_normalized[i, j] - 0.5
  }
}
  
mtx_normalized <- mtx_normalized[rownames(mtx_normalized) %in% RN_only_df$ID, ]
corrplot(corr = mtx_normalized)

# done: scale up RN frequency in MH_RN variable and try pca plot
# done: try correlation matrix/corrgram
# done: heat map between RN and MH
# done: in PCA plot, change color for MH and RN distinction
# done: read in multiple words for a phrase for MH and RN (try q_dtm [has keep.hyphen = TRUE], in gofastr) (try https://stackoverflow.com/questions/24038498/corpus-build-with-phrases)
# done: for fun and giggles, ^ replace with I or some letter and try it out since special characters are annoying
# done: read in 500k case reports and filter out by missing RN files
# to do: find hclust or other way of measuring distance for heatmap
# done: read in 500k maybe by filtering it down to only PMID, RN, and MH
# done: look into sql/sql lite or data.table/dplyr
# done: make heat map that has gender/demographics on x axis and on y-axis top MH or RN terms (2 different plots)
# done: make heat map ^ but two different ones based on distinct MH/RN for each gender
# to do: use either euclidean distance from PCA plot for each gender or do assoc() to do heat map
# done: by july 10/11 have heat map read
# done: find statistical significance...if 4 counts of aspiring and 3 in male...this is not significant...maybe use value from heat map and use 2 sample mean t-test
# to do: try running recreating labels without the n > 1 if statement
# to do: try running recreating labels w stringi instead of sub
# to do: learn if you can figure out how long a code will run before it's done executing
# to do: read in entire 24 million obs into dataframe overnight
# done: trim down dataframe by removing 0 () RN terms (general ones)
# done: create an image more representative of the work we're doing
# to do: read in the 90k test (from the reduced RN terms) 10k at a time
# to do: instead of using 2 mean prop z test on sample 10k data do it on full one
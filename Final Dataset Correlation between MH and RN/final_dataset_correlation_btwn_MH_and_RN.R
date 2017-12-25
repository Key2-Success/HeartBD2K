  library(stringi)
  library(tm)
  library(gofastr)
  library(ggplot2)
  library(ggrepel)
  library(reshape2)
  library(RColorBrewer)
  library(dplyr)
  library(stringr)
  library(scales)
  
  
  # load data
  load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub.Rdata")
  load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/final_data_sub_RN.Rdata")
  
  # make dtm to perform text mining on
  dtm_MH <- q_dtm(test_sub$MH_clean)
  dtm_RN <- q_dtm(test2$RN_demo, keep.hyphen = TRUE)
  
  # PCA plot on MH and RN
  dtm_tf2 <- weightTfIdf(dtm_male_infant_MH) # trying out RN_parsed
  m2 <- as.matrix(dtm_tf2)
  save(m2, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/m2.Rdata")
  rownames(m2) <- 1:nrow(m2)
  norm_eucl <- function(m2) m2/apply(m2, MARGIN = 1, FUN = function(x) sum(x^2)^0.5) # normalize vectors so Euclidean distance makes sense
  m_norm2 <- norm_eucl(m2)
  m_norm2 <- m_norm2[, order(colSums(-m_norm2))]
  m_norm2 <- t(m_norm2)
  m_norm2[is.na(m_norm2)] <- 0
  m_norm2 <- m_norm2[order(-rowSums(m_norm2)), ] # orders them 
  m_norm2_sub <- m_norm2[1:500, ] # take only top 100
  pca <- prcomp((m_norm2)) # temp changed from m_norm2_sub to see all values
  save (pca, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/pca_male_infant_MH.Rdata")
  load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/pca_full.Rdata")
  dat.loadings <- pca$x[ , 1:2]
  c <- kmeans(dat.loadings, centers = 10)
  pca1 <- pca$x[ , 1]
  pca2 <- pca$x[ , 2]
  mydf <- data.frame(ID = names(pca1), PCA1 = pca1, PCA2 = pca2, Cluster = factor(c$cluster))
  save(mydf, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Gender vs Age/mydf_male_infant_MH.Rdata")
  
  # plot pca plot on top 100 MH
  ggplot(mydf2, aes(x = PCA1, y = PCA2, label = ID, fill = Cluster)) + geom_point(aes(color = Cluster)) + geom_label_repel(aes(label = ID)) #+ geom_text()
  
  # create labels for female/male (demographics) vs MH terms in mydf
  mydf$demographics <- 0
  mydf$demographics <- as.numeric(mydf$demographics)
  mydf$ID <- as.character(mydf$ID)
  for (i in 1:nrow(mydf))
  {
    if (mydf$ID[i] == "female" || mydf$ID[i] == "male")
    {
      mydf$demographics[i] <- "demographics"
    }
    else
    {
      mydf$demographics[i] <- "MH"
    }
  }
  mydf$demographics <- as.factor(mydf$demographics)
  
  # clean up unneccesary MH terms in mydf
  a_vec <- c()
  for (i in 1:nrow(mydf))
  {
    if (mydf$ID[i] == "&" || mydf$ID[i] == "," || mydf$ID[i] == "~")
    {
      a_vec <- append(a_vec, i)
    }
  }
  mydf <- mydf[-c(a_vec), ] # remove terms from mydf
  save(mydf, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mydf_full.Rdata")
  
  # heat map
  dst <- Vectorize(function(i,j,dtset) sqrt(sum((dtset[i,2:3]-dtset[j,2:3])^2)), vectorize.args=c("i","j"))
  nr <- nrow(mydf) # calculate distance matrix
  mtx <- outer(1:nr, 1:nr, "dst", dtset = mydf)
  save(mtx, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mtx_full.Rdata")
  colnames(mtx) <- rownames(mtx) <- mydf[,1]
  demographics <- mydf[mydf$demographics == "demographics", c(1:3)] # create new dataframe for demographics
  mtx.long <- melt(mtx)
  mtx.long <- mtx.long[mtx.long$Var1 %in% demographics$ID, ]
  mtx.long$Var1 <- as.factor(mtx.long$Var1) # will need to change to factor for heat map
  mtx.long$Var2 <- as.factor(mtx.long$Var2) # will need to change to factor for heat map
  
  # remove cross matches between x and y axes
  b_vec <- c()
  for (i in 1:nrow(mtx.long))
  {
    if (mtx.long$Var2[i] == "female" || mtx.long$Var2[i] == "male")
    {
      b_vec <- append(b_vec, i)
    }
  }
  mtx.long <- mtx.long[-c(b_vec), ] # remove repeat terms from mtx.long
  
  save(mtx.long, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mtx.long_MH.Rdata")
  
  # keep on significant words
  #mtx.long <- mtx.long[mtx.long$Var2 %in% df_all_sig$word, ]
  
  # change color scale by reversing values
  mtx.long$value2 <- mtx.long$value*(-1)
  
  # normalize scale from 0 to 1
  mtx.long$correlation <- as.numeric(lapply(mtx.long$value2, FUN = function(x) (x - min(mtx.long$value2))/(max(mtx.long$value2) - min(mtx.long$value2))))
  save(mtx.long, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mtx.long_full.Rdata")
  load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mtx.long_MH.Rdata")
  
  # choose only a few RN terms
  mtx.long2 <- mtx.long[mtx.long$Var2 %in% df_all_sig$word, ]
  
  # look only into male and female mtx.long
  female <- mtx.long[mtx.long$Var1 == "female", ]
  male <- mtx.long[mtx.long$Var1 == "male", ]
  female <- female[order(-female$correlation), ] # order by correlation
  male <- male[order(-male$correlation), ] # order by correlation
  
  save(female, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/female_corr_topallMH.Rdata")
  save(male, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/male_corr_topallMH.Rdata")
  
  df_female2$Var1 <- "female"
  df_male2$Var1 <- "male"
  df_female2$value <- df_female2$prop*5582
  df_male2$value <- df_male2$prop*5582
  colnames(df_female2)[2] <- "occurrences"
  colnames(df_male2)[2] <- "occurrences"
  
  mtx_manual <- rbind(df_female2, df_male2)
  mtx_manual_choose <- mtx_manual[mtx_manual$word %in% df_pval2_sig$word, ]
  save (mtx_manual_choose, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mtx_sig_RN_ztest.Rdata")
  
  # scale diffs so it's between -1 and 1 where a = -1 and b = 1 # changed diffs to top10diffs
  for (i in 1:nrow(top10diffs))
  {
    top10diffs$scaled[i] <- (((2)*(top10diffs$differences[i] - min(top10diffs$differences)))/(max(top10diffs$differences) - min(top10diffs$differences))) - 1
  }

  diffs$words <- factor(diffs$words, levels = (diffs$words)[order(diffs$scaled)]) # in order
  diffs$words <- as.character(diffs$words) # random
  save(diffs, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/diffs_RN.Rdata")
  save(df_pval2_sig, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/df_pval2_sig.Rdata")
  
  top10diffs <- diffs[diffs$words %in% df_pval2_sig$word[1:10], ]
  
  # plot heat map
  mtx.long2 <- mtx_manual[1:40, ]
  ggplot(mtx_manual_choose, aes(x = Var1, y = word, fill = prop)) + geom_tile() + 
    xlab("Gender") + ylab("RN terms") + labs(title = "Correlation between RN terms and Gender") + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  
  # plot heat map against just demographic
  ggplot(top10diffs, aes(x = " ", y = words, fill = scaled)) + geom_tile() + 
    xlab("Correlation") + ylab("RN terms") + labs(title = "Correlation between Statistically Significant RN terms and Gender") + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(fill = "") +
    scale_fill_gradient(low = "blue", high = "red", 
                        breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("female", "-0.5", "0", "0.5", "male"))


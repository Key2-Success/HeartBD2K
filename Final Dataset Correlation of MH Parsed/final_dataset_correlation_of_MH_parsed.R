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
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/sig_MH_parsed_z-test_all_RN.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/df_female2_MH_all.Rdata")
load(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/df_male2_MH_all.Rdata")

# prepare for heat map
df_female2$Var1 <- "female"
df_male2$Var1 <- "male"
colnames(df_female2)[2] <- "occurrences"
colnames(df_male2)[2] <- "occurrences"

mtx_manual <- rbind(df_female2, df_male2)
mtx_manual_choose <- mtx_manual[mtx_manual$word %in% df_pval2_sig$word, ] # choose top 10 most significant terms
save (mtx_manual_choose, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/mtx_sig_MH_ztest_allMH.Rdata")

# make new dataframe that looks at differences between significant words' correlations
diffs <- as.data.frame(mtx_manual_choose[mtx_manual_choose$Var1 == "female", 1]) # only female so no repeats
colnames(diffs) <- "words"

diffs$differences <- "0"

# calculate differences between 16 significant terms
for (i in 1:nrow(diffs))
{
  word <- diffs$word[i]
  maleval <- mtx_manual_choose[mtx_manual_choose$word == word, ]
  maleval <- maleval[maleval$Var1 == "male", 3] # choose proportion since these are significant
  femaleval <- mtx_manual_choose[mtx_manual_choose$word == word, ]
  femaleval <- femaleval[femaleval$Var1 == "female", 3]
  diffs$differences[i] <- maleval - femaleval
}

diffs$differences <- as.numeric(diffs$differences) # make numeric for heat map
diffs <- diffs[order(diffs$differences), ]
save(diffs, file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/diffs_all_sig_MH.Rdata")

diffs$most_dif <- abs(diffs$differences)
diffs <- diffs[order(-diffs$most_dif), ]

top10diffs <- diffs

# choosing how many to plot
top10diffs <- diffs[1:20, ] # changed from diffs[diffs$word %in% df_pval2_sig$word[1:30], ] OR diffs[1:30]

# scale diffs so it's between -1 and 1 where a = -1 and b = 1 # changed diffs to top10diffs
for (i in 1:nrow(top10diffs))
{
  top10diffs$scaled[i] <- (((2)*(top10diffs$differences[i] - min(top10diffs$differences)))/(max(top10diffs$differences) - min(top10diffs$differences))) - 1
}

# heat map aesthetic
top10diffs$words <- factor(top10diffs$words, levels = (top10diffs$words)[order(top10diffs$scaled)]) # in order
top10diffs$words <- as.character(top10diffs$words) # random

# plot heat map for two genders on x-axis
ggplot(mtx_manual_choose, aes(x = Var1, y = word, fill = prop)) + geom_tile() + 
  xlab("Gender") + ylab("MeSH terms") + labs(title = "Correlation between MeSH terms and Gender") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# plot heat map against just demographic
ggplot(top10diffs, aes(x = " ", y = words, fill = scaled)) + geom_tile() + 
  xlab(" ") + ylab("MeSH terms") + labs(title = "Correlation of Most Distinct MeSH terms by Gender") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(fill = "") +
  scale_fill_gradient(low = "blue", high = "red", 
                      breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("female", "-0.5", "0", "0.5", "male")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12))

# save as an svg file
ggsave(file = "Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/20_most_distinct_MH.svg")

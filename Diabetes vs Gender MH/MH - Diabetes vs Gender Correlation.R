library(ggplot2)


load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/diabetes vs gender/sig_MH_male_btwn_diabetes-nondiabetes.Rdata")
save(mydf_sig, file = "Kitu/mydf_sig.Rdata")
# normalize diffs by factors instead of subtraction
for (i in 1:nrow(mydf_sig))
{
  if (mydf_sig$diffs[i] < 0)
  {
    mydf_sig$factor[i] <- -mydf_sig$prop_diabetes[i]/mydf_sig$prop_nondiabetes[i]
  }
  else if (mydf_sig$diffs[i] > 0)
  {
    mydf_sig$factor[i] <- mydf_sig$prop_nondiabetes[i]/mydf_sig$prop_diabetes[i]
  }
}

# substitite infinite values with value
for (i in 1:nrow(mydf_sig))
{
  if (mydf_sig$factor[i] == Inf)
  {
    mydf_sig$factor[i] <- mydf_sig$male_nondiabetes[i]
  }
  if (mydf_sig$factor[i] == -Inf)
  {
    mydf_sig$factor[i] <- mydf_sig$male_diabetes[i]
  }
}

# choose top strongest factors
mydf_sig <- mydf_sig[order(-abs(mydf_sig$factor)), ]

# choose only those with enough case reports
mydf_sig$add <- mydf_sig$male_diabetes + mydf_sig$male_nondiabetes
pval <- mydf_sig[mydf_sig$`p-value` < 0.01, ]
mydf_sig2 <- pval[pval$add > 40, ] # keep only MH that have appeared n times across both genders

# scale diffs to range from -1 to 1
for (i in 1:nrow(mydf_sig2))
{
  mydf_sig2$scaled[i] <- (((2)*(mydf_sig2$factor[i] - min(mydf_sig2$factor)))/(max(mydf_sig2$factor) - min(mydf_sig2$factor))) - 1
}

test <- mydf_sig2[1:12, ]

# heat map aesthetic
test$word <- factor(test$word, levels = (test$word)[order(test$scaled)]) # in order

# round to 2 digits
test$factor <- lapply(test$factor, round, 2)
test$factor <- as.numeric(test$factor)
test$type <- ifelse(test$factor < 0, "diabetes", "nondiabetes")
test$type <- as.factor(test$type)

# plot heat map against just demographic
ggplot(data = test, aes(x = " ", y = word, fill = factor)) + geom_tile() + labs(fill = "") +
  xlab(" ") + ylab("MeSH terms") + labs(title = "Prevalence Factors of Most Distinct MeSH terms\n in Diabetic vs Non-Diabetic Males") +
  scale_fill_gradientn(limits = c(-100, 100),
                       colours=c("blue", "purple", "pink", "orange", "red"),
                       breaks=c(-100, 0, 20, 100, 1100), labels = c("diabetic", "0", "", "nondiabetic", ""), na.value = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12)) + 
  geom_text(aes(label = test$factor, color = test$type), show.legend = FALSE) +
  scale_colour_manual(values=c("white", "black")) #+ theme(legend.position = "none")

# save as an svg file
ggsave(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/diabetes vs gender/12_most_distinct_MH_male_diabetic-nondiabetic_by_factor_displayed.svg")
ggsave(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/MH - Diabetes vs Gender/diabetes vs gender/12_most_distinct_MH_male_diabetic-nondiabetic_by_factor_displayed.pdf")


# women
# scale_fill_gradientn(limits = c(-100, 100),
#                      colours=c("blue", "purple", "pink", "orange", "red"),
#                      breaks=c(-100, 0, 20, 100, 1100), labels = c("diabetic", "0", "", "nondiabetic", ""), na.value = "red") +
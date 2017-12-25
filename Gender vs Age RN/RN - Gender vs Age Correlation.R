library(ggplot2)


load(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/adolescent vs gender/sig_RN_adolescent_btwn_genders.Rdata")

# normalize diffs by by factors instead of subtraction
for (i in 1:nrow(mydf_sig))
{
  if (mydf_sig$diffs[i] < 0)
  {
    mydf_sig$factor[i] <- -mydf_sig$prop_female[i]/mydf_sig$prop_male[i]
  }
  else if (mydf_sig$diffs[i] > 0)
  {
    mydf_sig$factor[i] <- mydf_sig$prop_male[i]/mydf_sig$prop_female[i]
  }
}

# substitite infinite values with value
for (i in 1:nrow(mydf_sig))
{
  if (mydf_sig$factor[i] == Inf)
  {
    mydf_sig$factor[i] <- mydf_sig$male_adolescent[i]
  }
  if (mydf_sig$factor[i] == -Inf)
  {
    mydf_sig$factor[i] <- -mydf_sig$female_adolescent[i]
  }
}

# choose top strongest factors
mydf_sig <- mydf_sig[order(-abs(mydf_sig$factor)), ]

# choose only those with enough case reports
mydf_sig$add <- mydf_sig$male_adolescent + mydf_sig$female_adolescent
#pval <- mydf_sig[mydf_sig$`p-value` < 0.01, ]
#mydf_sig2 <- pval[pval$add > 50, ] # keep only RN that have appeared n times across both genders
mydf_sig2 <- mydf_sig

# scale diffs to range from -1 to 1
for (i in 1:nrow(mydf_sig2))
{
  mydf_sig2$scaled[i] <- (((2)*(mydf_sig2$factor[i] - min(mydf_sig2$factor)))/(max(mydf_sig2$factor) - min(mydf_sig2$factor))) - 1
}

test <- mydf_sig2#[1:12, ]

# round to 2 digits
test$factor <- lapply(test$factor, round, 2)
test$factor <- as.numeric(test$factor)
test$type <- ifelse(test$factor < 0, "female", "male")
test$type <- as.factor(test$type)

# heat map aesthetic
test$word <- factor(test$word, levels = (test$word)[order(test$scaled)]) # in order

# plot heat map against just demographic
ggplot(test, aes(x = " ", y = word, fill = scaled)) + geom_tile() + 
  xlab(" ") + ylab("RN terms") + labs(title = "Prevalence Factors of Most Distinct RN terms\n by Gender in Adolescents") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(fill = "") +
  scale_fill_gradient(low = "blue", high = "red", 
                      breaks = c(-1, 0, 1), labels = c("female", "0", "male")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 20)) + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12)) + 
  geom_text(aes(label = test$factor, color = test$type), show.legend = FALSE) +
  scale_colour_manual(values=c("white", "black")) #+ theme(legend.position = "none")

# save as an svg file
ggsave(file = "~/Kitu/College/Junior Year/Extracurriculars/Data Science Research Internship/RN - Gender vs Age/adolescent vs gender/12_most_distinct_RN_gender_adolescent_by_factor_displayed.svg")

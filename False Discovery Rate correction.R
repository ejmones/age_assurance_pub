
## The False Discovery Rate
#FDR: The expected fraction of rejected null hypotheses that are actually false positives
#Compared to FWER, FDR is typically a more favorable procedure if there are more variables to consider
###

#Libraries
library(ISLR2)
library(readxl)


excel_data <- read_excel("test data excel.xlsx")
age_assur.mini <- excel_data[1:11, 20:34] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; first 11 obs & the 15 columns dedicated to age assurance opinions on websites.
summary(age_assur.mini)
str(age_assur.mini)

#Converts non-numeric data into numeric
age_assur.mini <- data.frame(lapply(age_assur.mini, as.numeric))
str(age_assur.mini)  # Check again to confirm all columns are numeric

#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 

#p.value tests
age_assur.pvalue <- rep(0,15)
for(i in 1:15)
  age_assur.pvalue[i] <- t.test(age_assur.mini[,i], mu = 0)$p.value
age_assur.pvalue

q.values.BH <- p.adjust(age_assur.pvalue, method = "BH")
q.values.BH[1:10]

#Revisit later
sum(q.values.BH <= 0.1) #In this case, it returns 14 for q-value <= 0.1. There's evidence to reject the null hypothesis (that the mean is zero) at the 10% FDR level for 14 of the website categories. This suggests that the attitudes measure in 14 of these categories significantly differ from zero, controlling for the FDR. Either 1 or 2 (10% of 14) of these websites are likely to be false discoveries.
sum(age_assur.pvalue <= (0.1 / 15)) #By contrast, using the Bonferroni method, we'd only reject 3 null hypotheses.



#Implementing Benjamini-Hochberg procedure
ps <- sort(age_assur.pvalue)
m <- length(age_assur.pvalue)
q <- 0.1
wh.ps <- which(ps < q * (1:m) / m)
if(length(wh.ps) > 0) {
  wh <- 1:max(wh.ps)
} else {
    wh <- numeric(0)
  }

plot(ps,log = "xy", ylim = c(4e-6,1), ylab = "P-value",
     xlab = "Index", main = "")
points(wh,ps[wh], col = 4)

abline(a = 0, b = (q / m), col = 2, untf = TRUE) # <- FWER: 3 null hypotheses are rejected
abline(h = 0.1 / 15, col = 3) # <- FDR: 14 null hypotheses are rejected


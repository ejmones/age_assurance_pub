#Family Wise Error Rate (FWER)
#If the null hypothesis is true for each of m independent hypothesis tests, then the FWER = 1 - (1 - α) ^ m

#Libraries
library(readxl)
library(ISLR2) 

excel_data <- read_excel("test data excel.xlsx")
age_assur.mini <- excel_data[1:100, 20:34] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; first 100 obs & the 15 columns dedicated to age assurance opinions on websites.
summary(age_assur.mini)
str(age_assur.mini)

#Converts non-numeric data into numeric
age_assur.mini <- data.frame(lapply(age_assur.mini, as.numeric))
str(age_assur.mini)  # Check again to confirm all columns are numeric

#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 

#t-tests
t.test(age_assur.mini[, 1], mu = 0)

#p.value tests
age_assur.pvalue <- rep(0,15)
for( i in 1:15)
  age_assur.pvalue[i] <- t.test(age_assur.mini[,i], mu = 0)$p.value
age_assur.pvalue


#Adjust p-values; choose any of the following two
p.adjust(age_assur.pvalue, method = "bonferroni")
p.adjust(age_assur.pvalue, method = "holm")
apply(age_assur.mini, 2, mean) # <- Applies mean function to each column

t.test(age_assur.mini[,1],age_assur.mini[,2], pair = T) #If p-value <= 0.05, this suggests a statistically significant difference between column 1 & 2. In this case, a statistically significant difference between online gaming sites and adult content/pornography.

#Use TukeyHSD()
scores <- as.vector(as.matrix(age_assur.mini))
website <- rep(colnames(age_assur.mini), each = nrow(age_assur.mini))
a1 <- aov(scores ~ website)
TukeyHSD(x = a1)
plot(TukeyHSD(x = a1)) #Any that overlap zero are said to NOT have a statistically significant difference when controlling FWER at 0.05.
#Note: FWER and Tukey is complicated if done with many variables like in this case.

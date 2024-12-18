#Re-Sampling Approach for multiple testing
#Useful if no theoretical null distribution is available which is typically the case if you're testing an unusual null hypothesis H0, or using an unusual test statistic T.

library(ISLR2)
library(MASS)
attach(Khan)

library(readxl)
excel_data <- read_excel("test data excel.xlsx")
age_assur.mini <- excel_data[1:100, 20:34] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; first 100 obs & the 15 columns dedicated to age assurance opinions on websites.
summary(age_assur.mini)
str(age_assur.mini)
#Converts non-numeric data into numeric
age_assur.mini <- data.frame(lapply(age_assur.mini, as.numeric))
str(age_assur.mini)  # Check again to confirm all columns are numeric
#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 


summary(age_assur.mini)
boxplot(age_assur.mini)




t.out <- t.test(age_assur.mini[,11], mu = 0)
TT <- t.out$statistic
TT
t.out$p.value


set.seed(1)
B <- 10000
Tbs <- rep(NA, B)
n <- nrow(age_assur.mini)

for(b in 1:B) {
  dat <- sample(age_assur.mini[,11],n,replace = TRUE)
  Tbs[b] <- t.test(dat, mu = 0)$statistic
  
  
  
}



x <- rbind(xtrain, xtest, data = "Khan")
y <- c(as.numeric(ytrain), as.numeric(ytest))
dim(x) #Observations of 83 patients for 2308 genes
dim(age_assur.mini) #Observations of 

table(y) #4 classes of cancer

x <- as.matrix(x)
x1 <- x[which(y == 2), ] #Compare the mean expression in the second class to the mean expression in the fourth class
x2 <- x[which(y == 4), ]
n1 <- nrow(x1)
n2 <- nrow(x2)
t.out <- t.test(x1[,11],x2[,11], var.equal = TRUE)
TT <- t.out$statistic
TT #Performing a 2-sample t-test on the 11th gene produces a test statistic of -2.09 and an associated p-value of 0.412, suggesting modest evidence of a difference in mean expression levels between the two cancer types.
t.out$p.value


#Split the 52 patients into groups of 29 and 25
set.seed(1)
B <- 10000 #Number of repeats
Tbs <- rep(NA, B)
for (b in 1:B) {
  dat <- sample(c(x1[,11], x2[,11]))
  Tbs[b] <- t.test(dat[1:n1], dat[(n1 + 1):(n1 + n2)],
                   var.equal = TRUE
                   )$statistic
}

mean((abs(Tbs) >= abs(TT))) # 0.0416, is the re-sampling based p-value for this case.


#Plotting the histogram of the re-sampling-based test statistics
hist(Tbs, breaks = 100, xlim = c(-4.2,4.2), main = "",
     xlab = "Null Distribution of Test Statistic", col = 7)
lines(seq(-4.2,4.2, len = 1000),
      dt(seq(-4.2,4.2, len = 1000),
         df = (n1 + n2 - 2)
      ) * 1000, col = 2, lwd = 3)
abline(v = TT, col = 4, lwd = 2)
text(TT + 0.5, 350, paste("T = ", round(TT,4), sep = ""),
     col = 4)
#In the above plot, the re-sampling-based null distribution is almost identical to the theoretical null distribution, which is displayed in red.


# Note: The below code takes a lot of time (~few minutes to run). If you're in a rush, you could set B equal to a smaller value.
m <- 100
B <- 100 #Number of repeats
set.seed(1)
index <- sample(ncol(x1), m)
Ts <- rep(NA, m)
Ts.star <- matrix(NA, ncol = m, nrow = B)
for(j in 1:m) {
  k <- index[j]
  Ts[j] <- t.test(x1[,k], x2[,k],
                  var.equal = TRUE
                  )$statistic
  for(b in 1:B) {
    dat <- sample(c(x1[,k],x2))
    Ts.star[b,j] <- t.test(dat[1:n1],
                           dat[(n1+1):(n1+n2)], var.equal = TRUE
                           )$statistic
}
}
  

cs <- sort(abs(Ts))
FDRs <- Rs <- Vs <- rep(NA,m)
for (j in 1:m) {
  R <- sum(abs(Ts) >= cs[j])
  V <- sum(abs(Ts.star) >= cs[j]) / B
  Rs[j] <- R
  Vs[j] <- V
  FDRs[j] <- V/ R
}
  
max(Rs[FDRs <= 0.1])
sort(index[abs(Ts) >= min(cs[FDRs < 0.1])])
max(Rs[FDRs <= 0.2])
sort(index[abs(Ts) >= min(cs[FDRs < 0.2])])


# The estimated false discovery rate vs the number of rejected null hypotheses, for 100 genes randomly selected from the Khan dataset.
plot(Rs, FDRs, xlab = "Number of Rejections", type = "l",
     ylab = "False Discovery Rate", col = 4, lwd = 3)


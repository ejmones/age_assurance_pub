



#Libraries
library(ISLR2)
library(readxl)

excel_data <- read_excel("test data excel.xlsx")
age_assur.mini <- excel_data[1:11, 20:34 37:50] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; first 11 obs & the 15 columns dedicated to age assurance opinions on websites.
summary(age_assur.mini)
str(age_assur.mini)

#Converts non-numeric data into numeric
age_assur.mini <- data.frame(lapply(age_assur.mini, as.numeric))
str(age_assur.mini)  # Check again to confirm all columns are numeric

#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 








x <- rnorm(100)
y <- rnorm(100)
plot(x,y)
plot(x,y, xlab = "this is the x-axis",
     ylab = "y-axis",
     main = "Plot of the X vs Y")


pdf("Figure.pdf")
plot(x,y, col = "green")
dev.off()


x <- seq(1,10)
x
x <- 1:10
x
x <- seq(-pi,pi, length = 50)

y <- x
f <- outer(x,y, function(x,y) cos(y) / (1 + x^2))
contour(x,y,f)
contour(x,y,f, nlevels = 45, add = T)

fa <- (f - t(f)) / 2
contour(x,y, fa, nlevels = 15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa, theta = 30)
persp(x,y,fa, theta = 30, phi = 20)
persp(x,y,fa, theta = 30, phi = 70)

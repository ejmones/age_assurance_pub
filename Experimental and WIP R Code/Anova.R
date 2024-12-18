

library(ISLR2)
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
# Display the first few rows of the data
head(age_assur.mini)



#You should be able to adapt the following code to fit the regressions of five countries and sequentially compare the fit of country1 to all the other countries.
fit1 <- lm(Online.gaming.sites~Online.gaming.sites, data = age_assur.mini)
summary(fit1)

fit2 <- lm(wage~poly(age,2), data = Wage)
fit3 <- lm(wage~poly(age,3), data = Wage)
fit4 <- lm(wage~poly(age,4), data = Wage)
fit5 <- lm(wage~poly(age,5), data = Wage)


#The anova() function performs a hypothesis test comparing the two models.
#The null hypothesis is that the two models it the data equally well,
#The alternative hypothesis is that there is a significant difference in the fit of the models between the countries.
anova(fit1,fit2,fit3,fit4,fit5) #In the example data I used, the p-value comparing the fit1 to fit2 is essentially zero (<10^-15), indicating that a linear fit isn't sufficient. So we reject the null hypothesis




# Possible linear regression model for US
US <- lm(website ~ acceptance * age_assurance, data = subset(data, country == "US"))

# Australia
Aus <- lm(website ~ acceptance * age_assurance, data = subset(data, country == "Aus"))

# India
Ind <- lm(website ~ acceptance * age_assurance, data = subset(data, country == "Ind"))

# France
Fr <- lm(website ~ acceptance * age_assurance, data = subset(data, country == "Fr"))

# China
PRC <- lm(website ~ acceptance * age_assurance, data = subset(data, country == "PRC"))





#The anova() function performs a hypothesis test comparing the two models.
#The null hypothesis is that the two models it the data equally well,
#The alternative hypothesis is that there is a significant difference in the fit of the models between the countries.
anova(fit1,fit2,fit3,fit4,fit5) #In the example data I used, the p-value comparing the fit1 to fit2 is essentially zero (<10^-15), indicating that a linear fit isn't sufficient. So we reject the null hypothesis.

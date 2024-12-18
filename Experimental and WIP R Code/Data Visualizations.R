


#Libraries
library(ISLR2)
library(readxl)
library(gapminder)
library(ggplot2)
library(dplyr)

excel_data <- read_excel("test data excel.xlsx")
age_assur.mini <- excel_data[1:110, 20:34] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; first 110 obs & the 15 columns dedicated to age assurance opinions on websites.
summary(age_assur.mini)
str(age_assur.mini)

#Converts non-numeric data into numeric
age_assur.mini <- data.frame(lapply(age_assur.mini, as.numeric))
str(age_assur.mini)  # Check again to confirm all columns are numeric

#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 
summary(age_assur.mini)



by_website <- age_assur.mini %>%
  group_by(continent) %>%
  filter(year == 1952) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) +
  geom_col()


age_assur.mini$Online.gaming.sites

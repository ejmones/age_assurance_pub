

install.packages("networkD3")
install.packages("dplyr")
install.packages("tidytext")

library(networkD3)
library(dplyr)
library(tidytext)
library(ggplot2)



excel_data <- read_excel("test data excel.xlsx")
age_assur.mini <- excel_data[1:250, 20:34] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; first 250 obs & the 15 columns dedicated to age assurance opinions on websites.
reasons.why <- excel_data[1:250, 35]

#Converts non-numeric data into numeric
age_assur.mini <- data.frame(lapply(age_assur.mini[, 1:15], as.numeric))
str(age_assur.mini)  # Check again to confirm all columns are numeric

age_assur.mini$reasons.why <- reasons.why
#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 


# Tokenize the Keywords column
keywords_df <- age_assur.mini %>% 
  unnest_tokens(word, Keywords) %>%
  count(word) %>%
  filter(n > 1) # Filter to keep frequent words for simplicity

# Create nodes data frame
nodes <- data.frame(name = c(as.character(unique(age_assur.mini$Dating.apps)), as.character(keywords_df$word)))

# Create source and target indexes
age_assur.mini_long <- age_assur.mini %>%
  unnest_tokens(word, Keywords)

links <- age_assur.mini_long %>%
  filter(word %in% keywords_df$word) %>%
  mutate(source = match(Dating.apps, nodes$name) - 1,
         target = match(word, nodes$name) - 1) %>%
  group_by(source, target) %>%
  summarise(value = n()) %>%
  ungroup()


# Create and plot the Sankey diagram
sankey <- sankeyNetwork(Links = links, Nodes = nodes,
                        Source = "source", Target = "target",
                        Value = "value", NodeID = "name",
                        units = "T", fontSize = 12, nodeWidth = 30)

# Print the Sankey Diagram
sankey


















library(dplyr)
library(readxl)
library(tidyr)
library(multcompView)
library(ggsignif)
library(stringr)
library(ggplot2)

#The datasets of Australia, India, Singapore, and United States respectively:
AUS.df <- read_excel("Datasets/Australia_ Age Assurance_August 2, 2024_08.36.xlsx")
IND.df <- read_excel("Datasets/India_ Age Assurance_August 2, 2024_08.37.xlsx")
SGP.df <- read_excel("Datasets/Singapore_ Age Assurance_August 2, 2024_07.10.xlsx")
USA.df <- read_excel("Datasets/USA_ Age Assurance_August 2, 2024_08.35.xlsx")


excel_data <- USA.df
excel_data.mini <- excel_data[-1,] #Excludes first row, which is dedicated to questions.
age_assur.mini <- excel_data[-1, 19:33] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; Skips the 1st row (which is a question for the survey participant) and selects columns 19 to 33, five star rating.
age_assur.withquestion <- excel_data[, 19:33] #ATTITUDES ABOUT AGE ASSURANCE TECHNOLOGY; Includes 1st row and selects columns 19 to 33, five star rating.

#Renames two columns
age_assur.mini <- age_assur.mini %>%
  rename(
    News = identity.att_15,
    `Mature Literature` = identity.att_16
  )

#NA check: handle missing values. Removes rows with any NAs in at least one column
age_assur.mini <- na.omit(age_assur.mini) 


#Platforms data frame
platforms <- age_assur.mini[c(colnames(age_assur.mini))]

sapply(platforms[, 1:15], function(x) sum(is.na(x)))
platforms <- mutate_at(platforms, vars(1:15), as.numeric)

# Stats
summary_stats <- summary(platforms, na.rm = TRUE)
summary_stats

gathered_data <- gather(platforms, key = "Website.Type", value = "Value")
#View(gathered_data)

# Calculate the overall mean
overall_mean <- mean(gathered_data$Value, na.rm = TRUE)
overall_mean

# FINAL: Plotting with 95% confidence intervals

ggplot(gathered_data, aes(x = reorder(Website.Type, Value, FUN = function(x) mean(x, na.rm = TRUE)), y = Value)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) +  # Add 95% CI
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black") +  # Add mean points
  geom_hline(yintercept = overall_mean, linetype = "dashed", color = "blue") +  # Add overall mean line
  labs(title = "USA: Support for Age Assurance by Website Type", x = "Website Type", y = "1= str. oppose, 5= str.support") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(1, 5, 1), limits = c(1, 5))  # Adjust limits accordingly


############################

#Bar plot
# Select columns 36 to 50
selected_columns <- excel_data.mini[, 36:50]

# Convert the dataframe to long format and separate values
long_data <- selected_columns %>%
  mutate_all(as.character) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  separate_rows(value, sep = ",") %>%
  filter(!is.na(value), value %in% c("1", "2", "3", "4", "5"))

# Count the occurrences of each value for each column
value_counts <- long_data %>%
  group_by(variable, value) %>%
  tally() %>%
  mutate(total = sum(n), proportion = n / total) %>%
  select(variable, value, proportion) %>%
  arrange(variable, value) %>%
  spread(value, proportion) %>%
  replace(is.na(.), 0)  # Replace NAs with 0 for missing proportions

# Transpose data for plotting
plot_data <- as.data.frame(t(value_counts[,-1]))  # Exclude the 'variable' column
colnames(plot_data) <- value_counts$variable
rownames(plot_data) <- c("1", "2", "3", "4", "5")

# View the proportions
print(value_counts)


# Adjust margins to accommodate x-axis labels and legend
par(mar = c(12, 4, 4, 2) + 0.1)  # Increase bottom margin to make space for legend

# Convert proportions to percentages
plot_data_percent <- plot_data * 100

# Create the bar plot with percentage y-axis
bar_positions <- barplot(
  as.matrix(plot_data_percent),
  beside = TRUE,
  col = rainbow(5),
  main = "USA: Desired Age Assurance Measures",
  ylab = "Proportion (%)",
  las = 2,  # Rotate x-axis labels
  cex.names = 0.8,  # Smaller font size for labels
  args.legend = list(x = "topright")
)

legend(
  x = mean(bar_positions) -30,  # Center the legend beneath the plot
  y = -40,  # Adjust the "y" position to lie beneath the x-axis labels
  legend = Method_list, #OG: plot_data
  fill = rainbow(5), 
  cex = 0.8,  # Smaller text size
  pt.cex = 0.8,  # Smaller points
  horiz = FALSE,  # Make the legend horizontal
  xpd = TRUE  # Allow drawing outside plot region
)



# Load required libraries
library(readxl)
library(tidyverse)
library(fossil)
library(vegan)
library(reshape2)
library(RColorBrewer)
library(ggplot2)

# Read the Excel file
documents_path <- "/Users/jamilshuvo/Desktop/Plot_result.xlsx"
result <- read_excel(documents_path)

# Filter data based on the specified Phylum (e.g., Arthropoda)
target_Phylum <- "Arthropoda"
filtered_data_Phylum <- result %>% filter(Phylum == target_Phylum)

# Specify the columns you want to keep
columns_to_keep <- c(90, 1:79)
Arthropoda <- filtered_data_Phylum[, columns_to_keep]

# Convert character columns to numeric
Arthropoda[, 2:80] <- sapply(Arthropoda[, 2:80], as.numeric)

# Remove rows with zero abundance
Arthropoda <- Arthropoda[rowSums(Arthropoda[, 2:80]) != 0, ]

# Reshape the data frame into long format
df <- Arthropoda %>%
  pivot_longer(cols = -Species, names_to = "Sample", values_to = "Abundance")

# Extract the source from the Sample column
df$Source <- sub("^(\\w+)_\\d+", "\\1", df$Sample)

# Select relevant columns
df <- df[, c(1, 4, 2, 3)]

# Calculate indices for each sample
ace_results_sample <- tapply(df$Abundance, df$Sample, specnumber)
chao1_results_sample <- tapply(df$Abundance, df$Sample, function(x) as.numeric(chao1(x)))
shannon_results_sample <- tapply(df$Abundance, df$Sample, function(x) diversity(x, index = "shannon"))
simpson_results_sample <- tapply(df$Abundance, df$Sample, function(x) diversity(x, index = "simpson"))

# Create data frames for visualization
ace_data_sample <- data.frame(Sample = names(ace_results_sample), Ace = unlist(ace_results_sample))
chao1_data_sample <- data.frame(Sample = names(chao1_results_sample), Chao1 = unlist(chao1_results_sample))
shannon_data_sample <- data.frame(Sample = names(shannon_results_sample), Shannon = unlist(shannon_results_sample))
simpson_data_sample <- data.frame(Sample = names(simpson_results_sample), Simpson = unlist(simpson_results_sample))

# Merge data frames
combined_data <- merge(ace_data_sample, chao1_data_sample, by = "Sample")
combined_data <- merge(combined_data, shannon_data_sample, by = "Sample")
combined_data <- merge(combined_data, simpson_data_sample, by = "Sample")

# Melt the data for ggplot
melted_data_sample <- melt(combined_data, id.vars = "Sample")

# Extract source from the Sample column
melted_data_sample$Source <- sub("^(\\w+)_\\d+", "\\1", melted_data_sample$Sample)

# Select relevant columns
melted_data_sample_2 <- melted_data_sample[, c(4, 2, 3)]

# Convert Source column to a factor
melted_data_sample_2$Source <- as.factor(melted_data_sample_2$Source)
summary(melted_data_sample_2)

# Define a custom color palette for sources
source_colors <- scales::hue_pal()(length(unique(melted_data_sample_2$Source)))

# Create box plots for Ace, Chao1, Shannon, and Simpson indices
index_boxplot <- function(data, variable_name) {
  ggplot(data, aes(x = Source, y = value, fill = Source)) +
    geom_boxplot(data = subset(data, variable == variable_name)) +
    labs( x = "Source", y = paste(variable_name, "Index")) +
    scale_fill_manual(values = source_colors) +  # Set custom source colors
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Create box plots for each index
ace_boxplot <- index_boxplot(melted_data_sample_2, "Ace")
chao1_boxplot <- index_boxplot(melted_data_sample_2, "Chao1")
shannon_boxplot <- index_boxplot(melted_data_sample_2, "Shannon")
simpson_boxplot <- index_boxplot(melted_data_sample_2, "Simpson")

# Print or save the plots as needed
print(ace_boxplot)
print(chao1_boxplot)
print(shannon_boxplot)
print(simpson_boxplot)

#save the file as SVG
#ggsave("ace_boxplot.svg", plot = ace_boxplot, device = "svg")
#ggsave("chao1_boxplot.svg", plot = chao1_boxplot, device = "svg")
#ggsave("shannon_boxplot.svg", plot = shannon_boxplot, device = "svg")
#ggsave("simpson_boxplot.svg", plot = simpson_boxplot, device = "svg")



################### Statistical test

# Assuming your data table is named "your_data_table"
library(dplyr)
# Filter the data to include only "Ace" values
ace_data <- melted_data_sample_2 %>% filter(variable == "Ace")
## Create a new Excel file with the selected columns
#new_file_path <- "ace_data.xlsx"
#write_xlsx(ace_data, new_file_path)
#new_file_path <- "Chao1_data.xlsx"
#write_xlsx(Chao1_data, new_file_path)
#new_file_path <- "Simpson_data.xlsx"
#write_xlsx(Simpson_data, new_file_path)
#new_file_path <- "Shannon_data .xlsx"
#write_xlsx(Shannon_data , new_file_path)


# Log-transform the data
ace_data$Value_log <- log(ace_data$value)

# Perform one-way ANOVA
# Perform one-way ANOVA on the transformed data
anova_result <- aov(Value_log ~ Source, data = ace_data)

# Summarize the ANOVA results
summary(anova_result)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(anova_result))
# Create a Q-Q plot for the Shapiro-Wilk test
qqnorm(residuals(anova_result))
qqline(residuals(anova_result))

#Is there any significant difference among the sources.


# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("There is a significant difference among the sources.\n")
} else {
  cat("There is no significant difference among the sources.\n")
}
# There is a significant difference among the sources.


# Levene's test for homogeneity of variances
library(car)
leveneTest(Value_log ~ Source, data = ace_data)

# Perform post-hoc tests (Tukey's HSD)
posthoc <- TukeyHSD(anova_result)
print(posthoc)

# Perform post-hoc tests (Bonferroni)
library(multcomp)
mc <- glht(anova_result, linfct = mcp(Source = "Tukey"))
summary(mc)



#########################
#######################################



# Filter the data to include only "Achao1" values
Chao1_data <- melted_data_sample_2 %>% filter(variable == "Chao1")
summary(Chao1_data)

# Log-transform the data
Chao1_data$Value_log <- log(Chao1_data$value)

# Perform one-way ANOVA
# Perform one-way ANOVA on the transformed data
anova_result_Chao1 <- aov(Value_log ~ Source, data = Chao1_data)

# Summarize the ANOVA results
summary(anova_result_Chao1)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(anova_result_Chao1))
# Create a Q-Q plot for the Shapiro-Wilk test
qqnorm(residuals(anova_result_Chao1))
qqline(residuals(anova_result_Chao1))
#Is there any significant difference among the sources.


# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (summary(anova_result_Chao1)[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("There is a significant difference among the sources.\n")
} else {
  cat("There is no significant difference among the sources.\n")
}
# There is a significant difference among the sources.


# Levene's test for homogeneity of variances
library(car)
leveneTest(Value_log ~ Source, data = Chao1_data)

# Perform post-hoc tests (Tukey's HSD)
posthoc <- TukeyHSD(anova_result_Chao1)
print(posthoc)

# Perform post-hoc tests (Bonferroni)
library(multcomp)
mc <- glht(anova_result_Chao1, linfct = mcp(Source = "Tukey"))
summary(mc)











######################################################
#####################################################################

# Filter the data to include only "Ace" values
Shannon_data <- melted_data_sample_2 %>% filter(variable == "Shannon")


# Log-transform the data
Shannon_data$Value_log <- log(Shannon_data$value)

# Perform one-way ANOVA
# Perform one-way ANOVA on the transformed data
anova_result_Shannon_data <- aov(Value_log ~ Source, data = Shannon_data)

# Summarize the ANOVA results
summary(anova_result_Shannon_data)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(anova_result_Shannon_data))
# Create a Q-Q plot for the Shapiro-Wilk test
qqnorm(residuals(anova_result_Shannon_data))
qqline(residuals(anova_result_Shannon_data))
#Is there any significant difference among the sources.


# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (summary(anova_result_Shannon_data)[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("There is a significant difference among the sources.\n")
} else {
  cat("There is no significant difference among the sources.\n")
}
# There is a significant difference among the sources.


# Levene's test for homogeneity of variances
library(car)
leveneTest(Value_log ~ Source, data = Shannon_data)

# Perform post-hoc tests (Tukey's HSD)
posthoc <- TukeyHSD(anova_result_Shannon_data)
print(posthoc)

# Perform post-hoc tests (Bonferroni)
library(multcomp)
mc <- glht(anova_result_Shannon_data, linfct = mcp(Source = "Tukey"))
summary(mc)
















# Filter the data to include only "Ace" values
Simpson_data <- melted_data_sample_2 %>% filter(variable == "Simpson")

# Log-transform the data
Simpson_data$Value_log <- log(Simpson_data$value)

# Perform one-way ANOVA
# Perform one-way ANOVA on the transformed data
anova_result_Simpson_data <- aov(Value_log ~ Source, data = Simpson_data)

# Summarize the ANOVA results
summary(anova_result_Simpson_data)

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(anova_result_Simpson_data))
# Create a Q-Q plot for the Shapiro-Wilk test
qqnorm(residuals(anova_result_Simpson_data))
qqline(residuals(anova_result_Simpson_data))
#Is there any significant difference among the sources.


# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (summary(anova_result_Simpson_data)[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("There is a significant difference among the sources.\n")
} else {
  cat("There is no significant difference among the sources.\n")
}
# There is a significant difference among the sources.


# Levene's test for homogeneity of variances
library(car)
leveneTest(Value_log ~ Source, data = Simpson_data)

# Perform post-hoc tests (Tukey's HSD)
posthoc <- TukeyHSD(anova_result_Simpson_data)
print(posthoc)

# Perform post-hoc tests (Bonferroni)
library(multcomp)
mc <- glht(anova_result_Simpson_data, linfct = mcp(Source = "Tukey"))
summary(mc)

##################################

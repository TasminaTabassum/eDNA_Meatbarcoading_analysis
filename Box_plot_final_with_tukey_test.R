library("phyloseq")
library("ggplot2")
library("dplyr")
library("ggpubr")
library("tidyverse")
library("readxl")



# Read the Excel file
documents_path <- "/Users/jamilshuvo/Desktop/Rearefraction 2.xlsx"
excel_data<- read_excel(documents_path)


# Convert columns to binary (0 for 0, 1 for any other value)
data_raw <- excel_data %>%
  mutate_at(vars(c(2:1033)), list(~ifelse(. == 0, 0, 1)))

# To sum the species
data_raw$OTUs <- rowSums(data_raw[, 2:1033])
columns_to_keep <- c(1,1034)
box <- data_raw[, columns_to_keep]


######
#to change the name of the sample
box$Sample <- gsub("(LDS|SDS|SS|SDBR)_\\d+", "\\1", box$Sample)


#######To see the summary

# Convert the column from character to factor
box$Sample <- as.factor(box$Sample )
str(box)
summary(box)


########

my_plot <-ggplot(box, aes(x=Sample, y=OTUs, fill=Sample)) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none") +
  scale_fill_brewer(palette = "Set1", aesthetics = "fill")+
  labs(
    title = "",
    x = "",
    y = "Observed OTUs"
  )
ggsave("my_plot.svg", width = 20, height = 20)
#########



####################

# Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals(anova_result))
# Create a Q-Q plot for the Shapiro-Wilk test
qqnorm(residuals(anova_result))
qqline(residuals(anova_result))

# Print the Shapiro-Wilk test results
print(shapiro_test)

#########

#Is there any significant difference among the sources.



# Combine the data into a single data frame
data <- data.frame(box
)

# Log-transform the data
data$Value_log <- log(data$OTUs)

# Perform one-way ANOVA
# Perform one-way ANOVA on the transformed data
anova_result <- aov(Value_log ~ Sample, data = data)

# Summarize the ANOVA results
summary(anova_result)

# Check if the p-value is less than your chosen significance level (e.g., 0.05)
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("There is a significant difference among the sources.\n")
} else {
  cat("There is no significant difference among the sources.\n")
}
# There is a significant difference among the sources.


# Levene's test for homogeneity of variances
library(car)
leveneTest(OTUs ~ Sample, data = data)

# Perform post-hoc tests (Tukey's HSD)
posthoc <- TukeyHSD(anova_result)
print(posthoc)

# Perform post-hoc tests (Bonferroni)
library(multcomp)
mc <- glht(anova_result, linfct = mcp(Sample = "Tukey"))
summary(mc)





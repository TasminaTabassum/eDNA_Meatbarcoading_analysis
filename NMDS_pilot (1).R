install.packages("readxl")
library(readxl)
library(vegan)
library(ggplot2)
library(BBmisc)
library(tidyverse)
library(ggnewscale)

# Read the Excel file
documents_path <- "/Users/jamilshuvo/Desktop/Rearefraction.xlsx"
df <- read_excel(documents_path)

# delete the first two character column and Convert character columns to numeric
df1 <- df[,-c(1:3)]


df2 <- data.frame(apply(df1, 2, function(x) as.numeric(as.character(x))))

df3 = normalize(df2, method = "range", range = c(0, 1))

# Calculate Bray-Curtis dissimilarity matrix
# Replace with your preferred distance/dissimilarity metric
dist_matrix <- dist(df3)

m_com = as.matrix(dist_matrix)

#ANOSIM statistic test
ano = anosim(m_com, df$Source, distance = "bray", permutations = 9999)
ano

#pairwise
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
############
library(pairwiseAdonis)

perm_dist = vegdist(df3, method = "bray")
pano = pairwise.adonis(perm_dist, df$Source)
pano

library(writexl)
write_xlsx(pano, 'pairwise.xlsx')

#NMDS plotting and bray distance
nmds = metaMDS(m_com, distance = "bray")

data.scores = as.data.frame(scores(nmds))

#add columns to data frame 
data.scores$Source = df$Source
data.scores$Plot = as.factor(df$Plot)

#Add/calculate spider diagram
centroids <- aggregate(cbind(NMDS1, NMDS2) ~ Source, data = data.scores, FUN = mean)
seg <- merge(data.scores, setNames(centroids, c('Source','oNMDS1','oNMDS2')),
             by = 'Source', sort = FALSE)

#plot for all

gg2 <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2, colour = Source, shape = Source)) +
  geom_point(data = centroids, size = 3) +                    # add centroids
  geom_point() +                                              
  coord_fixed(ratio = 1) +                                             
  theme_bw() +
  labs(colour = "Methods", shape = "Methods") +
  scale_color_manual(values = c("LDS" = "pink", "SDS" = "blue", "SS" = "purple", "TSRS" = "orange")) +  # Manually specify colors for each class
  scale_shape_manual(values = c("LDS" = 16, "SDS" = 17, "SS" = 15, "TSRS" = 18)) +  # Manually specify shapes for each class
  theme(legend.position="right", legend.text=element_text(size=10), legend.direction='vertical')
gg2
######
# Save the combined plot to a file with specific dimensions
ggsave("NMDS_Plots.jpeg", plot = gg2, width = 10, height = 5, units = "in", dpi = 300)

ggsave("NMDS_Plots.svg", plot = gg2, width = 10, height = 5, units = "in", dpi = 300)

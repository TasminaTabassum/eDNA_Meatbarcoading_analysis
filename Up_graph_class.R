# Load the package
install.packages("readxl")  # Install the readxl package if not installed
library(readxl)             
install.packages("dplyr")  # Install the dplyr package if not installed
library(dplyr)
install.packages("writexl")
library(writexl)
library(UpSetR)

# Read the Excel file
documents_path <- "/Users/jamilshuvo/Desktop/final_data_Class.xlsx"
Raw_data <- read_excel(documents_path)
columns_to_keep <- c(7, 11:15)
Class_data <- Raw_data[, columns_to_keep]
new_file_path <- "Class_final.xlsx"
write_xlsx(Class_data, new_file_path)

##################################################


#Sub class filter
# Filter data based on the specified Class
documents_path <- "/Users/jamilshuvo/Desktop/Class_final.xlsx"
df <- read_excel(documents_path)
# Insecta
target_Class <- "Insecta"
filtered_data_Insecta <- df[df$Class == target_Class, ]
print(filtered_data_Insecta)
new_file_path <- "Insecta_final.xlsx"
write_xlsx(filtered_data_Insecta, new_file_path)




########################
# Collembola
target_Class <- "Collembola"
filtered_data_Collembola <- df[df$Class == target_Class, ]
print(filtered_data_Collembola)
new_file_path <- "Collembola_final.xlsx"
write_xlsx(filtered_data_Collembola, new_file_path)


#############################################

# Arachnida
target_Class <- "Arachnida"
filtered_data_Arachnida <- df[df$Class == target_Class, ]
print(filtered_data_Arachnida)
new_file_path <- "Arachnida_final.xlsx"
write_xlsx(filtered_data_Arachnida, new_file_path)


###############################


# Others
target_Class <- c("Malacostraca","Hexanauplia","Pycnogonida","Chilopoda","Diplopoda")
filtered_data_others <- df[df$Class %in% target_Class, ]
print(filtered_data_others)
new_file_path <- "others_final.xlsx"
write_xlsx(filtered_data_others, new_file_path)

############################################



# Upset for Different classes
# Insecta
documents_path <- "/Users/jamilshuvo/Desktop/Insecta_final.xlsx"
Insecta_raw <- read_excel(documents_path)
columns_to_keep <- c(2:6)
Insecta <- Insecta_raw[ ,columns_to_keep]

# Update the source columns to show 0 for 0 and 1 for any other number
Insecta[, -1] <- ifelse(Insecta[, -1] == 0, 0, 1)
df_I <- as.data.frame(Insecta)
print(df_I)
# Upset plot code

upset(data = df_I, nsets = 4, nintersects = NA, sets = c("SDS", "LDS", "SS", "SDBR"),
      keep.order = F, set.metadata = NULL, intersections = NULL, matrix.color = "gray40",
      main.bar.color = "darkorange", mainbar.y.label = "Insecta")
# Load the necessary libraries
library(UpSetR)
library(ggplot2)

# Create the UpSet plot
upset_plot <- upset(data = df_I, nsets = 4, nintersects = NA, sets = c("SDS", "LDS", "SS", "SDBR"),
                    keep.order = FALSE, set.metadata = NULL, intersections = NULL,
                    matrix.color = "gray40", main.bar.color = "darkorange", mainbar.y.label = "Insecta")

# Save the plot as an SVG file
pdf("upset_plot.pdf", width = 7.17, height = 5.93)
print(upset_plot)
dev.off()


#######################################




#Upset plot for Collombola

documents_path <- "/Users/jamilshuvo/Desktop/Collembola_final.xlsx"
Collembola_raw <- read_excel(documents_path)
columns_to_keep <- c(2:6)
Collembola <- Collembola_raw[ ,columns_to_keep]

# Update the source columns to show 0 for 0 and 1 for any other number
Collembola[, -1] <- ifelse(Collembola[, -1] == 0, 0, 1)
df_C <- as.data.frame(Collembola)
print(df_C)
# Upset plot code

upset_plot_C <- upset(data = df_C, nsets = 4, nintersects = NA, sets = c("SDS", "LDS", "SS", "SDBR"),
      keep.order = F, set.metadata = NULL, intersections = NULL, matrix.color = "gray40",
      main.bar.color = "coral4", mainbar.y.label = "Collembola")
pdf("upset_plot_C.pdf", width = 7.17, height = 5.93)
print(upset_plot_C)
dev.off()

####################################

#Upset plot for Arachnida

documents_path <- "/Users/jamilshuvo/Desktop/Arachnida_final.xlsx"
Arachnida_raw <- read_excel(documents_path)
columns_to_keep <- c(2:6)
Arachnida <- Arachnida_raw[ ,columns_to_keep]

# Update the source columns to show 0 for 0 and 1 for any other number
Arachnida[, -1] <- ifelse(Arachnida[, -1] == 0, 0, 1)
df_A <- as.data.frame(Arachnida)
print(df_A)
# Upset plot code

upset_plot_A <- upset(data = df_A, nsets = 4, nintersects = NA, sets = c("SDS", "LDS", "SS", "SDBR"),
      keep.order = F, set.metadata = NULL, intersections = NULL, matrix.color = "gray40",
      main.bar.color = "chocolate", mainbar.y.label = "Arachnida")
pdf("upset_plot_A.pdf", width = 7.17, height = 5.93)
print(upset_plot_A)
dev.off()


###################################################


#Upset plot for Others

documents_path <- "/Users/jamilshuvo/Desktop/Others_final.xlsx"
Others_raw <- read_excel(documents_path)
columns_to_keep <- c(2:6)
Others <- Others_raw[ ,columns_to_keep]

# Update the source columns to show 0 for 0 and 1 for any other number
Others[, -1] <- ifelse(Others[, -1] == 0, 0, 1)
df_O <- as.data.frame(Others)
print(df_O)
# Upset plot code

upset_plot_O <- upset(data = df_O, nsets = 4, nintersects = NA, sets = c("SDS", "LDS", "SS", "SDBR"),
      keep.order = F, set.metadata = NULL, intersections = NULL, matrix.color = "gray40",
      main.bar.color = "lightgoldenrod1", mainbar.y.label = "Others")

pdf("upset_plot_O.pdf", width = 7.17, height = 5.93)
print(upset_plot_O)
dev.off()

###########################







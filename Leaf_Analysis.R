#packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

#create dataframe of leaf areas
df <- read_excel("~/Desktop/Spring 2024/Field Ecology Lab/Leafcutters Jan 2024/Excel Sheets/No statistics.xlsx")

#check that dataframe is created correctly
print(df)

# Reshape the data for ggplot2
df_long <- tidyr::gather(df, key = "Distance", value = "Area")

df_long$Distance <- as.character(df_long$Distance)

df_long_nona <- df_long[complete.cases(df_long$Area), ]

df_long_nona$Distance <- factor(df_long_nona$Distance, levels = unique(df_long_nona$Distance))

ggplot(df_long_nona, aes(x = Distance, y = Area)) +
  geom_boxplot() +
  labs(title = "",
       x = "Distance from Colony Main Entrance (m)",
       y = "Leaf Surface Area (cm^2)")

# Calculate means and standard errors using dplyr
summary_data <- df_long_nona %>%
  group_by(Distance) %>%
  summarize(Mean = mean(Area, na.rm = TRUE),
            SE = sd(Area, na.rm = TRUE) / sqrt(sum(!is.na(Area))))


# Plotting with ggplot2
ggplot(summary_data, aes(x = Distance, y = Mean, fill = factor(Distance))) +
  geom_bar(stat = "identity", position = "dodge", fill = ifelse(summary_data$Distance == "0", "firebrick3", "darkslategray4")) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), position = position_dodge(width = 0.8), width = 0.25) +
  labs(title = "",
       x = "Distance from Colony Main Entrance (m)",
       y = "Leaf Surface Area (cm^2)") +
  theme(legend.position = "none")

# Extract the means from the summary_data dataframe
means_vector <- summary_data$Mean
print(means_vector)

distance_vector <- summary_data$Distance
print(distance_vector)



excel_file_path <- "~/Desktop/Spring 2024/Field Ecology Lab/Leafcutters Jan 2024/Excel Sheets/Area Data.xlsx"

# Read the Excel file
df_2 <- read_excel(excel_file_path)

# Choose the specific row you want (e.g., row 5 in this example)
leaf_count_row <- df_2[2, ]

# Extract the values from the specific row into a vector
count_vector <- as.vector(unlist(leaf_count_row))
count_vector <- count_vector[count_vector != "count"]
count_vector <- count_vector[count_vector != "1553"]
count_vector <- as.numeric(count_vector)

print(count_vector)

# Choose the specific row you want (e.g., row 5 in this example)
leaf_mass_row <- df_2[1, ]

# Extract the values from the specific row into a vector
mass_vector <- as.vector(unlist(leaf_mass_row))
mass_vector <- mass_vector[mass_vector != "mass"]
mass_vector <- mass_vector[mass_vector != "39.5"]
mass_vector <- as.numeric(mass_vector)

print(mass_vector)

mass_per_leaf <- mass_vector / count_vector

mass_per_area <- mass_vector / means_vector

quadrat_data <- data.frame(
  Distance = distance_vector,
  Area = means_vector,
  Mass = mass_vector,
  Count = count_vector,
  MpreL = mass_per_leaf,
  MperA = mass_per_area
)





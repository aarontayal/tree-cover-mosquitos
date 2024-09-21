# Ent 6707 Group Project
# 9/5/24

# Libraries
library(tidyverse)
library(readxl)
library(ggplot2)

# Load in data
trees_mosquitoes<- read_excel("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/Data/mosquito_trap_locations_with_trees.xlsx")

mosquitoes<- read.csv("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/Data/2021_mosquito_trap_data.csv")
summary(mosquitoes)

# Variable corrections
mosquitoes$x <- mosquitoes$longitude
mosquitoes$y<- mosquitoes$latitude

mosquitoes$zone_name<- mosquitoes$zone.name
trees_mosquitoes$zone_name<- trees_mosquitoes$`Zone Name`

# Cleaning data so we can get count of trees at buffered traps and count of 
  # mosquitoes at buffered traps

mosquitoes_sub<- mosquitoes %>% group_by(zone_name, trap.type) %>%
  summarise(sum_bugs = sum(total.count))

mosquitoes_sub<- na.omit(mosquitoes_sub)

trees_mosquitoes_sub<- subset(trees_mosquitoes, select = c("zone_name", "OBJECTID_count",
                                                           "x", "y"))

trees_bugs_merge<- merge(mosquitoes_sub, trees_mosquitoes_sub, by = "zone_name", 
                         all.mosquitoes_sub = FALSE, all.trees_mosquitoes_sub = FALSE)

trees_bugs_unique<- trees_bugs_merge %>%
  distinct(trap.type, zone_name, .keep_all = TRUE)

# More variable corrections
trees_bugs_unique$tree_count<- trees_bugs_unique$OBJECTID_count

# Visualize
ggplot(mosquitoes, aes(x = zone_name, y = total.count)) +
  geom_bar(stat = "identity") +
  theme_classic()

ggplot(trees_bugs_unique, aes(x = tree_count, y = sum_bugs)) +
  geom_point() +
  theme_classic()

ggplot(trees_bugs_unique, aes(x = tree_count, y = sum_bugs, col = tree_count)) +
  geom_point() +
  theme_classic()
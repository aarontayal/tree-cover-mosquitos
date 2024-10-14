# Ent 6707 Group Project
# 9/5/24

# Libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(viridis)
library(car)

# Theme
theme_ew <- function (base_size=16, font=NA) { 
  theme(axis.title.x = element_text(face="bold", size=20, vjust=0.4),
        axis.text.x  = element_text(size=19, colour = "black"),
        plot.title=element_text(face="bold", size = 18,hjust=0.01),
        axis.title.y = element_text(face="bold",angle=90,size=20, vjust=2.0),
        axis.text.y  = element_text(size=19,colour = "black"),
        plot.background = element_rect(fill = NA ,colour = NA),
        axis.line=element_line(colour="black"),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =   element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.position = "right",
        legend.justification=c(1,1),
        legend.background =element_blank(),
        legend.key = element_blank(),
        legend.text =   element_text(size = rel(1.5)),
        legend.title =  element_text(size = rel(1.5), face = "bold", hjust = 0)) 
}

# Load in data
trees_mosquitoes<- read.csv("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/tree-cover-mosquitos/data/mosquito_trap_locations_with_trees.csv")
summary(trees_mosquitoes)

mosquitoes<- read.csv("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/tree-cover-mosquitos/data/2021_mosquito_trap_data.csv")
summary(mosquitoes)

# Variable corrections
mosquitoes$x <- mosquitoes$longitude
mosquitoes$y<- mosquitoes$latitude

mosquitoes$zone_name<- mosquitoes$zone.name
trees_mosquitoes$zone_name<- trees_mosquitoes$Zone.Name

# Cleaning data so we can get count of trees at buffered traps and count of 
  # mosquitoes at buffered traps

mosquitoes_sub<- mosquitoes %>% group_by(zone_name, trap.type) %>%
  summarise(sum_bugs = sum(total.count))

mosquitoes_sub<- na.omit(mosquitoes_sub)

trees_mosquitoes_sub<- subset(trees_mosquitoes, select = c("zone_name", "OBJECTID_count",
                                                           "x", "y", "Zone.ID"))

trees_bugs_merge<- merge(mosquitoes_sub, trees_mosquitoes_sub, by = "zone_name", 
                         all.mosquitoes_sub = FALSE, all.trees_mosquitoes_sub = FALSE)

trees_bugs_unique<- trees_bugs_merge %>%
  distinct(trap.type, zone_name, .keep_all = TRUE)

# More variable corrections
trees_bugs_unique$tree_count<- trees_bugs_unique$OBJECTID_count
trees_bugs_unique$zone_id<- as.factor(trees_bugs_unique$Zone.ID)

# Visualize
ggplot(mosquitoes, aes(x = zone_name, y = total.count)) +
  geom_bar(stat = "identity") +
  theme_ew()

ggplot(trees_bugs_unique, aes(x = tree_count, y = sum_bugs)) +
  geom_point() +
  theme_ew()

ggplot(trees_bugs_unique, aes(x = tree_count, y = sum_bugs, col = tree_count)) +
  geom_point(size = 4) +
  scale_x_continuous("Total number of trees") +
  scale_color_viridis() +
  scale_y_continuous("Total number of mosquitoes") +
  labs(col = "Total number of trees") +
  theme_ew()

# Summary statistics

trees_bugs_na_rm<- trees_bugs_unique[complete.cases(trees_bugs_unique), ]

trees_summary<- trees_bugs_na_rm %>% summarise(avg_trees = mean(tree_count))

bugs_summary<- trees_bugs_na_rm %>% summarise(avg_bugs = mean(sum_bugs))

# Subset for each trap type
trees_bugs_gravid<- subset(trees_bugs_unique, trap.type == "Gravid")

ggplot(trees_bugs_gravid, aes(x = zone_id, y = sum_bugs, col = tree_count)) +
  geom_point(size = 3) +
  scale_x_discrete("Zones") +
  scale_y_continuous("Total number of mosquitoes") +
  labs(col = "Total number of trees") +
  theme_ew() +
  theme(axis.text.x = element_text(size = 12))

trees_bugs_bg<- subset(trees_bugs_unique, trap.type == "BG")

ggplot(trees_bugs_bg, aes(x = zone_id, y = sum_bugs, col = tree_count)) +
  geom_point(size = 3) +
  scale_x_discrete("Zones") +
  scale_y_continuous("Total number of mosquitoes") +
  labs(col = "Total number of trees") +
  theme_ew() +
  theme(axis.text.x = element_text(size = 12))

trees_bugs_cdc<- subset(trees_bugs_unique, trap.type == "CDC")

ggplot(trees_bugs_cdc, aes(x = zone_id, y = sum_bugs, col = tree_count)) +
  geom_point(size = 3) +
  scale_x_discrete("Zones") +
  scale_y_continuous("Total number of mosquitoes") +
  labs(col = "Total number of trees") +
  theme_ew() +
  theme(axis.text.x = element_text(size = 12))

# Show species caught by each trap

traps_spp<- mosquitoes %>% group_by(trap.type) %>%
  summarise(unique_spp = n_distinct(species))

traps_spp<- subset(traps_spp, trap.type != "")

ggplot(traps_spp, aes(x = trap.type, y = unique_spp)) +
  geom_bar(stat = "identity", fill = "tomato3") +
  xlab("Trap type") +
  ylab("Number of species caught") +
  theme_ew()

traps_summary<- traps_spp %>% summarise(avg_bugs = mean(unique_spp))

# Statistics

# Mosquito abundance ~ tree density
  # We want to see if there is a relationship between tree density within a 100 
  # or 1000 m radius around CDC light traps and mosquito abundance caught by
  # these traps.

# Response: mosquito abundance, count
# Predictor: tree density within a fixed radius around mosquito trap, count

glm_abundance<- glm(sum_bugs ~ tree_count, data = trees_bugs_cdc, 
                    family = poisson(link = "log"))

summary(glm_abundance)

Anova(glm_abundance, type = "III")

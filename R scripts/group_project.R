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
trees_mosquitoes<- read.csv("data/mosquito_trap_locations_with_trees.csv")
summary(trees_mosquitoes)

mosquitoes<- read.csv("data/2021_mosquito_trap_data.csv")
summary(mosquitoes)

# Variable corrections
mosquitoes$x <- mosquitoes$longitude
mosquitoes$y<- mosquitoes$latitude

mosquitoes$zone_name<- mosquitoes$zone.name
trees_mosquitoes$zone_name<- trees_mosquitoes$Zone.Name

# Cleaning data so we can get count of trees at buffered traps and count of 
  # mosquitoes at buffered traps

mosquitoes_sub<- mosquitoes %>% group_by(zone_name, trap.type, species) %>%
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

trees_bugs_cdc<- subset(trees_bugs_merge, trap.type == "CDC")

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

#calculate richness
sp_abund<-table(trees_bugs_cdc$species,trees_bugs_cdc$zone_name)
sp_abund
sp_abund_df<-as.data.frame.matrix(sp_abund)
glimpse(sp_abund_df)

spr_cdc<-trees_bugs_cdc %>%
  group_by(zone_name, tree_count) %>%
  summarise(species.richness=n()) %>%
  arrange(-species.richness)
view(spr_cdc)

#merge tree data and richness
trees_cdc_rich<- merge(trees_cdc, spr_cdc, by = "zone_name",
                       all.trees_cdc = FALSE, all.spr_cdc = FALSE)

#calculate diversity

cdc_sp_count_by_zone <- read_excel("data/HRD trees and mosquitoes/CDC species count by site.xlsx")
View(cdc_sp_count_by_zone) 

tidy_mosq<- cdc_sp_count_by_zone %>%
  pivot_longer(-zone_name,names_to="species",values_to="abundance") %>%
  arrange(zone_name)

trees_div_cdc<-tidy_mosq %>%
  group_by(zone_name) %>%
  filter(abundance>0) %>%
  summarise(N=sum(abundance),
            shannon.di=-sum((abundance/sum(abundance))*log(abundance/sum(abundance))),
            simpson.di=1-sum((abundance/sum(abundance))^2),
            inv.simpson.di=1/sum((abundance/sum(abundance))^2)) %>%
  arrange(-shannon.di)
trees_div_cdc

#N is total mosquitoes
#shanon.di is Shanon's diversity index
#simpson.di is Simpson's diversity index
#inv.simpson.di is inverse Simpson's diversity index
# If we want to compare mosquito diversity, I think we should use Simpson's 
#https://stats.libretexts.org/Bookshelves/Applied_Statistics/Natural_Resources_Biometrics_(Kiernan)/10%3A_Quantitative_Measures_of_Diversity_Site_Similarity_and_Habitat_Suitability/10.01%3A_Introduction__Simpsons_Index_and_Shannon-Weiner_Index

#merge tree & richness data with diversity data
trees_cdc_rich_div<- merge(trees_cdc_rich, trees_div_cdc, by = "zone_name",
                           all.trees_cdc_rich = FALSE, trees_div_cdc = FALSE)

#removing Pickerington North and Pickerington South since there is no tree data (assuming that this is because Pickerington is not in Franklin County)

trees_cdc_rich_div<- trees_cdc_rich_div[-c(40, 41), ]

#finally, everything is in one place :)


#####GLMS----

##### N mosquitoes ~ number trees 100m----

fit_trees_100m_N <- glm (N~ number_trees_100m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_100m_N)

new_data_trees_100m_N <- data.frame(number_trees_100m = seq(32, 405, 0.001))
new_data_trees_100m_N$Predicted_N_poisson <- predict(fit_trees_100m_N,
                                                     newdata = new_data_trees_100m_N, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_100m, y=N))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_N,
            aes(x=number_trees_100m, y=Predicted_N_poisson), linewidth=1)

##### N mosquitoes ~ number trees 100m Figure----

ggplot()+
  geom_point(data=trees_cdc_rich_div, size=3, mapping=aes(x=number_trees_100m, y=N, color= "100m"))+
  geom_line(data=new_data_trees_100m_N, linewidth= 1, aes(x=number_trees_100m, y=Predicted_N_poisson, color= "100m"))+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.text = element_text(colour = "black", size = 24),
        axis.title = element_text(color = "black", size = 24, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t=0, r=20, l=0, b=0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, l=0, b=0)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x= "Tree Stem Count (within 100m)", y="Total Mosquitoes", title = "")+
  scale_color_manual("", values=c("100m" = "green3"))+
  scale_x_continuous(name= "Tree Stem Count (within 100m)",limits = c(0,410), breaks = seq(0,410,100))+
  scale_y_continuous(name= "Total Mosquitoes",limits = c(0,850), breaks = seq(0,850,200))+
  theme(plot.title = element_text(color="black", size = 24, hjust = 0.5))


##### N mosquitoes ~ number trees 500m----

fit_trees_500m_N <- glm (N~ number_trees_500m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_500m_N)

new_data_trees_500m_N <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_N$Predicted_N_poisson <- predict(fit_trees_500m_N,
                                                     newdata = new_data_trees_500m_N, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_500m, y=N))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_500m_N,
            aes(x=number_trees_500m, y=Predicted_N_poisson), linewidth=1)


##### N mosquitoes ~ number trees 500m Figure----

ggplot()+
  geom_point(data=trees_cdc_rich_div, size=3, mapping=aes(x=number_trees_500m, y=N, color= "500m"))+
  geom_line(data=new_data_trees_500m_N, linewidth= 1, aes(x=number_trees_500m, y=Predicted_N_poisson, color= "500m"))+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.text = element_text(colour = "black", size = 24),
        axis.title = element_text(color = "black", size = 24, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t=0, r=20, l=0, b=0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, l=0, b=0)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x= "Tree Stem Count (within 500m)", y="Total Mosquitoes", title = "")+
  scale_color_manual("", values=c("500m" = "brown"))+
  scale_x_continuous(name= "Tree Stem Count (within 500m)",limits = c(0,10057), breaks = seq(0,10057,2500))+
  scale_y_continuous(name= "Total Mosquitoes",limits = c(0,850), breaks = seq(0,850,200))+
  theme(plot.title = element_text(color="black", size = 24, hjust = 0.5))


##### species.richness mosquitoes ~ number trees 100m----

fit_trees_100m_species.richness <- glm (species.richness~ number_trees_100m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_100m_species.richness)

new_data_trees_100m_species.richness <- data.frame(number_trees_100m = seq(32, 405, 0.001))
new_data_trees_100m_species.richness$Predicted_species.richness_poisson <- predict(fit_trees_100m_species.richness,
                                                                                   newdata = new_data_trees_100m_species.richness, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_100m, y=species.richness))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_species.richness,
            aes(x=number_trees_100m, y=Predicted_species.richness_poisson), linewidth=1)

##### species.richness mosquitoes ~ number trees 100m Figure----

ggplot()+
  geom_point(data=trees_cdc_rich_div, size=3, mapping=aes(x=number_trees_100m, y=species.richness, color= "100m"))+
  geom_line(data=new_data_trees_100m_species.richness, linewidth= 1, aes(x=number_trees_100m, y=Predicted_species.richness_poisson, color= "100m"))+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.text = element_text(colour = "black", size = 24),
        axis.title = element_text(color = "black", size = 24, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t=0, r=20, l=0, b=0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, l=0, b=0)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x= "Tree Stem Count (within 100m)", y="Mosquito Species Richness", title = "")+
  scale_color_manual("", values=c("100m" = "green3"))+
  scale_x_continuous(name= "Tree Stem Count (within 100m)",limits = c(0,410), breaks = seq(0,410,100))+
  scale_y_continuous(name= "Mosquito Species Richness",limits = c(0,15), breaks = seq(0,15,5))+
  theme(plot.title = element_text(color="black", size = 24, hjust = 0.5))


##### species.richness mosquitoes ~ number trees 500m----

fit_trees_500m_species.richness <- glm (species.richness~ number_trees_500m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_500m_species.richness)

new_data_trees_500m_species.richness <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_species.richness$Predicted_species.richness_poisson <- predict(fit_trees_500m_species.richness,
                                                                                   newdata = new_data_trees_500m_species.richness, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_500m, y=species.richness))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_500m_species.richness,
            aes(x=number_trees_500m, y=Predicted_species.richness_poisson), linewidth=1)

##### species.richness mosquitoes ~ number trees 500m Figure----

ggplot()+
  geom_point(data=trees_cdc_rich_div, size=3, mapping=aes(x=number_trees_500m, y=species.richness, color= "500m"))+
  geom_line(data=new_data_trees_500m_species.richness, linewidth= 1, aes(x=number_trees_500m, y=Predicted_species.richness_poisson, color= "500m"))+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.text = element_text(colour = "black", size = 24),
        axis.title = element_text(color = "black", size = 24, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t=0, r=20, l=0, b=0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, l=0, b=0)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x= "Tree Stem Count (within 500m)", y="Mosquito Species Richness", title = "")+
  scale_color_manual("", values=c("500m" = "brown"))+
  scale_x_continuous(name= "Tree Stem Count (within 500m)",limits = c(0,10057), breaks = seq(0,10057,2500))+
  scale_y_continuous(name= "Mosquito Species Richness",limits = c(0,15), breaks = seq(0,15,5))+
  theme(plot.title = element_text(color="black", size = 24, hjust = 0.5))


##### simpson.di mosquitoes ~ number trees 100m----

fit_trees_100m_simpson.di <- glm (simpson.di~ number_trees_100m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_100m_simpson.di)

new_data_trees_100m_simpson.di <- data.frame(number_trees_100m = seq(32, 405, 0.001))
new_data_trees_100m_simpson.di$Predicted_simpson.di_poisson <- predict(fit_trees_100m_simpson.di,
                                                                       newdata = new_data_trees_100m_simpson.di, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_100m, y=simpson.di))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_simpson.di,
            aes(x=number_trees_100m, y=Predicted_simpson.di_poisson), linewidth=1)

##### simpson.di mosquitoes ~ number trees 100m Figure----

ggplot()+
  geom_point(data=trees_cdc_rich_div, size=3, mapping=aes(x=number_trees_100m, y=simpson.di, color= "100m"))+
  geom_line(data=new_data_trees_100m_simpson.di, linewidth= 1, aes(x=number_trees_100m, y=Predicted_simpson.di_poisson, color= "100m"))+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.text = element_text(colour = "black", size = 24),
        axis.title = element_text(color = "black", size = 24, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t=0, r=20, l=0, b=0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, l=0, b=0)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x= "Tree Stem Count (within 100m)", y="Mosquito Species Diversity", title = "")+
  scale_color_manual("", values=c("100m" = "green3"))+
  scale_x_continuous(name= "Tree Stem Count (within 100m)",limits = c(0,410), breaks = seq(0,410,100))+
  scale_y_continuous(name= "Mosquito Species Diversity",limits = c(0,1), breaks = seq(0,1,0.25))+
  theme(plot.title = element_text(color="black", size = 24, hjust = 0.5))


##### simpson.di mosquitoes ~ number trees 500m----

fit_trees_500m_simpson.di <- glm (simpson.di~ number_trees_500m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_500m_simpson.di)

new_data_trees_500m_simpson.di <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_simpson.di$Predicted_simpson.di_poisson <- predict(fit_trees_500m_simpson.di,
                                                                       newdata = new_data_trees_500m_simpson.di, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_500m, y=simpson.di))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_500m_simpson.di,
            aes(x=number_trees_500m, y=Predicted_simpson.di_poisson), linewidth=1)

##### simpson.di mosquitoes ~ number trees 500m Figure----

ggplot()+
  geom_point(data=trees_cdc_rich_div, size=3, mapping=aes(x=number_trees_500m, y=simpson.di, color= "500m"))+
  geom_line(data=new_data_trees_500m_simpson.di, linewidth= 1, aes(x=number_trees_500m, y=Predicted_simpson.di_poisson, color= "500m"))+
  theme(axis.line = element_line(colour = "black", linewidth = 1),
        axis.ticks = element_line(colour = "black", linewidth = 1),
        axis.text = element_text(colour = "black", size = 24),
        axis.title = element_text(color = "black", size = 24, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t=0, r=20, l=0, b=0)),
        axis.title.x = element_text(margin = margin(t=20, r=0, l=0, b=0)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(x= "Tree Stem Count (within 500m)", y="Mosquito Species Diversity", title = "")+
  scale_color_manual("", values=c("500m" = "brown"))+
  scale_x_continuous(name= "Tree Stem Count (within 500m)",limits = c(0,10057), breaks = seq(0,10057,2500))+
  scale_y_continuous(name= "Mosquito Species Diversity",limits = c(0,1), breaks = seq(0,1,0.25))+
  theme(plot.title = element_text(color="black", size = 24, hjust = 0.5))

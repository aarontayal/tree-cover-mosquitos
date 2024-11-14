

citation()
version$version.string
citation("tidyverse")
citation("readxl")
citation("ggplot2")
citation("viridis")
citation("AER")


library(tidyverse)
library(readxl)
library(ggplot2)
library(viridis)
library(AER)
library(MASS)
library(ggpubr)

#load data
trees<-read_excel("data/100m_500m_tree_count_buffers_HRD.xlsx")
summary(trees)

# Erika's directory mosquitoes<- read_excel("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/2021 FCPH surveillence_HRD.xlsx/2021 FCPH surveillence_HRD.xlsx")
# Aaron directory:
mosquitoes <- read_excel("C:/Users/Aaron/Desktop/2021 FCPH surveillence_HRD.xlsx")
summary(mosquitoes)

# Variable corrections
mosquitoes$x <- mosquitoes$longitude
mosquitoes$y<- mosquitoes$latitude

#subset data
mosq_sp_sub<- mosquitoes %>% 
  group_by(latitude, longitude, zone_name, trap_type, species) %>%
  summarise(sum_mosq_sp = sum(total_count))

mosq_sp_cdc<- subset(mosq_sp_sub, trap_type =="CDC")
mosq_sp_bg<- subset(mosq_sp_sub, trap_type =="BG")
mosq_sp_gravid<- subset(mosq_sp_sub, trap_type =="Gravid")

trees_sub<-trees%>%
  group_by(zone_name, trap_type, number_trees_100m, number_trees_500m)

trees_cdc<- subset(trees_sub, trap_type =="CDC")
trees_bg<- subset(trees_sub, trap_type =="BG")
trees_gravid<- subset(trees_sub, trap_type =="Gravid")

#merge data             
trees_mosquitoes_cdc<- merge(mosq_sp_cdc, trees_cdc, by = "zone_name",
                               all.mosq_sp_cdc = FALSE, all.trees_cdc = FALSE)

trees_mosquitoes_bg<- merge(mosq_sp_bg, trees_bg, by = "zone_name",
                             all.mosq_sp_bg = FALSE, all.trees_bg = FALSE)

trees_mosquitoes_gravid<- merge(mosq_sp_gravid, trees_gravid, by = "zone_name",
                           all.mosq_sp_gravid = FALSE, all.trees_gravid = FALSE)

# How many zone names are there for the CDC traps? (aka what's the sample size)
levels(as.factor(trees_mosquitoes_cdc$zone_name))
# Sample size is 62 zones

#calculate richness
sp_abund<-table(trees_mosquitoes_cdc$species, trees_mosquitoes_cdc$zone_name)
sp_abund
sp_abund_df<-as.data.frame.matrix(sp_abund)
glimpse(sp_abund_df)

spr_cdc<-trees_mosquitoes_cdc %>%
  group_by(zone_name) %>%
  summarise(species.richness=n()) %>%
  arrange(-species.richness)
view(spr_cdc)

#merge tree data and richness
trees_cdc_rich<- merge(trees_cdc, spr_cdc, by = "zone_name",
                       all.trees_cdc = FALSE, all.spr_cdc = FALSE)

#calculate diversity

cdc_sp_count_by_zone <- read_excel("data/CDC species count by site.xlsx")
View(cdc_sp_count_by_zone) 

tidy_mosq <- cdc_sp_count_by_zone %>%
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

trees_cdc_rich_div<- trees_cdc_rich_div[(trees_cdc_rich_div$zone_name != "Pickerington North" & 
                                           trees_cdc_rich_div$zone_name != "Pickerington South"), ]


#finally, everything is in one place :)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######Summary statistics:
# How many mosquitoes were caught in total?
sum(trees_cdc_rich_div$N) # 12,129 mosquitoes
# What was the average number of mosquitoes caught, and the standard deviation?
mean(trees_cdc_rich_div$N) # Average of 202 mosquitoes caught at each trap
sd(trees_cdc_rich_div$N) # Standard deviation of 168 mosquitoes
# What does the distribution of mosquito abundances look like?
hist(trees_cdc_rich_div$N, breaks=15) # Notice some traps caught way more
# mosquitoes than others.

# What was the most common species? The second-most common? Third?
species_counts <- tidy_mosq %>% group_by(species) %>% summarise(N=sum(abundance)) %>%
  arrange(-N)
# 1. Aedes vexans (4,329), 2. Aedes trivittatus (3,868), 3. Culex misc spp (1,207)
ggplot(data=species_counts, aes(x=species, y=N)) + geom_point()

# How many unique species were found at each trap, on average?
mean(trees_cdc_rich_div$species.richness) # 11.23333 species
sd(trees_cdc_rich_div$species.richness) # standard dev. of 2.110239 species

# What was the average Simpson's diversity?
mean(trees_cdc_rich_div$simpson.di)
sd(trees_cdc_rich_div$simpson.di)

# Does the Simpson's Diversity relate at all to the species richness?
ggplot(data=trees_cdc_rich_div, aes(x=species.richness, y=simpson.di, color=N))+
  geom_point()
fit_Simpson_div_to_mosq_richness <- 
  lm(simpson.di~species.richness, data=trees_cdc_rich_div) 
summary(fit_Simpson_div_to_mosq_richness) # On first look, it seems Simpson's diversity
# does NOT clearly vary with the species richness

jefferson_south <- tidy_mosq %>% filter(zone_name=="Jefferson South")
hist(x=jefferson_south$abundance, breaks=50) # it seems like traps that
# had high spp richness and low simpson diversity had a single species 
# in very high abundance

Reynoldsburg_Northwest <- tidy_mosq %>% filter(zone_name=="Reynoldsburg Northwest")
hist(x=Reynoldsburg_Northwest$abundance, breaks=50)


# I wonder whether traps that caught a high abundance of mosquitoes would also
# have a lower Simpson's diversity (because many of 1 species might be caught)
plot(trees_cdc_rich_div$N, trees_cdc_rich_div$simpson.di)
fit_Simpson_div_to_mosq_abundance <- lm(simpson.di~N, data=trees_cdc_rich_div)
summary(fit_Simpson_div_to_mosq_abundance) # Without doing too much analysis,
# it seems like a higher abundance of mosquitoes was related to a lower Simpson's
# diversity

# On average, how many trees were counted in the 100m buffer?
mean(trees_cdc_rich_div$number_trees_100m) # 204 trees
sd(trees_cdc_rich_div$number_trees_100m) # 70 trees (about 34% of the mean)
hist(trees_cdc_rich_div$number_trees_100m, breaks=20) # The distribution of this 
# predictor seems to follow a "bell curve" shape - not that it matters!

# On average, how many trees were counted in the 500m buffer?
mean(trees_cdc_rich_div$number_trees_500m) # 5,220 trees
sd(trees_cdc_rich_div$number_trees_500m) # Standard dev. of 1662 trees (about 32% of the mean)
hist(trees_cdc_rich_div$number_trees_500m, breaks=20)

# What is the correlation between the number of trees in 100m and the number 
# in 500m?
cor.test(trees_cdc_rich_div$number_trees_100m, trees_cdc_rich_div$number_trees_500m)
cor.test(trees_cdc_rich_div$number_trees_100m, trees_cdc_rich_div$number_trees_500m, 
         method="spearman")
plot(trees_cdc_rich_div$number_trees_100m, trees_cdc_rich_div$number_trees_500m)
# For each additional tree in the 100m, how many additional trees are in the 500m?
buffer_relationship <- lm(number_trees_500m~number_trees_100m, data=trees_cdc_rich_div)
summary(buffer_relationship)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#####GLMS----

##### N mosquitoes ~ number trees 100m----

fit_trees_100m_N <- glm(N~ number_trees_100m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_100m_N)

new_data_trees_100m_N <- data.frame(number_trees_100m = seq(32, 405, 0.1))
new_data_trees_100m_N$Predicted_N_poisson <- predict(fit_trees_100m_N,
                                                     newdata = new_data_trees_100m_N, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_100m, y=N))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_N,
            aes(x=number_trees_100m, y=Predicted_N_poisson), linewidth=1)

# Checking model assumptions
  # 1. overdispersion
dispersiontest(fit_trees_100m_N, trafo = 1)
  # Highly overdispersed

  # 2. Patterns in residuals
par(mfrow = c(2, 2))
plot(fit_trees_100m_N)
  # Residuals are clustered in the middle and spread out widely in the upper end
    # of the data.

# Remove Jackson and Jefferson South because they are outliers
trees_cdc_rich_div_rm_out<- trees_cdc_rich_div %>%
  slice(-c(28, 30))

# Summary stats
max(trees_cdc_rich_div_rm_out$N)

# Re-fit model
fit_trees_100m_N_2 <- glm (N~ number_trees_100m, data=trees_cdc_rich_div_rm_out, family=poisson(link = "log"))

new_data_trees_100m_N_2 <- data.frame(number_trees_100m = seq(32, 405, 0.001))
new_data_trees_100m_N_2$Predicted_N_poisson <- predict(fit_trees_100m_N_2,
                                                       newdata = new_data_trees_100m_N_2, type="response")

ggplot(data=trees_cdc_rich_div_rm_out, mapping=aes(x=number_trees_100m, y=N))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_N,
            aes(x=number_trees_100m, y=Predicted_N_poisson), linewidth=1)

plot(fit_trees_100m_N_2)
summary(fit_trees_100m_N_2)

# There is still a significant effect of number of tree stems on mosquito abundance
# after removing outliers.

# Checking model assumptions
  # 1. overdispersion
dispersiontest(fit_trees_100m_N_2, trafo = 1)
 # Slightly less overdispersed than fit 1, but still highly overdispersed.

  # 2. patterns in residuals
par(mfrow = c(2, 2))
plot(fit_trees_100m_N_2)
  # Residuals are clustered in the middle and spread out widely in the upper end
    # of the data.

# Model assumptions are not met.

# Re-fit model with a negative binomial distribution
fit_trees_100m_N_nb<- glm.nb(N ~ number_trees_100m, data = trees_cdc_rich_div_rm_out)
summary(fit_trees_100m_N_nb)

plot(fit_trees_100m_N_nb)

# While this model accounts for overdispersion, the residual plots do not look
  # much better than the poisson glm. Maybe we stick with poisson glm?

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

# Without outliers
abundance_100m<- ggplot()+
  geom_point(data=trees_cdc_rich_div_rm_out, size=3, mapping=aes(x=number_trees_100m, y=N, color= "100m"))+
  geom_line(data=new_data_trees_100m_N_2, linewidth= 1, aes(x=number_trees_100m, y=Predicted_N_poisson, color= "100m"))+
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

# Checking model assumptions
# 1. overdispersion
dispersiontest(fit_trees_500m_N, trafo = 1)
# Highly overdispersed.

# 2. patterns in residuals
plot(fit_trees_500m_N)

# Residuals look good overall, but they spread out a lot at the upper end of data.
  # There is an outlier high on the x axis and low on the y axis.

new_data_trees_500m_N <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_N$Predicted_N_poisson <- predict(fit_trees_500m_N,
                                                     newdata = new_data_trees_500m_N, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_500m, y=N))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_500m_N,
            aes(x=number_trees_500m, y=Predicted_N_poisson), linewidth=1)

# Remove additional outlier
trees_cdc_rich_div_rm_out_2<- trees_cdc_rich_div %>%
  slice(-c(28, 30, 39))

# Without outliers
fit_trees_500m_N_2 <- glm (N~ number_trees_500m, data=trees_cdc_rich_div_rm_out_2, family=poisson(link = "log"))

summary(fit_trees_500m_N_2)

new_data_trees_500m_N_2 <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_N_2$Predicted_N_poisson <- predict(fit_trees_500m_N_2,
                                                     newdata = new_data_trees_500m_N_2, type="response")

# There is still a significant effect of number of trees on mosquito abundance.

# Checking model assumptions
# 1. overdispersion
dispersiontest(fit_trees_500m_N_2, trafo = 1)
# Slightly less overdispersed.

# 2. patterns in residuals
plot(fit_trees_500m_N_2)

# Residuals better, though they are still spread out a bit at the upper end.

ggplot(data=trees_cdc_rich_div_rm_out_2, mapping=aes(x=number_trees_500m, y=N))+
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

# Without outliers
abundance_500m<- ggplot()+
  geom_point(data=trees_cdc_rich_div_rm_out_2, size=3, mapping=aes(x=number_trees_500m, y=N, color= "500m"))+
  geom_line(data=new_data_trees_500m_N_2, linewidth= 1, aes(x=number_trees_500m, y=Predicted_N_poisson, color= "500m"))+
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

ggarrange(abundance_100m, abundance_500m, ncol = 2, nrow = 1)
##### species.richness mosquitoes ~ number trees 100m----

fit_trees_100m_species.richness <- glm(species.richness~ number_trees_100m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_100m_species.richness)

# There is no significant effect of tree count on sp richness.

new_data_trees_100m_species.richness <- data.frame(number_trees_100m = seq(32, 405, 0.001))
new_data_trees_100m_species.richness$Predicted_species.richness_poisson <- predict(fit_trees_100m_species.richness,
                                                                                   newdata = new_data_trees_100m_species.richness, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_100m, y=species.richness))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_species.richness,
            aes(x=number_trees_100m, y=Predicted_species.richness_poisson), linewidth=1)

##### species.richness mosquitoes ~ number trees 100m Figure----

richness_100m<- ggplot()+
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

# Summary stats
max(trees_cdc_rich_div$species.richness)

# There is still no significant effect with 500 m buffer.

new_data_trees_500m_species.richness <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_species.richness$Predicted_species.richness_poisson <- predict(fit_trees_500m_species.richness,
                                                                                   newdata = new_data_trees_500m_species.richness, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_500m, y=species.richness))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_500m_species.richness,
            aes(x=number_trees_500m, y=Predicted_species.richness_poisson), linewidth=1)

##### species.richness mosquitoes ~ number trees 500m Figure----

richness_500m<- ggplot()+
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

ggarrange(richness_100m, richness_500m, ncol = 2, nrow = 1)
##### simpson.di mosquitoes ~ number trees 100m----

fit_trees_100m_simpson.di <- glm (simpson.di~ number_trees_100m, data=trees_cdc_rich_div, family=poisson(link = "log"))

summary(fit_trees_100m_simpson.di)

# There is no significant effect of tree count on sp diversity.

new_data_trees_100m_simpson.di <- data.frame(number_trees_100m = seq(32, 405, 0.001))
new_data_trees_100m_simpson.di$Predicted_simpson.di_poisson <- predict(fit_trees_100m_simpson.di,
                                                                       newdata = new_data_trees_100m_simpson.di, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_100m, y=simpson.di))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_100m_simpson.di,
            aes(x=number_trees_100m, y=Predicted_simpson.di_poisson), linewidth=1)

##### simpson.di mosquitoes ~ number trees 100m Figure----

simpson_100m<- ggplot()+
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

# There is no significant effect of tree count on sp diversity with 500 m buffer.

new_data_trees_500m_simpson.di <- data.frame(number_trees_500m = seq(2393,10057, 0.001))
new_data_trees_500m_simpson.di$Predicted_simpson.di_poisson <- predict(fit_trees_500m_simpson.di,
                                                                       newdata = new_data_trees_500m_simpson.di, type="response")

ggplot(data=trees_cdc_rich_div, mapping=aes(x=number_trees_500m, y=simpson.di))+
  geom_point()+theme_classic()+
  geom_line(data=new_data_trees_500m_simpson.di,
            aes(x=number_trees_500m, y=Predicted_simpson.di_poisson), linewidth=1)

##### simpson.di mosquitoes ~ number trees 500m Figure----

simpson_500m<- ggplot()+
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

ggarrange(simpson_100m, simpson_500m, ncol = 2, nrow = 1)


# Looking at particular species:
# Aedes triseriatus










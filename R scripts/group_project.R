# Ent 6707 Group Project
# 9/5/24

# Libraries
library(tidyverse)
library(readxl)
library(sf)
library(terra)
library(spData)
library(tmap)

# Load in data
cbus_tracts <- sf::st_read("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/Data/Columbus_TreeCanopy_2021_GDB_20230817/Columbus_TreeCanopy_2021.gdb", 
                           layer = "Cols_Tracts_AOI")

cbus_land_cover<- read.csv("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/Data/cbuslandcovertracts.csv",
                           header = TRUE)

summary(cbus_land_cover)

mosquitoes<- read_excel("C:/Users/erika/OneDrive - The Ohio State University/PhD/Courses/Ent Techniques fall 2024/Group Project/Data/HRD Data.xlsx")
summary(mosquitoes)

tracts_landcover_merge<- merge(cbus_land_cover, cbus_tracts, by = "TC_ID")

# Variable corrections
tracts_landcover_merge$TC_ID_fac<- as.factor(tracts_landcover_merge$TC_ID)

# Visualize
ggplot(tracts_landcover_merge, aes(x = TC_ID_fac, y = Can_P)) +
  geom_bar(stat = "identity") +
  theme_classic()

### Calculating principle component axes for vegetation, climate, and soil charactristics ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com)
### last updated: July 27th, 2017


# Set up workspace --------------------------------------------------------
require(vegan)
require(plyr)
require(ggplot2)
require(lme4)
require(pbkrtest)

setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")
source("Scripts\\06_1_calculate annual temp and precip for each site.R")

#setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")
write_path <- "Site characteristics\\pca_output\\"

# Compile climate information ---------------------------------------------
climate.chars <- mean.ppt.temp

# Compile soil information ------------------------------------------------
# Note: this file includes long-term climate information, and old vegetation characteristics
  # So, we are not using the climate or veg chars in this file
site.info.raw <- read.csv("Site characteristics//Site characteristics_soil veg and climate_all sites.csv")
soil.chars <- site.info.raw[,c("site", "BD","pSand", "pSilt", "pClay", "wilting.pt", "field.cap")]

# Compile vegetation information ------------------------------------------
veg.chars <- read.csv("..//Data//Species composition//EDGE functional proportions_all sites.csv")



# Multivariate Analyses ---------------------------------------------------

## Climate PCA ##
climate.pca.raw <- prcomp(scale(climate.chars[,2:ncol(climate.chars)]))

sink(paste0(write_path, "climate PCA_output.txt")) #write output to file
summary(climate.pca.raw)
climate.pca.raw
data.frame(site=climate.chars$site, climate.pca.raw$x)
sink() #close file

climate.pca <- data.frame(site=climate.chars$site, climate.pca.raw$x)
colnames(climate.pca) <- c("site",paste("climatePC",1:4,sep=""))
biplot(climate.pca.raw)

## soil PCA ##
soil.pca.raw <- prcomp(scale(soil.chars[,2:ncol(soil.chars)]))

sink(paste0(write_path, "soil PCA_output.txt")) #write output to file
summary(soil.pca.raw)
soil.pca.raw
data.frame(site=soil.chars$site, soil.pca.raw$x)
sink() #close file

soil.pca <- data.frame(site=soil.chars$site, soil.pca.raw$x)
colnames(soil.pca) <- c("site",paste("soilPC",1:6,sep=""))

## veg PCA ##
veg.pca.raw <- prcomp(scale(veg.chars[,2:ncol(veg.chars)]))

sink(paste0(write_path, "veg PCA_output.txt")) #write output to file
summary(veg.pca.raw)
veg.pca.raw
data.frame(site=veg.chars$site, veg.pca.raw$x)
sink() #close file
veg.pca <- data.frame(site=veg.chars$site, veg.pca.raw$x)
colnames(veg.pca) <- c("site",paste("vegPC",1:6,sep=""))

pca.all <- merge(climate.pca, merge(soil.pca, veg.pca, by="site"),
                   by="site")
site.char.all <- merge(climate.chars, merge(soil.chars, veg.chars, by="site"),
                       by="site")
#write.csv(pca.all.1, file="Site characteristics//PCA soil vegetation and climate_all sites.csv",row.names=FALSE)
#write.csv(site.char.all, file="Site characteristics//Site characteristics_soil veg and climate_all sites.csv",row.names=FALSE)




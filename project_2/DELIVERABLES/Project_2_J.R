#--------------------------- libraries--------------------------------
# If package not installed, install it
required.packages <- c("dplyr", "tidyverse", "readxl", "GGally", "ISLR","dlookr",
                       'visdat', 'ggplot2', 'gridExtra', 'ggcorrplot', 'scales',
                     'tidymodels')
new.packages <-
  required.packages[!(required.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

install.packages('factoextra')
install.packages('psych')
# Load packages
require(dplyr)
require(tidyverse)
require(readxl)
require(GGally)
require(ISLR)
require(dlookr)
require(visdat)
require(ggplot2)
require(gridExtra)
require(ggcorrplot)
require(tidymodels)
require(scales)
require(factoextra)
require(psych)


#-------------------------------------------------------------------------------

#---------------------------Tasks-----------------------------------------------
# 1: Plot data using pairwise plotting and get a sense of the relationships
# between the attributes

#links:
#https://towardsdatascience.com/generalized-pairs-plot-in-r-6bbfde2c98b8
#https://github.com/PacktPublishing/Hands-On-Exploratory-Data-Analysis-with-R/blob/master/Chapter06/Chapter_6.r
#https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html

directory = '/Users/josegarcia/Documents/GitHub/csci_6444/project_2/DryBeanDataset/Dry_Bean_Dataset.xlsx'   
drybeans_data <- read_excel(directory)

str(drybeans_data) 

pairs(drybeans_data[1:16]) #Pair plot visualization. Unable to understand graph.

plot_correlate(drybeans_data[1:16]) # correlation plot

drybeans_data %>%
  select(where(is.numeric)) %>% # correlation plot
  vis_cor()

ggplot(drybeans_data) +
  geom_point(aes(x=Eccentricity, y=ShapeFactor3, fill= Class, alpha=0.3)) #distribution plot
ggpairs(drybeans_data[1:16])

#drybeans_data %>% # Web report
#  eda_web_report(targe='Class',
 #                subtitle = "Bean Type",
  #               output_dir ="./",
   #              output_file= "EDA.html",
    #             theme = 'blue')
#-------------------------------------------------------------------------------

#2: Prepare the Data 
#a Basic statistics:
describe(drybeans_data)

#b Data subset: using PCA.

#Normalize data and PCA
drybeans.pr <- prcomp(drybeans_data[1:16], center = T, scale = T)
summary(drybeans.pr)

screeplot(drybeans.pr, type='l', npcs =16, main = 'Screeplot of 16 PCs')
abline(h=1, col ='green', lty=5)
legent('topright', legend=c('Eigenvalue = 1'),
       col=c('green'), lty=5, cex =0.6)

cumpro <- cumsum(drybeans.pr$sdev^2 / sum(drybeans.pr$sdev^2))
plot(cumpro[0:16], xlab = "PC #", ylab = "Amount of explained variance",
     main = "Comulative variance plot")

fviz_pca_ind(drybeans.pr, geom.ind = 'point', pointshape =21,
             pointsize=2,
             fill.ind = drybeans_data$Class,
             col.ind = 'black',
             palette = 'jco',
             addEllipses = T,
             label ='var',
             col.var = 'black',
             repel = T, 
             legend.title = 'Diagnosis') +
  ggtitle('2D PCA-plot') +
  theme(plot.title = element_text(hjust = 0.5))


#hepler fuction from: https://rstudio-pubs-static.s3.amazonaws.com/585948_abd70a6fc3e24d4fad8944197bc5dd25.html
  # to visualize contribution of variables on each component
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
  # Compute coordinates
loadings <- drybeans.pr$rotation
sdev <- drybeans.pr$sdev
var.coord <- t(apply(loadings, MARGIN = 1, var_coord_func, sdev))
head(var.coord[, 1:4])
  # compute the variable components squared 
var.cos2 <- var.coord^2
  # compute contributions
comp.cos2 <- apply(var.cos2, MARGIN = 2, FUN=sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2, MARGIN=1, contrib, comp.cos2))
var.contrib[,1:4]

var.coord[,1:3]

## features selected based on correlation of features and PCs:
# PC1:  Perimeter, MajorAxisLength, ConvexArea, EquivDiameter,shape factor 2. correlation > 0.8 or <  - 0.8
# PC2: MinorAxisLength, Compactness, SF3, SF1, AspectRation, Eccentriciy
# PC3: Solidity, SF4. 

#-------------------- Task 3: Clustering the Whole Data Set --------------------
#  links: 
# https://uc-r.github.io/kmeans_clustering

# k-means:
clean.data <- drybeans_data[, c('Perimeter', 'MajorAxisLength', 
                                           'ConvexArea','EquivDiameter', 
                                           'ShapeFactor2', 'MinorAxisLength',
                                           'Compactness', 'ShapeFactor3',
                                           'ShapeFactor1', 'AspectRation',
                                           'Eccentricity', 'Class'
                                           )]
clean.data <- scale(clean.data[,1:11])
as.data.frame.matrix(clean.data) 
head(clean.data)
K2 <- kmeans(clean.data[1:16], centers=5, n=25)
str(K2)

table(clean.data$Class, K2$cluster)
plot(clean.data[c("Area", "Extent")], col= K2$cluster)
points(K2$centers[,c("Area","Extent")], col=1:3, pch = 8, cex=2)

#--------------------------- libraries--------------------------------
# If package not installed, install it
required.packages <- c("dplyr", "tidyverse", "readxl", "GGally", "ISLR","dlookr",
                       'visdat', 'ggplot2', 'gridExtra', 'ggcorrplot', 'scales',
                     'tidymodels', 'glm', 'factoextra','psych')
new.packages <-
  required.packages[!(required.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

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
# HELPER_FUNCS ------------------------------------------------------------
train_test_split <- function(dataset, percentage_training) {
  train <-
    dplyr::sample_frac(tbl = dataset, size = percentage_training)
  train_obs <- as.numeric(x = rownames(x = train))
  test <- dataset[-train_obs,]
  return(list(train = train, test = test))
}

remove_last_column <-
  function(list.of.frames, frame.name = "test") {
    last.column <-
      names(list.of.frames[[frame.name]][, ncol(list.of.frames[[frame.name]])])
    return(dplyr::select(list.of.frames[[frame.name]], -last.column))
  }

plot_kmeans <- function(list.of.dfs, centers, train.test.ratio) {
  results_kmeans <-
    stats::kmeans(x = dplyr::select(list.of.dfs[["train"]], where(is.numeric)),
                  centers = centers)
  fitted(results_kmeans, method = c("classes"))
  factoextra::fviz_cluster(
    results_kmeans,
    dplyr::select(list.of.dfs[["train"]], where(is.numeric)),
    main = paste("Kmeans | Train-test", train.test.ratio, "| K =", centers)
  )
}


#---------------------------Tasks-----------------------------------------------
directory = '/Users/josegarcia/Documents/GitHub/csci_6444/project_2/DryBeanDataset/Dry_Bean_Dataset.xlsx'   
drybeans_data <- read_excel(directory)
str(drybeans_data) 



# 1. For this data set, plot the data using pairwise plotting to get a sense of 
#the relationships between the attributes. 

#a. Try plotting the data using several plotting functions to see what it looks 
#like. Use pairs (e.g., 2D plots) or 3 variables (3D plots) based on the packages. 

pairs(drybeans_data[1:16]) #Pair plot visualization. Unable to understand graph.

#b. Which pairs of attributes seem to be correlated? How are they correlated?
  
plot_correlate(drybeans_data[1:16]) # correlation plot

drybeans_data %>%
  select(where(is.numeric)) %>% # correlation plot
  vis_cor()

ggplot(drybeans_data) +
  geom_point(aes(x=Eccentricity, y=ShapeFactor3, fill= Class, alpha=0.3)) #distribution plot
ggpairs(drybeans_data[1:16])

#-------------------------------------------------------------------------------

#2: Prepare the Data 

#a. Investigate some of the statistics of the data set: summary(). Describe(). 
#What do you glean from this data?
describe(drybeans_data)
summary(drybeans_data)
#b b. To subset, pick the most correlated attributes to use – they may all be 
#relevant, so document your rationale for eliminating some attributes..

#                  *****PCA*********
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


#               ********** END OF PCA************

# DATA SUBSET:
drybeans.sset= drybeans_data[,c('Perimeter', 'MajorAxisLength', 
                                'ConvexArea','EquivDiameter', 
                                'ShapeFactor2', 'MinorAxisLength',
                                'Compactness', 'ShapeFactor3',
                                'ShapeFactor1', 'AspectRation',
                                'Eccentricity', 'Class')]

# c. Class features into numeric values:
drybeans.sset = drybeans.sset %>%
  mutate(num_class = case_when(
    Class == 'BARBUNYA' ~0,
    Class == 'BOMBAY' ~1,
    Class == 'CALI' ~2,
    Class == 'DERMASON' ~3,
    Class == 'HOROZ' ~4,
    Class == 'SEKER' ~5,
    Class == 'SIRA' ~6,
  ))

#DATA STANDARIZATION:

drybeans.stand <- as.data.frame(scale(drybeans.sset[1:11]))

# d. Split training and test sets: 70%
train_test= train_test_split(drybeans.stand, 0.7)
train_set = as.data.frame(train_test[1])
test_set = as.data.frame(train_test[2])

#e. For the Test Sets, remove the last column which are the labels of the beans and save them.


#-------------------- Task 3: Clustering the Whole Data Set --------------------

# k-means:
  # 5 clusters
K5 <- kmeans(train_set, centers=5)
K5
?kmeans
# 6 clusters
K6 <- kmeans(train_set, centers=6)
K6

# 7 cluseters
K7 <- kmeans(train_set, centers=7)
K7

K10 <- kmeans(train_set, centers=10)
# factoextra:
factoextra::fviz_cluster(K10, train_set)



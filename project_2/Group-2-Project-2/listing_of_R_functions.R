# PROJECT 1 ---------------------------------------------------------------
# If package not installed, install it
required.packages <-
  c("Boruta",
    "corrplot",
    "factoextra",
    "Hmisc",
    "tidyverse",
    "doParallel")
new.packages <-
  required.packages[!(required.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

# Load packages
require(corrplot)
require(factoextra)
require(Hmisc)
require(tidyverse)

# Setup computing environment
working.dir <- "~/Documents/ms_cs/csci_6444/csci_6444/project_2"
setwd(working.dir)
doParallel::registerDoParallel()
set.seed(2021)

# Load data as a dataframe
local.path.to.dataset <-
  "/Users/raymondhear/Documents/ms_cs/csci_6444/csci_6444/project_2/DryBeanDataset/Dry_Bean_Dataset.xlsx"
sheet <- "Dry_Beans_Dataset"
dry.bean.dataset <-
  readxl::read_excel(
    path = local.path.to.dataset,
    sheet = sheet,
    col_names = TRUE,
    trim_ws = TRUE,
    progress = readxl::readxl_progress(),
    guess_max = 1e5
  )


# HELPER_FUNCS ------------------------------------------------------------
train_test_split <- function(dataset, percentage_training) {
  train <-
    dplyr::sample_frac(tbl = dataset, size = percentage_training)
  train_obs <- as.numeric(x = rownames(x = train))
  test <- dataset[-train_obs,]
  return(list(train = train, test = test))
}

plot_correlation <-
  function(data.frame, correlation.method, title) {
    correlation.matrix.pearson <-
      cor(
        x = dplyr::select(data.frame, where(is.numeric)),
        method = correlation.method,
        use = "pairwise.complete.obs"
      )
    corrplot(corr = correlation.matrix.pearson,
             method = "circle",
             type = "lower")
    mtext(text = title, side = 3)
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
  fitted(results_kmeans, , method = c("classes"))
  factoextra::fviz_cluster(
    results_kmeans,
    dplyr::select(list.of.dfs[["train"]], where(is.numeric)),
    main = paste("Kmeans | Train-test", train.test.ratio, "| K =", centers)
  )
}

# 1 - Plot the data -------------------------------------------------------
#' For this data set, plot the data using pairwise plotting to get a sense of the relationships between the attributes.
#' a - Try plotting the data using several plotting functions to see what it looks like. Use pairs (e.g., 2D plots)
#' or 3 variables (3D plots) based on the packages.
## Correlation
plot_correlation(data.frame = dry.bean.dataset,
                 correlation.method = "pearson",
                 title = "Pearson Correlation")
plot_correlation(data.frame = dry.bean.dataset,
                 correlation.method = "spearman",
                 title = "Spearman Correlation")
## Pairwise
graphics::pairs(x = dplyr::select(dry.bean.dataset, where(is.numeric)),
                main = "General Pairwise Scatterplot")
## Boruta
boruta.output <-
  Boruta::Boruta(as.factor(dry.bean.dataset$Class) ~ .,
                 data = dry.bean.dataset,
                 doTrace = 2)
plot(
  boruta.output,
  cex.axis = .7,
  las = 2,
  xlab = "",
  main = "Variable Importance"
)
legend(
  'topleft',
  c("Confirmed", "Tentative", "Rejected", "Shadow"),
  col = c("green", "yellow", "red", "blue"),
  lwd = 10,
  xjust = 0.5,
  yjust = 0.5
)
#' b - Which pairs of attributes seem to be correlated? How are they correlated?

## All relevant.

# 2 - Prepare the data ----------------------------------------------------
#' a - Investigate some of the statistics of the data set: summary(). Describe(). What do you glean from this data?
#' Note: You may have to subset the data to run some of the algorithms on your machine.
dry.bean.dataset %>% base::summary()
dry.bean.dataset %>% Hmisc::describe()
#' b - To subset, pick the most correlated attributes to use – they may all be relevant, so document your rationale for
#' eliminating some attributes.

## All relevant.

#' c - You will need to translate alphanumeric (e.g., character) values into numeric values. Create a mapping for each
#' field of values to integers. Make sure you put these mappings into your report.
label.mapping <- c(
  "BARBUNYA" = 1,
  "BOMBAY" = 2,
  "CALI" = 3,
  "DERMASON" = 4,
  "HOROZ" = 5,
  "SEKER" = 6,
  "SIRA" = 7
)
label.encoded <- as.factor(x = dry.bean.dataset$Class)
dry.bean.dataset$Class <- as.factor(dry.bean.dataset$Class)
#' d - Split the original data set into Training and Test Sets. Use 70%, 60%, 50%
dataset <- dry.bean.dataset
split_vars <- c("split_70", "split_60", "split_50")
training_sizes <- c(0.7, 0.6, 0.5)
for (i in seq_along(split_vars))
  assign(
    x = split_vars[[i]],
    value = train_test_split(dataset = dataset, percentage_training = training_sizes[[i]])
  )
#' e - For the Test Sets, remove the last column which are the labels of the beans and save them.
split_dfs <- list(split_70, split_60, split_50)
## Assign last column
y_test_vars <- c("y_test_70", "y_test_60", "y_test_50")
for (i in seq_along(split_dfs))
  assign(x = y_test_vars[[i]], value = split_dfs[[i]][["test"]][, ncol(split_dfs[[i]][["test"]]), drop = FALSE])
## Drop last column
split_70$test <- remove_last_column(split_70, "test")
split_60$test <- remove_last_column(split_60, "test")
split_50$test <- remove_last_column(split_50, "test")

# 3 - Clustering on the whole dataset -------------------------------------
#' a - Apply three clustering techniques to the subsetted data: KMeans, kNN, and iClust. In the original data set, there
#' were seven classes according to the authors. You should analyze the data for clusters from k=5 to 10.
## Kmeans
for (k in seq(5, 10)){
  plot_kmeans(list.of.dfs = split_50,
              centers = k,
              train.test.ratio = "50-50")
  filename = paste("plot_kmeans_50_k", k, ".png", sep = "")
  message(filename)
  ggplot2::theme(aspect.ratio = 1)
  ggplot2::ggsave(
    filename = filename,
    plot = ggplot2::last_plot(),
    device = "png",
    scale = 1,
    limitsize = FALSE,
    width = 1280,
    height = 1280,
    dpi = 150,
    units = "px",
    
  )
}

for (k in seq(5, 10)){
  plot_kmeans(list.of.dfs = split_60,
              centers = k,
              train.test.ratio = "60-20")
  filename = paste("plot_kmeans_60_k", k, ".png", sep = "")
  message(filename)
  ggplot2::theme(aspect.ratio = 1)
  ggplot2::ggsave(
    filename = filename,
    plot = ggplot2::last_plot(),
    device = "png",
    scale = 1,
    limitsize = FALSE,
    width = 1280,
    height = 1280,
    dpi = 150,
    units = "px",
    
  )
}

for (k in seq(5, 10)){
  plot_kmeans(list.of.dfs = split_70,
              centers = k,
              train.test.ratio = "70-30")
  filename = paste("plot_kmeans_70_k", k, ".png", sep = "")
  message(filename)
  ggplot2::theme(aspect.ratio = 1)
  ggplot2::ggsave(
    filename = filename,
    plot = ggplot2::last_plot(),
    device = "png",
    scale = 1,
    limitsize = FALSE,
    width = 1280,
    height = 1280,
    dpi = 150,
    units = "px",
    
  )
}

## Knn
predictions <- class::knn(
  train = dplyr::select(split_70$train, where(is.numeric)),
  test = split_70$test,
  k = 3,
  cl = as.factor(dplyr::select(split_70$train, where(
    purrr::negate(is.numeric)
  ))$Class)
)


#' b - Build a table with your results succinctly displayed. Document your results in your report in separate sections.
#' Show screen shots of plots of the clusters (see class notes). Suggest using factoextra methods.

# 4 - Prediction (using training and test sets) ---------------------------
#' Following the rubric, try to predict the values of the test sets, then compare them with the kmeans labels on the
#' test set.
#' a - Use the glm(…) method to get linear fits for the data. Try clusters of 5,7,9 Remember to generate labels using
#' kmeans. Do a cross-tabulation of the results to see how good you did.
#' b - Compute the performance measurements for each table. Calculate the accuracy, false positive, etc. and display
#' in a table. Try different parameter values and combinations of attributes. Use predict(…) to predict the values
#' for the test set after training with the training set.
#' c - What differences to do you see between kmeans and kNN in processing the data. Be explicit?

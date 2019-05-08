#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 14:05:56 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'L MYTHILI AMARTHYA'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-05-08 14:09:19 x86_64-w64-mingw32 

# Load a dataset from file.

library(readxl, quietly=TRUE)

 crs$dataset <- read_excel("C:/Users/L MYTHILI AMARTHYA/Desktop/svyasa/R/Datasets/Yoga - Hospital adult patients.xlsx", guess_max=1e4)

 crs$dataset

#=======================================================================
# Rattle timestamp: 2019-05-08 14:09:20 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=100 train=70 validate=15 test=15

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("AGE", "EDUCATION LEVEL", "SMOKING STATUS",
                   "EXERCISE", "WEIGHT", "SERUM-CHOL", "SYSTOLIC",
                   "IQ", "SODIUM", "GENDER")

crs$numeric   <- c("AGE", "EXERCISE", "WEIGHT", "SERUM-CHOL",
                   "SYSTOLIC", "IQ", "SODIUM")

crs$categoric <- c("EDUCATION LEVEL", "SMOKING STATUS", "GENDER")

crs$target    <- "MARITAL-STATUS"
crs$risk      <- NULL
crs$ident     <- "ID-NUMBER"
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 14:09:32 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=100 train=70 validate=15 test=15

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("AGE", "EDUCATION LEVEL", "SMOKING STATUS",
                   "EXERCISE", "WEIGHT", "SERUM-CHOL", "SYSTOLIC",
                   "SODIUM", "GENDER", "MARITAL-STATUS")

crs$numeric   <- c("AGE", "EXERCISE", "WEIGHT", "SERUM-CHOL",
                   "SYSTOLIC", "SODIUM")

crs$categoric <- c("EDUCATION LEVEL", "SMOKING STATUS", "GENDER",
                   "MARITAL-STATUS")

crs$target    <- "IQ"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- "ID-NUMBER"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 14:09:37 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 14:10:15 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for AGE

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(MARITAL-STATUS=as.factor(MARITAL-STATUS)) %>%
  dplyr::select(AGE, MARITAL-STATUS) %>%
  ggplot2::ggplot(ggplot2::aes(x=AGE)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=MARITAL-STATUS, colour=MARITAL-STATUS), alpha=0.55) +
  ggplot2::xlab("AGE\n\nRattle 2019-May-08 14:10:15 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of AGE (sample)\nby MARITAL-STATUS") +
  ggplot2::labs(fill="MARITAL-STATUS", y="Density")

#=======================================================================
# Rattle timestamp: 2019-05-08 14:10:26 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation Yoga - Hospital adult patients.xlsx using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-05-08 14:10:34 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(IQ ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.13 secs

#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 12:12:04 x86_64-w64-mingw32 

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
# Rattle timestamp: 2019-05-08 12:12:29 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/L MYTHILI AMARTHYA/Desktop/svyasa/R/Datasets/HeatCapacity.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:12:30 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=12 train=8 validate=2 test=2

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

crs$input     <- c("X.U.FEFF.T...C.", "Sp..Enth...kJ.kg.")

crs$numeric   <- c("X.U.FEFF.T...C.", "Sp..Enth...kJ.kg.")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:12:38 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:13:01 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for X.U.FEFF.T...C.

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(X.U.FEFF.T...C.) %>%
  ggplot2::ggplot(ggplot2::aes(x=X.U.FEFF.T...C.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("X.U.FEFF.T...C.\n\nRattle 2019-May-08 12:13:01 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of X.U.FEFF.T...C. (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Sp..Enth...kJ.kg.

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Sp..Enth...kJ.kg.) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sp..Enth...kJ.kg.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Sp..Enth...kJ.kg.\n\nRattle 2019-May-08 12:13:01 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Sp..Enth...kJ.kg. (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:02 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=12 train=8 validate=2 test=2

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

crs$input     <- "X.U.FEFF.T...C."

crs$numeric   <- "X.U.FEFF.T...C."

crs$categoric <- NULL

crs$target    <- "Sp..Enth...kJ.kg."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:06 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:14:12 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for X.U.FEFF.T...C.

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(X.U.FEFF.T...C.) %>%
  ggplot2::ggplot(ggplot2::aes(x=X.U.FEFF.T...C.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("X.U.FEFF.T...C.\n\nRattle 2019-May-08 12:14:12 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of X.U.FEFF.T...C. (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Sp..Enth...kJ.kg.

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Sp..Enth...kJ.kg.) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sp..Enth...kJ.kg.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Sp..Enth...kJ.kg.\n\nRattle 2019-May-08 12:14:12 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Sp..Enth...kJ.kg. (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:15:06 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Sp..Enth...kJ.kg. ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:15:30 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Sp..Enth...kJ.kg. ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.03 secs

#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 09:59:15 x86_64-w64-mingw32 

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
# Rattle timestamp: 2019-05-08 10:17:03 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/L MYTHILI AMARTHYA/Desktop/svyasa/R/Datasets/Salary_Data.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 10:17:03 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=30 train=21 validate=4 test=5

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

crs$input     <- c("YearsExperience", "Salary")

crs$numeric   <- c("YearsExperience", "Salary")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 10:23:04 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=30 train=21 validate=4 test=5

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

crs$input     <- "YearsExperience"

crs$numeric   <- "YearsExperience"

crs$categoric <- NULL

crs$target    <- "Salary"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 10:23:38 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=30 train=21 validate=4 test=5

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

crs$input     <- "YearsExperience"

crs$numeric   <- "YearsExperience"

crs$categoric <- NULL

crs$target    <- "Salary"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 10:24:14 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 10:30:17 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for YearsExperience

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(YearsExperience) %>%
  ggplot2::ggplot(ggplot2::aes(x=YearsExperience)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("YearsExperience\n\nRattle 2019-May-08 10:30:17 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of YearsExperience (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Salary

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Salary) %>%
  ggplot2::ggplot(ggplot2::aes(x=Salary)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Salary\n\nRattle 2019-May-08 10:30:17 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Salary (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 10:30:25 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for YearsExperience

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(YearsExperience) %>%
  ggplot2::ggplot(ggplot2::aes(x=YearsExperience)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("YearsExperience\n\nRattle 2019-May-08 10:30:25 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of YearsExperience (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Salary

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Salary) %>%
  ggplot2::ggplot(ggplot2::aes(x=Salary)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Salary\n\nRattle 2019-May-08 10:30:25 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Salary (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 10:35:11 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Salary ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.13 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 10:52:04 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:\Users\L MYTHILI AMARTHYA\Desktop\svyasa\practical\Salary_Data.rattle", compress=TRUE)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:38:37 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/L MYTHILI AMARTHYA/Desktop/svyasa/R/Datasets/Advertising.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 11:38:38 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

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

crs$input     <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$numeric   <- c("Sl", "TV", "radio", "newspaper", "sales")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:38:56 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 11:42:09 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Sl

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Sl) %>%
  ggplot2::ggplot(ggplot2::aes(x=Sl)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Sl\n\nRattle 2019-May-08 11:42:09 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Sl (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for TV

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(TV) %>%
  ggplot2::ggplot(ggplot2::aes(x=TV)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("TV\n\nRattle 2019-May-08 11:42:09 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of TV (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for radio

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(radio) %>%
  ggplot2::ggplot(ggplot2::aes(x=radio)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("radio\n\nRattle 2019-May-08 11:42:09 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of radio (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for newspaper

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(newspaper) %>%
  ggplot2::ggplot(ggplot2::aes(x=newspaper)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("newspaper\n\nRattle 2019-May-08 11:42:09 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of newspaper (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for sales

# Generate the plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(sales) %>%
  ggplot2::ggplot(ggplot2::aes(x=sales)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("sales\n\nRattle 2019-May-08 11:42:09 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of sales (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05)

#=======================================================================
# Rattle timestamp: 2019-05-08 11:48:48 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

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

crs$input     <- c("TV", "radio", "newspaper")

crs$numeric   <- c("TV", "radio", "newspaper")

crs$categoric <- NULL

crs$target    <- "sales"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- "Sl"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:49:42 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=200 train=140 validate=30 test=30

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

crs$input     <- c("TV", "radio", "newspaper")

crs$numeric   <- c("TV", "radio", "newspaper")

crs$categoric <- NULL

crs$target    <- "sales"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- "Sl"
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:17 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(sales ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.11 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 11:50:47 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(sales ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 11:54:36 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:\Users\L MYTHILI AMARTHYA\Desktop\svyasa\practical\Advertising.rattle", compress=TRUE)

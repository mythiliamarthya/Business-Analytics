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

#=======================================================================
# Rattle timestamp: 2019-05-08 12:18:10 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/L MYTHILI AMARTHYA/Desktop/svyasa/R/Datasets/Thermocouple.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:18:11 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=24 train=17 validate=4 test=3

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

crs$input     <- c("X.U.FEFF.Temp....C.", "Voltage..mV.")

crs$numeric   <- c("X.U.FEFF.Temp....C.", "Voltage..mV.")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:19:31 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=24 train=17 validate=4 test=3

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

crs$input     <- "Voltage..mV."

crs$numeric   <- "Voltage..mV."

crs$categoric <- NULL

crs$target    <- "X.U.FEFF.Temp....C."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:19:38 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:19:44 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for X.U.FEFF.Temp....C.

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(X.U.FEFF.Temp....C.) %>%
  ggplot2::ggplot(ggplot2::aes(x=X.U.FEFF.Temp....C.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("X.U.FEFF.Temp....C.\n\nRattle 2019-May-08 12:19:44 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of X.U.FEFF.Temp....C. (sample)") +
  ggplot2::labs(y="Density")

# Use ggplot2 to generate histogram plot for Voltage..mV.

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::select(Voltage..mV.) %>%
  ggplot2::ggplot(ggplot2::aes(x=Voltage..mV.)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("Voltage..mV.\n\nRattle 2019-May-08 12:19:44 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Voltage..mV. (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:20:17 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(X.U.FEFF.Temp....C. ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:20:28 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(X.U.FEFF.Temp....C. ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.00 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:20:32 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(X.U.FEFF.Temp....C. ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:22:39 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/L MYTHILI AMARTHYA/Desktop/svyasa/R/Datasets/Yoga - Baseball data for correlation and regression.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:22:39 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=235 train=164 validate=35 test=36

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

crs$input     <- c("Team", "Season", "Runs_Scored", "at_bats",
                   "hits", "batting_avg", "homeruns", "walks",
                   "strikeouts", "stolen_bases", "OBP", "SLG", "OPS")

crs$numeric   <- c("Season", "Runs_Scored", "at_bats", "walks",
                   "strikeouts", "stolen_bases", "OBP", "SLG", "OPS")

crs$categoric <- c("Team", "hits", "batting_avg", "homeruns")

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:22:43 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=235 train=164 validate=35 test=36

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

crs$input     <- c("Team", "Season", "at_bats", "hits",
                   "batting_avg", "homeruns", "walks", "strikeouts",
                   "stolen_bases", "OBP", "SLG", "OPS")

crs$numeric   <- c("Season", "at_bats", "walks", "strikeouts",
                   "stolen_bases", "OBP", "SLG", "OPS")

crs$categoric <- c("Team", "hits", "batting_avg", "homeruns")

crs$target    <- "Runs_Scored"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:23:39 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=235 train=164 validate=35 test=36

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

crs$input     <- c("Team", "Season", "at_bats", "hits",
                   "batting_avg", "homeruns", "walks", "strikeouts",
                   "stolen_bases", "OBP", "SLG", "OPS")

crs$numeric   <- c("Season", "at_bats", "walks", "strikeouts",
                   "stolen_bases", "OBP", "SLG", "OPS")

crs$categoric <- c("Team", "hits", "batting_avg", "homeruns")

crs$target    <- "Runs_Scored"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:23:44 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:24:07 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Season

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(Season, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=Season)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("Season\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Season (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for Runs_Scored

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(Runs_Scored, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=Runs_Scored)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("Runs_Scored\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of Runs_Scored (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for at_bats

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(at_bats, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=at_bats)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("at_bats\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of at_bats (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for walks

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(walks, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=walks)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("walks\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of walks (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for strikeouts

# Generate the plot.

p05 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(strikeouts, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=strikeouts)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("strikeouts\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of strikeouts (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for stolen_bases

# Generate the plot.

p06 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(stolen_bases, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=stolen_bases)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("stolen_bases\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of stolen_bases (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for OBP

# Generate the plot.

p07 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(OBP, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=OBP)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("OBP\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of OBP (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for SLG

# Generate the plot.

p08 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(SLG, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=SLG)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("SLG\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of SLG (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Use ggplot2 to generate histogram plot for OPS

# Generate the plot.

p09 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(homeruns=as.factor(homeruns)) %>%
  dplyr::select(OPS, homeruns) %>%
  ggplot2::ggplot(ggplot2::aes(x=OPS)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=homeruns, colour=homeruns), alpha=0.55) +
  ggplot2::xlab("OPS\n\nRattle 2019-May-08 12:24:07 L MYTHILI AMARTHYA") +
  ggplot2::ggtitle("Distribution of OPS (sample)") +
  ggplot2::labs(fill="homeruns", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04, p05, p06, p07, p08, p09)

#=======================================================================
# Rattle timestamp: 2019-05-08 12:26:52 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Runs_Scored ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:29:20 x86_64-w64-mingw32 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

miss <- union(attr(na.omit(crs$dataset[,"walks"]), "na.action"), attr(na.omit(crs$dataset[,"at_bats"]), "na.action"))
correlationTest(na.omit(crs$dataset[-miss, "walks"]), na.omit(crs$dataset[-miss, "at_bats"]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:31:40 x86_64-w64-mingw32 

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
title(main="Correlation Yoga - Baseball data for correlation and regression.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:32:21 x86_64-w64-mingw32 

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
title(main="Correlation Yoga - Baseball data for correlation and regression.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:32:47 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Runs_Scored ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.03 secs

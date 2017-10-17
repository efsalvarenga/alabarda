# pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
# for (pkg in pkgs) {if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }}
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# pkgs <- c("statmod","RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/6/R")

library(h2o)
h2o.init()

library(MASS)
library(ggplot2)
library(dplyr)
View(Boston)

set.seed(1)
Boston_sampling  <- sample(1:nrow(Boston),nrow(Boston))
Boston_train     <- Boston[Boston_sampling <= 450,]
Boston_test      <- Boston[Boston_sampling >  450,]

Boston_Benchmark <- h2o.automl(
  x = 1:13,
  y = 14,
  training_frame   = as.h2o(Boston_train),
  validation_frame = as.h2o(Boston_test),
  max_runtime_secs = 3600
)

Bostom_rf       <- caret::train(
  medv ~ .,
  data = Boston_train,
  method = "rf", tuneLength = 10,
  trControl = caret::trainControl(method = "cv", number = 5)
)
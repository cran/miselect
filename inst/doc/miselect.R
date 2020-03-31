## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = F----------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("umich-cphds/miselect", build_opts = c())

## -----------------------------------------------------------------------------
library(miselect)

colMeans(is.na(miselect.df))

## -----------------------------------------------------------------------------
library(mice)

set.seed(48109)

# Using the mice defaults for sake of example only.
mids <- mice(miselect.df, m = 5, printFlag = FALSE)

## -----------------------------------------------------------------------------
# Generate list of completed data.frames
dfs <- lapply(1:5, function(i) complete(mids, action = i))

# Generate list of imputed design matrices and imputed responses
x <- list()
y <- list()
for (i in 1:5) {
    x[[i]] <- as.matrix(dfs[[i]][, paste0("X", 1:20)])
    y[[i]] <- dfs[[i]]$Y
}


## -----------------------------------------------------------------------------
# Calculate observational weights
weights  <- 1 - rowMeans(is.na(miselect.df))
pf       <- rep(1, 20)
adWeight <- rep(1, 20)
alpha    <- c(.5 , 1)

# Since 'Y' is a binary variable, we use 'family = "binomial"'
fit <- cv.saenet(x, y, pf, adWeight, weights, family = "binomial",
                 alpha = alpha, nfolds = 5)

# By default 'coef' returns the betas for (lambda.min , alpha.min)
coef(fit)

## -----------------------------------------------------------------------------
coef(fit, lambda = fit$lambda.1se, alpha = fit$alpha.1se)

## -----------------------------------------------------------------------------
adWeight <- 1 / (abs(coef(fit)[-1]) + 1 / nrow(miselect.df))

afit <- cv.saenet(x, y, pf, adWeight, weights, family = "binomial",
                  alpha = alpha, nfolds = 5)

coef(afit)


## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(MLSurvey)

## ----make-data----------------------------------------------------------------
set.seed(1)
n <- 240

survey_data <- data.frame(
  psu     = rep(rep(1:3, each = n / 12), times = 4),
  strata  = rep(1:4, each = n / 4),
  weight  = runif(n, 500, 5000),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  y  = rep(c(0, 1), length.out = n)  # binary outcome
)

head(survey_data)

## ----fit-single---------------------------------------------------------------
fit <- wElnet(
  data    = survey_data,
  col.y   = "y",
  col.x   = c("x1", "x2", "x3", "x4"),
  family  = "binomial",
  alpha   = 0.5,
  cluster = "psu",
  strata  = "strata",
  weights = "weight",
  method  = "dCV",
  k = 4, R = 1
)

fit

## ----plot-single, fig.alt = "Weighted error vs. log(lambda) for a single alpha"----
plot(fit)

## ----autoplot-single, fig.alt = "ggplot2 version of the error profile for a single alpha"----
library(ggplot2)
autoplot(fit)

## ----fit-parallel, eval = FALSE-----------------------------------------------
# alpha_grid <- c(0, 0.25, 0.5, 0.75, 1)
# 
# par_fit <- par_wElnet(
#   alpha   = alpha_grid,
#   n.cores = 2,
#   data    = survey_data,
#   col.y   = "y",
#   col.x   = c("x1", "x2", "x3", "x4"),
#   family  = "binomial",
#   cluster = "psu",
#   strata  = "strata",
#   weights = "weight",
#   method  = "dCV",
#   k = 4, R = 1
# )
# 
# par_fit

## ----design-example-----------------------------------------------------------
des <- survey::svydesign(
  ids = ~psu, strata = ~strata, weights = ~weight,
  nest = TRUE, data = survey_data
)

fit_des <- wElnet(
  col.y = "y", col.x = c("x1", "x2", "x3", "x4"),
  design = des,
  family = "binomial", alpha = 0.5,
  method = "dCV", k = 4, R = 1
)

fit_des$alpha

## ----jkn-example--------------------------------------------------------------
fit_jkn <- wElnet(
  data = survey_data, col.y = "y", col.x = c("x1", "x2", "x3", "x4"),
  family = "binomial", alpha = 0.5,
  cluster = "psu", strata = "strata", weights = "weight",
  method = "JKn"
)

fit_jkn


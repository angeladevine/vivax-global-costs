#####################################################################################################################
#   R-script:     3b-PSA-distributions.R
#   Project:		  "Global economic costs due to vivax malaria and the potential impact of its radical cure: A modelling study" by A Devine et al.
#
#   Purpose:  		Generate beta, gamma and normal functions required for PSA
#
#   Date:			    14-April-2021
#   Author: 		  Niamh Meagher & Angela Devine
#####################################################################################################################


### BETA DISTRIBUTIONS
# Functions for generating alpha and beta values
ssqs4findbeta2 <- function(a, landu95, targetmean) {
  b <- (a / targetmean) - a
  landu <- qbeta(c(0.025, 0.975), a, b)
  return((landu[1] - landu95[1]) ^ 2 + (landu[2] - landu95[2]) ^ 2)
}

findbeta2 <- function(tmean, l95, u95) {
  astart <- 1
  bstart <- (astart / tmean) - astart
  result <-
    optimize(
      f = ssqs4findbeta2,
      interval = c(0, 1000),
      landu95 = c(l95, u95),
      targetmean = tmean
    )
  a <- result$minimum
  b <- (a / tmean) - a
  return(data.frame("a" = a, "b" = b))
}

### GAMMA DISTRIBUTIONS
# Functions for generating shape and scale values
ssqs4findgamma2 <- function(shape, landu95, targetmean) {
  scale <- targetmean / shape
  landu <- qgamma(c(0.025, 0.975), shape = shape, scale = scale)
  return((landu[1] - landu95[1]) ^ 2 + (landu[2] - landu95[2]) ^ 2)
}

findgamma2 <- function(tmean, l95, u95) {
  shapestart <- 1
  scalestart <- tmean / shapestart
  result <-
    optimize(
      f = ssqs4findgamma2,
      interval = c(0, 1000),
      landu95 = c(l95, u95),
      targetmean = tmean
    )
  shape <- result$minimum
  scale <- (tmean / shape)
  return(data.frame("shape" = shape, "scale" = scale))
  
}

### NORMAL DISTRIBUTION
findnorm <- function(mean, L, U, conf.level = 0.95) {
  return(data.frame("mean" = mean, "sd" =  (U - L) / (2 * qnorm(
    1 - (1 - conf.level) / 2
  ))))
}

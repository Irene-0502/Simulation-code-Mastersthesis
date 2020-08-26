library(PearsonDS)
library(GenOrd)

# Data Generation:


# 1. Two standard distributions:
standard_standard <- function(n1,n2){
  group1 <- rnorm(n1, 0, 1)
  group2 <- rnorm(n2, 0, 1)
  return(list(group1 = group1, group2 = group2))
}

# 2. Variance ratio = 2
standard_variance_2 <- function(n1,n2){
  group1 <- rnorm(n1, 0, 1)
  group2 <- rnorm(n2, 0, sqrt(2))
  return(list(group1 = group1, group2 = group2))
}

# 3. Variance ratio = 1.35
standard_variance_1.35 <- function(n1,n2){
  group1 <- rnorm(n1, 0, 1)
  group2 <- rnorm(n2, 0, sqrt(1.35))
  return(list(group1 = group1, group2 = group2))
}

# 4. Distribtion 2 skewness = 0.8
standard_skewed_0.8 <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  moments2 <- c(mean=0,variance=1,skewness=0.8,kurtosis=3)
  moments2_adj.location <- c(mean=0-qpearson(0.5, moments = moments2),variance=1,skewness=0.8,kurtosis=3)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}


# 5. Distribution 2 skewness = -0.35
standard_skewed_min0.35 <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  moments2 <- c(mean=0,variance=1,skewness=-0.35,kurtosis=3)
  moments2_adj.location <- c(mean=0-qpearson(0.5, moments = moments2),variance=1,skewness=-0.35,kurtosis=3)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}

# 6. Both distributions skewness = 0.8:
skewed_0.8_both <- function(n1,n2){
  moments1 <- c(mean=0,variance=1,skewness=0.8,kurtosis=3)
  moments1_adj.location <- c(mean=0-qpearson(0.5, moments = moments1),variance=1,skewness=0.8,kurtosis=3)
  group1 <- rpearson(n1, moments = moments1_adj.location)
  moments2 <- c(mean=0,variance=1,skewness=0.8,kurtosis=3)
  moments2_adj.location <- c(mean=0-qpearson(0.5, moments = moments2),variance=1,skewness=0.8,kurtosis=3)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}


# 7. Distribution 2 kurtosis = 5
standard_kurtosis_5 <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  group2 <- rpearson(n2, moments = c(0,1,0,5))
  return(list(group1 = group1, group2 = group2))
}

# 8. Distribution 2 kurtosis = 2.5
standard_kurtosis_2.5 <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  group2 <- rpearson(n2, moments = c(0,1,0,2.5))
  return(list(group1 = group1, group2 = group2))
}

# 9. Both distributions kurtosis = 5
kurtosis_5_both <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,5))
  group2 <- rpearson(n2, moments = c(0,1,0,5))
  return(list(group1 = group1, group2 = group2))
}

# 10. Variance ratio = 1.35, skewness distribution 2 = 0.8 and kurtosis distribution 2 = 5
VR_Skew_Kurtosis <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  moments2 <- c(0,1.35,0.8,5)
  moments2_adj.location <- c(0-qpearson(0.5, moments = moments2), 1.35,0.8,5)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}


# 11. Ordina data, no median difference
ordinal_no_dif <- function(n1,n2){
  n <- n1+n2
  marginal <- list(c(0.20,0.40,0.60,0.80),c(0.20,0.40,0.60,0.80))
  Sigma <- matrix(c(1,0,0,1),2,2)
  ord_sample <- ordsample(n, marginal, Sigma)
  group1 <- ord_sample[1:n1,1]
  group2 <- ord_sample[1:n2,2]
  return(list(group1 = group1, group2 = group2))
}
  




############################################# Unequal Medians ###########################################################

# 1. Two standard distributions
standard_standard_dif <- function(n1,n2){
  group1 <- rnorm(n1,0,1)
  group2 <- rnorm(n2,0.5,1)
  return(list(group1 = group1, group2 = group2))
}

# 2. Variance ratio = 2
standard_variance_2_dif <- function(n1,n2){
  group1 <- rnorm(n1, 0, 1)
  group2 <- rnorm(n2, 0.5, sqrt(2))
  return(list(group1 = group1, group2 = group2))
}

# 3. Variance ratio = 1.35
standard_variance_1.35_dif <- function(n1,n2){
  group1 <- rnorm(n1, 0, 1)
  group2 <- rnorm(n2, 0.5, sqrt(1.35))
  return(list(group1 = group1, group2 = group2))
}

# 4. Distribution 2 skewness = 0.8
standard_skewed_0.8_dif <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  moments2 <- c(mean=0.5,variance=1,skewness=0.8,kurtosis=3)
  moments2_adj.location <- c(mean=0.5+0.5- qpearson(0.5, moments = moments2),variance=1,skewness=0.8,kurtosis=3)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}


# 5. Distribution 2 skewness = -0.35
standard_skewed_min0.35_dif <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  moments2 <- c(mean=0.5,variance=1,skewness=-0.35,kurtosis=3)
  moments2_adj.location <- c(mean=0.5+(0.5- qpearson(0.5, moments = moments2)),variance=1,skewness=-0.35,kurtosis=3)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}

# 6. Both distributions skewness = 0.8:
skewed_0.8_both_dif <- function(n1,n2){
  moments1 <- c(mean=0,variance=1,skewness=0.8,kurtosis=3)
  moments1_adj.location <- c(mean=0-qpearson(0.5, moments = moments1),variance=1,skewness=0.8,kurtosis=3)
  group1 <- rpearson(n1, moments = moments1_adj.location)
  moments2 <- c(mean=0.5,variance=1,skewness=0.8,kurtosis=3)
  moments2_adj.location <- c(mean=0.5+(0.5- qpearson(0.5, moments = moments2)),variance=1,skewness=0.8,kurtosis=3)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}


# 7. Distribution 2 kurtosis = 5
standard_kurtosis_5_dif <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  group2 <- rpearson(n2, moments = c(0.5,1,0,5))
  return(list(group1 = group1, group2 = group2))
}

# 8. Distribution 2 kurtosis = 2.5
standard_kurtosis_2.5_dif <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  group2 <- rpearson(n2, moments = c(0.5,1,0,2.5))
  return(list(group1 = group1, group2 = group2))
}

# 9. Both distributions kurtosis = 5
kurtosis_5_both_dif <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,5))
  group2 <- rpearson(n2, moments = c(0.5,1,0,5))
  return(list(group1 = group1, group2 = group2))
}

# 10. Variance ratio = 1.35, skewness distribution 2 = 0.8 and kurtosis distribution 2 = 5
VR_Skew_Kurtosis_dif <- function(n1,n2){
  group1 <- rpearson(n1, moments = c(0,1,0,3))
  moments2 <- c(0.5,1.35,0.8,5)
  moments2_adj.location <- c(mean=0.5+(0.5-qpearson(0.5, moments = moments2)),variance=1.35,skewness=0.8,kurtosis=5)
  group2 <- rpearson(n2, moments = moments2_adj.location)
  return(list(group1 = group1, group2 = group2))
}


# 11. Ordinal, with a median difference

ordinal_dif <- function(n1,n2){
  n <- n1+n2
  marginal <- list(c(0.20,0.40,0.60,0.80),c(0.20,0.30,0.40,0.70))
  Sigma <- matrix(c(1,0,0,1),2,2)
  ord_sample <- ordsample(n, marginal, Sigma)
  group1 <- ord_sample[1:n1,1]
  group2 <- ord_sample[1:n2,2]
  return(list(group1 = group1, group2 = group2))
}



############################################ Data Generation ############################################################

MyDataGeneration <- function(n, nr, dist) {
  n1 <- n
  n2 <- round(nr * n)
  dist <- eval(parse(text = dist))
  dat <- dist(n1, n2)
  return(dat)
}
# Analyze function:
analyze_tests <- function(n, nr, dist) {
  theData <- MyDataGeneration(n, nr, dist)
  bothGroups <- c(theData$group1,theData$group2)
  groups <- c(rep(1,length(theData$group1)),rep(2,length(theData$group2)))
  resMood <- mood.medtest(bothGroups, groups, exact = FALSE)$p.value
  resBP <- Bonet_Price_sim(theData$group1, theData$group2)
  resBCAB <- BCAB_sim(theData$group1, theData$group2)
  return(c(pMood = resMood, pBP = resBP, pBCAB = resBCAB))
}

# Job type 1 with confidence intervals:
myJob_type1_CI <- function(n, nr, dist, reps) {
  set.seed(0801)
  results <- data.frame(resMood = numeric(reps), resBP = numeric(reps), resBCAB = numeric(reps))
  for (i in 1:reps) {
    results[i, ] <- analyze_tests(n, nr,dist)
  }
  decisions <- results < 0.05
  decisions[isFALSE(decisions)] <- 0
  upper_bound <- data.frame((matrix(ncol=ncol(decisions),nrow=1)))
  colnames(upper_bound) <- c("ubMood", "ubBP","ubBCAB" )
  lower_bound <- data.frame((matrix(ncol=ncol(decisions),nrow=1)))
  colnames(lower_bound) <- c("lbMood", "lbBP","lbBCAB" )
  for(j in 1:ncol(decisions)){
    upper_bound[j] <- mean(decisions[,j])+(2*sd(decisions[,j])/sqrt(reps))
    lower_bound[j] <- mean(decisions[,j])-(2*sd(decisions[,j])/sqrt(reps))
  }
  type1_Values <- colSums(decisions, na.rm = 1) / nrow(decisions)
  return(c(type1_Values, upper_bound, lower_bound))
}

# Job power:
myJob_power <- function(n, nr, dist, reps) {
  set.seed(0801)
  results <- data.frame(resMood = numeric(reps), resBP = numeric(reps),resBCAB = numeric(reps))
  for (i in 1:reps) {
    results[i, ] <- analyze_tests(n, nr,dist)
  }
  decisions <- results < 0.05
  power <- colSums(decisions, na.rm = TRUE) / nrow(decisions)
  return(power)
}






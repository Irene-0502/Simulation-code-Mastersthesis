library(PearsonDS)
library(batchtools)
library(RVAideMemoire)
library(coxed)
library(GenOrd)

# Design factors type 1:
# for sample sizes of 10,20,50 and 100:
n <- c(10,20,50,100)
nr <- c(0.5,1,2)
# for sample size of 500 (this is separate because the sample size ratio of 2 is not possible here):
n <- 500
nr <- c(0.5,1)

dist <- c('standard_standard', 'standard_variance_2', 'standard_variance_1.35', 'standard_skewed_0.8', 'standard_skewed_min0.35', 
          'skewed_0.8_both', 'standard_kurtosis_5', 'standard_kurtosis_2.5', 'kurtosis_5_both', 'VR_Skew_Kurtosis', 'ordinal_no_dif')
dist <- c('VR_Skew_Kurtosis')
reps <- 1000

# Design type 1:
Design_type1 <- expand.grid(n=n, nr=nr, dist=dist, stringsAsFactors = FALSE)
Design_type1$reps <- reps




# Design factors power:
# for sample sizes of 10,20,50 and 100:
n <- c(10,20,50,100)
nr <- c(0.5,1,2)
# for sample size of 500
n <- c(500)
nr <- c(0.5,1)
dist <- c('standard_standard_dif', 'standard_variance_2_dif', 'standard_variance_1.35_dif', 'standard_skewed_0.8_dif', 
          'standard_skewed_min0.35_dif', 'skewed_0.8_both_dif', 'standard_kurtosis_5_dif', 'standard_kurtosis_2.5_dif', 
          'kurtosis_5_both_dif', 'VR_Skew_Kurtosis_dif', 'ordinal_dif')
dist <- c('VR_Skew_Kurtosis_dif')
reps <- 1000

# Design power:
Design_power <- expand.grid(n=n, nr=nr, dist=dist, stringsAsFactors = FALSE)
Design_power$reps <- reps

# run the adjusted VR-Skew-Kurtosis scenario:
reg = makeRegistry(file.dir='registry_power_Schlag_n500_ajustedVRSK',source =c('dataGeneration.R','meta Schlag.R'))
reg$cluster.functions = makeClusterFunctionsInteractive()
batchMap(myJob_power,args=Design_power)
submitJobs() 
waitForJobs()
getStatus()
reduceResultsList()







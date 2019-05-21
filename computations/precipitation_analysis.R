#file to test analysis on precipitation data
library( rEDM)
source("import_data.R")

modes_df <- na.omit(modes_df)

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

#grid data
grid_precip <- read.csv("../datasets/precip1.csv")
precip <- grid_precip$precipitation
#smapOut <- s_map( (grid_precip$precipitation), E=9, lib=c(1,400),pred=c(430,650))
#note	:	 predicion
simplex( precip )
smapOutput <- s_map( precip, theta=1:20 )
print(smapOutput)
plot(smapOutput$theta, smapOutput$rho )

#trying two related grid points
grid_precip1 <- read.csv("../datasets/precip1.csv")
grid_precip1 <- grid_precip1$precipitation
grid_precip2 <- read.csv("../datasets/precip2.csv")
grid_precip2 <- grid_precip2$precipitation
avg <- grid_precip1

for ( idx in 1:length(avg) ) {
	avg[idx] <- (grid_precip1[idx]+grid_precip2[idx])/2
}
plot(avg)
simplex(avg)

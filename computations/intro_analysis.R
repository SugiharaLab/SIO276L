#file to run simplex and smap on the indices

source ("import_data.R")
library(rEDM)

RUN_SIMPLEX_E	<- FALSE
RUN_SIMPLEX_TAU	<- TRUE
RUN_SMAP		<- FALSE

#how much to keep in range of best rho
epsilon <- .04 

options(width=as.integer(500))

#handpicked best E from simplex output
best_E <- function(mode) {
	switch(	mode,
			"ENSO" 	= 5,
			"NAO"	= 8,
			"NPI"	= 7,
			"PDO"	= 3,
			)
}

#get simplex and or smap output on each dataset

for ( idx in 1:ncol( modes_df )) {
	mode <- colnames(modes_df)[idx]
	print(paste("runing on ",mode))
	mode_vec <- modes_df[,idx]
	n_row <- length( mode_vec )
	lib	 = c(1, floor(2/3 * n_row))
	pred = c(floor(2/3 * n_row)+1, n_row)

	if ( RUN_SIMPLEX_E ) {
		simplex_output <- simplex( mode_vec, lib, pred)
		#plot the best E for this mode
		par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set margins for plotting
		plot(simplex_output$E, simplex_output$rho, type = "l", main=mode,
				xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")
	}
	if ( RUN_SIMPLEX_TAU ) {
		min_E <- 11 #keep track of the lowest E within range of the best tau
		simplex_output <- simplex( mode_vec, lib, pred, E=c(1:10),tau=c(1:10))
		best_rho <- max(simplex_output$rho)
		#get rows with near best rho. 
		for(i in 1:nrow(simplex_output)) {
			row <- simplex_output[i,]
			if ( abs(row$rho - best_rho) < epsilon && row$E < min_E ) {
				best_row <- row
			}
		}
		#print(paste("best E:Tau combination for modebest_row)")
		cat(sprintf("best E:Tau combination for mode:%s is %d:%d w rho %f\n", mode, best_row$E, best_row$tau, best_row$rho))
		#plot the best E for this mode
		par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set margins for plotting
		plot(simplex_output$tau, simplex_output$rho, type = "l", main=mode,
				xlab = "Time Lag Amount (Tau)", ylab = "Forecast Skill (rho)")
	}
	if ( RUN_SMAP ) {	
		smap_output <- s_map( mode_vec, lib, pred, E=best_E(mode) )
		#plot the best E for this mode
		par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set margins for plotting
		plot(smap_output$theta, smap_output$rho, type = "l", main=mode,
				xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
	}
}

summary <- ("
Simplex Summary:
		best rho, index:
ENSO: 		 .95, 	5
NAO : 		 .05, 	8
NPI : 		 .80, 	7
PDO : 		 .80, 	3

SMap Summary:
		best rho, index:
ENSO: 		 .99, 	3 	
NAO : 		 .08, 	1
NPI : 		 .75, 	4
PDO : 		 .85, 	0

Best E:Tau combinations for each mode:
		       E,  Tau, rho
ENSO: 		  10, 	3   .897
NAO : 		   8, 	9   .119
NPI : 		  10, 	7   .753
PDO : 		  10, 	6	.745
")

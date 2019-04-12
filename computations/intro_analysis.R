#file to run simplex and smap on the indices

source ("import_data.R")
library(rEDM)

RUN_SIMPLEX	 <- FALSE
RUN_SMAP 	 <- TRUE

#get simplex and or smap output on each dataset

for ( idx in 1:ncol( modes_df )) {
	mode <- colnames(modes_df)[idx]
	print(paste("runing on ",mode))
	mode_vec <- modes_df[,idx]
	n_row <- length( mode_vec )
	lib	 = c(1, floor(2/3 * n_row))
	pred = c(floor(2/3 * n_row)+1, n_row)

	if ( RUN_SIMPLEX ) {
		simplex_output <- simplex( mode_vec, lib, pred)
		print(simplex_output)

		#plot the best E for this mode
		par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set margins for plotting
		plot(simplex_output$E, simplex_output$rho, type = "l", main=mode,
				xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")
	}
	if ( RUN_SMAP ) {
		
		#handpicked best E from simplex output
		if ( mode == "ENSO" )  {
			best_E = 5
		}
		best_E <- function(mode) {
			switch(	mode,
					"ENSO" 	= 5,
					"NAO"	= 8,
					"NPI"	= 7,
					"PDO"	= 3,
					)
		}
		
		smap_output <- s_map( mode_vec, lib, pred, E=best_E(mode) )
		print(smap_output)

		#plot the best E for this mode
		par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set margins for plotting
		plot(smap_output$theta, smap_output$rho, type = "l", main=mode,
				xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
	}
}

cat ("
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

")

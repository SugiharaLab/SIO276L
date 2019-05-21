#file to try doing multiview analysis
source("import_data.R")
library(rEDM)
options(width=190)

k_list <- c(1, 3, "sqrt", "all")
multiview( modes_df, E=5, max_lag=5, k=k_list, target_column=2 )

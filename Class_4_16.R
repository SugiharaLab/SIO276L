library(readr)
library(hht)
library(rEDM)

ENSO_Clean <- read_csv("datasets/cleaned_data/ENSO_Clean.csv")
NAO_Clean <- read_csv("datasets/cleaned_data/NAO_Clean.csv")
NPI_Clean <- read_csv("datasets/cleaned_data/NPI_Clean.csv")
PDO_Clean <- read_csv("datasets/cleaned_data/PDO_Clean.csv")

ENSO = ENSO_Clean$Value
NAO = NAO_Clean$Value
NPI = NPI_Clean$Value
PDO = PDO_Clean$Value

for(i in c(1:length(NAO))){
  if(is.na(NAO[i])){
    NAO[i] = NAO [i-1]
  }}
for(i in c(1:length(NPI))){
  if(is.na(NPI[i])){
    NPI[i] = NPI [i-1]
  }}
for(i in c(1:length(PDO))){
  if(is.na(PDO[i])){
    PDO[i] = PDO [i-1]
  }}

plot(ENSO, type = 'l')
plot(NPI, type = 'l', lwd = 1)
plot(NAO, type = 'l', lwd = 1)
plot(PDO, type = 'l', lwd = 1)

ts = NAO
#ts = Signal_filtered

ts_smap <- s_map(ts, tp = 6, tau = 6, E=15)
plot(ts_smap$theta, ts_smap$rho, xlab = 'theta', ylab = 'rho', type = 'l')
lines(ts_smap$theta,ts_smap$rho, xlab = 'theta', ylab = 'rho', type = 'l', col = 'blue')

ts_simplex <- simplex(ts, E = 1:20, tp=1, tau = 12)
plot(ts_simplex$rho, xlab = 'E', ylab = 'rho', type = 'l', col = 'blue', ylim = c(0,1))



#######################################################################################################
for(j in c(1:4)){
  for(i in  c(1:length(modes_df[,1]))){
    if(is.na(modes_df[i,j])){
      modes_df[i,j] = modes_df[i-1,j]}
  }
}

f_NAO = Sig2IMF(modes_df$NAO, c(1:length(modes_df$NAO)))

PlotIMFs(f_NAO)

Signal = f_NAO$imf


Signal_filtered = rowSums(Signal[,3:6])
plot(Signal_filtered, type = 'l')


modes_df[,5]<- Signal_filtered
modes_df[,6]<- Signal[,1]


out<-ccm(modes_df, lib_column = 1, target_column =  2, E = 20, tp = 0, tau = 6, lib_sizes = c(100,200,500,800,1000))
plot(out$lib_size, out$rho, xlab = 'lib_size', ylab = 'rho', main = "ENSO_ccm_NAO")

out2<-ccm(modes_df, lib_column = 2, target_column =  1, E = 20, tp = 0, tau = 6, lib_sizes = c(100,200,500,800,1000))
plot(out2$lib_size, out2$rho, xlab = 'lib_size', ylab = 'rho', main = "NAO_ccm_ENSO")



out3<-ccm(modes_df, lib_column = 1, target_column =  4, E = 20, tp = 0, tau = 6, lib_sizes = c(100,200,500,800,1000))
plot(out3$lib_size, out3$rho, xlab = 'lib_size', ylab = 'rho', main = "ENSO_ccm_PDO")

out5<-ccm(modes_df, lib_column = 4, target_column =  1, E = 20, tp = 0, tau = 6, lib_sizes = c(100,200,500,800,1000))
plot(out5$lib_size, out5$rho, xlab = 'lib_size', ylab = 'rho', main = "PDO_ccm_ENSO")


out4<-ccm(modes_df, lib_column = 5, target_column =  1, E = 20, tp = 0, tau = 6, lib_sizes = c(100,200,500,800,1000))
plot(out4$lib_size, out4$rho, xlab = 'lib_size', ylab = 'rho', main = "NAOF_ccm_ENSO")

out7<-ccm(modes_df, lib_column = 1, target_column =  5, E = 20, tp = 0, tau = 6, lib_sizes = c(100,200,500,800,1000))
plot(out7$lib_size, out7$rho, xlab = 'lib_size', ylab = 'rho', main = "ENSO_ccm_NAOF")








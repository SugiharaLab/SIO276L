library(rEDM)
library(hht)
library(smooth)
library(Mcomp)
data = modes_df
for(j in c(1:4)){
  for(i in  c(1:length(data[,1]))){
    if(is.na(data[i,j])){
      data[i,j] = data[i-1,j]}
  }
}
NAO = data$PDO
plot(NAO, type = 'l')
out <- simplex(NAO, E = 1:20, exclusion_radius = 200)

####### EMD #################
sig = Sig2IMF(NAO, c(1:length(NAO)))
PlotIMFs(sig)
Signal = sig$imf
rhos = {}
i=2
Signal_filtered = rowSums(Signal[,i:8])
plot(Signal_filtered, type = 'l')
output1 <- simplex(Signal_filtered, E = 1, exclusion_radius = 200, stats_only = FALSE, tp = 6)
#output1 <- s_map(Signal_filtered, E = 6, exclusion_radius = 200, stats_only = FALSE, theta = 0.001, tp = 1)

predictions1<- output1[[1]][["model_output"]][["pred"]]
obs1 <- output1[[1]][["model_output"]][["obs"]]

predictions1= predictions1[101:(length(predictions1)-20)]

actual = NAO[101:(length(obs1)-20)]

correlate1 = cor(actual, predictions1) 
plot(actual[200:500], type = 'l', lwd = 3)
lines(predictions1[200:500], type = 'l', col = 'red', lwd = 3)
rhos = c(rhos, correlate1)


plot(rhos, type = 'l', lwd = 4, xlab = "IMF's Included", ylab = 'rho',xaxt = "n")
axis(1, at=1:10, labels=letters[1:10])
plot(actual[200:500], type = 'l', lwd = 3)
lines(predictions1[200:500], type = 'l', col = 'red', lwd = 3)



rhos = {}
for(E in c(1:20)){
output1 <- simplex(Signal_filtered, E = E, exclusion_radius = 100, stats_only = FALSE, tp = 1)

predictions1<- output1[[1]][["model_output"]][["pred"]]
obs1 <- output1[[1]][["model_output"]][["obs"]]

predictions1= predictions1[101:(length(predictions1)-20)]

actual = NAO[(101+tp):(length(obs1)-20+tp)]

correlate1 = cor(actual, predictions1) 
rhos = c(rhos, correlate1)
}
plot(c(1:20), rhos, type = 'l')

output2 <- s_map(Signal_filtered, E = 3, exclusion_radius = 200, stats_only = TRUE, theta = 0.001)
plot(output2$theta, output2$rho)

avg = {}
av = 1
for(i in c(av:(1425))){
  current = NAO[(i-av+1):i]
  print(length(avg)+1)
  print(current)
  avg = c(avg, mean(current))
}
plot(avg, type = 'l', lwd = 3, col = 'blue')
plot(c(12:1425), Signal_filtered[12:1425], col = 'red', type = 'l', lwd = 3) 
simplex(avg, exclusion_radius = 100, tp = 1)
simplex(Signal_filtered, exclusion_radius = 100, tp = 1)


rhos = {}
for(E in c(1:20)){
  output1 <- simplex(avg, E = E, exclusion_radius = 100, stats_only = FALSE, tp = 1)
  
  predictions1<- output1[[1]][["model_output"]][["pred"]]
  obs1 <- output1[[1]][["model_output"]][["obs"]]
  
  predictions1= predictions1[101:(length(predictions1)-20)]
  
  actual = NAO[(101+av+tp):(length(obs1)-20+av+tp)]
  
  correlate1 = cor(actual, predictions1) 
  rhos = c(rhos, correlate1)
}
plot(c(1:20), rhos, type = 'l')


output1 <- simplex(avg, E = 5, exclusion_radius = 100, stats_only = FALSE, tp = 1)

predictions1<- output1[[1]][["model_output"]][["pred"]]
predictions1= predictions1[101:(length(predictions1)-20)]

actual = NAO[(101+av-1):(length(obs1)-20+av-1)]
plot(actual[300:400], type = 'l', lwd = 3, col = 'black')
lines(predictions1[300:400], col = 'red')







par(mfrow=c(2,2))
plot(Signal_filtered, type = 'l')
plot(data$ENSO,type = 'l')
plot(data$PDO,type = 'l')
plot(data$NPI,type = 'l')
series = data$NAO
simplex(NPI, exclusion_radius = 100, E = 1:20, tp = 6)









NAO = series


simplex(NAO, exclusion_radius = 100, E = 1:20, tp = 6)
sig = Sig2IMF(NAO, c(1:length(NAO)))
PlotIMFs(sig)
Signal = sig$imf
rhos = {}
i=3
Signal_filtered = rowSums(Signal[,2:8])
acf(Signal_filtered, lwd = 5)
acf(NAO, lwd = 5)
plot(Signal_filtered, type = 'l', lwd = 3)
plot(NAO, type = 'l')


rhos = {}
tp = 6
for(E in c(1:20)){
  output1 <- simplex(Signal_filtered, E = E, 
                     exclusion_radius = 200, stats_only = FALSE, tp = tp)
  
  predictions1<- output1[[1]][["model_output"]][["pred"]]
  obs1 <- output1[[1]][["model_output"]][["obs"]]
  
  predictions1= predictions1[101:(length(predictions1)-20)]
  
  actual = NAO[(101+tp):(length(obs1)-20+tp)]
  obs1 = obs1[101:(length(predictions1)-20)]
  correlate1 = cor(actual, predictions1) 
  rhos = c(rhos, correlate1)
}
plot(c(1:20), rhos, type = 'l', lwd = 3)



# output1 <- simplex(Signal_filtered, E = which.max(rhos), exclusion_radius = 100, 
#                    stats_only = FALSE, tp = tp)
# 
# predictions1<- output1[[1]][["model_output"]][["pred"]]
# predictions1= predictions1[101:(length(predictions1)-20)]
# actual = NAO[(101+tp):(length(obs1)-20+tp)]
# 
# plot(actual[500:700], lwd = 3, type = 'l')
# lines(predictions1[500:700], col = 'red', lwd = 2)
# cor(actual[500:700],predictions1[500:700])



# rhos = {}
# for(theta in c(0, 1e-04, 3e-04,0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.5, 0.75, 1)){
#   output1 <- s_map(Signal_filtered, E = 3, exclusion_radius = 100, stats_only = FALSE, 
#                    theta = theta, tp = 6)
#   
#   predictions1<- output1[[1]][["model_output"]][["pred"]]
#   obs1 <- output1[[1]][["model_output"]][["obs"]]
#   
#   predictions1= predictions1[101:(length(predictions1)-20)]
#   
#   actual = NAO[102:(length(obs1)-19)]
#   
#   correlate1 = cor(actual, predictions1) 
#   rhos = c(rhos, correlate1)
# }

NAO = data$PDO
simplex(NAO, exclusion_radius = 100, tp = 12, E = 1:20)
s_map(NAO, exclusion_radius = 100, tp = 12, E = 11)
for( av in c(2:12)){
avg = {}
for(i in c(av:(1425))){
  current = NAO[(i-av+1):i]
  print(length(avg)+1)
  print(current)
  avg = c(avg, mean(current))
}
#plot(avg, type = 'l')
rhos = {}

ot <- simplex(avg, exclusion_radius = 100, tp= 12, E = 1:20)
plot(ot$rho, type = 'l')
ot2 <- s_map(avg, E = which.max(ot$rho), exclusion_radius = 100, tp= 12)
plot(ot2$theta, ot2$rho, type ='l', main = av)
}
for(E in c(1:20)){
  output1 <- simplex(avg, E = E, exclusion_radius = 100, stats_only = FALSE, tp = tp)
  
  predictions1<- output1[[1]][["model_output"]][["pred"]]
  obs1 <- output1[[1]][["model_output"]][["obs"]]
  
  predictions1= predictions1[101:(length(predictions1)-20)]
  
  actual = NAO[(101+av+tp):(length(obs1)-20+av+tp)]
  obs1 <- output1[101:(length(predictions1)-20)]
  correlate1 = cor(actual, predictions1) 
  rhos = c(rhos, correlate1)
}
plot(c(1:20), rhos, type = 'l', col = 'blue', lwd = 3)



tma = {}
libz = {}
ans = {}
for(i in seq(3,1425,12)){
  current = NAO[i:(i)]
  tma = c(tma, mean(current))
  libz = c(libz, mean(current))
  tma = c(tma, NAO[i+6])
  ans = c(ans, NAO[i+6])
}
total = c(lib, tma)
write.csv(tma, file = "tma.csv")
simplex(tma, exclusion_radius = 0, tau =2, lib = c(c(1,2), c(3,4), c(5,200)))

NAO = data$ENSO
PDO = data$PDO
avg = {}
for(i in c(av:(1425))){
  current = NAO[(i-av+1):i]
  print(length(avg)+1)
  print(current)
  avg = c(avg, mean(current))
}
NAO = avg

avg = {}
for(i in c(av:(1425))){
  current = PDO[(i-av+1):i]
  print(length(avg)+1)
  print(current)
  avg = c(avg, mean(current))
}
PDO = avg
plot(NAO, type = 'l')



for( tp in c(1:9)){
  for(m in c(1:12)){
  tma = {}
  for(i in seq(2+m,length(NAO)-12,12)){
    current = PDO[i:(i-2)]

    tma = c(tma, mean(current))
    #tma = c(tma, PDO[i+tp])
  }
  total = c(lib, tma)
  write.csv(tma, file = paste("tma_tp_",tp,"_month_", m, ".csv", sep = ""))
}}



tma = {}
pdos = {}
tp = 0
m=2
for(i in seq(2+m,length(NAO)-12,12)){
  current = NAO[i:(i-2)]
  tma = c(tma, mean(current))
  pdos = c(pdos, NAO[i+tp])
}
ddd = as.data.frame(matrix(0, length(pdos),2))
ddd[,1] <- pdos
ddd[,2] <- tma
k = ccm(ddd, E = 5, lib_column = 2, target_column = 1, num_samples = 1, lib_sizes = 10000)
print(k$rho)







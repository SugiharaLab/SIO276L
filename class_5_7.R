for(j in c(1:4)){
  for(i in  c(1:length(data[,1]))){
    if(is.na(data[i,j])){
      data[i,j] = data[i-1,j]}
  }
}
rain = read.csv( "percip.csv")
rain = rain[,2]
rain = rain[63:1487]

ts = data$ENSO
ts = rain
dates= seq(1900, 2018.7, 1/12)
colors = c('red','blue','green', 'purple')
plots = 1
# 

plt = FALSE
simplx = FALSE
smap = FALSE
auto_c = FALSE
make_gif = TRUE
letters = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p')
for( av in c(1:12)){
  avg = {}
  for(i in c(av:(1425))){
    current = ts[(i-av+1):i]
    avg = c(avg, mean(current))
  }
  
  if(av %in% c(1, 3,6 ,12) & plt == TRUE){
    
    title = paste(av, "Month Average")
    if(av == 1){title = "No Average"}
    plot(dates[av:1425], avg, col = colors[plots], type = 'l', lwd = 1, ylab = '', main = title, xlab = '')
    plots = plots +1
  }
  if(simplx == TRUE){
    out_simplex <- simplex(avg, exclusion_radius = 100, tp = 6)

    if(av %in% c(1, 3,6 ,12)){
      title = paste(av, "Month Average")
      if(av == 1){title = "No Average"}
      plot(out_simplex$E, out_simplex$rho, col = colors[plots], type = 'l', lwd = 4, ylab = 'rho', main = title, xlab = 'E')
      plots = plots +1
    }
  
    
  }
  if(smap == TRUE){
    if(av %in% c(1, 3,6 ,12)){
      out_smap <- s_map(avg, exclusion_radius = 100, tp = 6, E = 4)
      title = paste(av, "Month Average")
      if(av == 1){title = "No Average"}
      plot(out_smap$theta, out_smap$rho, col = colors[plots], type = 'l', lwd = 4, ylab = 'rho', main = title, xlab = 'Theta')
      plots = plots +1
    }
  }
  
  if(auto_c == TRUE){
    if(av %in% c(1, 3,6 ,12)){
      
      if(av == 1){title = "No Average"}
      correlation = acf(avg, main = title, na.action = na.pass, plot = FALSE, lag.max = 6)
      max_at_six = correlation[6]
      tit = (max_at_six$acf[1])
      tit = as.integer(tit * 100)/100
      title = paste(av, "Month Average - ", tit, "at lag 6")
      print(title)
      acf(avg, main = title, na.action = na.pass, plot = TRUE)
      plots = plots +1
    }
    
    
  }
  if(make_gif == TRUE){
    svg(paste("avg_",letters[av],".svg"))
    par(mfrow=c(2,2))
    title = paste(av, "Month Average")
    if(av == 1){title = "No Average"}
    plot(dates[av:1425], avg, type = 'l', lwd = 1, ylab = '', main = title, xlab = '', ylim = c(min(ts), max(ts)), xlim = c(1900, 2019))

    title = paste(av, "Month Average")
    if(av == 1){title = "No Average"}
    out_simplex <- simplex(avg, exclusion_radius = 100, tp = 12, E = 1:20)
    plot(out_simplex$E, out_simplex$rho, type = 'l', lwd = 4, ylab = 'rho', main = title, xlab = 'E', ylim=c(0, 0.9), col = 'blue')
    out_smap <- s_map(avg, exclusion_radius = 100, tp = 12, E = which.max(out_simplex$E),
                      theta = c(0, 1e-04, 3e-04,
                        0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 7.5)/2.5)
    title = paste(av, "Month Average")
    if(av == 1){title = "No Average"}
    plot(out_smap$theta, out_smap$rho, col = 'red', type = 'l', lwd = 4, ylab = 'rho', main = title, 
         xlab = 'Theta', ylim = c(min(out_smap$rho), min(out_smap$rho)+0.1)) 
    
   
    correlation = acf(avg, main = title, na.action = na.pass, plot = FALSE, lag.max = 12)
    max_at_six = correlation[12]
    tit = (max_at_six$acf[1])
    tit = as.integer(tit * 100)/100
    title = paste(av, "Month Average - ", tit, "at lag 6")
    if(av == 1){title = paste("No Average - ", tit, "at lag 6")}
    print(title)
    acf(avg, main = title, na.action = na.pass, plot = TRUE)
    
    dev.off()

   }
}
##########CCM#############3
ts = data$NAO
avg_NAO = {}
av=6
for(i in c(av:(1425))){
  current = ts[(i-av+1):i]
  avg_NAO = c(avg_NAO, mean(current))
}
avg_NAO = avg_NAO[(12-av):length(avg_NAO)]

av = 5
ts = data$ENSO
avg_ENSO = {}
for(i in c(av:(1425))){
  current = ts[(i-av+1):i]
  avg_ENSO = c(avg_ENSO, mean(current))
}
avg_ENSO = avg_ENSO[(12-av):length(avg_ENSO)]

ts = data$PDO
avg_PDO = {}
av = 1
for(i in c(av:(1425))){
  current = ts[(i-av+1):i]
  avg_PDO = c(avg_PDO, mean(current))
}
avg_PDO = avg_PDO[(12-av):length(avg_PDO)]

ts = rain
avg_rain = {}
av = 8
for(i in c(av:(1425))){
  current = ts[(i-av+1):i]
  avg_rain = c(avg_rain, mean(current))
}
avg_rain = avg_rain[(12-av):length(avg_rain)]



ts = data$NPI
avg_NPI = {}
av=10
for(i in c(av:(1425))){
  current = ts[(i-av+1):i]
  avg_NPI = c(avg_NPI, mean(current))
}
avg_NPI = avg_NPI[(12-av):length(avg_NPI)]

data_ccm <- as.data.frame(matrix(0, length(avg_NPI),5))
data_ccm[,1] <- avg_NPI
data_ccm[,2] <- avg_NAO
data_ccm[,3] <- avg_PDO
data_ccm[,4] <- avg_ENSO
data_ccm [,5] <- avg_rain

names = c('NPI', 'NAO', 'PDO', 'ENSO', 'Percipitation')
rhos1 = {}
rhos2 ={}
combinations = combn(c(1:5),2)
for(i in c(1:10)){
  combo = combinations[,i]
  a = combo[1]
  b = combo[2]
  rhos1 = {}
  rhos2 ={}
  cs1 = {}
  cs2={}
for(tp in c(-12:3)){
  out_ccm1 <- ccm(data, E = 4, tp = tp, lib_sizes = 1415, 
                 num_samples = 1, lib_column = a, target_column = b, exclusion_radius = 100)
  
  out_ccm2 <- ccm(data, E = 4, tp = tp, lib_sizes = 1415, 
                 num_samples = 1, lib_column = b, target_column = a, exclusion_radius = 100)
  rhos1 = c(rhos1, out_ccm1$rho)
  rhos2 = c(rhos2, out_ccm2$rho)


  cr = cor(data[,a][(130+tp):(1315+tp)], data[,b][130:(1315)])
  cs1 = c(cs1,cr)
  cr = cor(data[,b][(130+tp):(1315+tp)], data[,a][130:(1315)])
  cs2 = c(cs2,cr)}
  
  #pdf(paste(a,b, ".pdf"))
  plot(c(-12:3),rhos1, col = 'red', lwd = 3, type = 'l', ylim = c( min(c(rhos1,rhos2,cs1, cs2)), max(c(rhos1,rhos2,cs1,cs2))),
       xlab = "tp", ylab = "rho")
  abline(v = 0)
  
  lines(c(-12:3),rhos2, col = 'blue', lwd = 3)
  lines(c(-12:3),cs1, col = 'blue', lwd = 3, lty = 3)
  lines(c(-12:3),cs2, col = 'red', lwd = 3, lty = 3)
  
  legend(-12, max(c(rhos1,rhos2,cs2,cs1)),
         legend=c(paste(names[b], " drives ", names[a]), paste(names[a], " drives ", names[b])),
         col=c("red", "blue"), lty=1:1, cex=0.8)
  #dev.off()
}



##Percipitation

rain = read.csv( "percip.csv")
rain = rain[,2]
rain = rain[63:1487]

plot(rain[,2], type = 'l')









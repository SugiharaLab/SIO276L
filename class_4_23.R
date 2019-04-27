library(rEDM)
load("~/Desktop/Files/ocean_data.RData")
ENSO <- data$ENSO
PDO <- data$PDO
NPI <- data$NPI
NAO <- data$NAO
######################## SIMPLEX PLOTS ######################################
{
out1 <- simplex(ENSO, E = 1:20, tp = 6,tau = 6, exclusion_radius = 100)
plot(out1$rho, main = 'Simplex', type = 'l', ylab = 'rho', xlab = 'E', ylim = c(0,0.85), lwd = 4, col = 'red' )

out2 <- simplex(PDO, E = 1:20, tp = 6,tau = 6, exclusion_radius = 100)
lines(out2$rho,  type = 'l', ylab = 'rho', xlab = 'E', lwd = 4, col = 'blue' )

out3 <- simplex(NPI, E = 1:20, tp = 6,tau = 6, exclusion_radius = 100)
lines(out3$rho,  type = 'l', ylab = 'rho', xlab = 'E' , lwd = 4, col = 'green')

out4 <- simplex(NAO, E = 1:20, tp = 6,tau = 6, exclusion_radius = 100)
lines(out4$rho,  type = 'l', ylab = 'rho', xlab = 'E', lwd = 4, col = 'purple' )
legend(1, 0.87, legend=c("ENSO", "PDO", 'NPI', 'NAO'),
       col=c("red", "blue", "green","purple"), lty=1:1, cex=0.8)
}
##############################################################################


######################## S-MAP PLOTS ######################################
{
  out1 <- s_map(ENSO, E = 6, tp = 6,tau = 6, exclusion_radius = 100)
  plot(out1$theta, out1$rho, lwd = 4, main = 'ENSO', xlab = 'Theta', ylab = 'Rho', type = 'l')  
  
  out2 <- s_map(PDO, E = 6, tp = 6,tau = 6, exclusion_radius = 100)
  plot(out2$theta,out2$rho,  type = 'l', ylab = 'rho', xlab = 'Theta', lwd = 4, main = 'PDO')
  
  out3 <- s_map(NPI, E = 6, tp = 6,tau = 6, exclusion_radius = 100)
  plot(out3$theta,out3$rho,  type = 'l', ylab = 'rho', xlab = 'Theta', lwd = 4, main = 'NPI')
  
  out4 <- s_map(NAO, E = 6, tp = 6,tau = 6, exclusion_radius = 100)
  plot(out4$theta,out4$rho,  type = 'l', ylab = 'rho', xlab = 'Theta', lwd = 4, main = 'NAO')

}
##############################################################################

################################# CCM Matrix ###################################
ccm_matrix = as.data.frame(matrix(NA, 4, 4))
for(i in c(1:4)){
  for(j in c(1:4)){
    if(i!=j){
      out<-ccm(data, target_column = i, lib_column = j, num_samples = 1, 
               lib_sizes = 14250, exclusion_radius = 100, E = 6,
               tp = 0, tau = 6)
      
      ccm_matrix[i,j]<- out$rho

    }
  }
}
##############################################################################


################################# Lagged CCM ###################################
titles = c('ENSO','NAO', 'NPI', 'PDO')
combinations = combn(c(1:4), 2)
for(i in  c(1:NCOL(combinations))){
  pair = combinations[,i]
  rhos1 <- {}
  rhos2 <- {}
  for(tp in c(-12:6)){
        out1 <- ccm(data, target_column = pair[1], lib_column = pair[2], num_samples = 1, 
                    lib_sizes = 14250, exclusion_radius = 100, E = 20, tp = tp, tau = 1)
        
        out2 <- ccm(data, target_column = pair[2], lib_column = pair[1], num_samples = 1, 
                    lib_sizes = 14250, exclusion_radius = 100, E = 20, tp = tp, tau = 1)
        rho1 = out1$rho
        if(rho1 < 0){rho1 =0}
        rho2 = out2$rho
        if(rho2 < 0){rho2 =0}
        rhos1 <- c(rhos1, rho1)
        rhos2 <- c(rhos2, rho2)
        
      
        
        
  }
  plot(c(-12:6),rhos1, xlab = 'tp', ylab = 'Rho', 
       main = paste(titles[pair[1]],' and ',titles[pair[2]]), type = 'l', col = 'red', lwd = 4,
       ylim = c(-max(c(rhos1, rhos2))/5, max(c(rhos1, rhos2))))
  lines(c(-12:6),rhos2, xlab = 'tp', ylab = 'Rho',  type = 'l', col = 'blue', lwd = 4)
  abline(v = 0)
  legend(-10, 0, legend=c(paste(titles[pair[2]]," predicts ",titles[pair[1]]),paste(titles[pair[1]]," predicts ",titles[pair[2]])),
         col=c("red", "blue"), lty=1:1, cex=0.8)
  
  }
##############################################################################








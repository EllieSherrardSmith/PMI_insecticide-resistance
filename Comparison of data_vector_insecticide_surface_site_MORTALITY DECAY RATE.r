##########################################################
##
##  Are there differences in the insecticide spray
##   on surface type
##   on region 
##   on mosquito species
##
##########################################################

## First load the data

data_eth   <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Ethiopia_cleaned.csv",header=TRUE)
data_ghana <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Ghana_cleaned.csv",header=TRUE)
data_mali  <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mali_cleaned.csv",header=TRUE)
data_mozo  <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mozambique_cleaned.csv",header=TRUE)
data_mada  <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Madagascar_cleaned.csv",header=TRUE)
data_zamb  <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Zambia_inferred from reports.csv",header=TRUE)


## Based on the preliminary plots using "Cleaning Functions Wall_cone_assays.R"
## Fit functions to answer the following questions:

## 1. Does wall surface impact the decay rate of the insecticide?

## a: Bendiocarb

species_eth      = levels(data_eth$Species_corrected)
site_eth         = levels(data_eth$Site_District_corrected)
wall_surface_eth = levels(data_eth$Wall_surface_corrected)
insecticide_eth  = levels(data_eth$Insecticide.Sprayed)

data_eth_Ag_bend <- subset(data_eth, data_eth$Species_corrected == species_eth[2] &
                             data_eth$Insecticide.Sprayed == insecticide_eth[2]) 

species_mali      = levels(data_mali$Species_corrected)
site_mali         = levels(data_mali$Site_District_corrected)
wall_surface_mali = levels(data_mali$Wall_surface_corrected)
insecticide_mali  = levels(data_mali$Insecticide.Sprayed)

data_mali_Ag_bend <- subset(data_mali, data_mali$Species_corrected == species_mali[1] &
                              data_mali$Insecticide.Sprayed == insecticide_mali[1]) 
data_mali_Ag_piri <- subset(data_mali, data_mali$Species_corrected == species_mali[1] &
                              data_mali$Insecticide.Sprayed == insecticide_mali[2]) 

species_mada      = levels(data_mada$Species_corrected)
site_mada         = levels(data_mada$Site_District_corrected)
wall_surface_mada = levels(data_mada$Wall_surface_corrected)
insecticide_mada  = levels(data_mada$Insecticide.Sprayed)

data_mada_Ag_bend <- subset(data_mada, data_mada$Species_corrected == species_mada[2] &
                              data_mada$Insecticide.Sprayed == insecticide_mada[1]) 


## ETHIOPIA

dat_temp <- data.frame(c(data_eth_Ag_bend$mortality[data_eth_Ag_bend$Site_District_corrected == site_eth[2] &
                                                      data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]]),
                       c(data_eth_Ag_bend$diff_in_days[data_eth_Ag_bend$Site_District_corrected == site_eth[2] &
                                                         data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]]))
names(dat_temp) <- c("mort","time")
head(dat_temp)
dat_eth1 <- dat_temp[order(dat_temp$time),] 
ETH_site1 <- list(N = 9,
                  y = dat_eth1$mort/100,
                  x = dat_eth1$time)


test_data_eth1 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                       data=ETH_site1, 
                       sample_file = "test_data_eth1_output.csv",
                       iter=1000, chains=4)
print(test_data_eth1)
parm = extract(test_data_eth1);names(parm)
traceplot(test_data_eth1, inc_warmup = TRUE)
nc = seq(0,400,1)

test_data_eth1_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

dat_temp <- data.frame(c(data_eth_Ag_bend$mortality[data_eth_Ag_bend$Site_District_corrected == site_eth[4] &
                               data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]]),
                       c(data_eth_Ag_bend$diff_in_days[data_eth_Ag_bend$Site_District_corrected == site_eth[4] &
                                                         data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]]))
names(dat_temp) <- c("mort","time")
head(dat_temp)
dat_eth2 <- dat_temp[order(dat_temp$time),] 
ETH_site2 <- list(N = 25,
                  y = dat_eth2$mort/100,
                  x = dat_eth2$time)

test_data_eth2 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                       data=ETH_site2,
                       sample_file = "test_data_eth2_output.csv",
                       iter=1000, chains=4)
print(test_data_eth2)
parm2 = extract(test_data_eth2);names(parm2)
traceplot(test_data_eth2, inc_warmup = TRUE)
nc = seq(0,400,1)

test_data_eth2_pred  <- (1 / (1 + mean(parm2$alpha) * exp(mean(parm2$beta) * nc)))

dat_temp <- data.frame(c(data_eth_Ag_bend$mortality[data_eth_Ag_bend$Site_District_corrected == site_eth[6] &
                                                      data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]]),
                       c(data_eth_Ag_bend$diff_in_days[data_eth_Ag_bend$Site_District_corrected == site_eth[6] &
                                                         data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]]))
names(dat_temp) <- c("mort","time")
head(dat_temp)
dat_eth3 <- dat_temp[order(dat_temp$time),] 
ETH_site3 <- list(N = 34,
                  y = dat_eth3$mort/100,
                  x = dat_eth3$time)

test_data_eth3 <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                       data=ETH_site3,
                       sample_file = "test_data_eth3_output.csv",
                       iter=1000, chains=4)
print(test_data_eth3)
parm3 = extract(test_data_eth3);names(parm3)
traceplot(test_data_eth3, inc_warmup = TRUE)
nc = seq(0,400,1)

test_data_eth3_pred  <- (1 / (1 + mean(parm3$alpha) * exp(mean(parm3$beta) * nc)))

par(mfrow=c(2,2))
title_obj = list("ETHIOPIA",species_eth[2], wall_surface_eth[4],insecticide_eth[2])

plot(data_eth_Ag_bend$mortality[data_eth_Ag_bend$Site_District_corrected == site_eth[2] &
                                  data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]] ~ 
       data_eth_Ag_bend$diff_in_days[data_eth_Ag_bend$Site_District_corrected == site_eth[2] &
                                       data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]], 
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
lines(test_data_eth1_pred*100 ~ nc)

points(data_eth_Ag_bend$mortality[data_eth_Ag_bend$Site_District_corrected == site_eth[4] &
                                    data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]] ~ 
         data_eth_Ag_bend$diff_in_days[data_eth_Ag_bend$Site_District_corrected == site_eth[4] &
                                         data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]],
       pch=2)
lines(test_data_eth2_pred*100 ~ nc,lty=2)

points(data_eth_Ag_bend$mortality[data_eth_Ag_bend$Site_District_corrected == site_eth[6] &
                                    data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]] ~ 
         data_eth_Ag_bend$diff_in_days[data_eth_Ag_bend$Site_District_corrected == site_eth[6] &
                                         data_eth_Ag_bend$Wall_surface_corrected == wall_surface_eth[4]],
       pch=22)
lines(test_data_eth3_pred*100 ~ nc,lty=3)

legend(150,40,legend=c("Bako Tibe","Gobu Sayo","Kersa"),
       lty=c(1,2,3),cex=1.1,pch=c(1,2,22))
## MADAGASCAR
data_mada_Ag_bend


cleaner_func <- function(datbase,wall_surface,num1,site,num2){
  dat_temp <- data.frame(datbase$mortality[datbase$Wall_surface_corrected == wall_surface[num1] &
                           datbase$Site_District_corrected == site[num2]],
                         datbase$diff_in_days[datbase$Wall_surface_corrected == wall_surface[num1] &
                                             datbase$Site_District_corrected == site[num2]])
  names(dat_temp) <- c("mort","time")
  data_out <- dat_temp[order(dat_temp$time),]

  COUNTRY_site <- list(N = nrow(data_out),
                    y = data_out$mort/100,
                    x = data_out$time)
  return(COUNTRY_site)  
}

mada1 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,1)
mada2 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,2)
mada6 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,6)
mada7 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,7)
mada11 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,11)
mada13 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,13)

test_mada <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                       data=mada1,
                       #sample_file = "mada1_output.csv",
                       iter=1000, chains=4)
print(test_mada)
parm = extract(test_mada);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_mada1_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada2_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada6_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada7_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada11_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada13_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

title_obj = list("MADAGASCAR",species_mada[2], wall_surface_mada[3],insecticide_mada[1])

plot(mada2$y*100~mada2$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(mada1$y*100~mada1$x,pch=1)
points(mada6$y*100~mada6$x,pch=22)
points(mada7$y*100~mada7$x,pch=6)
points(mada11$y*100~mada11$x,pch=5)
points(mada13$y*100~mada13$x,pch=18)
lines(test_mada1_pred*100)
lines(test_mada2_pred*100,lty=2)
lines(test_mada6_pred*100,lty=3)
lines(test_mada7_pred*100,lty=4)
lines(test_mada11_pred*100,lty=5)
lines(test_mada13_pred*100,lty=6)

legend(10,55,legend=c("Ambatofinandrahana","Amboasary Sud","Ankazobe","Betafo","Fianarantsoa II","Tsaratanana"),
       lty=c(1,2,3,4,5,6),cex=1.1,pch=c(1,2,22,6,5,18))
## MALI

mali1 <- cleaner_func(data_mali_Ag_bend,wall_surface_mali,6,site_mali,2)
mali2 <- cleaner_func(data_mali_Ag_bend,wall_surface_mali,6,site_mali,3)
mali3 <- cleaner_func(data_mali_Ag_bend,wall_surface_mali,6,site_mali,4)

cleaner_func2 <- function(datbase){
  dat_temp <- data.frame(datbase$mortality,
                         datbase$diff_in_days)
  names(dat_temp) <- c("mort","time")
  data_out <- dat_temp[order(dat_temp$time),]
  
  COUNTRY_site <- list(N = nrow(data_out),
                       y = data_out$mort/100,
                       x = data_out$time)
  return(COUNTRY_site)  
}
mali1p <- cleaner_func2(data_mali_Ag_piri)


mali_all <- list(N=c(mali1$N + mali2$N + mali3$N),
                 y = c(mali1$y,mali2$y, mali3$y),
                 x = c(mali1$x,mali2$x, mali3$x))

test_mali <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                  data=mali_all,
                  #sample_file = "mada1_output.csv",
                  iter=1000, chains=4)
print(test_mali)
parm = extract(test_mali);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
#test_mali1_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
#test_mali2_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
#test_mali3_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mali_all_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_mali_p <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                  data=mali1p,
                  #sample_file = "mada1_output.csv",
                  iter=1000, chains=4)
print(test_mali_p)
parm = extract(test_mali_p);names(parm)
test_mali_all_pred_p <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

title_obj = list("MALI",species_mali[1])

plot(mali_all$y*100~mali_all$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]])))
#points(mali_all$y*100~mali_all$x,pch=1)
points(mali1p$y*100~mali1p$x,pch=22,col="blue")

#lines(test_mali1_pred*100~nc)
lines(test_mali_all_pred_p*100~nc,lty=2,col="blue")
lines(test_mali_all_pred*100~nc,lty=2,lwd=2)
text(150,15,"Bendiocarb")
text(150,85,"Pirimiphos methyl",col="blue")


## ETHIOPIA with An. arabiensis
data_eth_Aa_bend <- subset(data_eth, data_eth$Species_corrected == species_eth[1] &
                             data_eth$Insecticide.Sprayed == insecticide_eth[2])

eth_aa <- cleaner_func(data_eth_Aa_bend,wall_surface_eth,4,site_eth,6)

test_ethaa <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                  data=eth_aa,
                  #sample_file = "mada1_output.csv",
                  iter=1000, chains=4)
print(test_ethaa)
parm = extract(test_ethaa);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_ethaa_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

title_obj = list("ETHIOPIA",species_eth[1], wall_surface_eth[4],insecticide_eth[2])

plot(eth_aa$y*100~eth_aa$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
lines(test_ethaa_pred*100~nc,lty=2,col="blue",lwd=2)
text(150,90,"Kersa",col="blue",cex=1.1)
title(main=print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])), col.main="blue")

###############################################################
##
##
## Can we find differences in the surface used?
##    
##
################################################################

##Madagascar
species_mada      = levels(data_mada$Species_corrected)
site_mada         = levels(data_mada$Site_District_corrected)
wall_surface_mada = levels(data_mada$Wall_surface_corrected)
insecticide_mada  = levels(data_mada$Insecticide.Sprayed)

data_mada_Ag_bend <- subset(data_mada, data_mada$Species_corrected == species_mada[2] &
                              data_mada$Insecticide.Sprayed == insecticide_mada[1]) 

data_mada_Ag_piri <- subset(data_mada, data_mada$Species_corrected == species_mada[2] &
                              data_mada$Insecticide.Sprayed == insecticide_mada[3]) 

##Bendiocarb on Mud
mada1 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,1)
mada2 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,2)
mada6 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,6)
mada7 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,7)
mada11 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,11)
mada13 <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,3,site_mada,13)

##Bendiocarb on Wood
mada1w <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,6,site_mada,1)
mada2w <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,6,site_mada,2)
mada6w <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,6,site_mada,6)
mada7w <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,6,site_mada,7)
mada11w <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,6,site_mada,11)
mada13w <- cleaner_func(data_mada_Ag_bend,wall_surface_mada,6,site_mada,13)

##Pirimiphos on Mud
madaP4m <- cleaner_func(data_mada_Ag_piri,wall_surface_mada,3,site_mada,4)

##Pirimiphos on Wood
madaP4w <- cleaner_func(data_mada_Ag_piri,wall_surface_mada,6,site_mada,4)
madaP8w <- cleaner_func(data_mada_Ag_piri,wall_surface_mada,6,site_mada,8)

test_mada <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                  data=madaP8w,
                  #sample_file = "mada1_output.csv",
                  iter=1000, chains=4)
print(test_mada)
parm = extract(test_mada);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_mada1_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada2_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada6_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada7_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada11_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada13_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))


test_mada1_predw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada2_predw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada6_predw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada7_predw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada11_predw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada13_predw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

#test_mada4m_predP  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
#test_mada4w_predP  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mada8w_predP  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

par(mfrow=c(2,2))
title_obj = list("MADAGASCAR",species_mada[2], wall_surface_mada[3],insecticide_mada[1])

plot(mada2$y*100~mada2$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(mada1$y*100~mada1$x,pch=1)
points(mada6$y*100~mada6$x,pch=22)
points(mada7$y*100~mada7$x,pch=6)
points(mada11$y*100~mada11$x,pch=5)
points(mada13$y*100~mada13$x,pch=18)
lines(test_mada1_pred*100)
lines(test_mada2_pred*100,lty=2)
lines(test_mada6_pred*100,lty=3)
lines(test_mada7_pred*100,lty=4)
lines(test_mada11_pred*100,lty=5)
lines(test_mada13_pred*100,lty=6)

legend(10,55,legend=c("Ambatofinandrahana","Amboasary Sud","Ankazobe","Betafo","Fianarantsoa II","Tsaratanana"),
       lty=c(1,2,3,4,5,6),cex=1.1,pch=c(1,2,22,6,5,18))

title_obj = list("MADAGASCAR",species_mada[2], wall_surface_mada[6],insecticide_mada[1])

plot(mada2w$y*100~mada2w$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(mada1w$y*100~mada1w$x,pch=1)
points(mada6w$y*100~mada6w$x,pch=22)
points(mada7w$y*100~mada7w$x,pch=6)
points(mada11w$y*100~mada11w$x,pch=5)
points(mada13w$y*100~mada13w$x,pch=18)
lines(test_mada1_predw*100)
lines(test_mada2_predw*100,lty=2)
lines(test_mada6_predw*100,lty=3)
lines(test_mada7_predw*100,lty=4)
lines(test_mada11_predw*100,lty=5)
lines(test_mada13_predw*100,lty=6)

legend(10,55,legend=c("Ambatofinandrahana","Amboasary Sud","Ankazobe","Betafo","Fianarantsoa II","Tsaratanana"),
       lty=c(1,2,3,4,5,6),cex=1.1,pch=c(1,2,22,6,5,18))

title_obj = list("MADAGASCAR",species_mada[2], wall_surface_mada[3],insecticide_mada[3])

plot(madaP4m$y*100~madaP4m$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
legend(10,55,legend=c("Ambovombe"),
       cex=1.1,pch=2)

title_obj = list("MADAGASCAR",species_mada[2], wall_surface_mada[6],insecticide_mada[3])
plot(madaP4w$y*100~madaP4w$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(madaP8w$y*100~madaP8w$x,pch=1)
lines(test_mada8w_predP*100~nc,lty=1)

legend(10,55,legend=c("Ambovombe","Brickaville"),
       lty=c(NA,1),cex=1.1,pch=c(2,1))


##Ghana

species_gha      = levels(data_ghana$Species_corrected)
site_gha         = levels(data_ghana$Site_District_corrected)
wall_surface_gha = levels(data_ghana$Wall_surface_corrected)
insecticide_gha  = levels(data_ghana$Insecticide.Sprayed)

data_gha_Ag_piri <- subset(data_ghana, data_ghana$Species_corrected == species_gha[1] &
                             data_ghana$Insecticide.Sprayed == insecticide_gha[2]) 

data_gha_AgK_piri <- subset(data_ghana, data_ghana$Species_corrected == species_gha[3] &
                             data_ghana$Insecticide.Sprayed == insecticide_gha[2]) 

data_gha_AgK_alph <- subset(data_ghana, data_ghana$Species_corrected == species_gha[3] &
                             data_ghana$Insecticide.Sprayed == insecticide_gha[1]) 

##Pirimiphos An gambiae s.l. on Mud
gha1m <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,2,site_gha,1)
gha3m <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,2,site_gha,3)
gha4m <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,2,site_gha,4)


##Pirimiphos An gambiae s.l. on Wood
gha1w <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,3,site_gha,1)
gha3w <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,3,site_gha,3)
gha4w <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,3,site_gha,4)

##Pirimiphos An gambiae s.l. on Cement
gha1c <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,1,site_gha,1)
gha3c <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,1,site_gha,3)
gha4c <- cleaner_func(data_gha_Ag_piri,wall_surface_gha,1,site_gha,4)

test_ghana <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                  data=gha4c,
                  #sample_file = "mada1_output.csv",
                  iter=1000, chains=4)
print(test_ghana)
parm = extract(test_ghana);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_ghana_pred1m  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred3m  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred4m  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_ghana_pred1w  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred3w  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred4w  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_ghana_pred1c  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred3c  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred4c  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))


par(mfrow=c(3,3))
title_obj = list("GHANA",species_gha[1], wall_surface_gha[2],insecticide_gha[2])

plot(gha1m$y*100~gha1m$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(gha3m$y*100~gha3m$x,pch=1)
points(gha4m$y*100~gha4m$x,pch=22)
lines(test_ghana_pred1m*100 ~ nc)
lines(test_ghana_pred3m*100 ~ nc,lty=2)
lines(test_ghana_pred4m*100 ~ nc,lty=3)

title_obj = list("GHANA",species_gha[1], wall_surface_gha[3],insecticide_gha[2])

plot(gha1w$y*100~gha1w$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(gha3w$y*100~gha3w$x,pch=1)
points(gha4w$y*100~gha4w$x,pch=22)
lines(test_ghana_pred1w*100 ~ nc)
lines(test_ghana_pred3w*100 ~ nc,lty=2)
lines(test_ghana_pred4w*100 ~ nc,lty=3)

title_obj = list("GHANA",species_gha[1], wall_surface_gha[1],insecticide_gha[2])

plot(gha1c$y*100~gha1c$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(gha3c$y*100~gha3c$x,pch=1)
points(gha4c$y*100~gha4c$x,pch=22)
lines(test_ghana_pred1c*100 ~ nc)
lines(test_ghana_pred3c*100 ~ nc,lty=2)
lines(test_ghana_pred4c*100 ~ nc,lty=3)

#
##
#

##Pirimiphos An gambiae Kisumu strain on Mud
kgha1m <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,2,site_gha,1)
kgha3m <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,2,site_gha,3)
kgha4m <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,2,site_gha,4)


##Pirimiphos An gambiae Kisumu strain on Wood
kgha1w <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,3,site_gha,1)
kgha1w <- list(N = length(c(kgha1w$N[1:145],kgha1w$N[147:170])),
               y = c(kgha1w$y[1:145],kgha1w$y[147:170]),
               x = c(kgha1w$x[1:145],kgha1w$x[147:170]))
kgha3w <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,3,site_gha,3)
kgha4w <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,3,site_gha,4)

##Pirimiphos An gambiae Kisumu strain on Cement
kgha1c <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,1,site_gha,1)
kgha3c <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,1,site_gha,3)
kgha4c <- cleaner_func(data_gha_AgK_piri,wall_surface_gha,1,site_gha,4)

test_ghana <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                   data=kgha4c,
                   #sample_file = "mada1_output.csv",
                   iter=1000, chains=4)
print(test_ghana)
parm = extract(test_ghana);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_ghana_pred1mk  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred3mk  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred4mk  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_ghana_pred1wk  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
#test_ghana_pred3wk  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred4wk  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_ghana_pred1ck  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred3ck  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred4ck  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

title_obj = list("GHANA",species_gha[3], wall_surface_gha[2],insecticide_gha[2])

plot(kgha1m$y*100~kgha1m$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(kgha3m$y*100~kgha3m$x,pch=1)
points(kgha4m$y*100~kgha4m$x,pch=22)
lines(test_ghana_pred1mk*100 ~ nc)
lines(test_ghana_pred3mk*100 ~ nc,lty=2)
lines(test_ghana_pred4mk*100 ~ nc,lty=3)

title_obj = list("GHANA",species_gha[3], wall_surface_gha[3],insecticide_gha[2])

plot(kgha1w$y*100~kgha1w$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(kgha3w$y*100~kgha3w$x,pch=1)
points(kgha4w$y*100~kgha4w$x,pch=22)
lines(test_ghana_pred1wk*100 ~ nc)
#lines(test_ghana_pred3wk*100 ~ nc,lty=2)
lines(test_ghana_pred4wk*100 ~ nc,lty=3)

title_obj = list("GHANA",species_gha[3], wall_surface_gha[1],insecticide_gha[2])

plot(kgha1c$y*100~kgha1c$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(kgha3c$y*100~kgha3c$x,pch=1)
points(kgha4c$y*100~kgha4c$x,pch=22)
lines(test_ghana_pred1ck*100 ~ nc)
lines(test_ghana_pred3ck*100 ~ nc,lty=2)
lines(test_ghana_pred4ck*100 ~ nc,lty=3)

legend(10,60,legend=c("BYD","KD","SND"),
       lty=c(1,2,3),pch=c(1,2,22),cex=1.2)

#
##
#

##Alphercypermethrin An gambiae Kisumu strain on Mud
Agha1m <- cleaner_func(data_gha_AgK_alph,wall_surface_gha,2,site_gha,1)
Agha5m <- cleaner_func(data_gha_AgK_alph,wall_surface_gha,2,site_gha,5)


##Pirimiphos An gambiae s.l. on Wood
Agha1w <- cleaner_func(data_gha_AgK_alph,wall_surface_gha,3,site_gha,1)
Agha5w <- cleaner_func(data_gha_AgK_alph,wall_surface_gha,3,site_gha,5)

##Pirimiphos An gambiae s.l. on Cement
Agha1c <- cleaner_func(data_gha_AgK_alph,wall_surface_gha,1,site_gha,1)
Agha5c <- cleaner_func(data_gha_AgK_alph,wall_surface_gha,1,site_gha,5)

test_ghana <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                   data=Agha5c,
                   #sample_file = "mada1_output.csv",
                   iter=1000, chains=4)
print(test_ghana)
parm = extract(test_ghana);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_ghana_pred1mkAm  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred5mkAm  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_ghana_pred1mkAw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred5mkAw  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

test_ghana_pred1mkAc  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_ghana_pred5mkAc  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

title_obj = list("GHANA",species_gha[3], wall_surface_gha[2],insecticide_gha[1])

plot(Agha1m$y*100~Agha1m$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(Agha5m$y*100~Agha5m$x,pch=1)
lines(test_ghana_pred1mkAm*100 ~ nc)
lines(test_ghana_pred5mkAm*100 ~ nc,lty=2)

title_obj = list("GHANA",species_gha[3], wall_surface_gha[3],insecticide_gha[1])

plot(Agha1w$y*100~Agha1w$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(Agha5w$y*100~Agha5w$x,pch=1)
lines(test_ghana_pred1mkAw*100 ~ nc)
lines(test_ghana_pred5mkAw*100 ~ nc,lty=3)

title_obj = list("GHANA",species_gha[3], wall_surface_gha[1],insecticide_gha[1])

plot(Agha1c$y*100~Agha1c$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]],title_obj[[4]])))
points(Agha5c$y*100~Agha5c$x,pch=1)
lines(test_ghana_pred1mkAc*100 ~ nc)
lines(test_ghana_pred5mkAc*100 ~ nc,lty=2)

legend(10,60,legend=c("BYD","TKD"),
       lty=c(1,2),pch=c(1,2),cex=1.2)


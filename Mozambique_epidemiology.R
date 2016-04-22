#######################################
##
## Epi Data Mozambique
## Source: Elana Fiekowsky, March 2016, Health Facility Data
##
#######################################

library(rstan)
library(MASS)
library(boot)
library(coda)
library(R2OpenBUGS)
library(ggplot2)
library(reshape2)
library(adegenet)
library(plotrix)
library(lubridate)

###############
##
## EPI Data
###############

mozo_epi <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Epidemiological data\\Mozambique_csv.csv",header=TRUE)
mozo_epi$month_count <- ifelse(mozo_epi$month == "Janeiro", 1, 
                               ifelse(mozo_epi$month == "Fevereiro", 2,
                                      ifelse(mozo_epi$month == "Marco", 3,
                                             ifelse(mozo_epi$month == "Abril", 4,
                                                    ifelse(mozo_epi$month == "Maio", 5,
                                                           ifelse(mozo_epi$month == "Junho", 6,
                                                                  ifelse(mozo_epi$month == "Julho", 7,
                                                                         ifelse(mozo_epi$month == "Agosto", 8,
                                                                                ifelse(mozo_epi$month == "Setembro", 9,
                                                                                       ifelse(mozo_epi$month == "Outubro", 10,
                                                                                              ifelse(mozo_epi$month == "Novembro", 11, 12))))))))))) 

mozo_epi$time_series <- ifelse(mozo_epi$year == "2013", 0+mozo_epi$month_count,
                            ifelse(mozo_epi$year == "2014", 12+mozo_epi$month_count,24+mozo_epi$month_count))

mozo_epi <- mozo_epi[order(mozo_epi$time_series), ]
levels(mozo_epi$district)

par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
plot(mozo_epi$confirmed_cases_total ~ 
       mozo_epi$time_series,pch=NA,
     ylab = "Total Confirmed Malaria Cases per all tests performed",ylim=c(0,1),las=2,
     xlab = "Time (months)",xaxt="n",cex.lab=1.4,xlim=c(0,36),
     frame=FALSE)
axis(1,las=0,at=c(2,10,22,34),labels=c("Feb 2013","Oct 2013","Oct 2014","Oct 2015"))
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Maganja "]/mozo_epi$Total_tests_all[mozo_epi$district == "Maganja "] ~
        mozo_epi$time_series[mozo_epi$district == "Maganja "], lty=2,lwd=2)
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Mocuba"]/mozo_epi$Total_tests_all[mozo_epi$district == "Mocuba"] ~
        mozo_epi$time_series[mozo_epi$district == "Mocuba"], lty=2,lwd=2,col="red")
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Morrumbala"]/mozo_epi$Total_tests_all[mozo_epi$district == "Morrumbala"] ~
        mozo_epi$time_series[mozo_epi$district == "Morrumbala"], lty=2,lwd=2,col="blue")
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Milange"]/mozo_epi$Total_tests_all[mozo_epi$district == "Milange"] ~
        mozo_epi$time_series[mozo_epi$district == "Milange"], lty=2,lwd=2,col="darkgreen")
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Nicoadala"]/mozo_epi$Total_tests_all[mozo_epi$district == "Nicoadala"] ~
        mozo_epi$time_series[mozo_epi$district == "Nicoadala"], lty=2,lwd=2,col="grey")


legend(24,0.4,legend=c("Maganja (Control)", "Mocuba (IRS)","Morrumbala (IRS)", "Milange (IRS)", "Nicoadala"),
       col=c("black","red","blue","darkgreen","grey"),lty=2,lwd=2,bty="n")


par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
plot(mozo_epi$confirmed_cases_total ~ 
       mozo_epi$time_series,pch=NA,
     ylab = "Proportion Confirmed Malaria Cases",ylim=c(0,0.15),las=2,
     xlab = "Time (months)",xaxt="n",cex.lab=1.4,xlim=c(0,36),
     frame=FALSE)
axis(1,las=0,at=c(2,10,22,34),labels=c("Feb 2013","Oct 2013","Oct 2014","Oct 2015"))
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Maganja "]/ifelse(mozo_epi$time_series[mozo_epi$district == "Maganja "] < 13,306287,
                                                                             ifelse(mozo_epi$time_series[mozo_epi$district == "Maganja "] < 24 & 
                                                                                      mozo_epi$time_series[mozo_epi$district == "Maganja "] > 12, 310000,312000)) ~
        mozo_epi$time_series[mozo_epi$district == "Maganja "], lty=2,lwd=2)
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Mocuba"]/ifelse(mozo_epi$time_series[mozo_epi$district == "Mocuba"] < 13,365707,
                                                                           ifelse(mozo_epi$time_series[mozo_epi$district == "Mocuba"] < 24 & 
                                                                                    mozo_epi$time_series[mozo_epi$district == "Mocuba"] > 12, 375000,382000))  ~
        mozo_epi$time_series[mozo_epi$district == "Mocuba"], lty=2,lwd=2,col="red")
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Morrumbala"]/ifelse(mozo_epi$time_series[mozo_epi$district == "Morrumbala"] < 13,434696,
                                                                               ifelse(mozo_epi$time_series[mozo_epi$district == "Morrumbala"] < 24 & 
                                                                                        mozo_epi$time_series[mozo_epi$district == "Morrumbala"] > 12, 438000,440000))  ~
        mozo_epi$time_series[mozo_epi$district == "Morrumbala"], lty=2,lwd=2,col="blue")
lines(mozo_epi$confirmed_cases_total[mozo_epi$district == "Milange"]/ifelse(mozo_epi$time_series[mozo_epi$district == "Milange"] < 13,606499,
                                                                            ifelse(mozo_epi$time_series[mozo_epi$district == "Milange"] < 24 & 
                                                                                     mozo_epi$time_series[mozo_epi$district == "Milange"] > 12, 608000,610000))  ~
        mozo_epi$time_series[mozo_epi$district == "Milange"], lty=2,lwd=2,col="darkgreen")

legend(1,0.15,legend=c("Maganja (Control)", "Mocuba (IRS)","Morrumbala (IRS)", "Milange (IRS)"),
       col=c("black","red","blue","green"),lty=2,lwd=2,bty="n")

############
##
## Ento data, 1. Decay rates
##
##############

data_mozo  <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Abt Associates_susceptibility and decay rates\\Wall Bio_Assay Mozambique_cleaned.csv",header=TRUE)

species_mozo      = levels(data_mozo$Species_corrected)
site_mozo         = levels(data_mozo$Site_District_corrected)
wall_surface_mozo = levels(data_mozo$Wall_surface_corrected)
insecticide_mozo  = levels(data_mozo$Insecticide.Sprayed)

data_mozo_Ag_bend <- subset(data_mozo, data_mozo$Species_corrected == species_mozo[2] &
                              data_mozo$Insecticide.Sprayed == insecticide_mozo[2]) 

cleaner_func3 <- function(datbase,site,num2){
  dat_temp <- data.frame(datbase$mortality[datbase$Site_District_corrected == site[num2]],
                         datbase$diff_in_days[datbase$Site_District_corrected == site[num2]])
  names(dat_temp) <- c("mort","time")
  data_out <- dat_temp[order(dat_temp$time),]
  
  COUNTRY_site <- list(N = nrow(data_out),
                       y = data_out$mort/100,
                       x = data_out$time)
  return(COUNTRY_site)  
}

milange <- cleaner_func3(data_mozo_Ag_bend,site_mozo,1)
mocuba <- cleaner_func3(data_mozo_Ag_bend,site_mozo,2)
morrumb <- cleaner_func3(data_mozo_Ag_bend,site_mozo,3)
nicoad <- cleaner_func3(data_mozo_Ag_bend,site_mozo,4)

mocuba_new <- list(N = mocuba$N-1,
                   y = c(mocuba$y[1:27],mocuba$y[29:40]),
                   x = c(mocuba$x[1:27],mocuba$x[29:40]))

test_mozo <- stan(file="C:\\Users\\Ellie\\Documents\\RStudioProjects\\PMI_insecticide-resistance\\logist_stan_decays.stan", 
                  data=nicoad,
                  #sample_file = "mada1_output.csv",
                  iter=1000, chains=4)
print(test_mozo)
parm = extract(test_mozo);names(parm)
#traceplot(test_mada1, inc_warmup = TRUE)
nc = seq(0,400,1)
test_mil_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_moc_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_mor_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))
test_nic_pred  <- (1 / (1 + mean(parm$alpha) * exp(mean(parm$beta) * nc)))

title_obj = list("MOZAMBIQUE",species_mozo[2], insecticide_mozo[2])

plot(milange$y*100~milange$x,
     xlim = c(0,200), ylim = c(0,100),frame=FALSE,
     xlab = "Time since spraying (days)",
     ylab = "Wall Cone Bio-assay Mortality rate %",lty=2,pch=2,
     main = print(c(title_obj[[1]],title_obj[[2]],title_obj[[3]])))
lines(test_mil_pred*100~nc,lty=2,col="darkgreen",lwd=2)
points(milange$y*100~milange$x,pch=2,col="darkgreen")
text(30,70,"Milange",col="darkgreen",cex=1.1)

lines(test_moc_pred*100~nc,lty=2,col="red",lwd=2)
points(mocuba$y*100~mocuba$x,pch=4,col="red")
text(30,65,"Mocuba",col="red",cex=1.1)

lines(test_mor_pred*100~nc,lty=2,col="blue",lwd=2)
points(morrumb$y*100~morrumb$x,pch=5,col="blue")
text(30,60,"Morrumbala",col="blue",cex=1.1)

lines(test_nic_pred*100~nc,lty=2,col="purple",lwd=2)
points(nicoad$y*100~nicoad$x,col="purple",pch=6)
text(30,55,"Nicaodala",col="purple",cex=1.1)

########################
##
## Susceptibility dta
##
#########################
mozo_sus <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Per Country Data\\Mozambique_Susceptibility.csv",header=TRUE)
mozo_sus$mort <- mozo_sus$"Calculated..average.mortality.adjusted.for.control......1"
levels(mozo_sus$Species.tested)[1] <- "An. funestus s.l."


boxplot(c(mozo_sus$mort[mozo_sus$Year == "2013" & mozo_sus$chem_type == "Pyrethroids" & 
                mozo_sus$"Commune.3rd.admin.level" == "Zambezia"]),
        c(mozo_sus$mort[mozo_sus$Year == "2013" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" != "Zambezia"]),
        c(mozo_sus$mort[mozo_sus$Year == "2014" & mozo_sus$chem_type == "Pyrethroids" & 
                mozo_sus$"Commune.3rd.admin.level" == "Zambezia"]),
        c(mozo_sus$mort[mozo_sus$Year == "2014" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" != "Zambezia"]),
        c(mozo_sus$mort[mozo_sus$Year == "2015" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" == "Zambezia"]),
        c(mozo_sus$mort[mozo_sus$Year == "2015" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" != "Zambezia"]),
        xaxt="n",frame=FALSE,col=c("red","blue"),ylab="Mosquito Mortality",xlab="Year",cex.lab=1.4,
        ylim=c(0,1),main = "Susceptibility to Pyrethroids")
axis(1,at=c(1.5,3.5,5.5),labels=c("2013","2014","2014"))
legend(1,0.4,legend = c("Zambezia (IRS Region)", "Outside Zambezia"),
       col=c("red","blue"),pch=15,cex=1.4,bty="n")

par(mfrow=c(1,2))
for(i in 1:2){
  boxplot(c(mozo_sus$mort[mozo_sus$Year == "2013" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" == "Zambezia" &
                          mozo_sus$Species.tested == levels(mozo_sus$Species.tested)[i]]),
        c(mozo_sus$mort[mozo_sus$Year == "2013" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" != "Zambezia" &
                          mozo_sus$Species.tested == levels(mozo_sus$Species.tested)[i]]),
        c(mozo_sus$mort[mozo_sus$Year == "2014" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" == "Zambezia" &
                          mozo_sus$Species.tested == levels(mozo_sus$Species.tested)[i]]),
        c(mozo_sus$mort[mozo_sus$Year == "2014" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" != "Zambezia" &
                          mozo_sus$Species.tested == levels(mozo_sus$Species.tested)[i]]),
        c(mozo_sus$mort[mozo_sus$Year == "2015" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" == "Zambezia" &
                          mozo_sus$Species.tested == levels(mozo_sus$Species.tested)[i]]),
        c(mozo_sus$mort[mozo_sus$Year == "2015" & mozo_sus$chem_type == "Pyrethroids" & 
                          mozo_sus$"Commune.3rd.admin.level" != "Zambezia" &
                          mozo_sus$Species.tested == levels(mozo_sus$Species.tested)[i]]),
        xaxt="n",frame=FALSE,col=c("red","blue"),ylab="Mosquito Mortality",xlab="Year",cex.lab=1.4,
        ylim=c(0,1),main = "Susceptibility to Pyrethroids")
  axis(1,at=c(1.5,3.5,5.5),labels=c("2013","2014","2014"))
  legend(1,0.4,legend = c("Zambezia (IRS Region)", "Outside Zambezia"),
       col=c("red","blue"),pch=15,cex=1.4,bty="n")
  text(2,0.1,print(levels(mozo_sus$Species.tested)[i]))
  
}
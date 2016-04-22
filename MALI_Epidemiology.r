#######################################
##
## Epi Data Mali
## Source: Diadier March 2016, Health Facility Data
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

###############
##
## EPI Data
###############

mali <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Epidemiological data\\Mali_Diadier_March 2016_to clean.csv",header=TRUE)
mali <- mali[order(mali$YEAR,mali$MONTH),]
mali$time_series <- ifelse(mali$YEAR == "2011", 0+mali$MONTH,
                           ifelse(mali$YEAR == "2012", 12+mali$MONTH,
                                  ifelse(mali$YEAR == "2013", 24+mali$MONTH,
                                         ifelse(mali$YEAR == "2014", 36+mali$MONTH,
                                                ifelse(mali$YEAR == "2015", 48+mali$MONTH,60+mali$MONTH)))))

mali_epi_Koul <- subset(mali,mali$DISTRICT == "KOULIKORO")
mali_epi_Bara <- subset(mali,mali$DISTRICT == "BARAOUELI")
mali_epi_Bla <- subset(mali,mali$DISTRICT == "BLA")
mali_epi_con_Zam <- subset(mali,mali$CSCOM == "ZAMBOUGOU CINZANA" |
                           mali$CSCOM == "ZAMBOUGOU CENTRAL")



plot(mali_epi_con_Zam$"pw_total_suspected_malaria_cases" ~ 
       mali_epi_con_Zam$time_series,pch=NA,
     ylab = "PW Total suspected malaria cases",ylim=c(0,30),las=2,
     xlab = "Time (months)",xaxt="n",cex.lab=1.4,
     frame=FALSE)
axis(1,las=0,at=c(13,25,37,49,61),labels=c("Jan 2012","Jan 2013","Jan 2014","Jan 2015","Jan 2016"))
lines(mali_epi_con_Zam$"pw_total_suspected_malaria_cases" ~ mali_epi_con_Zam$time_series)

lines(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "TIENFALA"] ~ 
        mali$time_series[mali$CSCOM == "TIENFALA"],col="blue",lty=2)
lines(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "KOLEBOUGOU"] ~ 
        mali$time_series[mali$CSCOM == "KOLEBOUGOU"],col="blue",lty=2)


lines(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "TIGUI"] ~ 
        mali$time_series[mali$CSCOM == "TIGUI"],col="red",lty=3)
lines(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "KONOBOUGOU"] ~ 
        mali$time_series[mali$CSCOM == "KONOBOUGOU"],col="red",lty=3)
lines(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "TOUNA"] ~ 
        mali$time_series[mali$CSCOM == "TOUNA"],col="green",lty=4)

sum_Bla <- sum_Baraoueli <- sum_Koulikoro <-
mean_Zambougou1 <- mean_Zambougou2 <- mean_Bla <- mean_Baraoueli <- mean_Koulikoro <- mean_Segou <-
  upp_Bla <- upp_Baraoueli <- upp_Koulikoro <- upp_Segou <-
  low_Bla <- low_Baraoueli <- low_Koulikoro <- low_Segou <- numeric(length(unique(mali$time_series)))
for(i in 1:length(unique(mali$time_series))){
  sum_Bla[i] <- sum(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BLA" & mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  sum_Baraoueli[i] <- sum(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BARAOUELI"& mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  sum_Koulikoro[i] <- sum(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "KOULIKORO"& mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  
  mean_Bla[i] <- mean(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BLA" & mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  mean_Baraoueli[i] <- mean(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BARAOUELI"& mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  mean_Koulikoro[i] <- mean(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "KOULIKORO"& mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  upp_Bla[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BLA" & mali$time_series==unique(mali$time_series)[i]],0.975,na.rm=TRUE)
  upp_Baraoueli[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BARAOUELI"& mali$time_series==unique(mali$time_series)[i]],0.975,na.rm=TRUE)
  upp_Koulikoro[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "KOULIKORO"& mali$time_series==unique(mali$time_series)[i]],0.975,na.rm=TRUE)
  low_Bla[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BLA" & mali$time_series==unique(mali$time_series)[i]],0.025,na.rm=TRUE)
  low_Baraoueli[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "BARAOUELI"& mali$time_series==unique(mali$time_series)[i]],0.025,na.rm=TRUE)
  low_Koulikoro[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "KOULIKORO"& mali$time_series==unique(mali$time_series)[i]],0.025,na.rm=TRUE)
  
  mean_Segou[i] <- mean(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "SEGOU" & mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  upp_Segou[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "SEGOU" & mali$time_series==unique(mali$time_series)[i]],0.975,na.rm=TRUE)
  low_Segou[i] <- quantile(mali$"pw_total_suspected_malaria_cases"[mali$DISTRICT == "SEGOU" & mali$time_series==unique(mali$time_series)[i]],0.025,na.rm=TRUE)

  mean_Zambougou1[i] <- mean(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "ZAMBOUGOU CINZANA" & mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
  mean_Zambougou2[i] <- mean(mali$"pw_total_suspected_malaria_cases"[mali$CSCOM == "ZAMBOUGOU CENTRAL" & mali$time_series==unique(mali$time_series)[i]],na.rm=TRUE)
}
mean_Zambougou <- numeric(length(unique(mali_epi_con_Zam$time_series)))
for (i in 1:length(unique(mali_epi_con_Zam$time_series))){
  mean_Zambougou[i] <- mean(mali_epi_con_Zam$"pw_total_suspected_malaria_cases"[mali_epi_con_Zam$time_series==unique(mali_epi_con_Zam$time_series)[i]],na.rm=TRUE)
}

par(mar=c(5,5,5,8))
par(mfrow=c(1,1))
plot(mali_epi_con_Zam$"pw_total_suspected_malaria_cases" ~ 
       mali_epi_con_Zam$time_series,pch=NA,
     ylab = "PW Total suspected malaria cases",ylim=c(0,50),las=2,
     xlab = "Time (months)",xaxt="n",cex.lab=1.4,
     frame=FALSE)
axis(1,las=0,at=c(13,25,37,49,61),labels=c("Jan 2012","Jan 2013","Jan 2014","Jan 2015","Jan 2016"))

lines(mean_Bla ~ unique(mali$time_series),col="red",lty=2,lwd=4)
lines(mean_Baraoueli ~ unique(mali$time_series),col="darkgreen",lty=2,lwd=4)
lines(mean_Koulikoro ~ unique(mali$time_series),col="blue",lty=2,lwd=4)
#lines(mean_Zambougou[1:47] ~ unique(mali_epi_con_Zam$time_series),lwd=4)
lines(mean_Segou ~ unique(mali$time_series),col="black",lty=2,lwd=4)

plot(mali_epi_con_Zam$"pw_total_suspected_malaria_cases" ~ 
       mali_epi_con_Zam$time_series,pch=NA,
     ylab = "PW Total suspected malaria cases",ylim=c(0,500),las=2,
     xlab = "Time (months)",xaxt="n",cex.lab=1.4,
     frame=FALSE)
axis(1,las=0,at=c(13,25,37,49,61),labels=c("Jan 2012","Jan 2013","Jan 2014","Jan 2015","Jan 2016"))
lines(sum_Bla ~ unique(mali$time_series),col="red",lty=2,lwd=4)
lines(sum_Baraoueli ~ unique(mali$time_series),col="darkgreen",lty=2,lwd=4)
lines(sum_Koulikoro ~ unique(mali$time_series),col="blue",lty=2,lwd=4)

#polygon(c(unique(mali$time_series), rev(unique(mali$time_series))),c(upp_Bla,rev(low_Bla)),border=NA, col=transp("red",alpha=0.3))
#polygon(c(unique(mali$time_series), rev(unique(mali$time_series))),c(upp_Baraoueli,rev(low_Baraoueli)),border=NA, col=transp("green",alpha=0.3))
#polygon(c(unique(mali$time_series), rev(unique(mali$time_series))),c(upp_Koulikoro,rev(low_Koulikoro)),border=NA, col=transp("blue",alpha=0.3))


#############################
##
## Ento data - Mortality rates for local mosquitoes
##
#############################
ento <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Per Country Data\\Mali_Susceptibility.CSV",header=TRUE)

Susc_to_IRS_used <- c(rep(NA,6),rep(mean(ento$mortality[ento$Year == "2012" & ento$"Insecticide.tested" == "Bendiocarb 0.1%"],na.rm=TRUE),12),
                      rep(mean(ento$mortality[ento$Year == "2013" & ento$"Insecticide.tested" == "Bendiocarb 0.1%"],na.rm=TRUE),12),
                      rep(mean(ento$mortality[ento$Year == "2014" & ento$"Insecticide.tested" == "pirimiphos methyl"],na.rm=TRUE),12),
                      rep(mean(ento$mortality[ento$Year == "2015" & ento$"Insecticide.tested" == "pirimiphos methyl"],na.rm=TRUE),12),NA)
                      
par(new=T)                      
plot(Susc_to_IRS_used ~ unique(mali$time_series),ylim=c(0,1),
     frame=FALSE,yaxt="n",xaxt="n",ylab="",xlab="",col="red",pch=20)
axis(4,las=2, at=seq(0,1,0.2),labels=seq(0,1,0.2))
mtext("Local mosquito mortality for current IRS", side=4, line=3, cex.lab=2,las=0)                      

BLAperc_red_fromSEGOU <- (mean_Segou - mean_Bla) / mean_Segou
BARAperc_red_fromSEGOU <- (mean_Segou - mean_Baraoueli) / mean_Segou
KOUperc_red_fromSEGOU <- (mean_Segou - mean_Koulikoro) / mean_Segou

plot(BLAperc_red_fromSEGOU*100 ~ unique(mali$time_series),pch=20,
     ylab = "Percentage difference in cases: IRS regions vs CONTROL",ylim=c(-100,100),las=2,
     xlab = "Time (months)",xaxt="n",cex.lab=1.4,
     frame=FALSE,col="red")
lines(BLAperc_red_fromSEGOU*100 ~ unique(mali$time_series),col="red",lwd=2,lty=2)
lines(BARAperc_red_fromSEGOU*100 ~ unique(mali$time_series),col="darkgreen",lwd=2,lty=2)
lines(KOUperc_red_fromSEGOU*100 ~ unique(mali$time_series),col="blue",lwd=2,lty=2)
axis(1,las=0,at=c(13,25,37,49,61),labels=c("Jan 2012","Jan 2013","Jan 2014","Jan 2015","Jan 2016"))

abline(h=0,col="grey")

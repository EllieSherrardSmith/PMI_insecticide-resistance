
############################################################
##
##  Mapping
##  Version 1
##  Author: Ellie Sherrard-Smith
##  Date: 12 March 2016
##
############################################################

library(rworldmap)
library(ggmap)
library(rworldxtra)
library(GISTools)
library(mapplots)
library(adegenet)

########################################################
##  Data file
##
########################################################

dat <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Mapping\\IR_database_up to 2015_Ellie_11032016.csv",header=TRUE)

map_function <- function(data,chem) {
  par(mfrow=c(2,2))

  
  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(-17.6, 50), ylim = c(-26.9, 17.6), asp = 1,col="grey95", 
       border="grey65",main =substitute(paste("2012 ", chem)))
  north.arrow(xb=-16.8, yb=-23, len=1, lab="N",cex.lab=1.5,col='gray10')


  #points(data$"Longitude.UTM_Y.UPDATE2",
  #       data$"Latitude.UTM_X.UPDATE2",
  #              col = "red", cex = 0.6, pch=20)
  

  ## 2012
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2012"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2012"],
         col = "blue", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2012"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2012"],
         col = "yellow", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2012"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2012"],
       col = "red", cex = 0.8, pch=20)


  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(-17.6, 50), ylim = c(-26.9, 17.6), asp = 1,col="grey95", 
       border="grey65",main = "2013")

  
  ## 2013

  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2013"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2013"],
         col = "blue", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2013"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2013"],
         col = "yellow", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2013"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2013"],
       col = "red", cex = 0.8, pch=20)


  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(-17.6, 50), ylim = c(-26.9, 17.6), asp = 1,col="grey95", 
       border="grey65",main = "2014")
  legend(-18, -10, legend=c("Resistance", "Possible resistance", "Susceptible"),
         pch=20,col=c("red","yellow","blue"),cex=1)
  
  ## 2014 (and 2014-2015)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014"],
         col = "blue", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014"],
         col = "yellow", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014"],
       col = "red", cex = 0.8, pch=20)



  newmap <- getMap(resolution = "low")
  plot(newmap, xlim = c(-17.6, 50), ylim = c(-26.9, 17.6), asp = 1,col="grey95", 
       border="grey65",main = "2015")
 
  

  ## 2015
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014/2015"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014/2015"],
         col = "blue", cex = 0.8, pch=20)
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2015"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2015"],
         col = "blue", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2015"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2015"],
         col = "yellow", cex = 0.8, pch=20)
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014/2015"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014/2015"],
         col = "yellow", cex = 0.8, pch=20)
  
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2015"],
       col = "red", cex = 0.8, pch=20)
  points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014/2015"],
         data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014/2015"],
         col = "red", cex = 0.8, pch=20)

  

  
}

data <- subset(dat,dat$chem_type == "Pyrethroids")
map_function(data,"Pyrethroids")

data <- subset(dat,dat$chem_type == "Carbamates")
map_function(data,"Carbamates")

data <- subset(dat,dat$chem_type == "Organochlorides")
map_function(data,"Organochlorides")

data <- subset(dat,dat$chem_type == "Organophosphates")
map_function(data,"Organophosphates")

#############
##
##  Map Figure for most recent Pyrethroids per country covered by PMI
##  
##
##############
dat <- read.csv("C:\\Users\\Ellie\\Documents\\Insecticide resistance\\PMI data\\Mapping\\IR_database_up to 2015_Ellie_11032016_Removed Ghana Non-PMI funded.csv",header=TRUE)

par(mfrow=c(1,1))
##2015 Pyrethroids WHO tube bioassay resistance map PMI
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-17.6, 50), ylim = c(-26.9, 10.6), asp = 1,col="grey95", 
     border="grey65",main ="Evidence of resistance to Pyrethroids up to 2015")
north.arrow(xb=-16.8, yb=-23, len=1, lab="N",cex.lab=1.5,col='gray10')

data <- subset(dat,dat$chem_type == "Pyrethroids")

## 2014 and 2015



points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014"],
       col = "darkgreen", cex = 1, pch=20)


points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014/2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2014/2015"],
       col = "darkgreen", cex = 1, pch=20)
points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2015"],
       col = "darkgreen", cex = 1, pch=20)

points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014"],
       col = "yellow", cex = 1, pch=20)

points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2015"],
       col = "yellow", cex = 1, pch=20)
points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014/2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2014/2015"],
       col = "yellow", cex = 1, pch=20)

points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014"],
       col = "red", cex = 1, pch=20)

points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2015"],
       col = "red", cex = 1, pch=20)
points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014/2015"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2014/2015"],
       col = "red", cex = 1, pch=20)

## Benin 2012
points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2012" & data$Country == "Benin"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2012" & data$Country == "Benin"],
       col = "green", cex = 1, pch=20)
points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2012" & data$Country == "Benin"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2012" & data$Country == "Benin"],
       col = "yellow", cex = 1, pch=20)
points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2012" & data$Country == "Benin"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2012" & data$Country == "Benin"],
       col = "red", cex = 1, pch=20)

legend(-18, -6, 
       legend=c("Resistance (< 90% mortality)", "Possible resistance (90% - 98%)", "Susceptible (> 98%)"),
       pch=20,col=c("red","yellow","darkgreen"),cex=1.2,bty="n")

#####################################
##
## 2007 and 2008 Resistance map
##


newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-17.6, 50), ylim = c(-26.9, 17.6), asp = 1,col="grey95", 
     border="grey65")#,main =substitute(paste("2007 - 2008 Pyrethroids")))
north.arrow(xb=-16.8, yb=-23, len=1, lab="N",cex.lab=1.5,col='gray10')

data <- dat

points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2007"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2007"],
       col = "darkgreen", cex = 1, pch=20)


points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2008"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "S" & data$Year == "2008"],
       col = "darkgreen", cex = 1, pch=20)

points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2007"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2007"],
       col = "yellow", cex = 1, pch=20)


points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2008"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2008"],
       col = "yellow", cex = 1, pch=20)


points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2007"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "PR" & data$Year == "2007"],
       col = "red", cex = 1, pch=20)


points(data$"Longitude.UTM_Y.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2008"],
       data$"Latitude.UTM_X.UPDATE2"[data$"Resistance.code_IR.Mapper" == "R" & data$Year == "2008"],
       col = "red", cex = 1, pch=20)

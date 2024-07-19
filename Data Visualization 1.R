setwd("/Users/yi/Library/Data Visualization/Individual Project")
exoplanets = read.csv("exoplanets.csv")


##Data Cleaning
dim(exoplanets) ## 5458 x 25
summary(exoplanets)
exoplanets = exoplanets[!is.na(exoplanets$PlanetMassEarth),] ##remove NAs for mass
exoplanets = exoplanets[exoplanets$PlanetMassEarth<25,] ## focus down on smaller planets (Mass <20 Earths)
exoplanets = exoplanets[exoplanets$PlanetRadiusEarth<5,] ## focus down on Earth like and SuperEarth planets (Radius <5 Earths)
exoplanets$DiscoveryMethod = as.factor(exoplanets$DiscoveryMethod) ##change discovery method to factor
exoplanets$DiscFacility = as.factor(exoplanets$DiscFacility) ## change discovery facility to factor
summary(exoplanets)
dim(exoplanets) ## 3522 x 25

##Classify into Earthlike (<=1.25 Earth Radii) and SuperEarth (>1.25 Earth Radii)
earthlike = c()
earthlike[exoplanets$PlanetRadiusEarth<=1.5] = "Earthlike"
earthlike[exoplanets$PlanetRadiusEarth>1.5] = "Super Earth"
exoplanets = cbind(exoplanets,earthlike)
remove(earthlike)

##There are too many discovery observatories and discovery methods which makes any graph with them extremely cluttered. 
##It's necessary to classify them into the the common ones (which can be seen from summary(exoplanets$DiscFacility)), and "other"

##Classify by Discovery Observatory
OtherFac = c("Anglo-Australian Telescope", "Arecibo Observatory", "Calar Alto Observatory", "Cerro Tololo Inter-American Observatory", "CHaracterising ExOPlanets Satellite (CHEOPS)", "CoRoT", "European Southern Observatory",
             "Haute-Provence Observatory", "KMTNet", "KOINet", "Las Campanas Observatory", "Lick Observatory", "Lowell Observatory", "McDonald Observatory", "MEarth Project", "MOA","OGLE", "Paranal Observatory", "Roque de los Muchachos Observatory",
             "SPECULOOS Southern Observatory", "Spitzer Space Telescope", "Subaru Telescope", "W. M. Keck Observatory")
FacNames = c("Kepler", "K2", "Transiting Exoplanet Survey Satellite (TESS)", "Multiple Observatories", "Multiple Facilities", "La Silla Observatory", 
             "Other")
FacNamesNew = c()
for (i in OtherFac){ 
  FacNamesNew[exoplanets$DiscFacility==i]="Other"}
for (i in FacNames){
  FacNamesNew[exoplanets$DiscFacility==i]=i}
FacNamesNew[FacNamesNew == "K2"] = "Kepler" ##K2 is just the extension of the Kepler Mission--same space telescope
FacNamesNew[FacNamesNew == "Transiting Exoplanet Survey Satellite (TESS)"] = "TESS" ##the full name is too long and gets difficult in graphs.
FacNamesNew[FacNamesNew == "Multiple Facilities"] = "Multiple Observatories"
FacNamesNew=factor(FacNamesNew, levels=c("Kepler", "TESS","La Silla Observatory", "Multiple Observatories","Other"))
exoplanets = cbind(exoplanets,FacNamesNew)
remove(OtherFac,FacNamesNew,i,FacNames)

##Classify based on Discovery Method
discMethod = c()
discOther=c("Orbital Brightness Modulation", "Pulsar Timing","Transit Timing Variations")
discMethodGood=c("Microlensing","Radial Velocity","Transit")
for (i in discOther){
  discMethod[exoplanets$DiscoveryMethod==i]="Other"}
for (i in discMethodGood){
  discMethod[exoplanets$DiscoveryMethod==i]=i}
discMethod=factor(discMethod, levels=c("Transit", "Radial Velocity", "Microlensing", "Other"))
exoplanets = cbind(exoplanets,discMethod)
remove(discMethod,discOther,discMethodGood,i)

##add Mass/Radius (Density) as a column
density= c()
density = exoplanets$PlanetMassEarth/exoplanets$PlanetRadiusEarth
exoplanets=cbind(exoplanets,density)
remove(density)


##Summary Statistics
##Summary Boxplots for Radius, Mass, Density
par(mfrow = c(1,3))
fivNumSumCol="deepskyblue4"
textLoc=0.65
boxplot(exoplanets$PlanetRadiusEarth, main = "Planet Radii (Earth Radii)")
text(textLoc,1.474, "1.47", col=fivNumSumCol)
text(textLoc,2.1, "2.1", col=fivNumSumCol)
text(textLoc,2.728, "2.73", col=fivNumSumCol)
boxplot(exoplanets$PlanetMassEarth, main = "Planet Masses (Earth Masses)")
text(textLoc,2.86, "2.86", col=fivNumSumCol)
text(textLoc,5.22, "5.22", col=fivNumSumCol)
text(textLoc,8.25, "8.25", col=fivNumSumCol)
boxplot(exoplanets$density, main = "Planet Mass/Radius Ratio (Earth m/r)")
text(textLoc,1.885, "1.89", col=fivNumSumCol)
text(textLoc,2.45, "2.45", col=fivNumSumCol)
text(textLoc,2.99, "2.99", col=fivNumSumCol)

##Summary Pie Charts for EarthLike, Facility Name, Discovery Method
EartlikeTab = table(exoplanets$earthlike)
pie(EartlikeTab, cex = 1.1, col = c("blue", "green"), main = "Planet Type")
DiscFacTab = table(exoplanets$FacNamesNew)
pieColors = c("blue", "green", "orange", "lightgoldenrod1", "plum1")
pie(DiscFacTab, cex = 1.1, col = pieColors, main="Discovering Facility")
remove(pieColors)
DiscMetTab = table(exoplanets$discMethod)
pie(DiscMetTab,cex= 1.1, col =c("lightgrey", "pink1", "lightblue", "lightgreen"), main= "Discovery Method")
remove(EartlikeTab,DiscMetTab,DiscFacTab)
par(mfrow = c(1,1))

##Boxplots
##Boxplot of Mass/Radius (Earthlike v. Super Earth)
boxplot(density ~ earthlike, data = exoplanets, main = "Mass/Radius Ratio of Earthlike Planets v. Super Earths", ylab = "Mass/Radius Ratio (Multiples of Earth m/r ratio", xlab="")

##Boxplot of Mass by Discovery Telescope
boxplot(PlanetMassEarth ~ FacNamesNew, data = exoplanets, main = "Mass of Planets by Discovery Facility", ylab = "Planet Mass (Earth Masses)", xlab = "")


##Histograms
##Histogram of Mass by Discovery Telescope (Kepler and TESS only since they are the two most prolific)
width = 2
densKepMass=density(exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="Kepler"], bw=width)
densTESSMass=density(exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="TESS"])
hist(exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="Kepler"], main = "Histogram of Mass by Discovery Facilty", xlab = "Earth Masses", col = rgb(0,0,1,0.25), freq = F, breaks=seq(0,26,width), ylim = c(0,0.15))
hist(exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="TESS"], freq=F, col= rgb(1,0,1,0.25), breaks = seq(0,26,width), add=T)
legend("topright", c("Kepler Space Telescope", "TESS"), pch = c(15,15), col = c(rgb(0,0,1,1), rgb(1,0,1,1)), cex = 1.1)
lines(densKepMass, lw=2, col = rgb(0,0,1,1))
lines(densTESSMass, lw=2, col = rgb(1,0,1,1))

##Histogram of Mass/Radius Ratio by Type of Planet
width = 2
densEarthlikeDensity = density(exoplanets$density[exoplanets$earthlike=="Earthlike"], bw=width)
densSuperEarthDensity = density(exoplanets$density[exoplanets$earthlike=="Super Earth"], bw=width)
hist(exoplanets$density[exoplanets$earthlike=="Earthlike"], main="Histogram of Mass/Radius Ratio", xlab = "Multiples of Earth m/r Ratio", col = rgb(0,0,1,0.25), freq=F, ylim=c(0,0.5), breaks=seq(0,22,width))
hist(exoplanets$density[exoplanets$earthlike=="Super Earth"], freq=F, col = rgb(0,1,0,0.25), breaks=seq(0,22,width), add=T)
legend("topright", c("Earthlike", "Super Earths"), pch=c(15,15), col = c("blue", "green"), cex= 1.1)
lines(densEarthlikeDensity, lw=2, col = "blue")
lines(densSuperEarthDensity, lw=2, col = "green")


##BarPlot of Mass of Planet Discovered by Facility
##Not included in presentation because it doesn't really add much information
avgMassByScope = aggregate(PlanetMassEarth ~ FacNamesNew, data= exoplanets, FUN = mean)
barplot(avgMassByScope$PlanetMassEarth, names.arg = avgMassByScope$FacNamesNew, col = "darkolivegreen3", ylab="Earth Masses", ylim = c(0,10), xlab = "Telescope", main= "Average Planet Mass by Discovery Telescope")


##Plots
##Multiple examples of the plots of mass and radius of the planets labeled in different ways
##The first two are not in the presentation because the first (by discovery facility) is too cluttered
##and the second (labelled by earthlike/super earth), the colors don't really add anything--the squared model regression is more useful
##Plot 1: Plot of mass, radius labeled by discovery facility
plot(exoplanets$PlanetRadiusEarth[exoplanets$FacNamesNew=="Kepler"],exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="Kepler"],
     main = "Planet Mass by Radius",
     xlim= c(0,3), ylim = c(0,20), xlab = "Planet Radius",
     ylab = "Planet Mass", pch = 17, col = "blue")
points(exoplanets$PlanetRadiusEarth[exoplanets$FacNamesNew=="Multiple Observatories"],exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="Multiple Observatories"],
       pch = 18, col = "orange")
points(exoplanets$PlanetRadiusEarth[exoplanets$FacNamesNew=="TESS"],exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="TESS"],
       pch = 19, col = "plum1")
points(exoplanets$PlanetRadiusEarth[exoplanets$FacNamesNew=="La Silla Observatory"],exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="La Silla Observatory"],
       pch = 20, col = "green")
points(exoplanets$PlanetRadiusEarth[exoplanets$FacNamesNew=="Other"],exoplanets$PlanetMassEarth[exoplanets$FacNamesNew=="Other"],
       pch = 21, col = "lightgoldenrod1")
legend("topleft", c("Kepler Space Telescope", "TESS", "Multiple Observatories", "La Silla Observatory", "Other"), 
       col = c("blue", "orange", "plum1", "green", "lightgoldenrod1"),pch = c(17:21), cex=0.8)

##Plot 2: Plot with Regression 
modelEarthlike = lm(exoplanets$PlanetMassEarth[exoplanets$earthlike=="Earthlike"]~exoplanets$PlanetRadiusEarth[exoplanets$earthlike=="Earthlike"])
modelSuperEarth =lm(exoplanets$PlanetMassEarth[exoplanets$earthlike=="Super Earth"]~exoplanets$PlanetRadiusEarth[exoplanets$earthlike=="Super Earth"])
##plot(exoplanets$PlanetMassEarth~exoplanets$PlanetRadiusEarth, main = "Planet Mass by Radius", xlab="Planet Radius", ylab = "Planet Mass")
plot(exoplanets$PlanetRadiusEarth[exoplanets$earthlike=="Earthlike"],exoplanets$PlanetMassEarth[exoplanets$earthlike=="Earthlike"],
     main = "Planet Mass by Radius",
     xlim= c(0,5), ylim = c(0,20), xlab = "Planet Radius",
     ylab = "Planet Mass", pch = 17, col = rgb(0,0,1,0.25))
points(exoplanets$PlanetRadiusEarth[exoplanets$earthlike=="Super Earth"],exoplanets$PlanetMassEarth[exoplanets$earthlike=="Super Earth"],
       pch = 19, col = rgb(0,1,0,0.25))
legend("topleft", c("Earthlike; Density = 2.8xEarth", "Super Earths, Density = 4.3xEarth", "Earth"), col = c("blue", "green","lightgrey"), pch=c(17,19,15), cex= 0.8)
abline(modelEarthlike, col = "blue")
abline(modelSuperEarth, col = "green")
abline(a=0, b= 1, col = "lightgray") ##this line represents Earth's Density


##Plot 3: Plot mass, radius without regression labeled by discovery method
plot(exoplanets$PlanetRadiusEarth[exoplanets$discMethod=="Transit"],exoplanets$PlanetMassEarth[exoplanets$discMethod=="Transit"],
     main = "Planet Mass by Radius",
     xlim= c(0,5), ylim = c(0,20), xlab = "Planet Radius (Earth Radii)",
     ylab = "Planet Mass (Earth Masses)", pch = 17, col = "lightgrey")
points(exoplanets$PlanetRadiusEarth[exoplanets$discMethod=="Radial Velocity"],exoplanets$PlanetMassEarth[exoplanets$discMethod=="Radial Velocity"],
       pch = 18, col = "pink1")
points(exoplanets$PlanetRadiusEarth[exoplanets$discMethod=="Microlensing"],exoplanets$PlanetMassEarth[exoplanets$discMethod=="Microlensing"],
       pch = 19, col = "lightblue")
points(exoplanets$PlanetRadiusEarth[exoplanets$discMethod=="Other"],exoplanets$PlanetMassEarth[exoplanets$discMethod=="Other"],
       pch = 20, col = "lightgreen")
abline(v=1.5)
legend("topleft", c("Transit", "Radial Velocity", "Microlensing", "Other"), col=c("lightgrey", "pink1", "lightblue", "lightgreen"), pch = c(17:20), cex=0.9)
text(0.5,10, "Earth Like", col="blue")
text(4.5,10, "Superearth", col="darkgreen")

##Unfortunately you can see that the mass/radius is not straight, but actually rises more like as a function of the square of the radius
##as such, to plot the densities with a linear regression we need to square the radius.

##Plot 4: Squared Model Plot with regression
radiussquared=c()
radiussquared=exoplanets$PlanetRadiusEarth^2
exoplanets=cbind(exoplanets,radiussquared)
remove(radiussquared)
modelELSquared = lm(exoplanets$PlanetMassEarth[exoplanets$earthlike=="Earthlike"]~exoplanets$radiussquared[exoplanets$earthlike=="Earthlike"])
modelSESquared = lm(exoplanets$PlanetMassEarth[exoplanets$earthlike=="Super Earth"]~exoplanets$radiussquared[exoplanets$earthlike=="Super Earth"])
plot(exoplanets$radiussquared[exoplanets$earthlike=="Earthlike"],exoplanets$PlanetMassEarth[exoplanets$earthlike=="Earthlike"],
     main = "Planet Mass by Radius Squared",
     xlim= c(0,25), ylim = c(0,25), xlab = "Planet Radius Squared (Earth Radii Squared)",
     ylab = "Planet Masses (Earth Masses)", pch = 17, col = rgb(0,0,1,0.25))
points(exoplanets$radiussquared[exoplanets$earthlike=="Super Earth"],exoplanets$PlanetMassEarth[exoplanets$earthlike=="Super Earth"],
       pch = 19, col = rgb(0,1,0,0.25))
abline(modelELSquared, col = "blue")
abline(modelSESquared, col = "green")
abline(a=0, b= 1, col = "lightgray") ##this line represents Earth's density
abline(v=1.5^2, col = "black")
legend("bottomright", c("Earthlike; Mass/Radius^2 = 1.5xEarth^2", "Super Earths, Mass/Radius^2 = 0.8xEarth^2 + 1.96", "Earth"), col = c("blue", "green","lightgrey"), pch=c(17,19,15), cex= 0.8)

#___________________________________________________________________________________________________________________
#Define everything until the demarking line
#Import the Dataset excel sheet from Github

install.packages("RcmdrMisc") #Use this if the package not installed yet
library("RcmdrMisc")
setwd("C:/Users/Jacob/Desktop/R-data") #Set this one to the folder containing the Dataset
Dataset = readXL("Dataset.xlsx", rownames=FALSE, header=TRUE, na="", sheet="Ark1", stringsAsFactors=TRUE) #This reads the imported Excel sheet

#Functions
plotlines = function(Dependent, yname, showing, limit, letter){
  means = c()
  error = c()
  lab = c('', '', '', '')
  xlabel = ''
  if (showing == TRUE){
    lab = c("LL", "LH", "HL", "HH")
    xlabel = expression("Acclimation treatment (CO"[2]*", HCO"[3]^"-"*")")
  }
  for (i in c(0,4,1,5,2,6,3,7)){
    category = Dependent[(1+5*i):(5+5*i)]
    m = mean(category, na.rm = TRUE)
    e = sd(category, na.rm = TRUE)/sqrt(length(category[!is.na(category)]))
    means = append(means, m)
    error = append(error, e)
  }
  position = c(0.95, 1.05, 1.95, 2.05, 2.95, 3.05, 3.95, 4.05)
  plot(NA, NA, pch=19, xlab=xlabel, ylab = "", xaxt="n", xlim=c(0.5, 4.5), ylim = limit, cex.lab = 1.3, cex.axis = 1.1)
  for (i in c(1,3,5,7)){
    segments(x0 = position[i], y0 = means[i] - error[i], x1 = position[i], y1 = means[i] + error[i], col = 'deepskyblue', lwd = 3, lty = 1)
  }
  for (i in c(2,4,6,8)){
    segments(x0 = position[i], y0 = means[i] - error[i], x1 = position[i], y1 = means[i] + error[i], col = 'green4', lwd = 3, lty = 1)
  }
  for (i in c(1,2,3,4,5,6,7,8)){
  segments(x0 = position[i]-0.05, y0 = means[i], x1 = position[i]+0.05, y1 = means[i], col = "black", lwd = 3)
  }
  
  axis(side = 1, at = c(1, 2, 3, 4), labels = lab, cex.axis = 1.2)
  title(ylab = yname, line=2.3, cex.lab = 1.3)
  text(x = 0.5, y = limit[1] + (limit[2]-limit[1])*0.9, labels = letter, cex = 1.3, font = 2)
  #legend('topright', fill = c('deepskyblue', 'green4'), legend = c("R. aquatilis", "E. canadensis"), border = FALSE, cex = 1.3)
}
RGR = function(){
  Dependent = Dataset$Relative.Growth
  yname = expression("RGR (day"^" -1"*")")
  plotlines(Dependent, yname, FALSE, c(0,0.15), 'a')
}
Net_photosynthesis = function(){
  Dependent = Dataset$Specific.photosynthetic.rate
  yname = expression("P"["max"]*" (µmol O "[2]*" g"^" -1"*" DM h"^" -1"*")")
  plotlines(Dependent, yname, FALSE, c(0,500), 'b')
}
Dark_respiration = function(){
  Dependent = Dataset$Specific.dark.respiration.rate
  yname = expression("R"["d"]*" (µmol O "[2]*" g"^" -1"*" DM h"^" -1"*")")
  plotlines(Dependent, yname, FALSE, c(0,100), 'c')
}
pH_drift = function(){
  Dependent = Dataset$pH.drift
  yname = "pH"
  plotlines(Dependent, yname, TRUE, c(9.5,11), 'd')
}
Forestplot = function(){
  frame()
  plot.window(xlim=c(-0.3,0.3), ylim=c(0,5))
  axis(1)
  title(xlab = expression("PI"["diff"]), cex.lab=1.2)
  traits = c('RGR', expression("P"["max"]), expression("R"["d"]), 'pH-drift')
  text(x=-0.32, y=c(4,3,2,1), labels = traits, adj = 0, cex=1.2)
  text(x=c(-0.15,0.15), y=c(5,5), labels=c("R. aquatilis","E. canadensis"), font=3, cex=1.2)
  abline(v=0)
  points(x=c(0.128-0.023,0.160-0.295,0.090-0.126,0.012-0.010), y=c(4,3,2,1), pch=16, cex=1.5)
}
Legend = function(){
  frame()
  legend('topright', fill = c('deepskyblue', 'green4'), legend = c("R. aquatilis", "E. canadensis"), border = FALSE, cex = 1, horiz = TRUE)
}

#Renaming data categories
Categories = Dataset$Category
Treatment = factor(Dataset$Treatment, levels = c("LL", "LH", "HL", "HH"))
CO2 = Dataset$Carbon.dioxide
HCO3 = Dataset$Bicarbonate
Species = Dataset$Species

#Until here
#___________________________________________________________________________________________________________________

#Plots
par(mfrow = c(4,1), mar = c(4, 5, 1, 3))
RGR()
Net_photosynthesis()
Dark_respiration()
pH_drift()
par(mfrow = c(1,1))
Legend()
Forestplot()


#___________________________________________________________________________________________________________________

#Define only ONE Dependent at a time before running statistical tests
Dependent = Dataset$Relative.Growth
Dependent = Dataset$Specific.photosynthetic.rate
Dependent = Dataset$Specific.dark.respiration.rate
Dependent = Dataset$pH.drift

#____________________________________________________________________________________________________________________

#Testing homogeneity of variance
leveneTest(Dependent ~ Categories, center = "mean")

#Making and testing 3-way ANOVA
model = lm(Dependent ~ CO2 + HCO3 + Species)
summary(model)
anova(model)
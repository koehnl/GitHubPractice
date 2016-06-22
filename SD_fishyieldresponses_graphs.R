
AverageValues <- function(Biovalues) {
  n = nrow(Biovalues)
  average = vector()
  dev = vector()
  minvalues = vector()
  maxvalues = vector()
  for(i in 1:n) {
    animal = Biovalues[i,]
    animal2 = animal[animal < 1 & animal > -1]
    average[i] = mean(animal2)
    dev[i] = sd(animal2)
    minvalues[i] = min(animal2)
    maxvalues[i] = max(animal2)
  }
  return(list( "Mean" = average, "StandardDev" = dev, "Min" = minvalues, "Max" = maxvalues))
}
test = AverageValues(sardine0.7$Yield)
test2 = AverageValues(anchovy0.7$Yield)
test3 = AverageValues(herring0.7$Yield)
test4 = AverageValues(forage3_0.7$Yield)


par(mfrow = c(1,3))
par(oma = c(6,8.5,3,4)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(0,2, 0,0))
names = c("Yellowtail Rock.", "Black Rockfish", "Nearshore Rock.", "Yelloweye Rock.",
          "Greenstriped", "Shelf Rockfish", "Shortbelly Rock.", "Petrale Sole",
          "Halibut", "Hake", "Lingcod", "Arrowtooth", "Sablefish", "Albacore",
          "Salmon", "Grenadiers", "Dogfish", "Canary Rock.", "P. Ocean Perch", 
          "Widow Rock.", "Splitnose Rock.", "Slope Rockfish", "Short. Thorny.",
          "Long. Thorny.", "Flatfish", "Skates", "Sharks")
names = rev(names)
sar_avgs = test$Mean[9:35]; sar_avgs = rev(sar_avgs)
sar_SD = test$StandardDev[9:35]; sar_SD = rev(sar_SD)
#names = sardine0.7$yieldgroups[9:35]; names = rev(names)
x = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13)
plot(sar_avgs,x, frame = F, xlab = "", ylab = "", xlim = c(-0.1,0.6), axes = F, pch = 19)
arrows(sar_avgs - (sar_SD), x, sar_avgs + (sar_SD), x, length=0.05, angle=90, code=3)
axis(2, at = x, labels = c(names), las = 2, pos = -0.12)
axis(1, at=c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Sardine",side = 3)
#abline(v = 0, lty = 2)

par(mfrow = c(1,3))
par(oma = c(6,9,3,4)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(0,2, 0,0))

anc_avgs = test2$Mean[9:35]; anc_avgs = rev(anc_avgs)
anc_SD = test2$StandardDev[9:35]; anc_SD = rev(anc_SD)
plot(anc_avgs,x, frame = F, xlab = "", ylab = "", xlim = c(-0.05,0.05), axes = F, pch = 19)
arrows(anc_avgs - (anc_SD), x, anc_avgs + (anc_SD), x, length=0.05, angle=90, code=3)
axis(1, at=c(-0.05,-0.025, 0, 0.025, 0.5), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Anchovy", side = 3)
axis(2, at = x, labels = c(names), las = 2, pos = -0.06)
abline(v = 0, lty = 2)

her_avgs = test3$Mean[9:35]; her_avgs = rev(her_avgs)
her_SD = test3$StandardDev[9:35]; her_SD = rev(her_SD)
plot(her_avgs,x, frame = F, xlab = "", ylab = "", xlim = c(-0.05,0.05), axes = F, pch = 19)
arrows(her_avgs - (her_SD), x, her_avgs + (her_SD), x, length=0.05, angle=90, code=3)
axis(1, at=c(-0.05,-0.025, 0, 0.025, 0.5), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Herring", side = 3)
abline(v = 0, lty = 2)

for_avgs = test4$Mean[6:32]; for_avgs = rev(for_avgs)
for_SD = test4$StandardDev[6:32]; for_SD = rev(for_SD)
plot(for_avgs,x, frame = F, xlab = "", ylab = "", xlim = c(-0.05,0.05), axes = F, pch = 19)
arrows(for_avgs - (for_SD), x, for_avgs + (for_SD), x, length=0.05, angle=90, code=3)
axis(1, at=c(-0.05,-0.025, 0, 0.025, 0.5), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Combination", side = 3)
mtext("Average Predator Yield Response (\u00b1 Standard Deviation)", side = 1, outer = TRUE, line = 3, cex = 1)
abline(v = 0, lty = 2)

x = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5,13)
par(mfrow = c(1,4))
par(oma = c(1,1,1,1))
par(mar = c(3,3,0,0))

plot(x,sar_avgs, frame = F, xlab = "", ylab = "", ylim = c(-0.1,0.6), axes = F, pch = 19)
arrows(x, sar_avgs - (sar_SD), x, sar_avgs + (sar_SD), length=0.05, angle=90, code=3)
axis(1, at = x, labels = c(names), las = 2, pos = -0.11)
axis(2, at=c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6), cex.axis=1, tck=0.01, pos = -0.5)

plot(x, anc_avgs,frame = F, xlab = "", ylab = "", ylim = c(-0.1,0.6), axes = F, pch = 19)
arrows(x, anc_avgs - (anc_SD), x, anc_avgs + (anc_SD), length=0.05, angle=90, code=3)
axis(1, at = x, labels = c(names), las = 2, pos = -0.11)

plot(x, her_avgs, frame = F, xlab = "", ylab = "", ylim = c(-0.1,0.6), axes = F, pch = 19)
arrows(x, her_avgs - (her_SD), x, her_avgs + (her_SD),  length=0.05, angle=90, code=3)
axis(1, at = x, labels = c(names), las = 2, pos = -0.11)

plot(x, for_avgs, frame = F, xlab = "", ylab = "", ylim = c(-0.1,0.6), axes = F, pch = 19)
arrows(x, for_avgs - (for_SD), x, for_avgs + (for_SD),  length=0.05, angle=90, code=3)
axis(1, at = x, labels = c(names), las = 2, pos = -0.11)
mtext("Average Predator Yield Response (Standard Deviation)", side = 1, outer = TRUE, line = 1, cex = 1)


## Bar plot of SD
par(mfrow = c(1,4))
par(oma = c(10,10, 0,0)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(4, 4, 4,4))
barplot(sar_SD, names = names,  las = 1, horiz = TRUE, cex.names = 1.5, xlim = c(0,0.3), axes = F)
axis(1, at=c(0,0.1,0.2,0.3), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Sardine",side = 3)

barplot(anc_SD, names = "",  las = 1, horiz = TRUE, cex.names = 1.5, xlim = c(0,0.3), axes = F)
axis(1, at=c(0,0.1,0.2,0.3), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Anchovy", side = 3)

barplot(her_SD, names = "",  las = 1, horiz = TRUE, cex.names = 1.5, xlim = c(0,0.3), axes = F)
axis(1, at=c(0,0.1,0.2,0.3), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Herring", side = 3)

barplot(for_SD, names = "",  las = 1, horiz = TRUE, cex.names = 1.5, xlim = c(0,0.3), axes = F)
axis(1, at=c(0,0.1,0.2,0.3), cex.axis=1, tck=0.01, pos = -0.5)
mtext("Combination", side = 3)
mtext("Standard Deviation of Predator Yield Responses", side = 1, outer = TRUE, line = 1, cex = 1)



# SD barplot 
SD = c(0.045,0.0012,0.0021,0.0014)
mp = barplot(SD, axes = F, ylim = c(0,0.05))
axis(2, at=seq(0 , 0.05, by=0.01), las = 2, tck = -0.01, pos = 0.1)
mtext("Average Standard Deviation of Predator Yield Responses", cex = 1, side = 2, line = 2.5)
names = c("Sardine", "Anchovy","Herring", "Combination")
text(mp,(par("usr")[3]-0.003), labels = names, srt = 45,adj = c(1.5,1.1), xpd = TRUE, cex = 0.9,pos = 1)

########## Percentiles #############
PercentileValues <- function(Biovalues, n, x) {
  lower = vector()
  upper = vector()
  lower2 = vector()
  upper2 = vector()
  half = vector()
  for(i in 1:n) {
    animal = Biovalues[i+x,] # 8 for singles, 5 for aggregated
    animal2 = animal[animal < 1 & animal > -1]
    lower[i] = quantile(animal2, 0.025, names = F)
    upper[i] = quantile(animal2, 0.975, names = F)
    lower2[i] = quantile(animal2, 0.25, names = F)
    upper2[i] = quantile(animal2, 0.75, names = F)
    half[i] = quantile(animal2, 0.5, names = F)
  }
  return(list("lower" = lower, "upper" = upper, "lower2" = lower2,
              "upper2" = upper2, "half" = half))
}
test = PercentileValues(sardine0$Yield, n = 27, x = 8)
test2 = PercentileValues(anchovy0$Yield, n = 27, x = 8)
test3 = PercentileValues(herring0$Yield, n = 27, x = 8)
test4 = PercentileValues(forage3_0$Yield, n = 27, x = 5)

testA = PercentileValues(sardine0.7$Yield, n = 27, x = 8)
testB = PercentileValues(anchovy0.7$Yield, n = 27, x = 8)
testC = PercentileValues(herring0.7$Yield, n = 27, x = 8)
testD = PercentileValues(forage3_0.7$Yield, n = 27, x = 5)
names = c("Yellowtail Rock.", "Black Rockfish", "Nearshore Rock.", "Yelloweye Rock.",
          "Greenstriped", "Shelf Rockfish", "Shortbelly Rock.", "Petrale Sole",
          "Halibut",  "Lingcod", "Arrowtooth", "Sablefish", "Albacore",
          "Salmon", "Grenadiers", "Dogfish", "Canary Rock.", "P. Ocean Perch", 
          "Widow Rock.", "Splitnose Rock.", "Slope Rockfish", "Short. Thorny.",
          "Long. Thorny.", "Flatfish", "Skates", "Sharks")
par(mfrow = c(2,4))
par(oma = c(2,14.5,1,2)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(1,1.5,2,1.5))

# NOTE RANGES CHANGE DEPEND ON IF EPSILON = 0.7 or 0
mat = matrix(c(1,1,1,1,5,1,1,1,1,5,2,2,2,2,6,2,2,2,2,6,3,3,3,3,7,3,3,3,3,7,4,4,4,4,8,4,4,4,4,8), nrow = 5, ncol = 8)
layout(mat)
x = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5)
plot(test$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.005,0.005), axes = F, pch = 19, col = 'white')
arrows(test$lower[-10], x, test$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test$lower2[-10], x, test$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(2, at = x, labels = c(names), las = 2, pos = -0.006, cex.axis = 1.5)
axis(1, at=c(-0.005,-0.0025,0,0.0025,0.005), cex.axis=1.5, tck=0.01, pos = -0.5)
mtext("Sardine",side = 3, cex = 1.5)
abline(v = 0, lty = 2)

plot(test2$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.005,0.005), axes = F, pch = 19, col = 'white')
arrows(test2$lower[-10], x, test2$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test2$lower2[-10], x, test2$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.005,-0.0025,0,0.0025,0.005), cex.axis=1.5, tck=0.01, pos = -0.5)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
mtext("Anchovy", side = 3, cex = 1.5)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.06)
abline(v = 0, lty = 2)

plot(test3$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.005, 0.005), axes = F, pch = 19, col = 'white')
arrows(test3$lower[-10], x, test3$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test3$lower2[-10], x, test3$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.005,-0.0025,0,0.0025,0.005), cex.axis=1.5, tck=0.01, pos = -0.5)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
mtext("Herring", side = 3, cex = 1.5)
abline(v = 0, lty = 2)

plot(test4$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.005, 0.005), axes = F, pch = 19, col = 'white')
arrows(test4$lower[-10], x, test4$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test4$lower2[-10], x, test4$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.005,-0.0025,0,0.0025,0.005), cex.axis=1.5, tck=0.01, pos = -0.5)
mtext("Combination", side = 3, cex = 1.5)
#mtext("Percentiles of Predator Yield Responses", side = 1, outer = TRUE, line = 3, cex = 1)
abline(v = 0, lty = 2)

#### Hake's own separate panel ########
#par(mar = c(6,1.5,4,1.5))
par(mar = c(5.5,1.5,2,1.5))
x = c(1)
plot(test$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.01, 0.01), axes = F, pch = 19, col = 'white')
arrows(test$lower[10], x, test$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test$lower2[10], x, test$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(2, at = x, labels = c("Hake"), las = 2, pos = -0.012, cex.axis = 1.5)
axis(1, at=c(-0.01, -0.005,0,0.005,0.01), cex.axis=1.5, tck=0.075, pos = 0.4)
abline(v = 0, lty = 2)

plot(test2$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.01,0.01), axes = F, pch = 19, col = 'white')
arrows(test2$lower[10], x, test2$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test2$lower2[10], x, test2$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.01, -0.005,0,0.005,0.01), cex.axis=1.5, tck=0.075, pos = 0.4)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.06)
abline(v = 0, lty = 2)

plot(test3$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.01,0.01), axes = F, pch = 19, col = 'white')
arrows(test3$lower[10], x, test3$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test3$lower2[10], x, test3$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.01, -0.005,0,0.005,0.01), cex.axis=1.5, tck=0.075, pos = 0.4)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
abline(v = 0, lty = 2)

plot(test4$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.01,0.01), axes = F, pch = 19, col = 'white')
arrows(test4$lower[10], x, test4$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(test4$lower2[10], x, test4$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.01, -0.005,0,0.005,0.01), cex.axis=1.5, tck=0.075, pos =0.4)
abline(v = 0, lty = 2)
mtext("Percentiles of Predator Catch Responses (Epsilon = 0)", side = 1, outer = TRUE, line = -0.45, cex = 1.5)

##### Which runs give values > 1 or < -1?
for(i in 1:500) {
  x = which(sardine0.7$Biomass[50:92,i] > 1 || sardine0.7$Biomass[50:92,i] < -1)
  if(length(x) > 0) {
    print(i)
  }
}

######## NO SARDINE ##########
par(mfrow = c(2,3))
par(oma = c(4,14,2,2)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(2,2,2,2))

# NOTE RANGES CHANGE DEPEND ON IF EPSILON = 0.7 or 0
mat = matrix(c(1,1,1,4,1,1,1,4,2,2,2,5,2,2,2,5,3,3,3,6,3,3,3,6), nrow = 4, ncol = 6)
layout(mat)
x = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5,11,11.5,12,12.5)

plot(testB$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.02,0.02), axes = F, pch = 19, col = 'white')
arrows(testB$lower[-10], x, testB$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(testB$lower2[-10], x, testB$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.02,-0.01,0,0.01,0.02), cex.axis=1.5, tck=0.01, pos = -0.5)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
mtext("Anchovy", side = 3, cex = 1.5)
axis(2, at = x, labels = c(names), las = 2, pos = -0.023, cex.axis = 1.5)
abline(v = 0, lty = 2)

plot(testC$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.02, 0.02), axes = F, pch = 19, col = 'white')
arrows(testC$lower[-10], x, testC$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(testC$lower2[-10], x, testC$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.02,-0.01,0,0.01, 0.02), cex.axis=1.5, tck=0.01, pos = -0.5)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
mtext("Herring", side = 3, cex = 1.5)
abline(v = 0, lty = 2)

plot(testD$half[-10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.02, 0.02), axes = F, pch = 19, col = 'white')
arrows(testD$lower[-10], x, testD$upper[-10], x, length=0, angle=90, code=3, lwd = 1)
arrows(testD$lower2[-10], x, testD$upper2[-10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.02,-0.01,0,0.01,0.02), cex.axis=1.5, tck=0.01, pos = -0.5)
mtext("Combination", side = 3, cex = 1.5)
#mtext("Percentiles of Predator Yield Responses", side = 1, outer = TRUE, line = 3, cex = 1)
abline(v = 0, lty = 2)

#### Hake's own separate panel ########
#par(mar = c(6,1.5,4,1.5))
par(mar = c(5.5,1.5,2,1.5))
x = c(1)

plot(testB$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.3,0.3), axes = F, pch = 19, col = 'white')
arrows(testB$lower[10], x, testB$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(testB$lower2[10], x, testB$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.3, -0.15,0,0.15,0.3), cex.axis=1.5, tck=0.075, pos = 0.4)
axis(2, at = x, labels = c("Hake"), las = 2, pos = -0.35, cex.axis = 1.5)

abline(v = 0, lty = 2)

plot(testC$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.3,0.3), axes = F, pch = 19, col = 'white')
arrows(testC$lower[10], x, testC$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(testC$lower2[10], x, testC$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.3, -0.15,0,0.15,0.3), cex.axis=1.5, tck=0.075, pos = 0.4)
#axis(2, at = x, labels = c(names), las = 2, pos = -0.25)
abline(v = 0, lty = 2)

plot(testD$half[10],x, frame = F, xlab = "", ylab = "", xlim = c(-0.3,0.3), axes = F, pch = 19, col = 'white')
arrows(testD$lower[10], x, testD$upper[10], x, length=0, angle=90, code=3, lwd = 1)
arrows(testD$lower2[10], x, testD$upper2[10], x, length=0, angle=90, code=3, lwd = 4)
axis(1, at=c(-0.3, -0.15,0,0.15,0.3), cex.axis=1.5, tck=0.075, pos =0.4)
abline(v = 0, lty = 2)
mtext("Percentiles of Predator Catch Responses (Epsilon = 0.7)", side = 1, outer = TRUE, line = 0.5, cex = 1.5)


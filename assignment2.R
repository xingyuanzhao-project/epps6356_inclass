# Assignment 2 - Plotting functions using Happy Planet Index data
# http://happyplanetindex.org

# Load required libraries
library(readxl)

# Read the HPI data
df_hpi <- read_excel("HPI.xlsx", sheet=2, skip=7)

# Remove rows with missing HPI values
df_hpi <- df_hpi[!is.na(df_hpi$HPI), ]

# i. par() - Set graphical parameters
png("a2p8.png", width=800, height=600)
par(mfrow=c(2,1), mar=c(4,4,3,2), bg="lightyellow")
plot(df_hpi$HPI, main="par() - Setting graphical parameters", 
     xlab="Country Index", ylab="HPI", pch=16, col="blue")
plot(df_hpi$`Life Expectancy (years)`, main="Multiple plots with par()", 
     xlab="Country Index", ylab="Life Expectancy", pch=16, col="red")
dev.off()

# ii. lines() - Add lines to plot
png("a2p9.png", width=800, height=600)
plot(df_hpi$`Life Expectancy (years)`, df_hpi$HPI, 
     xlab="Life Expectancy (years)", ylab="Happy Planet Index",
     main="lines() - Adding trend line", pch=16, col="gray")
valid_idx <- !is.na(df_hpi$`Life Expectancy (years)`) & !is.na(df_hpi$HPI)
lines(lowess(df_hpi$`Life Expectancy (years)`[valid_idx], df_hpi$HPI[valid_idx]), 
      col="red", lwd=3)
dev.off()

# iii. points() - Add points to plot
png("a2p10.png", width=800, height=600)
plot(df_hpi$`Carbon Footprint (tCO2e)`, df_hpi$HPI, type="n",
     xlab="Carbon Footprint (tCO2e)", ylab="Happy Planet Index",
     main="points() - Different point styles")
points(df_hpi$`Carbon Footprint (tCO2e)`, df_hpi$HPI, 
       pch=21, bg="lightblue", col="darkblue", cex=1.5)
dev.off()

# iv. axis() - Custom axes
png("a2p11.png", width=800, height=600)
plot.new()
plot.window(xlim=range(df_hpi$`Life Expectancy (years)`, na.rm=TRUE),
            ylim=range(df_hpi$HPI, na.rm=TRUE))
points(df_hpi$`Life Expectancy (years)`, df_hpi$HPI, pch=16, col="purple")
axis(1, at=seq(50, 90, 10), col="blue", col.axis="blue", lwd=2)
axis(2, at=seq(0, 60, 10), col="red", col.axis="red", lwd=2, las=1)
title(main="axis() - Custom colored axes", xlab="Life Expectancy", ylab="HPI")
box()
dev.off()

# v. box() - Draw box around plot
png("a2p12.png", width=800, height=600)
plot(df_hpi$HPI, df_hpi$`GDP per capita ($)`,
     xlab="Happy Planet Index", ylab="GDP per capita ($)",
     main="box() - Custom box styles", pch=16, col="darkgreen")
box(lwd=4, col="orange")
dev.off()

# vi. text() - Add text labels
png("a2p13.png", width=800, height=600)
plot(df_hpi$`Life Expectancy (years)`, df_hpi$HPI,
     xlab="Life Expectancy (years)", ylab="Happy Planet Index",
     main="text() - Labeling top countries", pch=16, col="lightgray")
top_idx <- order(df_hpi$HPI, decreasing=TRUE)[1:10]
text(df_hpi$`Life Expectancy (years)`[top_idx], df_hpi$HPI[top_idx],
     labels=df_hpi$Country[top_idx], pos=4, cex=0.7, col="darkred")
dev.off()

# vii. mtext() - Margin text
png("a2p14.png", width=800, height=600)
par(mar=c(6,6,4,2))
plot(df_hpi$`Carbon Footprint (tCO2e)`, df_hpi$`Ladder of life (Wellbeing) (0-10)`,
     xlab="", ylab="", main="mtext() - Margin text annotations", 
     pch=16, col="steelblue")
mtext("Carbon Footprint (tCO2e)", side=1, line=4, cex=1.3, col="darkgreen")
mtext("Wellbeing Score (0-10)", side=2, line=4, cex=1.3, col="darkblue")
mtext("Data source: Happy Planet Index 2021", side=3, line=0.5, cex=0.8, col="gray40")
mtext("Sustainability Analysis", side=4, line=1, cex=0.9, col="purple")
dev.off()

# viii. hist() - Histogram
png("a2p15.png", width=800, height=600)
hist(df_hpi$HPI, 
     breaks=25, 
     col="lightcoral", 
     border="darkred",
     main="hist() - Distribution of Happy Planet Index",
     xlab="Happy Planet Index",
     ylab="Frequency")
dev.off()

# ix. boxplot() - Box and whisker plot
png("a2p16.png", width=800, height=600)
life_groups <- cut(df_hpi$`Life Expectancy (years)`, 
                   breaks=4, 
                   labels=c("Low", "Medium", "High", "Very High"))
boxplot(df_hpi$HPI ~ life_groups,
        col=c("lightblue", "lightgreen", "lightyellow", "lightcoral"),
        main="boxplot() - HPI by Life Expectancy Groups",
        xlab="Life Expectancy Groups",
        ylab="Happy Planet Index",
        border="darkblue",
        notch=TRUE)
dev.off()

# x. legend() - Add legend
png("a2p17.png", width=800, height=600)
continents <- sort(unique(df_hpi$Continent[!is.na(df_hpi$Continent)]))
colors <- rainbow(length(continents))
plot(df_hpi$`Life Expectancy (years)`, df_hpi$HPI, type="n",
     xlab="Life Expectancy (years)", ylab="Happy Planet Index",
     main="legend() - HPI by Continent")
for(i in 1:length(continents)) {
  idx <- df_hpi$Continent == continents[i] & !is.na(df_hpi$Continent)
  points(df_hpi$`Life Expectancy (years)`[idx], df_hpi$HPI[idx],
         col=colors[i], pch=16, cex=1.2)
}
legend("topleft", legend=continents, col=colors, pch=16, 
       cex=0.8, title="Continents", bg="white")
dev.off()

# xi. persp() - 3D perspective plot
png("a2p18.png", width=800, height=600)
x <- seq(min(df_hpi$`Life Expectancy (years)`, na.rm=TRUE), 
         max(df_hpi$`Life Expectancy (years)`, na.rm=TRUE), length=30)
y <- seq(min(df_hpi$`Ladder of life (Wellbeing) (0-10)`, na.rm=TRUE), 
         max(df_hpi$`Ladder of life (Wellbeing) (0-10)`, na.rm=TRUE), length=30)
z <- outer(x, y, function(x, y) x * y / 100)
persp(x, y, z, theta=45, phi=25, expand=0.6, col="lightblue", shade=0.5,
      xlab="Life Expectancy", ylab="Wellbeing", zlab="Score",
      main="persp() - 3D Surface Plot")
dev.off()

# xii. names() - Extract and use names
png("a2p19.png", width=800, height=600)
top10 <- order(df_hpi$HPI, decreasing=TRUE)[1:10]
top10_hpi <- df_hpi$HPI[top10]
names(top10_hpi) <- df_hpi$Country[top10]
barplot(top10_hpi, las=2, col="skyblue", border="darkblue",
        main="names() - Top 10 Countries by HPI",
        ylab="Happy Planet Index", cex.names=0.8)
dev.off()

# xiii. pie() - Pie chart
png("a2p20.png", width=800, height=600)
continent_counts <- table(df_hpi$Continent)
pie(continent_counts, 
    col=rainbow(length(continent_counts)),
    main="pie() - Distribution of Countries by Continent",
    labels=paste(names(continent_counts), "\n", continent_counts))
dev.off()

print("All 13 plots saved successfully!")
print(paste("Created plots a2p8.png through a2p20.png"))

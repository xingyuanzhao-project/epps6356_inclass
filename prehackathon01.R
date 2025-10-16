Installed <- TRUE  # For checking if package is installed
toInstall <- c("vroom", "finalfit", "tidyverse", "descr", "RColorBrewer", "scales")
if(Installed){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, require, character.only = TRUE) # call into library

# Reading all real time data
# vroom is the champion in reading github date, < 3 sec.
owidall = vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true")

owideu = subset(owidall, continent=="Europe")

# Europe data
y = owideu$new_deaths
x = as.Date(owideu$date)
plot(x,y, pch=20, col="#E7298A", cex = .5, xaxt='n', xlab = "Date", ylab = "COVID Deaths in Europe (Daily)")
axis(1, x, format(x, "%Y-%m"), cex.axis = .7, las = 3 , gap.axis =1.5, tick = FALSE)
identify(x,y,owideu$location, ps=8, atpen=TRUE) # Manually identify cases by mouse click
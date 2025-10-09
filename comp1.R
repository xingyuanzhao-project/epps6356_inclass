if (!require("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

df_hpi_data_raw <- read_excel("HPI.xlsx", sheet = 2, skip = 8)


print(df_hpi_data_raw)
colnames(df_hpi_data_raw)

png("boxplot_life_expectancy.png", width = 800, height = 600)
boxplot(df_hpi_data_raw$`Life Expectancy (years)`, main = "Life Expectancy Boxplot", ylab = "Life Expectancy (years)")
dev.off()

# Assignment 4 - Chart Suggestions

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Read HPI data
df_hpi <- read_excel("HPI.xlsx", sheet = 2, skip = 7)
df_hpi <- df_hpi[!is.na(df_hpi$HPI), ]
df_hpi <- df_hpi[, !names(df_hpi) %in% c("...4")]

# 1. Variable Width Column Chart - Two Variables per Item
# Using HPI (height) and Population (width) for top 10 countries
top10 <- df_hpi %>% arrange(desc(HPI)) %>% head(10)
top10$Rank <- seq_len(nrow(top10))

png("a4p1.png", width = 800, height = 600)
# Among Items → Two Variables per Item
ggplot(
  top10,
  aes(
    # items being compared
    x = reorder(Country, -Rank),
    # first variable per item (encoded as height)
    y = HPI,
    # second variable per item (encoded as width)
    width = `Population (thousands)` / max(`Population (thousands)`)
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    # chart title
    title = "1. Variable Width Column Chart: HPI by Country",
    subtitle = "Bar width represents population size (normalized)",
    # label for items
    x = "Country (Ranked by HPI)",
    # label for first variable
    y = "Happy Planet Index"
  )
dev.off()

# 2. Table or Table with Embedded Charts - Many Categories
# Create academic standard regression summary table
# Among Items → One Variable per Item → Many Categories
model1 <- lm(HPI ~ `Life Expectancy (years)`, data = df_hpi)
model2 <- lm(HPI ~ `GDP per capita ($)`, data = df_hpi)
model3 <- lm(
  HPI ~ `Life Expectancy (years)` + `GDP per capita ($)`,
  data = df_hpi
)

# create academic style table: variables in rows, models in columns
df_regtable <- data.frame(
  Variable = c(
    "Intercept",
    "",
    "Life Expectancy (years)",
    "",
    "GDP per capita ($)",
    "",
    "R-squared",
    "Adj. R-squared",
    "N"
  ),
  Model_1 = c(
    sprintf("%.3f", coef(model1)[1]),
    sprintf("(%.3f)", summary(model1)$coefficients[1, 2]),
    sprintf("%.3f***", coef(model1)[2]),
    sprintf("(%.3f)", summary(model1)$coefficients[2, 2]),
    "",
    "",
    sprintf("%.3f", summary(model1)$r.squared),
    sprintf("%.3f", summary(model1)$adj.r.squared),
    as.character(nrow(df_hpi))
  ),
  Model_2 = c(
    sprintf("%.3f", coef(model2)[1]),
    sprintf("(%.3f)", summary(model2)$coefficients[1, 2]),
    "",
    "",
    sprintf("%.5f***", coef(model2)[2]),
    sprintf("(%.5f)", summary(model2)$coefficients[2, 2]),
    sprintf("%.3f", summary(model2)$r.squared),
    sprintf("%.3f", summary(model2)$adj.r.squared),
    as.character(nrow(df_hpi))
  ),
  Model_3 = c(
    sprintf("%.3f", coef(model3)[1]),
    sprintf("(%.3f)", summary(model3)$coefficients[1, 2]),
    sprintf("%.3f***", coef(model3)[2]),
    sprintf("(%.3f)", summary(model3)$coefficients[2, 2]),
    sprintf("%.5f***", coef(model3)[3]),
    sprintf("(%.5f)", summary(model3)$coefficients[3, 2]),
    sprintf("%.3f", summary(model3)$r.squared),
    sprintf("%.3f", summary(model3)$adj.r.squared),
    as.character(nrow(df_hpi))
  )
)

png("a4p2.png")
# displays regression table
grid.table(df_regtable, rows = NULL)
dev.off()

# 3. Bar Chart - Many Items, Few Categories
# Top 10 and Bottom 10 countries by HPI
# Among Items → One Variable per Item → Few Categories → Many Items
bottom10 <- df_hpi %>% arrange(HPI) %>% head(10)

png("a4p3.png", width = 1200, height = 600)
# creates side-by-side plots (1 row, 2 columns)
par(mfrow = c(1, 2))
# determine common x-axis limits for both plots
xlim_range <- c(0, max(c(top10$HPI, bottom10$HPI)))
barplot(
  # variable being compared
  top10$HPI,
  # many items arranged for comparison
  names.arg = top10$Country,
  # horizontal orientation for readability
  horiz = TRUE,
  # rotates labels horizontally
  las = 1,
  # color for first category
  col = "coral",
  # title for first subplot
  main = "Top 10 Countries by HPI",
  # label for variable
  xlab = "Happy Planet Index",
  # same x-axis scale for both plots
  xlim = xlim_range
)
barplot(
  # variable being compared
  bottom10$HPI,
  # many items arranged for comparison
  names.arg = bottom10$Country,
  # horizontal orientation for readability
  horiz = TRUE,
  # rotates labels horizontally
  las = 1,
  # color for second category
  col = "lightblue",
  # title for second subplot
  main = "Bottom 10 Countries by HPI",
  # label for variable
  xlab = "Happy Planet Index",
  # same x-axis scale for both plots
  xlim = xlim_range
)
dev.off()

# 4. Column Chart - Few Items, Few Categories
# Top 5 countries with grouped bars showing HPI and Life Expectancy
# Among Items → One Variable per Item → Few Categories → Few Items
top5 <- df_hpi %>% arrange(desc(HPI)) %>% head(5)

# scale Life Expectancy to be comparable with HPI
scaling_factor <- 100 / max(top5$`Life Expectancy (years)`)

png("a4p4.png", width = 800, height = 600)
ggplot(top5, aes(x = reorder(Country, HPI))) +
  # HPI bars
  geom_col(
    aes(y = HPI, fill = "HPI"),
    position = position_nudge(x = -0.2),
    width = 0.4
  ) +
  # Life Expectancy bars (scaled)
  geom_col(
    aes(
      y = `Life Expectancy (years)` * scaling_factor,
      fill = "Life Expectancy (years)"
    ),
    position = position_nudge(x = 0.2),
    width = 0.4
  ) +
  # secondary y-axis for Life Expectancy
  scale_y_continuous(
    name = "Happy Planet Index (HPI)",
    sec.axis = sec_axis(
      ~ . / scaling_factor,
      name = "Life Expectancy (years)"
    )
  ) +
  labs(
    # chart title
    title = "4. Grouped Column Chart: Top 5 Countries",
    # subtitle explaining dual axes
    subtitle = "HPI (left axis) and Life Expectancy (right axis)",
    # label for few items
    x = "Country",
    # legend for variables
    fill = "Metric"
  ) +
  # rotates x-axis labels for readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

print("All 4 plots saved as a4p1.png through a4p4.png")

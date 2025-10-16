# Assignment 3 - Anscombe's Quartet with Fine-tuned Visualizations

data(anscombe)

# Create four model objects
lm1 <- lm(y1 ~ x1, data=anscombe)
lm2 <- lm(y2 ~ x2, data=anscombe)
lm3 <- lm(y3 ~ x3, data=anscombe)
lm4 <- lm(y4 ~ x4, data=anscombe)

## Plot 1: Simple version (first simple plot)
png("a3p1.png", width=800, height=600)
par(family = "serif")
plot(anscombe$x1, anscombe$y1,
     pch = 17,
     col = "darkslateblue",
     cex = 1.5,
     xlab = "x1", ylab = "y1",
     main = "Simple Plot - Dataset 1",
     col.main = "navy",
     font.main = 2)
dev.off()

## Plot 2-5: Individual plots with regression lines (fine-tuned)
png("a3p2.png", width=800, height=600)
par(family = "serif")
plot(anscombe$x1, anscombe$y1,
     pch = 17,
     col = "darkslateblue",
     cex = 1.5,
     xlab = "x1", ylab = "y1",
     main = "Dataset 1 with Regression",
     col.main = "navy",
     font.main = 2)
abline(lm1, col = "darkorange3", lwd = 2.5)
dev.off()

png("a3p3.png", width=800, height=600)
par(family = "serif")
plot(anscombe$x2, anscombe$y2,
     pch = 17,
     col = "darkgreen",
     cex = 1.5,
     xlab = "x2", ylab = "y2",
     main = "Dataset 2 with Regression",
     col.main = "navy",
     font.main = 2)
abline(lm2, col = "firebrick", lwd = 2.5)
dev.off()

png("a3p4.png", width=800, height=600)
par(family = "serif")
plot(anscombe$x3, anscombe$y3,
     pch = 17,
     col = "darkorchid",
     cex = 1.5,
     xlab = "x3", ylab = "y3",
     main = "Dataset 3 with Regression",
     col.main = "navy",
     font.main = 2)
abline(lm3, col = "goldenrod3", lwd = 2.5)
dev.off()

png("a3p5.png", width=800, height=600)
par(family = "serif")
plot(anscombe$x4, anscombe$y4,
     pch = 17,
     col = "indianred4",
     cex = 1.5,
     xlab = "x4", ylab = "y4",
     main = "Dataset 4 with Regression",
     col.main = "navy",
     font.main = 2)
abline(lm4, col = "seagreen4", lwd = 2.5)
dev.off()

## Plot 6: Fancy version (2x2 grid) with fine-tuning
png("a3p6.png", width=1000, height=1000)
par(mfrow = c(2, 2), 
    mar = c(4.5, 4.5, 2.5, 1), 
    oma = c(0, 0, 3, 0),
    family = "serif")

# Dataset 1
plot(anscombe$x1, anscombe$y1,
     pch = 17,
     col = "darkslateblue",
     cex = 1.5,
     xlim = c(3, 19), ylim = c(3, 13),
     xlab = "x1", ylab = "y1",
     main = "Dataset 1",
     col.main = "navy",
     col.lab = "darkblue",
     font.main = 2)
abline(lm1, col = "darkorange3", lwd = 2.5)

# Dataset 2
plot(anscombe$x2, anscombe$y2,
     pch = 17,
     col = "darkgreen",
     cex = 1.5,
     xlim = c(3, 19), ylim = c(3, 13),
     xlab = "x2", ylab = "y2",
     main = "Dataset 2",
     col.main = "navy",
     col.lab = "darkblue",
     font.main = 2)
abline(lm2, col = "firebrick", lwd = 2.5)

# Dataset 3
plot(anscombe$x3, anscombe$y3,
     pch = 17,
     col = "darkorchid",
     cex = 1.5,
     xlim = c(3, 19), ylim = c(3, 13),
     xlab = "x3", ylab = "y3",
     main = "Dataset 3",
     col.main = "navy",
     col.lab = "darkblue",
     font.main = 2)
abline(lm3, col = "goldenrod3", lwd = 2.5)

# Dataset 4
plot(anscombe$x4, anscombe$y4,
     pch = 17,
     col = "indianred4",
     cex = 1.5,
     xlim = c(3, 19), ylim = c(3, 13),
     xlab = "x4", ylab = "y4",
     main = "Dataset 4",
     col.main = "navy",
     col.lab = "darkblue",
     font.main = 2)
abline(lm4, col = "seagreen4", lwd = 2.5)

mtext("Anscombe's Quartet (Base R - Fine-tuned)", 
      outer = TRUE, cex = 1.8, font = 2, col = "navy")

dev.off()


## ggplot2 Version
library(tidyverse)

# Reshape data for ggplot
anscombe_long <- anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "dataset"),
               names_pattern = "(.)(.)")

png("a3p7.png", width=1000, height=1000)

ggplot(anscombe_long, aes(x = x, y = y, color = dataset)) +
  geom_point(size = 3, shape = 17) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  facet_wrap(~dataset, ncol = 2, 
             labeller = labeller(dataset = c("1" = "Dataset 1", 
                                             "2" = "Dataset 2",
                                             "3" = "Dataset 3", 
                                             "4" = "Dataset 4"))) +
  scale_color_manual(values = c("1" = "darkslateblue", 
                                "2" = "darkgreen",
                                "3" = "darkorchid", 
                                "4" = "indianred4")) +
  labs(title = "Anscombe's Quartet (ggplot2)",
       x = "x values",
       y = "y values") +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "navy"),
    strip.text = element_text(size = 12, face = "bold", color = "navy"),
    axis.title = element_text(size = 12, face = "bold", color = "darkblue"),
    legend.position = "none"
  )

dev.off()

print("Created plots a3p1.png through a3p7.png")

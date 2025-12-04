# Assignment 5 - Data Visualization
# Author: Xingyuan Zhao
# Course: EPPS6356

# Load required library for task 2
library(ggplot2)

# Read the CFR country total data
cfr_data <- read.csv("CFR_country_total_with_ISO2 (1).csv")

# Remove Grand Total and blank rows
cfr_data <- cfr_data[cfr_data$iso3 != "Grand Total" & cfr_data$iso3 != "(blank)", ]

# ==============================================================================
# TASK 1: Base R Graphics (No Packages)
# ==============================================================================

# 1a. Histogram
hist(cfr_data$total_attacks,
     main = "Distribution of Cyber Attacks by Country",
     xlab = "Number of Attacks",
     ylab = "Frequency",
     col = "#4A90E2",
     border = "white",
     breaks = 20,
     las = 1,
     cex.main = 1.4,
     cex.lab = 1.2,
     font.main = 2)

# 1b.i. Barchart - Vertical (Top 15 countries)
top15 <- head(cfr_data[order(-cfr_data$total_attacks), ], 15)
barplot(top15$total_attacks,
        names.arg = top15$ISO2,
        main = "Top 15 Countries by Cyber Attacks",
        xlab = "Country (ISO2 Code)",
        ylab = "Number of Attacks",
        col = "#E74C3C",
        border = "white",
        las = 1,
        cex.names = 0.9,
        cex.main = 1.4,
        cex.lab = 1.2,
        font.main = 2)

# 1b.ii. Barchart - Horizontal (Top 10 countries)
top10 <- head(cfr_data[order(-cfr_data$total_attacks), ], 10)
barplot(top10$total_attacks,
        names.arg = top10$ISO2,
        main = "Top 10 Countries by Cyber Attacks",
        xlab = "Number of Attacks",
        ylab = "Country (ISO2 Code)",
        col = "#2ECC71",
        border = "white",
        horiz = TRUE,
        las = 1,
        cex.names = 0.9,
        cex.main = 1.4,
        cex.lab = 1.2,
        font.main = 2)

# 1c. Piechart (Top 8 countries + Others)
top8 <- head(cfr_data[order(-cfr_data$total_attacks), ], 8)
others_total <- sum(cfr_data$total_attacks) - sum(top8$total_attacks)
pie_data <- c(top8$total_attacks, others_total)
pie_labels <- c(top8$ISO2, "Others")
pie_colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", 
                "#9B59B6", "#1ABC9C", "#E67E22", "#34495E", "#95A5A6")

pie(pie_data,
    labels = pie_labels,
    main = "Cyber Attacks Distribution by Country",
    col = pie_colors,
    border = "white",
    cex = 1.1,
    cex.main = 1.4,
    font.main = 2)

# 1d. Boxplot
boxplot(cfr_data$total_attacks,
        main = "Distribution of Cyber Attacks Across Countries",
        ylab = "Number of Attacks",
        col = "#9B59B6",
        border = "#34495E",
        pch = 19,
        cex = 1.2,
        las = 1,
        cex.main = 1.4,
        cex.lab = 1.2,
        font.main = 2,
        outcol = "#E74C3C",
        outbg = "#E74C3C")

# 1e. Scatterplot
plot(cfr_data$total_attacks,
     main = "Cyber Attacks by Country Index",
     xlab = "Country Index",
     ylab = "Number of Attacks",
     pch = 19,
     col = "#3498DB",
     cex = 1.2,
     las = 1,
     cex.main = 1.4,
     cex.lab = 1.2,
     font.main = 2)
grid(col = "gray80", lty = 2)

# ==============================================================================
# TASK 2: ggplot2 Graphics
# ==============================================================================

# 2a. Histogram - ggplot2
p1 <- ggplot(cfr_data, aes(x = total_attacks)) +
  geom_histogram(bins = 20, fill = "#4A90E2", color = "white", alpha = 0.9) +
  labs(title = "Distribution of Cyber Attacks by Country",
       x = "Number of Attacks",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())
print(p1)

# 2b.i. Barchart Vertical - ggplot2 (Top 15 countries)
p2 <- ggplot(top15, aes(x = reorder(ISO2, -total_attacks), y = total_attacks)) +
  geom_bar(stat = "identity", fill = "#E74C3C", color = "white", width = 0.8) +
  labs(title = "Top 15 Countries by Cyber Attacks",
       x = "Country (ISO2 Code)",
       y = "Number of Attacks") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())
print(p2)

# 2b.ii. Barchart Horizontal - ggplot2 (Top 10 countries)
p3 <- ggplot(top10, aes(x = total_attacks, y = reorder(ISO2, total_attacks))) +
  geom_bar(stat = "identity", fill = "#2ECC71", color = "white", width = 0.8) +
  labs(title = "Top 10 Countries by Cyber Attacks",
       x = "Number of Attacks",
       y = "Country (ISO2 Code)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())
print(p3)

# 2c. Piechart - ggplot2 (Top 8 countries + Others)
pie_df <- data.frame(
  category = pie_labels,
  value = pie_data
)
pie_df$percentage <- round(pie_df$value / sum(pie_df$value) * 100, 1)

p4 <- ggplot(pie_df, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pie_colors) +
  labs(title = "Cyber Attacks Distribution by Country",
       fill = "Country") +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.text = element_text(size = 10))
print(p4)

# 2d. Boxplot - ggplot2
p5 <- ggplot(cfr_data, aes(x = "", y = total_attacks)) +
  geom_boxplot(fill = "#9B59B6", color = "#34495E", alpha = 0.9, outlier.color = "#E74C3C", outlier.size = 2) +
  labs(title = "Distribution of Cyber Attacks Across Countries",
       x = "",
       y = "Number of Attacks") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank())
print(p5)

# 2e. Scatterplot - ggplot2
cfr_data$index <- seq_len(nrow(cfr_data))
p6 <- ggplot(cfr_data, aes(x = index, y = total_attacks)) +
  geom_point(color = "#3498DB", size = 3, alpha = 0.8) +
  labs(title = "Cyber Attacks by Country Index",
       x = "Country Index",
       y = "Number of Attacks") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"))
print(p6)

# ==============================================================================
# TASK 3: Export Charts in Different Formats
# ==============================================================================

# Define export formats
formats <- c("pdf", "jpg", "svg", "tiff", "bmp")

# Function to save base R plots
save_base_plot <- function(plot_func, filename) {
  for (fmt in formats) {
    filepath <- paste0("plots/", filename, ".", fmt)
    
    if (fmt == "pdf") {
      pdf(filepath, width = 8, height = 6)
    } else if (fmt == "jpg") {
      jpeg(filepath, width = 800, height = 600, quality = 95)
    } else if (fmt == "svg") {
      svg(filepath, width = 8, height = 6)
    } else if (fmt == "tiff") {
      tiff(filepath, width = 800, height = 600, compression = "lzw")
    } else if (fmt == "bmp") {
      bmp(filepath, width = 800, height = 600)
    }
    
    plot_func()
    dev.off()
  }
  cat("Saved:", filename, "in all formats\n")
}

# Function to save ggplot2 plots
save_ggplot <- function(plot_obj, filename) {
  for (fmt in formats) {
    filepath <- paste0("plots/", filename, ".", fmt)
    ggsave(filepath, plot = plot_obj, width = 8, height = 6, dpi = 300)
  }
  cat("Saved:", filename, "in all formats\n")
}

# Save Task 1 plots (Base R)
cat("\nSaving Task 1 plots (Base R)...\n")

# a5p1a - Histogram
save_base_plot(function() {
  hist(cfr_data$total_attacks,
       main = "Distribution of Cyber Attacks by Country",
       xlab = "Number of Attacks",
       ylab = "Frequency",
       col = "#4A90E2",
       border = "white",
       breaks = 20,
       las = 1,
       cex.main = 1.4,
       cex.lab = 1.2,
       font.main = 2)
}, "a5p1a")

# a5p1bi - Barchart Vertical
save_base_plot(function() {
  barplot(top15$total_attacks,
          names.arg = top15$ISO2,
          main = "Top 15 Countries by Cyber Attacks",
          xlab = "Country (ISO2 Code)",
          ylab = "Number of Attacks",
          col = "#E74C3C",
          border = "white",
          las = 1,
          cex.names = 0.9,
          cex.main = 1.4,
          cex.lab = 1.2,
          font.main = 2)
}, "a5p1bi")

# a5p1bii - Barchart Horizontal
save_base_plot(function() {
  barplot(top10$total_attacks,
          names.arg = top10$ISO2,
          main = "Top 10 Countries by Cyber Attacks",
          xlab = "Number of Attacks",
          ylab = "Country (ISO2 Code)",
          col = "#2ECC71",
          border = "white",
          horiz = TRUE,
          las = 1,
          cex.names = 0.9,
          cex.main = 1.4,
          cex.lab = 1.2,
          font.main = 2)
}, "a5p1bii")

# a5p1c - Piechart
save_base_plot(function() {
  pie(pie_data,
      labels = pie_labels,
      main = "Cyber Attacks Distribution by Country",
      col = pie_colors,
      border = "white",
      cex = 1.1,
      cex.main = 1.4,
      font.main = 2)
}, "a5p1c")

# a5p1d - Boxplot
save_base_plot(function() {
  boxplot(cfr_data$total_attacks,
          main = "Distribution of Cyber Attacks Across Countries",
          ylab = "Number of Attacks",
          col = "#9B59B6",
          border = "#34495E",
          pch = 19,
          cex = 1.2,
          las = 1,
          cex.main = 1.4,
          cex.lab = 1.2,
          font.main = 2,
          outcol = "#E74C3C",
          outbg = "#E74C3C")
}, "a5p1d")

# a5p1e - Scatterplot
save_base_plot(function() {
  plot(cfr_data$total_attacks,
       main = "Cyber Attacks by Country Index",
       xlab = "Country Index",
       ylab = "Number of Attacks",
       pch = 19,
       col = "#3498DB",
       cex = 1.2,
       las = 1,
       cex.main = 1.4,
       cex.lab = 1.2,
       font.main = 2)
  grid(col = "gray80", lty = 2)
}, "a5p1e")

# Save Task 2 plots (ggplot2)
cat("\nSaving Task 2 plots (ggplot2)...\n")

# a5p2a - Histogram
save_ggplot(p1, "a5p2a")

# a5p2bi - Barchart Vertical
save_ggplot(p2, "a5p2bi")

# a5p2bii - Barchart Horizontal
save_ggplot(p3, "a5p2bii")

# a5p2c - Piechart
save_ggplot(p4, "a5p2c")

# a5p2d - Boxplot
save_ggplot(p5, "a5p2d")

# a5p2e - Scatterplot
save_ggplot(p6, "a5p2e")

cat("\n=============================================================\n")
cat("All tasks completed successfully!\n")
cat("=============================================================\n")
cat("\nFile Format Notes:\n")
cat("- PDF: Vector format, scalable, best for printing and publications\n")
cat("- JPG: Raster format, compressed, smaller file size, lossy compression\n")
cat("- SVG: Vector format, scalable, editable, best for web and graphics software\n")
cat("- TIFF: Raster format, lossless compression, high quality, larger file size\n")
cat("- BMP: Raster format, uncompressed, very large file size, highest quality\n")
cat("=============================================================\n")


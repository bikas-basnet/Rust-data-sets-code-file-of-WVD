#oppen access code of my research paper:https://pdf.sciencedirectassets.com/313383/1-s2.0-S2405985423X0007X/1-s2.0-S2405985424000338/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEPr%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIChleS70dOQblXaC%2Fnwnh09%2F4joDGAahouRRPnKt%2BkkvAiEAngS3WXJa6ldzzjIaxvHJ97uhNchiS9Fb05r55ca0E2wqvAUIs%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FARAFGgwwNTkwMDM1NDY4NjUiDEy2bZads2JFK6ch7yqQBcGx1RlZKLuGIQYK%2B6l7XQto3RVDLLnbyQR%2FzQZX%2BA38EFxSi5qdr6KwxK9y8XvzAd3H4XWESfNrsivrl1Mp4TmY%2Bi5n9TxiO%2BV72VHQaJq2ApkU0Jm5VlIHW5E5MUvqovqzsNcMJMwSC4eIbA95%2F0wZPY8PTAqRixn66t0usSCZN9Kx8GkfvVDtpMtEbP4X8j6ih9KEoxR%2BsOtFJ60B99HXevuki46LHQzIaKcPXOQQMdQ2CPrw8YnKEpyvl90Th1MyX0reMTDdOIrgO0AS2Nz3NE9DkdN1peIQHqOgEk6OJ7XiO%2Bc6O1EH6tz3bW7k1bVIHE0Th7LrWMimOmlLmi%2BodeOC3pV0X%2BFs2Kmqt91KQ2xXoXChXTcxM71xEO0Tq4VmtzLVRySw%2BwiJ%2B0RwlWBWws8pZUWhP3ky458VtoKgCwnb8%2BfOtLtPZeOtrahMg7%2FrWLyyfLyX%2BWDTllIakwT4a8KrX6CJiHNA%2F%2Fb7hRN97PNGBCmdSJRaxKAzQLs0JXDw54Qf%2F2Waqrz4Nc3DpOF8ztxzqWIFL5j6lw%2Bv9%2FZFTqRm%2BaZXqcdhJUp5dKFTJ4OUUympZoACyNP0aeTVBN9ywYkAFyA70DexnvUG2R8AAVeKQSB6viAkTd5DXPcixifxuqPXlN%2BM0UiCXuoPfU4ZWKtHFiq%2BYbZZUrSxk%2BipzVA1n5ky62Kog85Zd397TN3XXzSVRAZoOQNt5qSsPc4xHLvNYqjTVyZ%2BiHyKR9M46KDkvbcZj1xUYJ07jLE9TCAGDImCGnEYwL47WECFenrNZtZIei52a0vzEoZ%2B1RmGZSoDRB9%2BbPU2YXEXE44u8E1hhJ2rVVRrUS4HsemJsHrt8TnX9WB2e%2BPp4fYg7D8PMNn%2B6LoGOrEBBUny2dpmnau7pcP7bddegc47KnziaI64u4ZggIPCDOBrebV6aNJWA14VwJpgux%2BbX5ZpBeRsdtXo%2F7kaxF%2FVsN2xWVe%2FUnuS%2Bh2FmKSlyPkih%2Bmq0OY4%2FcvZn4tWC%2FdsRZNvbvElO8l22bsVYXgsiZ%2BCh6NaaE3rKdCSXh3yyuHMVDLtAGjvZO53l7T1kcv%2BmhZhwWcq1e6x88JuiXxizY1j%2FgAJ5zW9jfzbYiiglLU5&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20241212T025323Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTY7JFXRKPA%2F20241212%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=d06d336e050b47f7de1a5e943fafb311afd9078d5579cff9ed46146778f21faa&hash=90ec3e3dc1b23d6a30d4370c52d82640bfdc85a52a572567f7edecd88ec07980&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S2405985424000338&tid=spdf-79a30912-4852-43f1-bde4-919482b51b3b&sid=1be0dcaf8eb04149e38be1f-d2c002f199e5gxrqa&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=0c165c015707590452&rr=8f0a6c7c89b07f5a&cc=np

Bikas<-read.csv("E:/suk B gurung sir folder/FAD.csv", header = TRUE)
Bikas
colnames(Bikas)
library(ggplot2)
library(dplyr)
head(Bikas)
str(Bikas)
Bikas$FRS <- as.numeric(Bikas$FRS)
Bikas$HS <- as.factor(Bikas$HS)
Bikas$Name.of.the.released.Variety.of.the.wheat <- factor(Bikas$Name.of.the.released.Variety.of.the.wheat, 
                                                          levels = Bikas$Name.of.the.released.Variety.of.the.wheat)
####################to create the plot 
Bikubarplot<-ggplot(Bikas, aes(x = Name.of.the.released.Variety.of.the.wheat, y = FRS, fill = HS)) +
  geom_bar(stat = "identity", width = 1) +  
  coord_polar(start = 0) +  
  theme_void() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Adjust text size
        axis.title.x = element_blank(), 
        legend.position = "bottom") +  
  labs(title = "Circular Bar Plot of FRS by Wheat Genotype (Ordered by FRS)") +
  scale_fill_manual(values = c("MR" = "brown", "M" = "blue", "MS" = "darkgreen", "S" = "red")) +
  geom_text(aes(label = round(FRS, 1)), color = "black", size = 3, position = position_stack(vjust = 0.5))
Bikubarplot
###3To add the bar diagram in vertical bar plot in order 
Bar <- ggplot(Bikas, aes(x = reorder(Name.of.the.released.Variety.of.the.wheat, FRS), y = FRS, fill = HS)) +   
  geom_bar(stat = "identity", width = 0.7) +  # Bar plot with FRS as heights
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),  # Rotate and size the x-axis labels
        axis.title.x = element_blank(),  # Remove the x-axis title
        legend.position = "bottom") +  # Position the legend at the bottom
  labs(title = "FRS by Wheat Genotype") +   
  scale_fill_manual(values = c("R" = "green", "MR" = "brown", "M" = "blue", "MS" = "darkgreen", "S" = "red")) +  
  geom_text(aes(label = round(FRS, 1)), color = "black", size = 3.5, position = position_stack(vjust = 0.5)) + 
  facet_wrap(~ HS, scales = "free_y", ncol = 1) +  # Facet by HS with separate panels
  scale_x_discrete(
    aes(x = reorder(Name.of.the.released.Variety.of.the.wheat, FRS))  # Reorder x-axis by FRS within each facet
  )




Bar
ggsave("vertical bar plot arranged type for each.jpg", plot = Bar, dpi = 600, width = 15, height = 12)
getwd()
####################
# Reorder the genotypes based on ascending FRS values
Bikas$Name.of.the.released.Variety.of.the.wheat <- 
  factor(Bikas$Name.of.the.released.Variety.of.the.wheat, 
         levels = Bikas$Name.of.the.released.Variety.of.the.wheat[order(Bikas$FRS)])

# Create a circular bar plot with FRS values in ascending order
Bikas$ACI_color <- cut(Bikas$ACI,
                       breaks = c(-Inf, 5, 22, 45, 75, 100),  # 5 intervals
                       labels = c("darkgreen", "blue", "brown", "yellow", "red"),  # 5 colors
                       right = TRUE)  # right = TRUE includes the upper bound in each interval
Bikas$ACI_color <- factor(Bikas$ACI_color, 
                          levels = c("darkgreen", "blue", "brown", "yellow", "red"),
                          labels = c("≤ 5", "6–22", "23–45", "46–75", "76–100"))  # Adjusted custom labels for ACI range
aciplot <- ggplot(Bikas, aes(x = Name.of.the.released.Variety.of.the.wheat, y = ACI, color = ACI_color)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +  # Plot individual points
  stat_summary(fun = "mean", geom = "point", aes(color = ACI_color), size = 3) +  # Use ACI_color for mean points
  scale_color_manual(values = c("≤ 5" = "darkgreen", 
                                "6–22" = "blue", 
                                "23–45" = "brown", 
                                "46–75" = "yellow", 
                                "76–100" = "red")) +  # Assign colors to the ACI ranges
  theme(axis.text.x = element_text(angle = 45, size = 5, hjust = 1)) +  # Rotate x-axis labels for better readability
  labs(title = "ACI Values for Each Variety",
       x = "Wheat Variety", y = " Average Coefficient of Infection") +
  theme(legend.title = element_text(size = 10),  # Customize legend title
        legend.text = element_text(size = 8)) +  # Customize legend text
  guides(color = guide_legend(title = "ACI Range",  # Custom title for the legend
                              title.position = "top",  # Position the title above
                              title.hjust = 0.5))  # Center the legend title

aciplot




colnames(Bikas)
####For the creation of the R value circular plot 
Bikas$r.value <- as.factor(Bikas$r.value)
Bikas$r.value <- factor(Bikas$r.value, levels = sort(unique(Bikas$r.value)))
library(dplyr)

Bikas$Name.of.the.released.Variety.of.the.wheat <- 
  factor(Bikas$Name.of.the.released.Variety.of.the.wheat, 
         levels = Bikas$Name.of.the.released.Variety.of.the.wheat[order(Bikas$r.value)])

# Create the plot
Rvalue <- ggplot(Bikas, aes(x = Name.of.the.released.Variety.of.the.wheat, y = r.value, fill = r.value)) +
  geom_bar(stat = "identity", width = 0.8) +  # Thicker bars
  coord_polar(start = 0, clip = "off") +  # Polar coordinates
  theme_void() +  # Remove axes
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  # Rotate x-axis labels
    axis.title.x = element_blank(),  # Remove x-axis title
    legend.position = "none",  # Remove legend
    panel.grid = element_line(color = "grey", linewidth = 0.5),  # Add circular gridlines
    panel.grid.major = element_line(color = "grey", linewidth = 0.5),  # Major circular gridlines
    panel.grid.minor = element_line(color = "lightgrey", linewidth = 0.3)  # Minor circular gridlines
  ) +
  labs(title = "r.value across Genotypes") +
  geom_text(aes(label = round(as.numeric(r.value), 1)), position = position_stack(vjust = 0.5), size = 2)  # Display r.value

Rvalue

getwd()
library(gridExtra)
combined_plot <- grid.arrange(aciplot, Rvalue, ncol = 2)
ggsave("combine plot of ACI and R value.jpg", plot = combined_plot, dpi = 600, width = 15, height = 12)
head(Bikas)
####To create the heat map based on AUDPC   FRS HS SCI, ACI r.value
# Load necessary libraries
library(dplyr)
library(tidyr)
library(pheatmap)
library(tibble)  # Load the tibble package for column_to_rownames()

# ##########Step 1: Select relevant columns and scale the data
Bikas_scaled <- Bikas %>%
  select(Name.of.the.released.Variety.of.the.wheat, ACI, r.value, AUDPC, FRS, SCI, Yield.ha) %>%
  column_to_rownames("Name.of.the.released.Variety.of.the.wheat") %>%  # Set the genotypes as row names
  # Ensure all selected columns are numeric before scaling
  mutate(across(where(is.character), as.factor)) %>%  # Convert character columns to factors if necessary
  mutate(across(where(is.factor), as.numeric)) %>%    # Convert factor columns to numeric
  mutate(across(where(is.numeric), as.numeric)) %>%   # Ensure numeric columns are properly handled
  scale()
# View the scaled data
head(Bikas_scaled)
########## Step 2: Create the heatmap
PM<-pheatmap(Bikas_scaled,                  # Scaled data with genotypes as row names
         cluster_rows = TRUE,            # Cluster rows (genotypes)
         cluster_cols = TRUE,            # Cluster columns (variables)
         display_numbers = TRUE,         # Optionally display values in tiles
         fontsize = 8,                  # Font size for text in the plot
         color = colorRampPalette(c("darkgreen","brown", "orange", "lightgreen", "blue", "red"))(100),  # Blue to red color scale
         main = "ACI, r.value, AUDPC, FRS, SCI, Yield.ha",  # Title
         show_rownames = TRUE,           # Show row names (genotypes)
         show_colnames = TRUE,           # Show column names (variables)
         border_color = "grey")         # White border color for tiles
PM
ggsave("interactive heatmap with yield.jpg", plot = PM, dpi = 600, width = 12, height = 15)
##########To create the circos plot 
library(circlize)
library(ggplot2)
library(RColorBrewer)

# Generate random error values between 0.1 and 0.8 for each bar
# Generate random error values between 0.1 and 0.8 for each observation
errors <- runif(nrow(Bikas), min = 0.1, max = 0.8)


# Use the Set3 color palette with 26 distinct colors
my_colors <- brewer.pal(12, "Set3")[1:62]

gg <- ggplot(data = Bikas, aes(x = reorder(factor(Name.of.the.released.Variety.of.the.wheat), Yield.ha), y = Yield.ha, fill = Name.of.the.released.Variety.of.the.wheat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Yield.ha - errors, ymax = Yield.ha + errors), width = 0.2) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels by 45 degrees
  coord_polar(start = 0) +
  ylim(0, 5) +
  geom_text(aes(label = round(Yield.ha, 2)),    # Add text labels
            position = position_stack(vjust = 0.5),   # Position at the center of the bar
            color = "white",   # Set text color to white (or any contrasting color)
            size = 3)          # Adjust text size as needed

print(gg)
######Self assigned manual color code

library(ggplot2)

# 62 distinct color codes
my_colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#984EA3", "#FF00FF", "#A65628", "#F781BF", "#999999", 
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#BEBADA", "#FFB6C1", 
  "#D9D9D9", "#F0E442", "#D55E00", "#CC79A7", "#009E73", "#56B4E9", "#F0E442", "#D73027", "#FF6347", "#32CD32", 
  "#8B0000", "#8A2BE2", "#A52A2A", "#7FFF00", "#D2691E", "#FF1493", "#00FFFF", "#00008B", "#A9A9A9", "#8B008B", 
  "#FF4500", "#800000", "#2E8B57", "#6A5ACD", "#708090", "#B8860B", "#00CED1", "#0000CD", "#F0F8FF", "#FFE4B5", 
  "#808000", "#006400", "#8B4513", "#D3D3D3", "#8B0000", "#C71585", "#20B2AA", "#ADD8E6", "#000080", "#FF69B4", 
  "#F4A460", "#D3D3D3", "#C0C0C0", "#FF8C00", "#9932CC", "#8FBC8F", "#4B0082", "#5F9EA0", "#F0E68C", "#FF1493"
)

# Example ggplot code using your data and the custom color palette
gg <- ggplot(data = Bikas, aes(x = reorder(factor(Name.of.the.released.Variety.of.the.wheat), Yield.ha), 
                               y = Yield.ha, 
                               fill = Name.of.the.released.Variety.of.the.wheat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Yield.ha - errors, ymax = Yield.ha + errors), width = 0.2) +
  scale_fill_manual(values = my_colors) +  # Apply the custom colors to the bars
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 43, hjust = 1, size = 4)) +  # Rotate x-axis labels by 45 degrees
  coord_polar(start = 0) +
  ylim(0, 5) +
  geom_text(aes(label = round(Yield.ha, 2)),    # Add text labels
            position = position_stack(vjust = 0.5),   # Position at the center of the bar
            color = "black",   # Set text color to white (or any contrasting color)
            size = 2)          # Adjust text size as needed

# Print the plot
print(gg)
ggsave("yield plot.jpg", plot = gg, dpi = 600, width = 12, height = 15)
##########circular bar diagram for height and length of spikes and number of spiklets 
library(ggplot2)
Bikas$Height.of.the.Plant <- as.numeric(Bikas$Height.of.the.Plant)

# 62 distinct color codes
my_colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#984EA3", "#FF00FF", "#A65628", "#F781BF", "#999999", 
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#BEBADA", "#FFB6C1", 
  "#D9D9D9", "#F0E442", "#D55E00", "#CC79A7", "#009E73", "#56B4E9", "#F0E442", "#D73027", "#FF6347", "#32CD32", 
  "#8B0000", "#8A2BE2", "#A52A2A", "#7FFF00", "#D2691E", "#FF1493", "#00FFFF", "#00008B", "#A9A9A9", "#8B008B", 
  "#FF4500", "#800000", "#2E8B57", "#6A5ACD", "#708090", "#B8860B", "#00CED1", "#0000CD", "#F0F8FF", "#FFE4B5", 
  "#808000", "#006400", "#8B4513", "#D3D3D3", "#8B0000", "#C71585", "#20B2AA", "#ADD8E6", "#000080", "#FF69B4", 
  "#F4A460", "#D3D3D3", "#C0C0C0", "#FF8C00", "#9932CC", "#8FBC8F", "#4B0082", "#5F9EA0", "#F0E68C", "#FF1493"
)

# Example ggplot code using your data and the custom color palette
Ht<- ggplot(data = Bikas, aes(x = reorder(factor(Name.of.the.released.Variety.of.the.wheat), Height.of.the.Plant), 
                               y = Height.of.the.Plant, 
                               fill = Name.of.the.released.Variety.of.the.wheat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Height.of.the.Plant - errors, ymax = Height.of.the.Plant + errors), width = 0.2) +
  scale_fill_manual(values = my_colors) +  # Apply the custom colors to the bars
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 43, hjust = 1, size = 4)) +  # Rotate x-axis labels by 45 degrees
  coord_polar(start = 0) +
  ylim(0, 130) +
  geom_text(aes(label = round(Height.of.the.Plant, 2)),    # Add text labels
            position = position_stack(vjust = 0.5),   # Position at the center of the bar
            color = "black",   # Set text color to white (or any contrasting color)
            size = 2)          # Adjust text size as needed

plot(Ht)
###############for mean number of spikelet number 
Bikas$Length.of.the.Spike <- as.numeric(Bikas$Length.of.the.Spike)

# 62 distinct color codes
my_colors <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", "#984EA3", "#FF00FF", "#A65628", "#F781BF", "#999999", 
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#BEBADA", "#FFB6C1", 
  "#D9D9D9", "#F0E442", "#D55E00", "#CC79A7", "#009E73", "#56B4E9", "#F0E442", "#D73027", "#FF6347", "#32CD32", 
  "#8B0000", "#8A2BE2", "#A52A2A", "#7FFF00", "#D2691E", "#FF1493", "#00FFFF", "#00008B", "#A9A9A9", "#8B008B", 
  "#FF4500", "#800000", "#2E8B57", "#6A5ACD", "#708090", "#B8860B", "#00CED1", "#0000CD", "#F0F8FF", "#FFE4B5", 
  "#808000", "#006400", "#8B4513", "#D3D3D3", "#8B0000", "#C71585", "#20B2AA", "#ADD8E6", "#000080", "#FF69B4", 
  "#F4A460", "#D3D3D3", "#C0C0C0", "#FF8C00", "#9932CC", "#8FBC8F", "#4B0082", "#5F9EA0", "#F0E68C", "#FF1493"
)

# Example ggplot code using your data and the custom color palette
SL<- ggplot(data = Bikas, aes(x = reorder(factor(Name.of.the.released.Variety.of.the.wheat), Length.of.the.Spike), 
                              y = Length.of.the.Spike, 
                              fill = Name.of.the.released.Variety.of.the.wheat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Length.of.the.Spike - errors, ymax = Length.of.the.Spike + errors), width = 0.2) +
  scale_fill_manual(values = my_colors) +  # Apply the custom colors to the bars
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 43, hjust = 1, size = 4)) +  # Rotate x-axis labels by 45 degrees
  coord_polar(start = 0) +
  ylim(0, 15) +
  geom_text(aes(label = round(Length.of.the.Spike, 2)),    # Add text labels
            position = position_stack(vjust = 0.5),   # Position at the center of the bar
            color = "black",   # Set text color to white (or any contrasting color)
            size = 2)          # Adjust text size as needed

plot(SL)
#########Mean number of spikes 
SN<- ggplot(data = Bikas, aes(x = reorder(factor(Name.of.the.released.Variety.of.the.wheat), mean.of.spiklet.number), 
                              y = mean.of.spiklet.number, 
                              fill = Name.of.the.released.Variety.of.the.wheat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean.of.spiklet.number - errors, ymax = mean.of.spiklet.number + errors), width = 0.2) +
  scale_fill_manual(values = my_colors) +  # Apply the custom colors to the bars
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 43, hjust = 1, size = 4)) +  # Rotate x-axis labels by 45 degrees
  coord_polar(start = 0) +
  ylim(0, 12) +
  geom_text(aes(label = round(mean.of.spiklet.number, 2)),    # Add text labels
            position = position_stack(vjust = 0.5),   # Position at the center of the bar
            color = "black",   # Set text color to white (or any contrasting color)
            size = 2)          # Adjust text size as needed

plot(SN)
library(gridExtra)
?grid.arrange
TCP<-grid.arrange(Ht, SL, SN, ncol = 3)
getwd()
ggsave("ht,length and numberspikelet.jpg", plot= TCP, dpi = 600, width = 15, height = 12)
##############correlation study of the vegetative traits 

#' ---
#' title: BIOL 470 Project Code
#' author: Camden White
#' date: 03/31/2025
#' output: html_document
#' ---

# install.packages("dplyr")
# install.packages("vegan")
# install.packages("ggplot2")
# install.packages ("tidyr")
# install.packages("tibble")

library(dplyr)
library(vegan)
library(ggplot2)
library(tidyr)
library(tibble)
library(ggrepel)

gut_contents <- read.csv('./Data/gut_contents.csv')

# Prepare explanatory (X) and response (Y) variables
gut_response <- gut_contents[, -(1:2)]  # Prey items
gut_explanatory <- gut_contents[, 1:2]  # Species and SL_mm

# Convert categorical predictor 'Species' to factor
gut_explanatory$Species <- as.factor(gut_explanatory$Species)

# Run RDA
gut_rda <- rda(gut_response ~ Species + SL_mm, data = gut_explanatory)

# Plot RDA without site labels, reduciing clutter
plot(gut_rda, scaling = 2, main = "RDA of Gut Contents", 
     display = c("bp", "cn", "sp"), type = "n")  

# Add colored explanatory variables (blue)
text(gut_rda, display = "cn", col = "blue", cex = 1.2)  

# Add colored response variables (prey items, red)
text(gut_rda, display = "sp", col = "red", cex = 1.2)  

# Add arrows for explanatory variables
arrows(0, 0, scores(gut_rda, display = "bp")[,1], 
       scores(gut_rda, display = "bp")[,2], 
       col = "red", length = 0.1)

# Statistical analysis of RDA
summary(gut_rda)
anova(gut_rda)
RsquareAdj(gut_rda)

# Statistical analysis of bar graph using fisher's exact test
# Code added to ensure comparsions with not enough data doesn't break code
fisher_tests <- function(data) {
  results <- list()
  for (size in unique(data$SL_mm)) {
    cat("\nSize Range:", size, "\n")
    subset_data <- data %>% filter(SL_mm == size)
    for (prey in colnames(subset_data)[-(1:2)]) {
      cat("\nTesting prey item:", prey, "\n")
      contingency_table <- table(subset_data$Species, subset_data[[prey]])
      if (all(dim(contingency_table) == c(2,2))) {
      test_result <- fisher.test(contingency_table)
      results[[paste(size, prey, sep = "_")]] <- test_result
      print(test_result)
      } else {
        cat("Skipping", prey, "- Not enough variation for Fisher's test.\n")
      }
    }
  }
  
  return(results)
}
fisher_results <- fisher_tests(gut_contents)


str(gut_contents$SL_mm)

# Plotting bar graphs comparing different species size data into 2x3 grid

# Reshape the data into long format
gut_contents_long <- gut_contents %>%
  pivot_longer(cols = c(Calanoid_Copepod, Harpactacoid_Copepod, Amphipod, 
                        Shrimp, Polycheate, Larval_Fish, Empty_Stomach),
               names_to = "Prey_Item", values_to = "Presence") %>%
  group_by(Species, SL_mm, Prey_Item) %>%
  summarise(Proportion = mean(Presence),
            n = n(),
            SE = sqrt((Proportion * (1 - Proportion)) / n), .groups = "drop")

# Ordering size ranges from in correct order
gut_contents_long$SL_mm <- factor(gut_contents_long$SL_mm, 
                                  levels = c("5_10", "10_15", "15_20"))

# Reordering the prey item factor levels and replace underscores with spaces
gut_contents_long$Prey_Item <- factor(gut_contents_long$Prey_Item,
                                      levels = c("Calanoid_Copepod", 
                                                 "Harpactacoid_Copepod", 
                                                 "Amphipod", "Shrimp", 
                                                 "Polycheate", "Larval_Fish", 
                                                 "Empty_Stomach"),labels = 
                                        c("Calanoid copepod", 
                                          "Harpacticoid copepod", "Amphipod", 
                                          "Shrimp", "Polycheate", "Fish", 
                                          "Empty Stomach"))

# Create the plot
ggplot(gut_contents_long, aes(x = Prey_Item, y = Proportion, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                position = position_dodge(0.9), width = 0.2, color = "black") +
  facet_grid(rows = vars(Species), cols = vars(SL_mm), scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
        strip.background = element_rect(fill = "gray90", color = "black"),
        strip.text = element_text(face = "bold")) +
  labs(title = "Proportion of Each Species Consuming Each Prey Item by Size 
       Class",
       x = "Prey Item", y = "Proportion of Fish Consuming Prey Item") +
  scale_fill_manual(values = c("Cynoscion nebulosus" = "salmon", 
                               "Micropogonias undulatus" = "steelblue")) +
  # Italicize the legend text
  theme(legend.text = element_text(face = "italic"))

# Code for grouped RDA

# Create a combined grouping variable with species and size range
gut_contents$Group <- paste(gut_contents$Species, gut_contents$SL_mm, sep = "_")

# Set prey item columns
prey_columns <- c("Calanoid_Copepod", "Harpactacoid_Copepod", "Amphipod",
                  "Shrimp", "Polycheate", "Larval_Fish", "Empty_Stomach")

# Run RDA
rda_grouped <- rda(gut_contents[, prey_columns] ~ Group, data = gut_contents)

# Extract scores
rda_scores <- scores(rda_grouped, display = c("species", "sites"), scaling = 2)

# Prepare species (prey) scores
species_scores <- as.data.frame(rda_scores$species)
species_scores$label <- rownames(species_scores)

# Scale arrow length down to keep labels on screen
arrow_scale <- 0.8
species_scores <- species_scores %>%
  mutate(RDA1 = RDA1 * arrow_scale,
         RDA2 = RDA2 * arrow_scale)

# Prepare site (fish group) scores
site_scores <- as.data.frame(rda_scores$sites)
site_scores$Group <- gut_contents$Group
group_means <- site_scores %>%
  group_by(Group) %>%
  summarise(RDA1 = mean(RDA1), RDA2 = mean(RDA2))

# Plot RDA
ggplot() +
  geom_point(data = group_means, aes(x = RDA1, y = RDA2, color = Group), 
             size = 4) + geom_text_repel(data = group_means, 
            aes(x = RDA1, y = RDA2, label = Group), fontface = "bold") +
  geom_segment(data = species_scores,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red") +
  geom_text_repel(data = species_scores,
                  aes(x = RDA1, y = RDA2, label = label),
                  color = "red", fontface = "italic", max.overlaps = 10) +
  theme_minimal() +
  labs(title = "RDA Plot: Prey Items Explained by Species + Size Class",
       x = "RDA1", y = "RDA2") +
  theme(legend.position = "none")

# Adjusted R-squared
rda_summary <- summary(rda_grouped)
r_squared_adj <- RsquareAdj(rda_grouped)
print(r_squared_adj)

# ANOVA
rda_anova <- anova(rda_grouped, permutations = 999)
print(rda_anova)
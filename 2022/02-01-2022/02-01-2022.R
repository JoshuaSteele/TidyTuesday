
library(tidyverse)
library(skimr)
library(patchwork)
library(ggtext)

font_add_google("Roboto Condensed","roboto")


breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

## Explore your data first
skim(breed_traits)
as.tibble(breed_traits)
skim(trait_description)
as.tibble(trait_description)
as.tibble(breed_rank_all)


# Function source: https://bit.ly/3rngs1c
round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}


# Not much tidyverse this week
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)

var1 <- breed_traits$`Coat Type` 
var2 <- breed_traits$`Coat Length` 

coat_type_table <- round_preserve_sum(table(var1) * ((nrows*nrows)/(length(var1))), digits=0)
coat_length_table <- round_preserve_sum(table(var2) * ((nrows*nrows)/(length(var2))), digits=0)

df$coat_type <- factor(rep(names(coat_type_table), coat_type_table))  
df$coat_length <- factor(rep(names(coat_length_table), coat_length_table))  

palette <- c("#23120b", "#B7AEA7", "#FFCC47", 
             "#5a3825", "#E6BE8A", "#5D1916", 
             "#cc9966", "#3d2314", "#100C07")


plt_type <- ggplot(df, aes(x = x, y = y, fill = coat_type)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual(values = palette) +
  theme_void() +
  coord_fixed() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 120),
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20, unit = "pt"))

plt_length <- ggplot(df, aes(x = x, y = y, fill = coat_length)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual(values = palette) +
  theme_void() +
  coord_fixed() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 120),
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20, unit = "pt"))

plt_type + plt_length + 
  plot_annotation(title = "**Distribution of dog coat types and lengths**",
                  caption = "Data Source: <i>American Kennel Club</i> | Data Viz: Joshua Steele | TidyTuesday 2022: <b>Week 5</b>",
                  theme = theme(
                    text=element_text(family="roboto"),
                    plot.title = element_markdown(size = 160),
                    plot.caption = element_markdown(size = 120),
                    plot.background = element_rect(color = "grey80", fill = "grey80"),
                    plot.margin = margin(20, 20, 20, 20, unit = "pt")
                  ))

ggsave(filename = "doggos.png", dpi = 320, width = 30, height = 15, units = "in")

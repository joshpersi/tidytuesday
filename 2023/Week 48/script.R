# Setup ------------------------------------------------------------------------
# Load pacman for easier package management
library(pacman)

# Load required packages, installing if necessary
p_load(
  dplyr,
  ggplot2,
  purrr,
  tidytuesdayR
)

# Load tidytuesday data for week 48
tuesdata <- tidytuesdayR::tt_load(2023, week = 48)

# Merge individual datasets
drwho <- reduce(tuesdata, left_join)

# Load helper functions
source("utils.R")

# Exploration ------------------------------------------------------------------

# How much total variation is there in rating?
# - ratings range from 75 to 91, with a mean of 83.92
ggplot(drwho, aes(rating)) + 
  geom_density()

# Did episode ratings tend to increase over time?
# - ratings peaked around 2010 and declined pretty thereafter
ggplot(drwho, aes(first_aired, rating)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# What could explain variability in episode rating? 
# - appears to be variation among writers and directors, but not other character
#   type variables
map(names(select(drwho, where(is.character))), plot_simple_boxplot)

# - apparent increase in viewership with increasing ratings
ggplot(drwho, aes(rating, uk_viewers)) + 
  geom_jitter() + 
  geom_smooth(se = FALSE)

# - less clear of a relationship between duration and relation
ggplot(drwho, aes(rating, duration)) + 
  geom_jitter() + 
  geom_smooth(se = FALSE)



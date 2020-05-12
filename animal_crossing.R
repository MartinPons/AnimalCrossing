
# Script for tidytuesday. Dataset from 2020-05-05. Animal Crossing - New Horizons
# Data comes from the VillgerDB project (https://github.com/jefflomacy/villagerdb) and Metacritic (https://www.metacritic.com/game/switch/animal-crossing-new-horizons/critic-reviews). 
# Link to tidytuesday dataset: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-05-05/readme.md

# Author: Martín Pons


# INITIAL SETTINGS ---------------------------------------------------------

# libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(ggradar)
library(extrafont)
library(grid)
library(gridExtra)
library(here)

# helpler functions

get_data_ready_for_radar <- function(dat, group) {
  
  # data.frame, character -> data.frame
  
  # produces nrc sentiment data ready to be plotted with the ggradar function, 
  # using nrc sentiment dictionary and getting tidy tokenized data 
  # witn tokens labeled with the word "word". "group" determines the
  # group to be used in ggradar
  
  dat %>% 
    
    # joining main data frame with nrc sentiment dictionary
    inner_join(get_sentiments("nrc"), by = "word") %>% 
    
    # filtering words with raw negative and positive sentiment
    filter(!sentiment %in% c("positive", "negative")) %>% 
    
    # counting words for each sentiment
    count(sentiment) %>% 
    
    # capitalize words for plot style
    mutate(sentiment = str_to_title(sentiment)) %>% 
    
    # reescaling word count
    mutate(n = rescale(n)) %>% 
    
    # unpivot tables (this is the format ggradar wants as input)
    pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% 
    mutate(group = group) %>% 
    
    # rearranging variables: grouping variable must be in the first column
    select(group, Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, Trust)
  
  
}

# LOAD DATA ---------------------------------------------------------------

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')


# DATA WRANGLING ----------------------------------------------------------

## Reviews grades and length ##

critic <- critic %>% 
  mutate(review_length = str_length(text))

user_reviews <- user_reviews %>% 
  mutate(review_length = str_length(text))

## Users and Critics grouped scores
review_comparison <- 
  
  # wrangling critics dataset
  critic %>%
  
  # rescalation of grades and review length 
  mutate(rescaled_grade = round(rescale(grade, to = c(0, 10))),
         review_length = rescale(review_length)) %>% 
  
  # geting review grade distribution and average review length
  group_by(rescaled_grade) %>% 
  summarise(n_critics = n(), 
            avelength_critics = mean(review_length, na.rm = T)) %>% 
  mutate(f_critics = n_critics /sum(n_critics)) %>% 
  
  # join with users wrangled data
  full_join(
    user_reviews %>% 
      
      # rescale review length
      mutate(review_length = rescale(review_length)) %>% 
      
      # geting review grade distribution and average review length
      group_by(grade) %>% 
      summarise(n_users = n(), 
                avelength_users = mean(review_length, na.rm = T)) %>% 
      mutate(f_users = n_users /sum(n_users)),
    
    # definition of joining variables
    by = c("rescaled_grade" = "grade")
  ) %>% 
  
  # remove absolute counts. We no longer need them
  select(-matches("^n"))

# pivoting the data. Tidy format for ggplot
review_comparison_long <- review_comparison %>% 
  pivot_longer(-rescaled_grade, names_to = c(".value", "review_group"), names_sep = "_")


## Reviews nrc sentiment ##

# tidyng data for sentiment analysis #
tidy_critic <- critic %>% 
  unnest_tokens(word, text) %>% 
  ungroup()

tidy_user_reviews <- user_reviews %>% 
  unnest_tokens(word, text)

# getting data ready for radar plot # 

# wrangling of each data set (see helpler functions above)
critic_reviews_nrc <- get_data_ready_for_radar(tidy_critic, "Critics")
user_reviews_nrc <- get_data_ready_for_radar(tidy_user_reviews, "Users")

# biding the to datasets
radar_data <- user_reviews_nrc %>% 
  bind_rows(critic_reviews_nrc)


# VISUALIZATION -----------------------------------------------------------

# Note: you'll have to uncomment the two lines in the next paragraph if you use extrafont library 
# for the first time in order for the code to detect the Georgia Font. please note that fonts are 
# loaded from Windows operating system

# font_import()
# loadfonts(device = "win")


# We'll arrange the different plots using grid.arrange, passing to this functions
# grob object we'll have been built individually
# Legends and titles with be added as separate grobs, thus not appearing in the
# individual plots

# color palette
plots_palette <- c("#ad5d51", "grey55")


## Grade and review length plot ##

# creation of grob ibject
grade_plot <- ggplotGrob(
  
  review_comparison_long %>% 
    
    # plot coordinates. Change in f sign for barplot
    ggplot(aes(y = ifelse(review_group == "critics", f, -f), x = -rescaled_grade)) + 
    
    # bar geom
    geom_col(aes(fill = review_group)) + 
    
    # line geom
    geom_line(aes(y = ifelse(review_group == "critics",
                            avelength, 
                            -avelength), color = review_group), 
             size = 2) + 
    
    # point geom. Along with lines
    geom_point(aes(y = ifelse(review_group == "critics",
                             avelength, 
                             -avelength), color = review_group), size = 5) + 
    
    # flip coordinates: grade must be in y azis
    coord_flip() + 
    
    # title and caption
    labs(
      x = "Grade", 
      caption = "Critics' reviews have been rescaled to 0-10 assigning min. value to 0 and 
       max. value to 10, in order to match them with the review scale for users") +

    # color and coordinate scales
    scale_fill_manual(values = plots_palette) +
    
    # this palette for points is similar to plot_palette but slightly darker
    scale_color_manual(values = c("#754038", "grey45")) +
    scale_x_continuous(breaks = 0:-10, labels = 0:10) + 
    scale_y_continuous(limits = c(-0.5, 0.5), labels = c(0.5, 0.25, 0, 0.25, 0.5), n.breaks = 5) +
   
    # removing legend
    guides(fill = F, color = F) + 
    
    # theme
    theme_bw() + 
    theme(text = element_text(family = "Georgia", color = "grey45"), 
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(), 
         panel.border = element_blank(),
         axis.title.x = element_blank(), 
         axis.title.y = element_text(size = 14),
         axis.text = element_text(size = 12),
         plot.caption = element_text(size = 12))
  
)
   
    
## NRC sentiment radar plot ##

# grob object
sent_plot <- ggplotGrob(
  
  radar_data %>% 
    
    # radar plot
  ggradar(group.point.size = 5, group.colours = plots_palette,
          plot.legend = F))


## Plot arrangement ##

# main plots list
plot_grobs <- arrangeGrob(grobs = list(grade_plot, sent_plot), ncol = 2)

# plot titles as separate grobs
title_grob_grades <- textGrob("Grade distribution (bars) and review average relative length (lines)", 
                              gp = gpar(fontfamily = "Georgia", col = "grey55", cex = 1.5))

title_grob_sent <- textGrob("Sentiments expressed in reviews", 
                            gp = gpar(fontfamily = "Georgia", col = "grey55", cex = 1.5))

# joining title grobs
title_grobs <- arrangeGrob(title_grob_grades, title_grob_sent, ncol = 2)

  
# legend grob
plot_legend <- legendGrob(labels = c("Users", "Critics"), pch = 21, 
                          gp = gpar(fill = c("grey55", "#ad5d51"), fontfamily = "Georgia", cex = 1.5), ncol = 2)

# caption grob
caption_grob <- textGrob("Data comes from the VillagerDB project and Metacritic", 
                         gp = gpar(fontfamily = "Georgia", cex = 1, col = "grey45"))
## Drawing plot ##
grid.arrange(plot_legend, title_grobs, plot_grobs, caption_grob, ncol = 1, heights = c(1, 1, 8, 0.5),
             top = textGrob("Animal Crossing. Critics and users review comparison", 
                            gp = gpar(fontfamily = "Georgia", col = "grey55", cex = 2)))


# uncomment the following section to convert the plot as a grob and save it in disk
# final_plot <- arrangeGrob(plot_legend, title_grobs, plot_grobs, caption_grob, ncol = 1, heights = c(1, 1, 8, 0.5),
#                           top = textGrob("Review comparison of Animal Crossing. Users vs. Critics",
#                                          gp = gpar(fontfamily = "Georgia", col = "grey55", cex = 2)))
# 
# ggsave(here::here("animal_crossing.png"), final_plot, width = 16, height = 8)

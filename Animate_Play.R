library(tidyverse)
library(gganimate)
library(ggplot2)
library(cowplot)
library(repr)
library(gridExtra)


#loading command to make NFL field in ggplot (credit to Marschall Furman)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")

plays <- read.csv("https://media.githubusercontent.com/media/cnickol26/BigDataBowl2023/main/nfl-big-data-bowl-2023/plays.csv")
plays$uniqueplayId <- as.numeric(paste(as.character(plays$gameId), 
                                       as.character(plays$playId), sep=""))

# select all the players besides dbs and wrs
players <- read.csv("https://media.githubusercontent.com/media/cnickol26/BigDataBowl2023/main/nfl-big-data-bowl-2023/players.csv")

positions <- c('DE', 'OLB','DT', 'ILB', 'NT', 'MLB', 'LB', 'RB', 'T', 'TE','G','QB','C','FB')

positions_df <- players[players$officialPosition %in% positions,]
players_list <- positions_df$nflId

# Read in all the weeks but only for the positons above
locations <- data.frame()
for (i in 1:8){
  url <- paste('https://media.githubusercontent.com/media/cnickol26/BigDataBowl2023/main/nfl-big-data-bowl-2023/week', as.character(i), '.csv', sep="")
  week_data <- read.csv(url)
  week_data <- week_data[((week_data$nflId %in% players_list) | 
                            (week_data['team'] == 'football')), ]
  locations <- rbind(locations, week_data)
  print(i)
}
locations$uniqueplayId <- as.numeric(paste(as.character(locations$gameId), 
                                       as.character(locations$playId), sep=""))


subset <-read.csv("https://github.com/cnickol26/BigDataBowl2023/blob/main/data_not_flipped.csv?raw=true")



animate_play_subset <- function(play_id){
  df_examplePlay <- plays %>%
    select(uniqueplayId, playDescription) %>%
    filter(uniqueplayId==play_id)
  
  #merging tracking data to play
  df_examplePlayTracking <- inner_join(df_examplePlay,
                                       subset,
                                       by = "uniqueplayId") %>%
    #Standardizing tracking data so its always in direction of offensive team.
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
  
  
  ball_location <- which(sort(unique(df_examplePlayTracking$team))=="football")
  
  cols_fill <- c(1, 2, 3)
  cols_fill[ball_location] <- "#663300"
  cols_fill[as.numeric(cols_fill[1:3!=ball_location])] <- c("#013369", "#d50a0a")
    
  cols_col <- c("#000000", "#000000", "#000000")
  cols_col[ball_location] <- "#663300"
      
  size_vals <- c(8, 8, 8)
  size_vals[ball_location] <- 5
    
  shape_vals <- c(21, 21, 21)
  shape_vals[ball_location] <- 16
    
  plot_title <- df_examplePlay$playDescription
  nFrames <- max(df_examplePlayTracking$frameId)
  
  
  
  field_min <- floor(min(df_examplePlayTracking$x))-5
  field_max <- floor(max(df_examplePlayTracking$x))+5
  
  #plotting
  anim <- ggplot() +
    
    
    #creating field underlay
    gg_field(yardmin = field_min, yardmax = field_max) +
    
    #filling forest green for behind back of endzone
    theme(panel.background = element_rect(fill = "forestgreen",
                                          color = "forestgreen"),
          panel.grid = element_blank()) +
    
    
    #setting size and color parameters
    scale_size_manual(values = size_vals, guide = "none") + 
    scale_shape_manual(values = shape_vals, guide = "none") +
    scale_fill_manual(values = cols_fill, guide = "none", na.value=NA) + 
    scale_colour_manual(values = cols_col, guide = "none") +
    
    
    #adding players
    geom_point(data = df_examplePlayTracking, aes(x = x,
                                                  y = y, 
                                                  shape = team,
                                                  fill = team,
                                                  group = nflId,
                                                  size = team,
                                                  colour = team), alpha=0.7) +  
    
    #adding jersey numbers
    geom_text(data = df_examplePlayTracking,
              aes(x = x, y = y, label = jerseyNumber),
              colour = "white", 
              vjust = 0.36, size = 3.5) + 
    
    
    #titling plot with play description
    labs(title = plot_title) +
    
    #setting animation parameters
    transition_time(frameId)  +
    ease_aes("linear")
  
  
  animate(anim, width = 720, height = 440,fps = 10, nframes = nFrames)
  
}


animate_play_full <- function(play_id){
  df_examplePlay <- plays %>%
    select(uniqueplayId, playDescription) %>%
    filter(uniqueplayId==play_id)
  
  #merging tracking data to play
  df_examplePlayTracking <- inner_join(df_examplePlay,
                                       locations,
                                       by = "uniqueplayId") %>%
    #Standardizing tracking data so its always in direction of offensive team.
    mutate(x = ifelse(playDirection == "left", 120-x, x),
           y = ifelse(playDirection == "left", 160/3 - y, y))
  
  
  ball_location <- which(sort(unique(df_examplePlayTracking$team))=="football")
  
  cols_fill <- c(1, 2, 3)
  cols_fill[ball_location] <- "#663300"
    cols_fill[as.numeric(cols_fill[1:3!=ball_location])] <- c("#013369", "#d50a0a")
    
    cols_col <- c("#000000", "#000000", "#000000")
    cols_col[ball_location] <- "#663300"
      
    size_vals <- c(8, 8, 8)
    size_vals[ball_location] <- 5
    
    shape_vals <- c(21, 21, 21)
    shape_vals[ball_location] <- 16
    
    plot_title <- df_examplePlay$playDescription
    nFrames <- max(df_examplePlayTracking$frameId)
    
    
    
    field_min <- floor(min(df_examplePlayTracking$x))-5
    field_max <- floor(max(df_examplePlayTracking$x))+5
    
    #plotting
    anim <- ggplot() +
      
      
      #creating field underlay
      gg_field(yardmin = field_min, yardmax = field_max) +
      
      #filling forest green for behind back of endzone
      theme(panel.background = element_rect(fill = "forestgreen",
                                            color = "forestgreen"),
            panel.grid = element_blank()) +
      
      
      #setting size and color parameters
      scale_size_manual(values = size_vals, guide = "none") + 
      scale_shape_manual(values = shape_vals, guide = "none") +
      scale_fill_manual(values = cols_fill, guide = "none", na.value=NA) + 
      scale_colour_manual(values = cols_col, guide = "none") +
      
      
      #adding players
      geom_point(data = df_examplePlayTracking, aes(x = x,
                                                    y = y, 
                                                    shape = team,
                                                    fill = team,
                                                    group = nflId,
                                                    size = team,
                                                    colour = team), alpha=0.7) +  
      
      #adding jersey numbers
      geom_text(data = df_examplePlayTracking,
                aes(x = x, y = y, label = jerseyNumber),
                colour = "white", 
                vjust = 0.36, size = 3.5) + 
      
      
      #titling plot with play description
      labs(title = plot_title) +
      
      #setting animation parameters
      transition_time(frameId)  +
      ease_aes("linear")
    
    
    animate(anim, width = 720, height = 440,fps = 10, nframes = nFrames)
    
}







animate_play_subset(2021091902389)


set.seed(2023)
random_plays <- sample(subset$uniqueplayId, size=20)

sub_1 <- animate_play_subset(random_plays[1])
full_1 <- animate_play_full(random_plays[1])

sub_2 <- animate_play_subset(random_plays[2])
full_2 <- animate_play_full(random_plays[2])

sub_3 <- animate_play_subset(random_plays[3])
full_3 <- animate_play_full(random_plays[3])

sub_4 <- animate_play_subset(random_plays[4])
full_4 <- animate_play_full(random_plays[4])

sub_5 <- animate_play_subset(random_plays[5])
full_5 <- animate_play_full(random_plays[5])

sub_6 <- animate_play_subset(random_plays[6])
full_6 <- animate_play_full(random_plays[6])

sub_7 <- animate_play_subset(random_plays[7])
full_7 <- animate_play_full(random_plays[7])

sub_8 <- animate_play_subset(random_plays[8])
full_8 <- animate_play_full(random_plays[8])









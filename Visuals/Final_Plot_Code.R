library(tidyverse)
library(ggimage)
library(plotly)
library(RColorBrewer)
library(png)
library(base64enc)

data <- read_csv('./Plot Data/starters_both.csv')
power <- read_csv('./Plot Data/starters_power.csv')
speed <- read_csv('./Plot Data/starters_speed.csv')

data_filt <- data %>% select(Player, Team, `Score (Equal Weight S:P)`, `Equal Weight Ranking`, 
                `Total Rushes Faced`)
power_filt <- power %>% select(Player, `Power Score`)
speed_filt <- speed %>% select(Player, `Speed Score`)

#anti_join(power_filt, speed_filt, by='Player')
#anti_join(speed_filt, power_filt, by='Player')
# The anti joins above show that there are some players without speed or power so they are 
#   removed in the line of code below (only 56 players remain)

plot_data <- left_join(speed_filt, power_filt, by="Player") %>% filter(!is.na(`Power Score`)) %>% 
  left_join(data_filt, by="Player") %>% rename(`Overall Score` = `Score (Equal Weight S:P)`)



L <- list()
for (i in 1:nrow(plot_data)){
  split <- str_split(tolower(plot_data[i,1]), " ", simplify=TRUE)
  png <- paste(gsub("\\.", "", split[1]), substr(split[2], 1, 1), ".png", sep="")
  L[[i]] <- list(source=dataURI(file=paste("./Lineman Pictures/", png, sep="")),
                 x=plot_data[[i, 3]], y=plot_data[[i, 2]], 
                 xref="x", yref="y", xanchor="center", yanchor="middle",
                 sizex=0.35, sizey=0.35, opacity=0.85)
}






plot_ly(plot_data, x=~`Power Score`, y=~`Speed Score`, type="scatter", mode="markers",
        text=~`Speed Score`, hoverinfo="text",
        hoverlabel=list(bgcolor="black", font=list(color="white")),
        hovertext=paste(plot_data$Player, 
                        "<br>Overall Score:", plot_data$`Overall Score`,
                        "<br>Overall Ranking:", paste(plot_data$`Equal Weight Ranking`, "/64", sep=""),
                        "<br>Total Rushes Faced:", plot_data$`Total Rushes Faced`)) %>%
  hide_guides() %>% 
  layout(title="Evaluating Offensive Linesmen",
         xaxis=list(title="Power Score"),
         yaxis=list(title="Speed Score"),
         images=L)



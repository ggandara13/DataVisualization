

###########################################################################
#                                                                         #
# Purpose:       Visualization Techniques !                               #
#                                                                         #
# Author:        S1-5                                                     #
# Contact:       Gianni Miller <gimiller@student.ie.edu>                  #
#                Gok Asci <gok.asci@student.ie.edu>                       #  
#                Joaquin Calderon <joaquin.calderon@student.ie.edu>       #
#                Maria Gonzalez <mariagonzalbez@student.ie.edu>           #
#                Yannick Van Dam <yannickvandam@student.ie.edu>           #
#                Zohak Mirza <Zohak.mirza@student.ie.edu>                 #                           #
#                Gerardo Gandara <gerardo.gandara@student.ie.edu>         #
#                                                                         #
# Client:        SANDRA BECKER                                            #
#                                                                         #
# Code created:  2021-02-06                                               #
# Last updated:  2021-02-12                                               #
# Source:        /Users/gandara/Documents/IE Master 2020                  #
#                                                                         #
# Comment:       It is all about Visual Encoding and good color choice    #
#                                                                         #
###########################################################################

#devtools::install_github("hrbrmstr/hrbrthemes")
#install.packages("fmsb")
#install.packages("wordcloud2")
#install.packages("webshot")
#webshot::install_phantomjs()

# Libraries
library(hrbrthemes)
library(GGally)
library(viridis)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gcookbook)
library(tidyverse)
library(fmsb)
library(scales)
library(wordcloud2) 



# Set the folder (path) that contains this R file as the working directory
getwd()
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- "/Users/gandara/Documents/IE Master 2021 - Term 2/Visualization/Assigment-Spotify/data"
setwd(dir)

data<-read.csv("Spotify_Dataset8.csv",header=TRUE)

#Select some relevant data columns
colnames(data) 

df1 <- dplyr::select(data, c(
  'artists', 
  'NewGenre', 
  'popularity',
  'release_date',
  'year',
  'name',
  'acousticness', 
  'danceability', 
  'energy',
  'explicit',
  'instrumentalness', 
  'liveness', 
  'loudness',
  'speechiness',
  'tempo',
  'valence',
  'duration_ms',
  'mode',
  'key.1'
  )
)

#Let's focus on the last 30 years
df_filtred <- df1 %>% 
  filter(year>=1990)


# -------------------"df_filtred is the main DataSet with 1990 - 2021"-------------#
str(df_filtred)
# -------------------"df_filtred is the main DataSet with 1990 - 2021"-------------#




#-------------------------------------------------------------------------#
#--------------------------- Time Series ---------------------------------#
#-------------------------------------------------------------------------#

# ------------------First apporach ------------------------#

# Filter by year
df_filtered_first <- df1 %>% 
  filter(year>=1980 & year<=2020 & NewGenre != "Other")

# Group by year
df_grouped <- df_filtered_first %>% group_by(year, NewGenre)
df_grouped

# Summarise mean popularity
by_genre <- df_grouped %>% summarise(
  popularity = mean(popularity)
)
by_genre

#Check the options
#?ggplot

# graph # 1: Spaguetti graph
ggplot() +
  geom_line(data=by_genre, aes (year, popularity, group=NewGenre, color="black"), lwd=0.2, show.legend=FALSE) +
  labs(title="Popularity of genre per year")+
  theme_ipsum() +
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )

# graph # 2: Highlighting reggaeton in popularity by year

# Creating subset of reggaeton 
genre_reggaeton <- dplyr::filter(by_genre, NewGenre=="Reggaeton")

# Plotting the graph
ggplot() +
  geom_line(data=transform(by_genre), aes (year, popularity, group=NewGenre), alpha=0.6, lwd=0.1, colour="black") +
  geom_line(data=genre_reggaeton, aes (year, popularity, group=NewGenre), lwd=0.5, show.legend=FALSE, color="blue") +
  theme(strip.background=element_blank(), strip.placement="outside") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title = "Popularity per year highlight Reggaeton") +
  theme_ipsum()+
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )

# Graph # 3: split popularity graph by genre
ggplot() +
  geom_line(data=by_genre, aes (year, popularity, group=NewGenre, colour=NewGenre), lwd=0.2, show.legend=TRUE) +
  facet_wrap(~ NewGenre) + theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Mean popularity per year for each genre") +
  theme_ipsum()+
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )

# EXTRA: ****** Zooming in to see only Reggaeton
ggplot(subset(by_genre, NewGenre=="Reggaeton")) +
  geom_line(aes(year, popularity, group=NewGenre), lwd=0.2, color="blue", show.legend=FALSE) +
  labs(title = "Popularity per year for Reggaeton") +
  theme_ipsum()+
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )

# ----------------------------- END Time Series --------------------------#





#-------------------------------------------------------------------------#
#---------------------- PARALLEL coordinates (3) -------------------------#
#----------------------------------- -------------------------------------#

# Plot Paralle Coordinate focus on Reggaeton - Valence
df_reg <- df_filtred %>% 
      filter(NewGenre == 'Reggaeton')

colnames(df_reg)



# Valence
agg <- aggregate(valence ~ year , data=df_reg, mean)
df_parallel_coord <- data.frame(t(agg))
df_parallel_coord <- df_parallel_coord[2,]
colnames(df_parallel_coord) <- agg$year
df_parallel_coord$Attribute<-rownames(df_parallel_coord)

# speechiness
agg <- aggregate(speechiness ~ year , data=df_reg, mean)
df_parallel_coord2 <- data.frame(t(agg))
df_parallel_coord2 <- df_parallel_coord2[2,]
colnames(df_parallel_coord2) <- agg$year
df_parallel_coord2$Attribute<-rownames(df_parallel_coord2)

# acousticness
agg <- aggregate(acousticness ~ year , data=df_reg, mean)
df_parallel_coord3 <- data.frame(t(agg))
df_parallel_coord3 <- df_parallel_coord3[2,]
colnames(df_parallel_coord3) <- agg$year
df_parallel_coord3$Attribute<-rownames(df_parallel_coord3)

# energy
agg <- aggregate(energy ~ year , data=df_reg, mean)
df_parallel_coord4 <- data.frame(t(agg))
df_parallel_coord4 <- df_parallel_coord4[2,]
colnames(df_parallel_coord4) <- agg$year
df_parallel_coord4$Attribute<-rownames(df_parallel_coord4)

# danceability
agg <- aggregate(danceability ~ year , data=df_reg, mean)
df_parallel_coord5 <- data.frame(t(agg))
df_parallel_coord5 <- df_parallel_coord5[2,]
colnames(df_parallel_coord5) <- agg$year
df_parallel_coord5$Attribute<-rownames(df_parallel_coord5)

# liveness
agg <- aggregate(liveness ~ year , data=df_reg, mean)
df_parallel_coord6 <- data.frame(t(agg))
df_parallel_coord6 <- df_parallel_coord6[2,]
colnames(df_parallel_coord6) <- agg$year
df_parallel_coord6$Attribute<-rownames(df_parallel_coord6)

# instrumentalness
agg <- aggregate(instrumentalness ~ year , data=df_reg, mean)
df_parallel_coord7 <- data.frame(t(agg))
df_parallel_coord7 <- df_parallel_coord7[2,]
colnames(df_parallel_coord7) <- agg$year
df_parallel_coord7$Attribute<-rownames(df_parallel_coord7)


# explicit
agg <- aggregate(explicit ~ year , data=df_reg, mean)
df_parallel_coord8 <- data.frame(t(agg))
df_parallel_coord8 <- df_parallel_coord8[2,]
colnames(df_parallel_coord8) <- agg$year
df_parallel_coord8$Attribute<-rownames(df_parallel_coord8)

#colnames(df_parallel_coord8)

agg_together1 <- rbind(df_parallel_coord, df_parallel_coord2, df_parallel_coord3, df_parallel_coord4)
agg_together2 <- rbind(df_parallel_coord5, df_parallel_coord6, df_parallel_coord7)


# YoY change in average : Valence, speechiness, acousticness, energy
GGally::ggparcoord(agg_together1,
                   scale='globalminmax',
                   columns = 1:29,
                   groupColumn='Attribute') +
  theme(text = element_text(size=5),axis.text.x = element_text(angle=90, hjust=1))+
  xlab('Year') +
  ylab('Atribute Average Value')+
  ggtitle("Annual Change in average : Reggaeton Music Elements") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )

# YoY change in average : instrumentalness, liveness, danceability

GGally::ggparcoord(agg_together2,
                   scale='globalminmax',
                   columns = 1:29,
                   groupColumn='Attribute') +
  theme(text = element_text(size=5),axis.text.x = element_text(angle=90, hjust=1))+
  xlab('Year') +
  ylab('Atribute Average Value')+
  ggtitle("Annual Change in average : Reggaeton Music Elements") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )


# YoY change in average :Explicit

GGally::ggparcoord(df_parallel_coord8,
                   scale='globalminmax',
                   columns = 1:29,
                   groupColumn='Attribute') +
  theme(text = element_text(size=5),axis.text.x = element_text(angle=90, hjust=1))+
  xlab('Year') +
  ylab('Atribute Average Value')+
  ggtitle("Annual Change in average : Reggaeton Music Elements") +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme( plot.title = element_text(size=10), 
         axis.text.x = element_text(angle = 45, hjust = 1, size=8)
  )

#--------------------- END Parallel Coordinate --------------------------------#




#-------------------------------------------------------------------------#
#---------------------- RADAR Song Attributes ----------------------------#
#----------------------------------- -------------------------------------#

#--------- Create the df FORMAT for the RADAR ---------------#

#initialize the dataset with the first filtered one
df = df_filtred

df <- aggregate(.~NewGenre, df, mean)
#colnames(df)
df <- subset( df, select = c(13, 8, 9, 7, 11, 12, 16 ) )
rownames(df) <- c("Blues Soul", "Country","Dance", "Disco",
                  "Instrumental", "Jazz","Latin", "Metal",
                  "Other", "Pop","R&B", "Reggaeton",
                  "Rock"
)
df <- df[-9,] #eliminate Other Genre Category
#  --------- END Create the df FORMAT -------------------------------

# Define the variable ranges: maximum and minimum
max_min <- data.frame(
  loudness = c(0.8, -55), danceability = c(1, 0), energy = c(1, 0),
  acousticness = c(1, 0), instrumentalness = c(1, 0), liveness = c(1, 0), valence=c(1,0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df_radial <- rbind(max_min, df)


regeton_data <- df_radial[c("Max", "Min", "Reggaeton"), ]
create_beautiful_radarchart <- function(df_radial, color = "#00AFBB", 
                                        vlabels = colnames(df_radial), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...)
{
  radarchart(
    df_radial, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Regeton
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(regeton_data, caxislabels = c(0, 5, 10, 15, 20))
par(op)


# -------------------let's create a radar per genre (3) ----------------------#
# Define colors and titles
colors <- c("#bec4a0", "#c2b0bc", "#e58672")
titles <- c("Blues Soul", "Country", "Dance")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))

# Create the radar chart
for(i in 1:3){
  create_beautiful_radarchart(
    df_radial[c(1, 2, i+2), ], caxislabels = c(0, 5, 10, 15, 20),
    color = colors[i], title = titles[i]
  )
}
par(op)

# -------------let's create a radar per other genre (3) ------------------#
# Define colors and titles
colors <- c("#00AFBB", "#E7B800", "#FC4E07")
titles <- c("R&B", "Reggaeton", "Rock")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))

# Create the radar chart
for(i in 10:12){
  create_beautiful_radarchart(
    df_radial[c(1, 2, i+2), ], caxislabels = c(0, 5, 10, 15, 20),
    color = colors[i-9], title = titles[i-9]
  )
}
par(op)



# ------------------   averages  ---------------------#

# df has all the averages per attribute

df_scaled <- round(apply(df, 2, scales::rescale), 2)
df_scaled <- as.data.frame(df_scaled)
head(df_scaled)


# Variables summary
# Get the minimum and the max of every column  
col_max <- apply(df_scaled, 2, max)
col_min <- apply(df_scaled, 2, min)
# Calculate the average profile 
col_mean <- apply(df_scaled, 2, mean)
# Put together the summary of columns
col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))


# Bind variables summary to the data
df_scaled2 <- as.data.frame(rbind(col_summary, df_scaled))
head(df_scaled2)




# ----------------- now we do the plots ---------------#
opar <- par() 
# Define settings for plotting in a 3x4 grid, with appropriate margins:
par(mar = rep(0.8,4))
par(mfrow = c(3,4))
# Produce a radar-chart for each student
for (i in 4:nrow(df_scaled2)) {
  radarchart(
    df_scaled2[c(1:3, i), ],
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(df_scaled2)[i]
  )
}
# Restore the standard par() settings
par <- par(opar)

# -----------------------END  RADAR plots  -------------------------------#





# ------------------------   sample -------------------------------#
#top = 200
#df_top <- df_filtred[with(df_filtred,order(-popularity)),][1:top,]

# ------------------------   sample -------------------------------#





#-------------------------------------------------------------------------#
#----------------- STACKED BAR chart Multiple Categories -----------------#
#----------------------------------- -------------------------------------#

#  --------- How loveable KEY vs Mode Attribute  -------------------------------

df_filtro <- df_filtred %>% 
  filter(year == 2020 & popularity >50)


keys <- data.frame("key.1"=0:11, 
                   "fullkey"=c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"))

df3 <- merge(df_filtro, keys, by="key.1", all=TRUE)

df_top <- df3
df_top3 <- df_top %>%
  group_by(fullkey, mode) %>%
  summarize(value = n())

df_top3 <- as.data.frame(df_top3)
df_top3$mode <- as.factor(df_top3$mode)

# Small multiple
ggplot(df_top3, aes(fill=mode, y=value, x=fullkey)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  #ggtitle("How lovable was 2020 ? ... ") +
  ylab("Number of songs") +
  xlab("Full Key") +
  theme_ipsum() +
  labs(title = "How loveable was 2020 ....",
       subtitle = "The yellower ...the happier",
       caption  = "Mode: modality (major or minor) of a track. Major is represented by 1 and minor is 0. ")


#  --------- END - How loveable KEY vs Mode Attribute-----------------------------#





#-------------------------------------------------------------------------#
#---------------------- Polar Bar Chart ----------------------------------#
#----------------------------------- -------------------------------------#

#------- Valence for Top 40 -------------------------#

#To understan the valance, let's see the to 40 songs in 2020
df <- df_filtred %>% 
  filter(year == 2020)
top = 40
df_top <- df[with(df,order(-popularity)),][1:top,]

#df_top$`song name` <- df_top$name  # create new column for car names
df_top$`song name` <- paste(df_top$artists, " - ", df_top$name)




df_top$valence_z <- round((df_top$valence - mean(df_top$valence))/sd(df_top$valence), 2)
df_top$valence_type <- ifelse(df_top$valence_z < 0, "below", "above")  # above / below 0.5
df_top <- df_top[order(df_top$valence_z), ]  # sort
df_top$`song name` <- factor(df_top$`song name`, levels = df_top$`song name`)

# Diverging Barcharts
ggplot(df_top, aes(x=`song name`, y=valence_z, label=valence_z)) + 
  geom_bar(stat='identity', aes(fill=valence_type), width=.5)  +
  scale_fill_manual(name="Happy Mood", 
                    labels = c("Above ", "Below "), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Valence Behavoir", 
       title= "TOP 40 songs 2020 ") + 
  coord_flip() +
  #theme_minimal() +
  theme_ipsum() +
  theme( plot.title = element_text(size=12), 
         axis.text.x = element_text(size=8),
         axis.text.y = element_text(size=8)) +
  theme(panel.border=element_blank())

#------- END Valence for Top 40 -------------------------#





#-------------------------------------------------------------------------#
#----------------------      DUMB BELL Chart        ----------------------#
#----------------------------------- -------------------------------------#

# we will work with the filtred Dataset

colnames(df_filtred)
agg <- aggregate(cbind(df_filtred$popularity) ~ NewGenre  + year, data = df_filtred, mean, na.rm = TRUE)
names(agg)[3] <- "popularity_mean"

# create subset years
years <- filter(agg, year %in% c(1990, 2020)) %>% select(NewGenre, year, popularity_mean)

#Convert data to wide format
years2 <- spread(years, year, popularity_mean)
#view(years2)

#names(years2) <- c("NewGenre", "y1990", "y2000", "y2010", "y2020")
names(years2) <- c("NewGenre", "y1990", "y2020")

#Genre without presence initialize in 1 for chart purposes
years2[is.na(years2)] = 1
years2

#Sorted by 1990
years3 <- arrange(years2, desc(y1990))
years3$NewGenre <- factor(years3$NewGenre, levels=rev(years3$NewGenre))
years3

line_col="#a3c4dc"
# for the legend
df2 = tidyr::gather(years3, group, value, -NewGenre)

gg <- ggplot(years3, aes(y = NewGenre)) + 
  geom_point(data = df2, aes(x = value, color = group), size = 1.5) +
  geom_dumbbell(aes(x = y1990, xend = y2020), size_x=1.5, size_xend=2.5, size=1, color="grey", 
                colour_x = "#e28743", colour_xend = "#2596be",
                dot_guide=FALSE, dot_guide_size=0.15) +
  scale_color_manual(name = "", values = c("#e28743", "#2596be") ) +
 labs(x=NULL, y=NULL, title="Popularity Songs 1990 vs 2020") 
gg <- gg + theme_bw()
#gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
#gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.major.x=element_line())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(panel.border=element_blank())

# we have taken out all the horizontal lines and the color in the X's in order to focus on the line only
gg

# ------------------- END DumbBell Chart  ---------------------#



#-------------------------------------------------------------------------#
#---------------------- Animated Dumb Bell Chart -------------------------#
#----------------------------------- -------------------------------------#

# -------------------"Dataset Creation for the Animated Chart ---------------------#
## For the animated
agg <- aggregate(cbind(df_filtred$popularity) ~ NewGenre  + year, data = df_filtred, mean, na.rm = TRUE)
names(agg)[3] <- "popularity_mean"

# create subset years
years <- filter(agg, year %in% c(1990, 2000, 2010, 2020)) %>% select(NewGenre, year, popularity_mean)


#For the annimation 1990 vs 2020 / by Decade
df <- df_filtred
decades <- c(1990,2000,2010, 2020)
df$decade<- decades[findInterval(df$year, decades)]
df_decade <- aggregate(cbind(popularity) ~ NewGenre + decade , data = df, mean)
write.csv(df_decade, "1990-2020-byGenre.csv")
#view(df_decade)


df <- df_filtred
decades <- c(1990, 2020)
df$decade<- decades[findInterval(df$year, decades)]
df_decade <- aggregate(cbind(popularity) ~ NewGenre + decade , data = df, mean)
write.csv(df_decade, "1990-2020-byGenre.csv")
view(df_decade)



# ------------------- END Dataset Creation for the Animated Chart ---------------------#






#-------------------------------------------------------------------------#
#---------------------- WORLD CLOUD  Song Lyrics -------------------------#
#----------------------------------- -------------------------------------#


# Set the folder (path) that contains this R file as the working directory
getwd()
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- "/Users/gandara/Documents/IE Master 2021 - Term 2/Visualization/Assigment-Spotify/data"
setwd(dir)

palabras <-read.csv("palabras.csv",header=TRUE)

# Basic plot
wordcloud2(data=palabras, size=1.8)
wordcloud2(palabras, size=1.8, color='random-light', backgroundColor="black")
wordcloud2(palabras, size = 2.0, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

#export to HTML
hw <- wordcloud2(palabras,size = 3)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)










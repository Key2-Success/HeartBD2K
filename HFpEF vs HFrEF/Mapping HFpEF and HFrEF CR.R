# load libraries
library(leaflet)
library(ggmap)
library(rgdal)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(plotly)
library(grDevices)

# make PMIDs vectors
p_pmid <- p_pmid$V1
r_pmid <- r_pmid$V1

# new dataset containing only those PMIDs
data_p <- data[data$PMID %in% p_pmid, ]
data_r <- data[data$PMID %in% r_pmid, ]

# ----- make graph for HFpEF ----- #

# make dataframe of count occurence of all US states and of country counts
US_count <- as.data.frame(table(data_p$us_state))
countries_count <- as.data.frame(table(data_p$country))

# merge country and US states dataframe
all_count <- rbind(countries_count, US_count)
all_count <- all_count[-which(all_count$Var1 == "United States"), ] # remove the USA row


######## Joel's code ########

# world and state map data
world <- map_data(map = "world")
state <- map_data(map = "state")

# add alaska and hawaii
alaska <- world[which(world$subregion == "Alaska"), ]
alaska[ , 6] <- NA
alaska$region <- "alaska"
state <- rbind(alaska, state)
alaska$group <- 100

hawaii <- world[which(world$subregion == "Hawaii"), ]
hawaii[ , 6] <- NA
hawaii$region <- "hawaii"
hawaii$group <- 99

state <- rbind(hawaii, state)
restWorld <- world[which(world$region != "USA"),]


######## Kitu's code ########

# merge state with US data
US_count$Var1 <- tolower(US_count$Var1) # lowercase just like in the state df
names(US_count)[1] <- "region" # rename just like in the state df
state <- left_join(state, US_count)

# merge world with countries data
names(countries_count)[1] <- "region" # rename just like in the world df
world <- left_join(world, countries_count)

# clean up
names(state)[5] <- "Region"
names(state)[7] <- "Frequency"
names(world)[5] <- "Region"
names(world)[7] <- "Frequency"
state$Region <- str_to_title(state$Region)

# make map of world
p1 <- ggplot(data = world, aes(x = long, y = lat, fill = Frequency, Region = Region, frequency = Frequency, group = group)) + geom_polygon(color = "white", show.legend = FALSE)

# combine map of world with US states map
p2 <- p1 + geom_polygon(data = state, color = "white" , aes(fill = Frequency)) + guides(fill = guide_legend()) + 
  ggtitle(label = "Frequency of HFpEF Case Reports by Region") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", 
                        guide = "colorbar", na.value = "grey") + theme(plot.title = element_text(size = 22)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"), 
        axis.line = element_line(colour = "white"),
        axis.ticks= element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

# plot it!
ggplotly(p2, tooltip = c("Region", "frequency"))




# ----- make graph for HFrEF ----- #

# make dataframe of count occurence of all US states and of country counts
US_count <- as.data.frame(table(data_r$us_state))
countries_count <- as.data.frame(table(data_r$country))

# merge country and US states dataframe
all_count <- rbind(countries_count, US_count)
all_count <- all_count[-which(all_count$Var1 == "United States"), ] # remove the USA row


######## Joel's code ########

# world and state map data
world <- map_data(map = "world")
state <- map_data(map = "state")

# add alaska and hawaii
alaska <- world[which(world$subregion == "Alaska"), ]
alaska[ , 6] <- NA
alaska$region <- "alaska"
state <- rbind(alaska, state)
alaska$group <- 100

hawaii <- world[which(world$subregion == "Hawaii"), ]
hawaii[ , 6] <- NA
hawaii$region <- "hawaii"
hawaii$group <- 99

state <- rbind(hawaii, state)
restWorld <- world[which(world$region != "USA"),]


######## Kitu's code ########

# merge state with US data
US_count$Var1 <- tolower(US_count$Var1) # lowercase just like in the state df
names(US_count)[1] <- "region" # rename just like in the state df
state <- left_join(state, US_count)

# merge world with countries data
names(countries_count)[1] <- "region" # rename just like in the world df
world <- left_join(world, countries_count)

# clean up
names(state)[5] <- "Region"
names(state)[7] <- "Frequency"
names(world)[5] <- "Region"
names(world)[7] <- "Frequency"
state$Region <- str_to_title(state$Region)

# make map of world
p1 <- ggplot(data = world, aes(x = long, y = lat, fill = Frequency, Region = Region, frequency = Frequency, group = group)) + geom_polygon(color = "white", show.legend = FALSE)

# combine map of world with US states map
p2 <- p1 + geom_polygon(data = state, color = "white" , aes(fill = Frequency)) + guides(fill = guide_legend()) + 
  ggtitle(label = "Frequency of HFrEF Case Reports by Region") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "") +
  scale_fill_continuous(low = "lightblue", high = "darkblue", 
                        guide = "colorbar", na.value = "grey") + theme(plot.title = element_text(size = 22)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"), 
        axis.line = element_line(colour = "white"),
        axis.ticks= element_blank(), axis.text.x=element_blank(),
        axis.text.y=element_blank())

# plot it!
ggplotly(p2, tooltip = c("Region", "frequency"))

# Jason Laso                                      #
# 2/2/17                                          #
# Data analysis on the fall of WWE TV viewership  #
###################################################


library(dplyr)
library(ggplot2)

# Set your working directory where the dataset is stored
setwd("\\\\net.ucf.edu\\ikm\\Home\\ja041718\\Documents\\")
ratings = read.csv(file = "wwetvratings.csv", header=T, sep=",")

# Let's start off with a graph of all Raw and Smackdown viewers since Raw went to 3 hours on 7/23/12.

# Set up a filter for the starting date of the graph
date_filter = "2012-07-23"

# Timeline of ratings of both shows since the expansion.
ggplot(subset(ratings, date>=date_filter & viewers>1000000), aes(x=date, y=viewers, col=show)) + 
  geom_line() + geom_smooth() + 
  scale_y_discrete(limits=c(2e+06, 3e+06, 4e+06, 5e+06, 6e+06), 
                   labels=c("2M", "3M", "4M", "5M", "6M")) +
  ggtitle(paste("WWE Viewership Since", date_filter)) + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=32, hjust=0))



# Summarize the average viewers by month & year (removing all shows without viewer data)
months = ratings %>% group_by(show, month, year) %>% filter(viewers>0 & year >= 2012) %>%
  summarize(avg.viewers = mean(viewers)) 

# Plot the trend on a monthly basis
ggplot(months, aes(x=year, y=avg.viewers, col=show)) + geom_point() + geom_smooth() + facet_wrap(~month)

# Another look at the months. This time all on the same plot for better comparison.
ggplot(months, aes(x=year, y=avg.viewers, color=as.factor(month))) + geom_line()  + facet_wrap(~show)

# Those results are kind of hard to interpret. Perhaps if we break the months into seasons, then it will
# paint a clearer picture.

# Create a seasons variable in ratings
ratings$season = ifelse(ratings$month %in% 1:3, "Winter", 
                        ifelse(ratings$month %in% 4:6, "Spring", 
                               ifelse(ratings$month %in% 7:9, "Summer", 
                                      "Fall") ) )

# Analyze season variable to make sure they're somewhat equal
table(ratings$season)

# Calculate the average viewers by season, year
seasons = ratings %>% group_by(show, season, year) %>% filter(viewers>0 & year >= 2012) %>%
  summarize(avg.viewers = mean(viewers)) 

# Plot the seasons by year and show
ggplot(seasons, aes(x=year, y=avg.viewers, color=season)) + geom_point() + geom_smooth()  + facet_wrap(~show)

# We can see a clear trend that January - March are consistently the highest viewed months, even in a decline.
# That makes sense as the shows are building to the WWE's top-branded show, Wrestlemania, in early April.

# It also shows that summer and fall, the times furthest removed from Wrestlemania, are consistently weakest.

# In fact, fall 2016 Raws drew less on average than winter 2012 SDs, the only Raw season on the graph that
# was outdrawn by any SD season on the graph.

# To look more into this pre-Wrestlemania spike, let's see how April does compared to Jan.-March and 
# see if there's any type of post-WM holdover.
ggplot(subset(months, month %in% 1:4), 
       aes(x=year, y=avg.viewers, color=as.factor(month))) + geom_point() + 
          geom_smooth()  + facet_wrap(~show)

# The graph shows that April consistently comes down from the first 3 months. This implies a rush of fans tune
# in to build up WM, but WM itself has failed to keep those viewers watching beyond then.

# Let's see if there are any other months that seem to have importance. First, let's add seasons to the months
# table.
months$season = ifelse(months$month %in% 1:3, "Winter", 
                        ifelse(months$month %in% 4:6, "Spring", 
                               ifelse(months$month %in% 7:9, "Summer", 
                                      "Fall") ) )

# Now we can compare each month within each season for Raw.
ggplot(subset(months, months$show=="raw"),
       aes(x=year, y=avg.viewers, col=as.factor(month))) + geom_point() + geom_smooth() + facet_wrap(~season)

# It seems most months within each season are generally interchangeable, except April and September. April 
# consistently is the highest drawing month in spring and September the least of the fall.

# April figures to be tied to the timing of Wrestlemania. Even if viewers decline after the show, there does 
# appear to be some holdover rate that slowly erodes into the spring as opposed to one giant sudden drop.

# September's decline is likely tied to the start of the new fall TV season and the NFL starting.

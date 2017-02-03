# Jason Laso
# 2/2/17
# Introduction to scraping & data preparation using WWE TV ratings


library(rvest)
library(stringr)
library(tidyr)

# scrape example given on website just for reference on how to use rvest
url = "http://wrestlingdata.com/index.php?befehl=quoten"
webpage = read_html(url)
sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table, fill = T, header = T)[[13]]
head(sb)

#initialize vector for loop
ratings=c()

# Loop to scrape all pages with Raw info through 1/31/17. There are 23 pages.
for(i in 1:23){
  
  #assemble list of URLs
  url[i] = paste("http://wrestlingdata.com/index.php?befehl=quoten&art=2&liga=1&show=1&sort=0&seite=",i, sep="")
  #read the url and isoltate the nodes
  webpage = read_html(url[i])
  htmlnodes <- html_nodes(webpage, 'table')
  
  #read in the 13th node (ratings data) as a table
  page.data <- html_table(htmlnodes, fill = T, header = T)[[13]]
  
  #append each page into ratings data frame
  ratings = rbind(ratings, page.data)
  
  #progress tracker for loop
  print(i)
}

# Loop to scrape all pages with Smackdown info through 1/31/17. There are 17 pages.
for(i in 1:17){
  
  #assemble list of URLs
  url[i] = paste("http://wrestlingdata.com/index.php?befehl=quoten&art=2&liga=1&show=2&sort=0&seite=",i, sep="")
  #read the url and isoltate the nodes
  webpage = read_html(url[i])
  htmlnodes <- html_nodes(webpage, 'table')
  
  #read in the 13th node (ratings data) as a table
  page.data <- html_table(htmlnodes, fill = T, header = T)[[13]]
  
  #append each page into ratings data frame
  ratings = rbind(ratings, page.data)
  
  #progress tracker for loop
  print(i)
}

#rename columns
colnames(ratings) = c("date", "event", "rating","viewers")

#reformat dates
ratings$date = as.Date(ratings$date, "%Y/%m/%d")

#let's also extract year & month into their own columns for possible seasonal analysis.
ratings$year = as.numeric(format(ratings$date, "%Y"))
ratings$month = as.numeric(format(ratings$date, "%m"))

#ratings and views read in as characters. change missings to 0 and then convert to numeric
ratings$rating[ratings$rating == "-"] = 0
ratings$rating = as.numeric(ratings$rating)

#viewers data reads in commas in numbers. remove the commas before conversion
ratings$viewers = gsub('\\,', '', ratings$viewers)
ratings$viewers[ratings$viewers == "-"] = 0
ratings$viewers = as.numeric(ratings$viewers)

#check structure
str(ratings)
summary(ratings)

# We need to extract "Raw" and "Smackdown" from the event variable. This loop searches each row and assigns
# a much simpler show name to a new column, show.
for(i in 1:nrow(ratings)){
  
  # This is searching for "RAW" or "Smack" in the event string and returning the new show column.
  
  if(grepl("RAW", ratings$event[i])){
        ratings$show[i] = "raw"
  }else if(grepl("Smack", ratings$event[i])){
        ratings$show[i] = "smackdown"
  }else{
        ratings$show[i] = "misc"
  }
}
#Check results
table(ratings$show)

# The scrape reads in an extra row, "Brawl to End it All" since it includes the word "raw". Remove that row.
ratings = ratings[-which(ratings$show == "misc"),] 

# Raw went to 3 hours on 7/23/12.
date_filter = "2012-07-23"

# Timeline of ratings of both shows since the expansion.
ggplot(subset(ratings, date>=date_filter & viewers>1000000), aes(x=date, y=viewers, col=show)) + 
  geom_line() + geom_smooth() + 
  scale_y_discrete(limits=c(2e+06, 3e+06, 4e+06, 5e+06, 6e+06), 
                   labels=c("2M", "3M", "4M", "5M", "6M")) +
  ggtitle(paste("WWE Viewership Since", date_filter)) + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="blue", face="bold", size=32, hjust=0))



# Let's write a csv to our directory with our cleaned up dataset now.
write.csv(ratings, "wwetvratings.csv", row.names=F)

# this gets the data organized and calculates the favorites h-index

# load libraries
library(ggplot2)
library(cowplot)
library(lubridate)
library(dplyr)
library(beepr)
library(stringr)
library(rtweet)
library(scales)

# This function returns the H-INDEX when the favorite or retweet count
# is a sorted vector, with the favorites or retweet count in descending order
H_INDEX <- function(x) {
  y <- 1:length(x)
  step1 <- x >= y
  step2 <- max(which(step1 == TRUE))
  return(step2)
}

d1 <- readRDS("~/Dropbox/R/t22_1_25.rds")
d2 <- readRDS("~/Dropbox/R/t22_26_1000.rds")
d3 <- readRDS("~/Dropbox/R/t22_1001_3000.rds")
d4 <- readRDS("~/Dropbox/R/t22_3001_4364.rds")
d5 <- readRDS("~/Dropbox/R/t22_4365_5000.rds")
d6 <- readRDS("~/Dropbox/R/t22_5001_6932.rds")
d7 <- readRDS("~/Dropbox/R/t22_6933_7000.rds")
d8 <- readRDS("~/Dropbox/R/t22_7001_9000.rds")
d9 <- readRDS("~/Dropbox/R/t22_9001_11216.rds")

col_names <- c("screen_name",
                  "created_at",
                  "id_str",
                  "full_text",
                  "is_quote_status",
                  "retweet_count",
                  "favorite_count",
                  "text",
               "quoted_status_id")

colnames(d1) <- col_names
colnames(d2) <- col_names
colnames(d3) <- col_names
colnames(d4) <- col_names
colnames(d5) <- col_names
colnames(d6) <- col_names
colnames(d7) <- col_names
colnames(d8) <- col_names
colnames(d9) <- col_names

d_all <- rbind.data.frame(d1, d2, d3,
                          d4, d5, d6,
                          d7, d8, d9)

d_all$screen_name <- d_all$screen_name$screen_name


# total tweets include RT 
totalTweets <- length(d_all$text) 

# total accounts checked
totalAccts <- length(levels(as.factor(d_all$screen_name))) 

d1_clean <- subset(d_all, !grepl('^RT', d_all$full_text, ignore.case = TRUE))

# for rapidity
#write_as_csv(d2022, '~/Dropbox/R/all_turf_tweets_2021.csv')

# read it new
##d2022 <- read_twitter_csv("~/Dropbox/R/all_turf_tweets_2021.csv")

d2022 <- d1_clean

d2022$date <- ymd_hms(d2022$created_at)

# get only those with 12+ tweets, for once a month
d2022$tweet_count <- as.numeric(ave(d2022$id_str, d2022$screen_name, FUN = length))
d22_active <- subset(d2022, tweet_count >= 12)

## Remove the jennifer_360_ fishing account
d22_active <- subset(d22_active, screen_name != "Jennifer_360_")

# unique accounts
# started at 9,630 to scrape
# but naturally some did not tweet in 2021, some retweet only, etc
accs <- length(levels(as.factor(d22_active$screen_name)))

d22_active_order_fav <- d22_active[with(d22_active, order(screen_name, -favorite_count)), ]

# I will get the same results if I remove all tweets with no faves
# this will save processing time
d22_active_order_fav <- subset(d22_active_order_fav, favorite_count > 0)
# I expect that I can slice this up by screen_name, 
# then output to a new file of screen_name + h-index

# refactor the screen names and number them for easy subsetting
d22_active_order_fav$screen_name <- factor(d22_active_order_fav$screen_name)
d22_active_order_fav$user_integer <- as.numeric(d22_active_order_fav$screen_name)

d22_active_order_fav$screen_name <- as.character(d22_active_order_fav$screen_name)
# create a file to hold the output
fav_index_file <- data.frame()

j <- max(d22_active_order_fav$user_integer)

# create progress bar
pb <- txtProgressBar(min = 0, max = j, style = 3)


# everything below this runs but was giving h-index in the 2000 range which seems improbable
for (i in 1:j) {
  # this gets us a single user to work with
  working_user <- subset(d22_active_order_fav, user_integer == i)
  user <- working_user[1, 1]
  
  # get the vector now of favorites for the working user
  favs <- working_user[, 7]
  
  fav_h_index <- H_INDEX(favs$favorite_count)
  
  # now for that user we should have a fav_h_index
  # I'd like to now write that
  newline <- cbind.data.frame(user, fav_h_index)
  fav_index_file <- rbind.data.frame(fav_index_file, newline)
  
  # show a progress bar
  setTxtProgressBar(pb, i)
}
beep(sound = 3)

fav_index_file$user <- reorder(fav_index_file$screen_name, fav_index_file$fav_h_index)

# order this, then select top 50
order_fav <- fav_index_file[with(fav_index_file, order(-fav_h_index)), ]

# forPlot <- order_fav[1:50, ]
# 
# p <- ggplot(data = forPlot, aes(x = fav_h_index, y = user))  
# p + theme_cowplot(font_family = "Gillius ADF No2 Cond") +
#   background_grid(major = "xy") +
#   geom_point(shape = 1) +
#   labs(x = "favorites h-index",
#        y = "Twitter username",
#        caption = "h-index: number of tweets in 2021 (n) with at least (n) favorites\nan h-index of 50 means that account had 50 tweets\nin 2021 that were favorited 50 or more times",
#        title = "Most likeable turfgrass Twitter accounts in 2019",
#        subtitle = "top 50 of 8,572 accounts")

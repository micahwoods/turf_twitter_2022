## this is the general workflow order for the analysis

## get the data
## and I have to do some manual intervention/restarting
## because accounts that have been deleted/renamed/set private between the time
## of classification as an industry account and the time
## the tweets are gathered will stop the script. Also the 
## accounts that block me stopped the script prior to 2023. 
source("r/get_data.R")

## those data are saved, then I read them in and clean them up
## this is a lot of clearning data, then I save again as one big file
## on Dropbox 
## this file also does a calculation of the favorites H-index
source("r/turf_twitter_2022_read_raw.R")

## now I go through a series of files to calculate some 
## influence statistics or rankings in various categories,
## and will merge together at the end
source("r/turf_twitter_2022_retweet_analysis.R")
source("r/turf_twitter_2022_followers.R")
source("r/tweet_creation_rate_2022.R")
source("r/quoted_2022.R")
source("r/turf_twitter_2022_mentions.R")
## source("r/turf_twitter_normalize_2022.R")  ## I don't do anything with the normalized ranking
source("r/twitter_all_around_rank_2022.R")
source("r/underdogs_turf_2022.R")

## the above scripts take a while to run, especially the one counting mentions
## at the end the scripts put together tables with rankings by category 
## that is summarized and cleaned up in this script which makes a simple summary
## csv file for use in the Shiny app
source("r/prepare_for_shiny_data_22.R")

## I make a series of standard charts for sharing on my blog (svg, perhaps?)
## and for sharing on Twitter
source("r/rank_charts_22.R")

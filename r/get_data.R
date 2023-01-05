# get data from selected turf twitter accounts in 2022

library(rtweet)
library(dplyr)

## Read this vignette for information on authentication 
## vignette("auth", package = "rtweet")
## authentication needs to be set before proceeding

# get id of accounts following 
atc <- get_followers("asianturfgrass", n = 20000, retryonratelimit = TRUE)
pace <- get_followers("paceturf", n = 20000, retryonratelimit = TRUE)
unl <- get_followers("pgrbill", n = 20000, retryonratelimit = TRUE)
dempsey <- get_followers("J_J_Dempsey", n = 20000, retryonratelimit = TRUE)
jyri <- get_followers("EnvuTurfAUS", n = 20000, retryonratelimit = TRUE)
jk <- get_followers("iTweetTurf", n = 20000, retryonratelimit = TRUE)
unruh <- get_followers("jbunruh", n = 20000, retryonratelimit = TRUE)

# get accounts following industry organizations, which I'll use as a confirmation
# that the account is probably interested in turf
gcsaa <- get_followers("gcsaa", n = 26000, retryonratelimit = TRUE)
bigga <- get_followers("BIGGAltd", n = 20000, retryonratelimit = TRUE)
canada <- get_followers("GolfSupers", n = 20000, retryonratelimit = TRUE)
agcsa <- get_followers("TheASTMA", n = 20000, retryonratelimit = TRUE)
iog <- get_followers("thegma_", n = 20000, retryonratelimit = TRUE)
stma <- get_followers("FieldExperts", n = 20000, retryonratelimit = TRUE)

# that's a lot of accounts, and many are duplicates
# to get the accounts to check, first select all those that are following one of the
# turf scientists

followers <- unique(rbind.data.frame(atc, pace, unl, dempsey, jyri, jk, unruh))

# make list of unique accounts following the associations
assocations <- unique(rbind.data.frame(gcsaa, bigga, canada, agcsa, iog, stma))

# use the intersect function to select only those accounts that are in both follow lists
followers_assoc <- as.data.frame(base::intersect(followers$from_id, assocations$from_id))

colnames(followers_assoc) <- "user_id"
followers_assoc$user_id <- as.character(followers_assoc$user_id)

# get basic data on these accounts
turf_follower <- lookup_users(followers_assoc$user_id)

# this removes the private accounts, also the least active ones
turf_follower2 <- subset(turf_follower, protected == FALSE & statuses_count >= 50)

# check which have massive following
automated <- subset(turf_follower2, friends_count >= 1e4)

# this attempts to remove those that seem to have automated follow/follower system
# in order to get a large audience, few removed here are turf related
turf_follower3 <- subset(turf_follower2, friends_count < 1e4 |
                           screen_name == "WhitbyGC_Greens" |
                           screen_name == "TurfMatters" |
                           screen_name == "iTweetTurf" |
                           screen_name == "FieldsInTrust" |
                           screen_name == "DennisMowers" |
                           screen_name == "fcbusiness")

# in 2022 it is n = 11216  ## perhaps because I include here in 2022 JJ Dempsey substituting for STRI Tomy
# in 2021 it is n = 9630
# in 2020 it is n = 9222
# at this 2019 point it is 8572 accounts
# in 2018 it was 7689 accounts. In 2017 it was 6271 accounts
# I want to loop through these, getting the tweets and making some calculations
# I'm fine with 3000

# t3b <- t3[, 1:18]
## write.csv(t3b, '~/Dropbox/R/t22_turf_follower3.csv')
turf_follower3 <- read_twitter_csv("~/Dropbox/R/t22_turf_follower3.csv")

turf_timelines <- data.frame()

turf_follower3[4365, 5]

for (i in 7001:9000) {
  new_timeline <-
    get_timeline(
    user = as.character(turf_follower3[i, 5]),
      n = Inf,
      home = FALSE,
      parse = TRUE,
      check = TRUE,
      retryonratelimit = TRUE,
      since_id = "1476700398874365956"
    )
  new_timeline_2022 <- subset(
    new_timeline,
    created_at >= "2022-01-01 00:00:00" &
      created_at <= "2022-12-31 23:59:59"
  )
  
  new_timeline_2022$screen_name <- turf_follower3[i, 5]
  
  ## I get only the data I will use for the analysis to keep it simple
  ## and to reduce file size
  new_timeline_2022 <- select(
    new_timeline_2022,
    screen_name,
    created_at,
    id_str,
    full_text,
    is_quote_status,
    retweet_count,
    favorite_count,
    text,
    quoted_status_id_str
  )
  
  turf_timelines <- rbind(turf_timelines, new_timeline_2022)
  
  print(paste("completed", i, "/11,216 accounts"))
  Sys.sleep(6) # needs this in 2021
}

saveRDS(turf_timelines, "~/Dropbox/R/t22_7001_9000.rds")


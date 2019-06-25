library(rtweet)

app_name <- ""
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_token_secret <- ""

create_token(app=app_name, consumer_key=consumer_key, consumer_secret=consumer_secret, access_token=access_token,
             access_token_secret=access_token_secret, set_renv = TRUE)


# Get tweets from Chicago
## Will need google maps api
chi_tweets <- search_tweets("lang:en", geocode = lookup_coords("chicago"), 
                           type="recent", include_rts=FALSE)

# Trending in Chicago
trends <- get_trends("Chicago")
trends$trend


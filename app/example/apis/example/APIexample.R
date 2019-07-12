#######################################################################
# This is an toy example of Plumber API for downloading reddit submissions and comments from the Pushshift. You can run the API by clicking
# the 'Run API' button above.
#TITLE: Reddit API example
#AUTHOR: Carlo  R. M. A. Santagiustina
#CREATION DATE: 19-01-2019

# Find out more about building APIs with Plumber here:
#     https://www.rplumber.io/
# Find out more about the rreddit package here:
# https://github.com/mkearney/rreddit

library(plumber)
library(rreddit)
require(jsonlite)

#* @apiTitle RRedditAPI

#* Return Reddit submissions from Pushshift API. For more info on your Pushshift API please visit: https://pushshift.io/api-parameters/ 
#* @param query (char string)	subreddit to be searched
#* @param n (integer) specifying the total number of desired submissions to return (by default n=1000, maximum n=10000)
#* @post /searchsubmissions_simplified

function(query="",
         n=1000) {
   
   n=as.integer(n)
   if(n<1000){
      n=1000
   }
   if(n>10000){
      n=10000
   }
   
   return(
      jsonlite::toJSON(
      list(
         "execution date" = Sys.time(),
         "parameters" = list(
            "query" = query,
            "n" = n
         ),
         "data" = rreddit::get_r_reddit(subreddit =query,
                                n = n)),
      force = T)
   )
}

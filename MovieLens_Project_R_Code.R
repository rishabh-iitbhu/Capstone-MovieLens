## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,message = FALSE, fig.align = 'center', cache=FALSE, cache.lazy = FALSE)


## ----include=FALSE-------------------------------------------------------
## Installing necessary libraries ###

if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggplot2)) install.packages("ggthemes")


## ----include=FALSE-------------------------------------------------------

# Calling required libraries

library(data.table)
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(ggthemes)

######### Initial Code provided on edx ################

#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)



## ------------------------------------------------------------------------
# We define the RMSE function as following:
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
    sqrt(mean((true_ratings - predicted_ratings)^2))
}


## ------------------------------------------------------------------------
edx %>% summarize(Users = n_distinct(userId),
              Movies = n_distinct(movieId)) 
summary(edx)

sum(is.na(edx))


## ------------------------------------------------------------------------

sum(is.na(validation))

validation %>% summarize(Users = n_distinct(userId),
              Movies = n_distinct(movieId)) 

summary(validation)


## ------------------------------------------------------------------------
# Converting timestamp to a date format #

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")


## ------------------------------------------------------------------------
# Extracting the year and month of dates in both he datasets # 

edx <- edx %>% mutate(Rating_year=format(edx$date,"%Y"),Rating_month=format(edx$date,"%m"))
validation <- validation %>% mutate(Rating_year=format(validation$date,"%Y"),Rating_month=format(validation$date,"%m"))


## ------------------------------------------------------------------------
# Extracting the year of realse in both the datasets and changing the column name 

names(edx)[names(edx) == "timestamp"] <- "rating_year"

release_year_edx <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE) %>% 
    as.numeric()


edx <- edx %>% mutate(release_year = release_year_edx)


names(validation)[names(validation) == "timestamp"] <- "rating_year"

release_year_validation <- stringi::stri_extract(validation$title, regex = "(\\d{4})", 
    comments = TRUE) %>% as.numeric()


validation <- validation %>% mutate(release_year = release_year_validation)



## ------------------------------------------------------------------------
######### Making a new grouped genre column for both the edx and validation datasets  ########

#For edx dataset

  
   # edx <- edx  %>%
   # mutate(genre = fct_explicit_na(genres,
   #                                     na_level = "Genre not available")) %>%
   # separate_rows(genre,
   #               sep = "\\|")
   # 

#For validation dataset
# 
# validation <- validation %>%
#    mutate(genre = fct_explicit_na(genres,
#                                        na_level = "Genre not available")
#           ) %>%
#    separate_rows(genre,
#                  sep = "\\|")



## ------------------------------------------------------------------------
# Filtering important columns on edx and validation dataset

# edx <- edx %>% select(userId, movieId, rating, title, genre, release_year, Rating_year, Rating_month)
# 
# validation <- validation %>% select(userId, movieId, rating, title, genre, release_year,Rating_year, Rating_month)



## ------------------------------------------------------------------------
##### Data type updation #####

edx$Rating_year <- as.numeric(edx$Rating_year)
edx$Rating_month <- as.numeric(edx$Rating_month)
edx$release_year <- as.numeric(edx$release_year)

validation$Rating_year <- as.numeric(validation$Rating_year)
validation$Rating_month <- as.numeric(validation$Rating_month)
validation$release_year <- as.numeric(validation$release_year)


## ------------------------------------------------------------------------
# Snapshot of starting rows 

head(edx) 


## ------------------------------------------------------------------------
hist(edx$rating, main="Ratings distribution", xlab="Rating",col='red')


## ------------------------------------------------------------------------
   ggplot(edx, aes(movieId))  + theme_solarized_2(light = FALSE)+
   geom_histogram(bins=500) +
   labs(title = "Frequency of ratings per movie",
        x = "Movie",
        y = "Frequency")


## ------------------------------------------------------------------------
hist(edx$Rating_month, main="Monthwise Frequency of ratings", xlab="Month",col='red',xlim=range(0,12))

hist(edx$Rating_year, main="Yearly Frequency of ratings", xlab="Years",col='red')


## ------------------------------------------------------------------------
 edx %>% group_by(rating) %>% summarise(ratings_distribution_sum = n()) %>% 
    arrange(desc(ratings_distribution_sum))

 ggplot(edx, aes(rating, fill = cut(rating, 100))) + theme_solarized_2(light = FALSE) + geom_histogram(color = "blue", 
    binwidth = 0.2) + scale_x_continuous(breaks = seq(0.5, 5, 0.5))



## ------------------------------------------------------------------------
paste("The mean is:", as.character(mean(edx$rating)))


## ------------------------------------------------------------------------
# Calculating the overall average for all movies

mu <- mean(edx$rating)

# Calculating the RMSE on the validation set

rmse_Naive_mean_model <- RMSE(validation$rating, mu)

# Creating a results dataframe that contains all RMSE results

results <- data.frame(model="Naive Mean-Baseline Model", RMSE=rmse_Naive_mean_model)


## ------------------------------------------------------------------------
# # Calculating the overall average for all movies

mu <- mean(edx$rating)

# Calculating the mean deviation from average rating per movie

avg_rating_by_movie <- edx %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu))

# Compute the predicted ratings on validation dataset

rmse_movie_based_model <- validation %>%
   left_join(avg_rating_by_movie, by='movieId') %>%
   mutate(pred = mu + b_i) %>%
   pull(pred)

rmse_movie_based_model_result <- RMSE(validation$rating, rmse_movie_based_model)

# Adding the results to the results dataset

results <- results %>% add_row(model="Movie-Based Model", RMSE=rmse_movie_based_model_result)


## ------------------------------------------------------------------------
# # Calculating the overall average for all movies

mu <- mean(edx$rating)

# Calculating the mean deviation from average rating per movie

avg_rating_by_movie <- edx %>%
   group_by(movieId) %>%
   summarize(b_i = mean(rating - mu))


# Average rating by user

Average_user_rating <- edx %>%
   left_join(avg_rating_by_movie, by='movieId') %>%
   group_by(userId) %>%
   summarize(b_u = mean(rating - mu - b_i))

# Compute the predicted ratings on validation dataset

rmse_user_and_movie_model <- validation %>%
   left_join(avg_rating_by_movie, by='movieId') %>%
   left_join(Average_user_rating, by='userId') %>%
   mutate(pred = mu + b_i + b_u) %>%
   pull(pred)

rmse_user_and_movie_model_result <- RMSE(validation$rating, rmse_user_and_movie_model)

# Adding the results to the results dataset

results <- results %>% add_row(model="Movie+User Based Model", RMSE=rmse_user_and_movie_model_result)


## ------------------------------------------------------------------------
regularization_index <-createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)

edx_train<-edx[-regularization_index,]

edx_test_temp<- edx[regularization_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set

edx_test <- edx_test_temp %>% 
     semi_join(edx_train, by = "movieId") %>%
     semi_join(edx_train, by = "userId")

# Add rows removed from edx_test set back into edx_train set

removed <- anti_join(edx_test_temp, edx_test)
edx_train <- rbind(edx_train, removed)

# Calculating the  average of all movies in test set

mu <- mean(edx_test$rating)



## ------------------------------------------------------------------------

lambdas <- seq(0, 10, 0.1)

# Function to calculate the predicted ratings on validation dataset using different values of lambda

rmse <- sapply(lambdas, function(lambda) {
   
  # Calculate the average by user
  
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu) / (n() + lambda))
   
   # Compute the predicted ratings on edx_test dataset
   
   predicted_ratings <- edx_test %>%
      left_join(b_i, by='movieId') %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
   
   # Predict the RMSE on the edx_test set
   
   return(RMSE(edx_test$rating, predicted_ratings))
})

# Plot of lambda vs RMSE

df <- data.frame(RMSE = rmse, lambdas = lambdas)

ggplot(df, aes(lambdas, rmse))  + theme_solarized_2(light = FALSE)+
   geom_point() +
   labs(title = "RMSEs vs Lambdas - Regularized Movie Based Model",
        y = "RMSEs",
        x = "lambdas")

#### Lambda value that minimizes the RMSE

min_lambda <- lambdas[which.min(rmse)]

### Predicting the RMSE on the validation set with the lambda that minimizes the RMSE in the edx_test data

   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu) / (n() + min_lambda))
   
# Compute the predicted ratings on validation dataset
   
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      mutate(pred = mu + b_i) %>%
      pull(pred)
   
 # RMSE obtained on the validation set
   
  rmse_regularized_movie_model <-RMSE(validation$rating, predicted_ratings)

# Adding the results to the results dataset

results <- results %>% add_row(model="Regularized Movie-Based Model", RMSE=rmse_regularized_movie_model)


## ------------------------------------------------------------------------
#Generating a sequence of lambdas

lambdas <- seq(0, 15, 0.1)

# Compute the predicted ratings on validation dataset using different values of lambda

rmse <- sapply(lambdas, function(lambda) {

   # Average by movieID
   
   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu) / (n() + lambda))
   
   # Average by user
   
   b_u <- edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu) / (n() + lambda))
   
   # Predicting ratings on validation dataset
   
   predicted_ratings <- edx_test %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
   
   # Predict the RMSE on the validation set
   
   return(RMSE(edx_test$rating, predicted_ratings))
})

# Plot of lambda vs RMSE

df <- data.frame(RMSE = rmse, lambdas = lambdas)

ggplot(df, aes(lambdas, rmse))  + theme_solarized_2(light = FALSE)+
   geom_point() +
   labs(title = "RMSEs vs Lambdas - Regularized Movie and Average User rating based Model",
        y = "RMSEs",
        x = "lambdas")

# Lambda value that minimizes the RMSE

min_lambda <- lambdas[which.min(rmse)]

### Predicting the RMSE on the validation set with the lambda that minimizes the RMSE in the edx_test data

   b_i <- edx %>%
      group_by(movieId) %>%
      summarize(b_i = sum(rating - mu) / (n() + min_lambda))
   
   # Average by user
   
   b_u <- edx %>%
      left_join(b_i, by='movieId') %>%
      group_by(userId) %>%
      summarize(b_u = sum(rating - b_i - mu) / (n() + min_lambda))
   
   # Predicting ratings on validation dataset
   
   predicted_ratings <- validation %>%
      left_join(b_i, by='movieId') %>%
      left_join(b_u, by='userId') %>%
      mutate(pred = mu + b_i + b_u) %>%
      pull(pred)
   
 # RMSE obtained on the validation set
   
  rmse_regularized_movie_user_model <-RMSE(validation$rating, predicted_ratings)

# Adding the results to the results dataset

results <- results %>% add_row(model="Regularized Movie and Average User rating based Model", RMSE=rmse_regularized_movie_user_model)


## ------------------------------------------------------------------------
# Shows the results

results %>% 
   kable() %>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
             position = "center",
             font_size = 10,
             full_width = FALSE)


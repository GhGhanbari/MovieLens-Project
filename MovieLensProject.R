

## Downloading the data set and building the train and test data sets


##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


library(dplyr)
library(caret)
library(data.table)
library(tinytex)
library(latexpdf)
library(ggplot2)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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



## Data analysis

head(edx)


# The number of distinct movies, users, and genres are as follows:
  
number_of_distinct_users = n_distinct(edx$userId)
number_of_distinct_movies = n_distinct(edx$movieId)
number_of_distinct_genres = n_distinct(edx$genres)
data.frame(number_of_distinct_users,number_of_distinct_movies,number_of_distinct_genres)





#Transforming timestamp column to the year the movie was rated and extract the year each movie was released. So, we get the following data set: 
  

library(lubridate)

edx <- mutate(edx,rating_year = as_datetime(timestamp))
edx$rating_year <- format(edx$rating_year, "%Y")
head(edx)

edx_with_dates <- edx %>% select(-timestamp) %>% mutate(released_year= as.numeric(stringi::stri_extract(edx$title, regex = "(?<=\\()\\d{4}(?=\\))", comments = TRUE )))

head(edx_with_dates)


#Calculating age of the movies at the time of rating and adding a new column called age_of_movie to the data set:

edx_with_dates$rating_year <- as.numeric(edx_with_dates$rating_year)
edx_with_dates <- edx_with_dates %>% mutate(age_of_movie=rating_year- released_year)

head(edx_with_dates)



#reviewing age of the movies

print("unique values of age of movies:")
unique(edx_with_dates$age_of_movie)


#removing negative elements of the age_of_movies column:

edx_with_dates <- edx_with_dates %>% filter(age_of_movie > 0)



# A new data set consisting of the average of ratings based on age of the movies

avg_rating_by_age <- edx_with_dates %>% group_by(age_of_movie) %>% summarize(age_ave_rating=mean(rating))%>% arrange(desc(age_ave_rating))

head(avg_rating_by_age)



#graphing the average of rating based on age of movies

avg_rating_by_age %>% ggplot(aes(age_of_movie,age_ave_rating)) + geom_point(size=2,color="blue") +geom_line(color="red")
+xlab("Age of movie") + ylab("Average of rating") + ggtitle("Average of ratings based on movie ages")




#Plot of average of rating vs the year movie was rated:

Avg_rating_rated_year <- edx_with_dates %>% group_by(rating_year) %>% summarize(Avg_rating=mean(rating)) %>% arrange(desc(Avg_rating))

head(Avg_rating_rated_year)

Avg_rating_rated_year %>% ggplot(aes(rating_year,Avg_rating)) + geom_point()+geom_smooth() + ggtitle("average of rating vs the year movie was rated")





#Exploring the variability of number of ratings for whole star and half star rating values.

whole_stars <- c(0,1,2,3,4,5)
rating_group <- ifelse(edx$rating %in% whole_stars, "whole_star","half_star")
edx_with_stars <- data.frame(rating_value=edx$rating, group=rating_group)

#Plotting the histogram of rating values

edx_with_stars %>% ggplot(aes(x=rating_value, fill=group)) + geom_histogram(binwidth = 0.25) + scale_x_continuous(breaks=seq(0, 5, by= 0.5))  + xlab("rating value") + ylab("number of ratings") 



#Exploring the top 10 movies based on average of ratings:

edx_with_dates %>% group_by(title) %>% summarize(n=n(),avg_rate= mean(rating)) %>% top_n(10, avg_rate)



#The movie with the greatest number of ratings:

edx_with_dates %>% group_by(title) %>% summarize(n=n()) %>% arrange(desc(n))


  
#histogram of number of ratings for movie Ids

edx_with_dates %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) + geom_histogram(binwidth = 0.2, color="blue") + xlab("Movie Id")+ ylab("number of ratings")+
  scale_x_log10() +
  ggtitle("Number of Ratings for Movie Ids")


#histogram of number of ratings for user Ids

edx_with_dates %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(count)) + geom_histogram(binwidth = 0.2, color="blue") + xlab("User Id")+ ylab("number of ratings")+
  scale_x_log10() +
  ggtitle("Number of Ratings for user Ids")




#Extracting genres of movies 

memory.limit(size = 20000)
edx_genre <- edx_with_dates %>% separate_rows(genres, sep ="\\|") %>% group_by(genres) %>% summarize(count=n(),avg_rating_genre=mean(rating),distinctMovies = n_distinct(movieId), distinctUserId=n_distinct(userId))

head(edx_genre)


#Arranging the genres based on the number of ratings in each genre

edx_genre %>% arrange(desc(count))
edx_genre %>% ggplot(aes(reorder(genres,count),count,fill=genres)) + geom_col()+ xlab("Genre")+ ylab("number of ratings")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Number of ratings based on genres")



#Arranging the genres based on the average of rating:
  
edx_genre %>% arrange(desc(avg_rating_genre))


# Plotting graph of the average of rating based on genre of movies:
  
edx_genre %>% ggplot(aes(reorder(genres,avg_rating_genre),avg_rating_genre,fill=genres)) + geom_col()+ xlab("Genre")+ ylab("Average of ratings")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Average of ratings based on movie genres")






## Methods


#RMSE function:

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}


#Naive model:

mu<- mean(edx$rating)
naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

#Table of result:

rmse_results <- data_frame(Method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable("pipe")



# Model with Movie effect:

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>% .$b_i

model_b_i_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie Effect Model",
                                    RMSE = model_b_i_rmse ))

#Table of results:
  
rmse_results %>% knitr::kable("pipe")




# Model with Movie and User effect:

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_b_i_u_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User Effects Model",  
                                     RMSE = model_b_i_u_rmse ))




#Table of results:
  
rmse_results %>% knitr::kable("pipe")





#Regularization for movie and user effects.


lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating, predicted_ratings))
})



#Plot of RMSE versus lambda values:

qplot(lambdas, rmses)


lambdas[which.min(rmses)]


# Results


rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie + User Effects Model",  
                                     RMSE = min(rmses) ))
rmse_results %>% knitr::kable("pipe")


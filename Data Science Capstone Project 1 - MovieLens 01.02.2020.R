#################################################
## Nik S.
## HarvardX: PH125.9x Data Science: Capstone
## # GitHub repository is here: https://github.com/nik-labs/MovieLens
## Jan2, 2020  
################################################

#################################
#Project MovieLens  
#Create edx set, validation set
#################################

######################################################################
# Introduction - MovieLens Dataset
######################################################################
## Aim is to train a machine learning algorithm using the inputs in one subset to predict movie ratings in the validation set
## Note: this process could take a couple of minutes because it is loading tidyverse and caret packages

# Package Instals
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#Libraries Referenced
library(tidyverse)
library(caret)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Check download
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
# Using R 3.6.2 version
set.seed(1, sample.kind="Rounding")
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


save(edx, validation, file = datafile)

} else {
  load(datafile)
}

#### Methods and Analysis ####

# Summarize Data:Total unique movies and users
head(edx) %>%
  print.data.frame()
summary(edx)

# Movies, Users and Genres in Database
edx %>%
#  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId))

summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId),
	    n_genres = n_distinct(genres))


# Ratings Mean
mean(edx$rating)

# Histogram: Ratings distribution in blue color font
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "blue") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating Distribution")

# Plot number of ratings per movie in blue color font
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue") +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Number of Ratings per Movie")


# Movies rated only once
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, n_rating = count) %>%
  slice(1:20) %>%
  knitr::kable()


# Plot Ratings Users - Number of Ratings in blue color font
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of Ratings") + 
  ylab("Number of Users") +
  ggtitle("User provided Number of Ratings")

# Plot Ratings Users - Mean
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean Rating") +
  ylab("Number of Users") +
  ggtitle("User provided mean movie ratings") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()


######################################################################
# Data Analysis - MovieLens Dataset
######################################################################

## Average movie rating model ##

## Simple Prediction based on Mean Rating
mu <- mean(edx$rating)
mu

naive_rmse <- RMSE(validation$rating, mu)
naive_rmse


# Validate and Save Results in date frame called rmse_ouputs
rmse_ouputs <- data_frame(method = "Average movie rating model", RMSE = naive_rmse)
rmse_ouputs %>% knitr::kable()

## Movie effect model ##

# Simple model considerating movie effect b_i
# Subtract the rating minus the mean for each rating the movie received
# Plot number of movies with the computed b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
                     ylab = "Number of movies", main = "Number of movies with the computed b_i")


# Test and save rmse results 
predicted_rating_values <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
rmse_model1 <- RMSE(predicted_rating_values, validation$rating)
rmse_ouputs <- bind_rows(rmse_ouputs,
                          data_frame(method="Movie effect model",  
                                     RMSE = rmse_model1 ))


# Validate Results
rmse_ouputs %>% knitr::kable()

## Movie and user effect model ##

# Plot penalty term user effect #
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))


user_avgs <- edx %>%
left_join(movie_avgs, by='movieId') %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu - b_i))
                                                             

# Test and save rmse results 
predicted_rating_values <- validation%>%
left_join(movie_avgs, by='movieId') %>%
left_join(user_avgs, by='userId') %>%
mutate(pred = mu + b_i + b_u) %>%
pull(pred)
                                                             
model_2_rmse <- RMSE(predicted_rating_values, validation$rating)
rmse_ouputs <- bind_rows(rmse_ouputs,
data_frame(method="Movie and user effect model",  
RMSE = model_2_rmse))

# Check result
rmse_ouputs %>% knitr::kable()
                                                    

## Regularisation ovie and user effect model
# Predict via regularisation, movie and user effect model.
lambdas <- seq(0, 10, 0.25)


# For each lambda,find b_i & b_u, followed by rating prediction & testing
# note:the below code could take some time  
rmses <- sapply(lambdas, function(l){
                                                               
mu <- mean(edx$rating)
 
b_i <- edx %>% 
group_by(movieId) %>%
summarize(b_i = sum(rating - mu)/(n()+l))
                                                               
b_u <- edx %>% 
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - mu)/(n()+l))
                                                               
predicted_rating_values <- 
validation %>% 
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
mutate(pred = mu + b_i + b_u) %>%
pull(pred)
                                                               
return(RMSE(predicted_rating_values, validation$rating))
})
                                                             
                                                             
# Plot RMSE against Lambdas to find optimal lambda                                                           
qplot(lambdas, rmses)  
                                                             
                                                             
# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambda
                                                            
# Test and save results                                                             
rmse_ouputs <- bind_rows(rmse_ouputs,
data_frame(method="Regularized movie and user effect model",  
RMSE = min(rmses)))

# Check result
rmse_ouputs %>% knitr::kable()

######################################################################
# Final Results - MovieLens Dataset
######################################################################                                                             
# RMSE Results - Providing the appropriate score given the reported RMSE                                                                                                                 
rmse_ouputs %>% knitr::kable()

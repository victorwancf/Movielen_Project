# ##########################################################
# # Create edx set, validation set (final hold-out test set)
# ##########################################################
# 
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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


#Basic info from quiz
#Dimension of edx data: (9000055,6)
#Numbre of zero and three in rating: 0,2121240
#Number of different movie in edx: 10677
#Number of different users in edx: 69878
#Number ofMovie Rating for each Genre: Drama (3910127), Comedy (3540930), Thriller (2325899), Romance (1712100)
#Movie with greatest number of rating:  Pulp Fiction (1994) (31362)
#Most given rating order: 4 (2588430)
head(edx)

#Seperating Years from title and converting timestamp format
library(stringr)
library(lubridate)
year <- str_extract_all(edx$title, "\\((\\d{4})\\)", simplify = T)
year<- str_extract_all(year, "(\\d{4})", simplify = T)
edx <- mutate(edx,year = year)
edx <- edx %>%
  mutate(title = str_replace_all(title,"\\((\\d{4})\\)","")) %>%
  mutate(date = as_datetime(timestamp))
edx$timestamp <- NULL
head(edx)

#Create train_set and test_set
y <- edx$rating
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- edx %>% slice(-test_index)
test_set <- edx %>% slice(test_index)
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Define RMSE
RMSE <- function(true_value, predicted_value){
  sqrt(mean((true_value - predicted_value)^2))
}

#Rating Average in the train set
mu <- mean(train_set$rating)
cat("The average rating of movie in train set is",mu)

avg_rmse <- RMSE(test_set$rating, mu)
rmse_results <- tibble(method = "Average rating", RMSE = avg_rmse)
rmse_results %>% knitr::kable()

#Plotting Movie effects
train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) %>% 
  ggplot(aes(b_i)) +
  geom_histogram(bins = 10,color="black", fill="lightblue") +
  ggtitle("Least square estimate of movie effect") +
  ylab("count")

#Movie Effect
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

moviemodel_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effects Model",  
                                     RMSE = moviemodel_rmse))
rmse_results %>% knitr::kable()
  
#Plotting rating vs Users
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30,color="black", fill="lightblue") +
  ggtitle("Average rating for user who have rated over 100 movies") +
  ylab("count") 

#User Effect
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

usermodel_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = usermodel_rmse ))
rmse_results %>% knitr::kable()

#Plotting Year effect
train_set %>%
  group_by(year) %>%
  summarize(b_y = mean(rating)) %>%
  ggplot(aes(year,b_y)) +
  geom_bar(stat='identity',fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Average Movie rating per year") +
  scale_x_discrete(breaks=seq(1915,2010,10))

#Year Effect
year_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by = 'year')%>%
  group_by(year) %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred

yearmodel_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year Effects Model",
                                     RMSE = yearmodel_rmse ))
rmse_results %>% knitr::kable()

#Plotting Genre Effect
edx %>% group_by(genres) %>%
  summarize(n = n(), avg_g = mean(rating)) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg_g)) %>%
  ggplot(aes(x = genres, y = avg_g)) +
  geom_point(col = "lightblue") +
  theme(axis.text.x = element_blank()) +
  ggtitle("Average Movie rating per genres for movie have over 1,000 ratings")

#Genre Effect
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by = 'year')%>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u - b_y))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by = 'year')%>%
  left_join(genre_avgs,by = 'genres')%>%
  group_by(genres) %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred

genremodel_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User +Year + Genre Effects Model",
                                     RMSE = genremodel_rmse ))
rmse_results %>% knitr::kable()


#Regularization
lambdas <- seq(-10, 10, .5)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="year") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "year") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_g + b_i + b_u + b_y) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)    
lambdas[which.min(rmses)]

#Regularization with optimal lamda
rmses_min <- rmses[which.min(rmses)]

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User +Year + Genre Effects Model after regularized",
                                     RMSE = rmses_min))
rmse_results %>% knitr::kable()


#Evaluation on validation set
year1 <- str_extract_all(validation$title, "\\((\\d{4})\\)", simplify = T)
year1 <- str_extract_all(year1, "(\\d{4})", simplify = T)
validation <- mutate(validation,year = year1)

mu <- mean(edx$rating)
l <- 4.5

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_y <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l))

b_g <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+l))

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "year") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_g + b_i + b_u + b_y) %>%
  pull(pred)

RMSE_Final <- RMSE(predicted_ratings, validation$rating)

rmse_results <- data_frame(method = "Movie + User +Year + Genre Effects Model after regularized", RMSE = RMSE_Final)
rmse_results %>% knitr::kable()


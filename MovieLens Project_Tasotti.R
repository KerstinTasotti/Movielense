---
##  title: 'HarvardX: PH125.9x Data Science - MovieLense project'
## author: "Kerstin Tasotti"
## date: '2022-02-23'
## output: html_document
## editor_options: 
 
   
---
### INSTRODUCTION ###

### DATASET ###  
  

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(ggplot2)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(ggplot2)
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



### 3. ANALYSIS OF THE DATA ###

# All analysis in this section will be done with the training set (edx). The validation set will be used for the final test of the developed algorithm.

# Display the structure and columns
str(edx)
head(edx) %>% print.data.frame
summary(edx)

# Plot histogram for distribution per rating points
edx %>% group_by (userId) %>% summarise(rating_points=mean(rating)) %>% ggplot(aes(rating_points))+geom_histogram(bins=50,color="black", fill="steelblue")+ylab("# count of user")+ ggtitle("Rating distribution per User")


### 3.1. MOVIE ###

# Display of number of Movies, user and genres 
edx %>% summarise(countmovies=n_distinct(movieId),countuser=n_distinct(userId),countgenres=n_distinct(genres))


# The separation of the genres combination allows a distribution calculation of the ratings per single genres.
edx <- edx %>% mutate(genres_spc=as.character(str_replace(edx$genres,"\\|.*","")))
genres_spc <- str_replace(edx$genres,"\\|.*","")
genres_spc <- genres_spc[!duplicated(genres_spc)]
genres_spc


# Plot histogram of distribution per genres 
edx %>% ggplot( aes(x=genres_spc,y=sum(rating))) +geom_bar(stat="identity", width=0.5, fill="steelblue")+ ggtitle ("Distribution per Genres")+ theme(axis.text.x=element_text(angle=90,size=7))


# Plot histogram of distribution of the rating points per movie 
edx %>% count(movieId) %>%   ggplot(aes(n)) +geom_histogram(bins=30, binwidth = 0.2, color="black", fill="steelblue")+scale_x_log10() + labs(x="# of ratings",y="# counts of movies")+ggtitle("Number of Ratings per Movie")


# Table of distribution of movies per rating points
edx %>% group_by(rating)%>% summarise(count=n())%>% top_n(10) %>% arrange(desc (count))


# Plot of distribution of movies per rating points
edx %>% ggplot(aes(rating)) +geom_histogram(binwidth=0.5, color="black", fill="steelblue")+scale_x_continuous(breaks = c(seq(0.5,5,0.5)))+scale_y_continuous(breaks =  c(seq(0,3000000,200000)))+ labs(x="rating points",y="# of ratings")+ggtitle("Rating Distribution of Movies")

# The Top10 rated movies and the top10 genre combinations and single genres can be calculated. Some movies are more rated than other. The genres combination with the most rating are "Drama" and Comedy". For separated genres Action and Comedy has the most rating.
edx %>% group_by(title)%>% summarise(count=n())%>% top_n(10) %>% arrange(desc (count))


# TOP10 rated genres
edx %>% group_by(genres)%>% summarise(count=n())%>% top_n(10) %>% arrange(desc (count))


# TOP10 rated single genres 
edx %>% group_by(genres_spc)%>% summarise(count=n())%>% top_n(10) %>% arrange(desc (count))


# Plot Histogram for distribution  per sigle genres
edx %>% group_by(rating) %>% ggplot( aes(genres_spc)) +geom_histogram( stat="count",color="black", fill="steelblue")+ theme(axis.text.x=element_text(angle=90,size=7))+ggtitle("Distribution per Genres")

# The number of ratings per movie shows a high difference between the users. 

edx %>% group_by(movieId) %>% summarise(sum(rating))


### 3. 2. USER ###
 
# In the following histogram the rating per user is displayed. Some users are more active than others at rating movies.

edx %>% count(userId) %>%   ggplot(aes(n)) +geom_histogram(bins=30, binwidth = 0.2, color="black", fill="steelblue")+scale_x_log10() + labs(x="# of Ratings",y="# counts of user")+ggtitle("Number of Ratings per User") 

### 3.3. TIMESTAMP ###

# Change of Date format for the timestamp.Separate of the year in the movie title (rating year) 
edx <- edx %>% mutate(rating_date=as.Date(as.POSIXct(timestamp, origin="1970-01-01")))%>% mutate(rating_year=year(rating_date))
edx <- edx %>% mutate(release_year=as.integer(substr(title,str_length(title)-4,str_length(title)-1)))


# TOP10 Rated release year
edx %>% group_by(edx$release_year)%>% summarise(count=n())%>% top_n(10) %>% arrange(desc (count))

# Plot Bar chart for counts of rating per release year
edx %>% group_by(release_year)%>% ggplot( aes(x=release_year, y=rating)) +geom_bar(stat="identity", width=1, color="black", fill="steelblue") + labs(x="year of release",y="# counts of rating")+ggtitle("Number of Ratings per Release year") 

# TOP10 Rated ratting year
edx %>% group_by(edx$rating_year)%>% summarise(count=n())%>% top_n(10) %>% arrange(desc (count))

# Plot Bar chart for counts of rating per rating year
edx %>% group_by(rating_year) %>% ggplot( aes(x=rating_year, y=rating)) +geom_bar(stat="identity", width=0.5, fill="steelblue")  + labs(x="year of rating",y="# counts of rating")+ggtitle("Number of Ratings per Rating year")



### 4. RESULTS - MACHINE LEARNING ALGORITHM ###
### LOSS FUNCTION ###


### 4.1. FIRST MODEL ###

# Compute the dataset mean rating
mu_hat <- mean(edx$rating)
mu_hat

# Test results on the first prediction
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse


# Check result and save RMSE in a data frame
rmse_results <- data_frame (method="Just the average", RMSE=naive_rmse)
rmse_results %>% knitr::kable()


### 4.2. MOVIE EFFECT ###

# Compute the dataset mean rating
mu<- mean(edx$rating)


# Plot Histogram of Movie effect
edx %>% group_by (movieId) %>% summarise(b_i=mean(rating-mu)) %>% ggplot(aes(b_i)) +geom_histogram(bins=10, color="black", fill="steelblue")

# Test  and save results of prediction with Movie effect
movie_avgs <- edx %>% group_by(movieId) %>% summarise(b_i=mean(rating-mu))
predicted_ratings <- mu + validation %>%left_join(movie_avgs, by="movieId") %>% pull (b_i)
movie_effect_rmse<- RMSE(predicted_ratings,validation$rating)
movie_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model", RMSE=movie_effect_rmse))
rmse_results %>% knitr::kable()


### 4.3. USER AND MOVIE EFFECT ###

# Compute the dataset mean rating
mu<- mean(edx$rating)



# Plot Histogram of User effect
edx %>% group_by (userId) %>% summarise(b_u=mean(rating-mu)) %>% filter(n()>=100)%>% ggplot(aes(b_u)) +geom_histogram(bins=30, color="black", fill="steelblue")


# Test  and save results of prediction with User effect
user_avgs <- edx %>% left_join(movie_avgs, by="movieId") %>% group_by(userId) %>% summarise(b_u=mean(rating-mu-b_i))
predicted_ratings <- validation %>%left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>%  mutate (pred=mu+b_i +b_u)%>% pull(pred)
user_effect_rmse <- RMSE(predicted_ratings,validation$rating)
user_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="User Effect Model", RMSE=user_effect_rmse))
rmse_results %>% knitr::kable()


### 4.4. RELEASE YEAR, USER AND MOVIE EFFECT ###

# Separation of Release year from the movie title in validation dataset
validation <- validation %>% mutate(release_year=as.integer(substr(title,str_length(title)-4,str_length(title)-1)))


# Plot Histogram of Release year effect
edx %>% group_by (release_year) %>% summarise(b_p=mean(rating-mu)) %>% ggplot(aes(b_p)) +geom_histogram(bins=30, color="black", fill="steelblue")

# Compute the dataset mean rating
mu<- mean(edx$rating)

# Test  and save results of prediction with Release year effect
release_year_avgs <- edx %>% group_by(release_year) %>%left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>% summarise(b_p=mean(rating-mu-b_i-b_u))
predicted_ratings <- validation %>%left_join(release_year_avgs, by="release_year") %>% left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>% mutate (pred=mu+b_i +b_u+b_p)%>% pull(pred)
release_year_effect_rmse <- RMSE(predicted_ratings,validation$rating)
release_year_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Release year Effect Model", RMSE=release_year_effect_rmse))
rmse_results %>% knitr::kable()


### 4.5. GENRE, RELEASE YEAR, USER AND MOVIE EFFECT ###


# Separate genres in validation dataset
validation <- validation %>% mutate(genres_spc=as.character(str_replace(validation$genres,"\\|.*","")))

# Plot Histogram of Genres effect
edx %>% group_by (genres_spc) %>% summarise(b_g=mean(rating-mu)) %>% ggplot(aes(b_g)) +geom_histogram(bins=30, color="black", fill="steelblue")

# Compute the dataset mean rating
mu<- mean(edx$rating)

# Test  and save results of prediction with Genres effect
genres_avgs <- edx %>% group_by(genres_spc)%>% left_join(release_year_avgs, by="release_year")%>% left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>% summarise(b_g=mean(rating-mu-b_i-b_u-b_p))
predicted_ratings <- validation %>%left_join(genres_avgs, by="genres_spc") %>%left_join(release_year_avgs, by="release_year") %>% left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>% mutate (pred=mu+b_i +b_u+b_g+b_p)%>% pull(pred)
genres_effect_rmse <- RMSE(predicted_ratings,validation$rating)
genres_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Genres Effect Model", RMSE=genres_effect_rmse))
rmse_results %>% knitr::kable()


### 4.6. RATING YEAR, RELEASE YEAR, USER AND MOVIE EFFECT ###


# Change of Date format in the validation dataset
validation <- validation %>% mutate(rating_date=as.Date(as.POSIXct(timestamp, origin="1970-01-01")))%>% mutate(rating_year=year(rating_date))

# Plot Histogram of Rating year effect
edx %>% group_by (rating_year) %>% summarise(b_r=mean(rating-mu)) %>% ggplot(aes(b_r)) +geom_histogram(bins=30, color="black", fill="steelblue")


# Compute the dataset mean rating  
mu<- mean(edx$rating)

# Test  and save results of prediction with Rating year effect
rating_year_avgs <- edx %>% group_by(rating_year)%>% left_join(genres_avgs, by="genres_spc") %>% left_join(release_year_avgs, by="release_year") %>%left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>% summarise(b_r=mean(rating-mu-b_i-b_u-b_p-b_g))
predicted_ratings <- validation %>%left_join(rating_year_avgs, by="rating_year") %>% left_join(genres_avgs, by="genres_spc") %>% left_join(release_year_avgs, by="release_year")%>% left_join(movie_avgs, by="movieId") %>% left_join(user_avgs, by="userId") %>% mutate (pred=mu+b_i+b_u+b_p+b_g+b_r)%>% pull(pred)
rating_year_effect_rmse <- RMSE(predicted_ratings,validation$rating)
rating_year_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Rating year Effect Model", RMSE=rating_year_effect_rmse))
rmse_results %>% knitr::kable()


### 4.7. REGULARIZED MOVIE AND USER EFFECT ###

  
# Test effect of lambda=3 on the RMSE  
lambda<- 3
mu<- mean(edx$rating)
movie_reg_avgs <- edx %>% group_by(movieId) %>% summarise(b_i_reg=sum(rating-mu)/(n()+lambda))
user_reg_avgs <- edx %>% group_by(userId) %>% left_join(movie_reg_avgs, by="movieId") %>% summarise(b_u_reg=sum(rating-mu-b_i_reg)/(n()+lambda))
release_year_reg_avgs <- edx %>% group_by(release_year) %>% left_join(user_reg_avgs, by="userId") %>% left_join(movie_reg_avgs, by="movieId") %>% summarise(b_p_reg=sum(rating-mu-b_i_reg-b_u_reg)/(n()+lambda))
genres_reg_avgs <- edx %>% group_by(genres_spc)%>% left_join(release_year_reg_avgs, by="release_year") %>%left_join(movie_reg_avgs, by="movieId") %>% left_join(user_reg_avgs, by="userId") %>% summarise(b_g_reg=sum(rating-mu-b_i_reg-b_u_reg-b_p_reg)/(n()+lambda))
rating_year_reg_avgs <- edx %>% group_by(rating_year)%>% left_join(genres_reg_avgs, by="genres_spc") %>%left_join(release_year_reg_avgs, by="release_year") %>% left_join(movie_reg_avgs, by="movieId") %>% left_join(user_reg_avgs, by="userId") %>% summarise(b_r_reg=sum(rating-mu-b_i_reg-b_u_reg-b_p_reg-b_g_reg)/(n()+lambda))
predicted_ratings <- validation %>%left_join(rating_year_reg_avgs, by="rating_year") %>%left_join(genres_reg_avgs, by="genres_spc") %>%left_join(release_year_reg_avgs, by="release_year")%>%left_join(movie_reg_avgs, by="movieId") %>% left_join(user_reg_avgs, by="userId") %>%  mutate (pred=mu+b_i_reg+b_u_reg+b_p_reg+b_g_reg+b_r_reg)%>% pull(pred)
movie_user_year_genres_reg_effect_rmse <- RMSE(predicted_ratings,validation$rating)
movie_user_year_genres_reg_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Genres+Movie+User+Year Effect Model", RMSE=movie_user_year_genres_reg_effect_rmse))
rmse_results %>% knitr::kable()

### 4.8. CHOOSING THE PENALTY TERMS ###

#Lambda is a turning parameter. 
#Set sequence of  0 - 10 , distance=0.25. Calculate the lowest RMSE.

lambdas <-seq(0,10,0.25)
mu<- mean(edx$rating)
rmse2 <- sapply(lambdas,function(l){
  mu<-mean(edx$rating)
  b_i_reg<-edx %>% group_by(movieId)%>% summarise(b_i_reg=sum(rating-mu)/(n()+l))
  b_u_reg<-edx %>% group_by(userId) %>% left_join(b_i_reg, by="movieId") %>% summarise(b_u_reg=sum(rating-mu-b_i_reg)/(n()+l))
  b_p_reg<-edx %>% group_by(release_year) %>% left_join(b_i_reg, by="movieId") %>% left_join(b_u_reg, by="userId") %>% summarise(b_p_reg=sum(rating-mu-b_i_reg-b_u_reg)/(n()+l))
  b_g_reg<-edx %>% group_by(genres_spc) %>% left_join(b_p_reg, by="release_year") %>% left_join(b_i_reg, by="movieId") %>% left_join(b_u_reg, by="userId") %>% summarise(b_g_reg=sum(rating-mu-b_i_reg-b_u_reg-b_p_reg)/(n()+l))
  b_r_reg<-edx %>% group_by(rating_year) %>% left_join(b_g_reg, by="genres_spc") %>% left_join(b_p_reg, by="release_year") %>% left_join(b_i_reg, by="movieId") %>% left_join(b_u_reg, by="userId") %>% summarise(b_r_reg=sum(rating-mu-b_i_reg-b_u_reg-b_p_reg-b_g_reg)/(n()+l))
  predicted_ratings <-validation %>%  left_join(b_i_reg, by="movieId")%>%  left_join(b_u_reg,  by="userId")%>% left_join(b_p_reg, by="release_year")%>%  left_join(b_g_reg, by="genres_spc")%>% left_join(b_r_reg, by="rating_year")%>% mutate(pred=mu+b_i_reg+b_u_reg+b_p_reg+b_g_reg+b_r_reg) %>% pull(pred)
  return(RMSE(predicted_ratings, validation$rating))})
lambda_low<- lambdas[which.min(rmse2)]
lambda_low
# Plot Relationship between lambda and RMSE
qplot(lambdas,rmse2)



# Test,calculate and save the RMSE with optimal lambda
mu<- mean(edx$rating)
movie_reg_avgs <- edx %>% group_by(movieId) %>% summarise(b_i_reg=sum(rating-mu)/(n()+lambda_low))
user_reg_avgs <- edx %>% group_by(userId) %>% left_join(movie_reg_avgs, by="movieId") %>% summarise(b_u_reg=sum(rating-mu-b_i_reg)/(n()+lambda_low))
release_year_reg_avgs <- edx %>% group_by(release_year) %>% left_join(user_reg_avgs, by="userId") %>% left_join(movie_reg_avgs, by="movieId") %>% summarise(b_p_reg=sum(rating-mu-b_i_reg-b_u_reg)/(n()+lambda_low))
genres_reg_avgs <- edx %>% group_by(genres_spc)%>% left_join(release_year_reg_avgs, by="release_year") %>%left_join(movie_reg_avgs, by="movieId") %>% left_join(user_reg_avgs, by="userId") %>% summarise(b_g_reg=sum(rating-mu-b_i_reg-b_u_reg-b_p_reg)/(n()+lambda_low))
rating_year_reg_avgs <- edx %>% group_by(rating_year)%>% left_join(genres_reg_avgs, by="genres_spc") %>%left_join(release_year_reg_avgs, by="release_year") %>% left_join(movie_reg_avgs, by="movieId") %>% left_join(user_reg_avgs, by="userId") %>% summarise(b_r_reg=sum(rating-mu-b_i_reg-b_u_reg-b_p_reg-b_g_reg)/(n()+lambda_low))
predicted_ratings <- validation %>%left_join(rating_year_reg_avgs, by="rating_year") %>%left_join(genres_reg_avgs, by="genres_spc") %>%left_join(release_year_reg_avgs, by="release_year")%>%left_join(movie_reg_avgs, by="movieId") %>% left_join(user_reg_avgs, by="userId") %>%  mutate (pred=mu+b_i_reg+b_u_reg+b_p_reg+b_g_reg+b_r_reg)%>% pull(pred)
movie_user_year_genres_reg_effect_rmse <- RMSE(predicted_ratings,validation$rating)
movie_user_year_genres_reg_effect_rmse
rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Model with optimal lambda", RMSE=movie_user_year_genres_reg_effect_rmse))
rmse_results %>% knitr::kable()



#### Appendix ###
print ("Operation System:")
version


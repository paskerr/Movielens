
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
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

# rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Initial Analysis####

#before moving forward with the analysis, we check the dataset in a tidy format 

movielens %>%as_tibble() #data is tidy, we can start 

##Partitioning the dataset into validation and edx data sets####


# Validation set will be 10% of movieLens data
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


#make a quick check on the edx data set
head(edx)
nrow(edx) #9000055
ncol(edx)#6
n_distinct(edx$movieId)
n_distinct(edx$userId)


# I will continue with exploratory data analysis by only using the train data set (edx). 
edx%>%summarize(n_users=n_distinct(userId),n_movies=n_distinct(movieId)) #n_user=69878 #n_movies=10677


#check if there is any zero-rating
edx%>%filter(rating==0 )%>%tally()


#check if there is null or na values in the rating column
sum(is.na(edx$rating))
sum(is.na(edx$rating)) # there is no missing values in the rating column

#Before further analysis, I split the edx dataset into train and set dataset by 80% and 20% percent.  
set.seed(1,sample.kind = "Rounding")
index<-createDataPartition(y=edx$rating,times=1,p=0.2,list=FALSE)
train_edx=edx[-index,]
test_edx=edx[index,]


#to make sure not to including users and movies in the test set that do not appear in the training set,these entries are removed using the semi_join function:
test_edx<-test_edx%>%semi_join(train_edx,by="movieId")%>%
  semi_join(train_edx, by="userId")



#Visualization####

# graph of movieId vs the number of rating 
train_edx%>%group_by(movieId)%>%
  summarize(n=n())%>%ggplot(aes(n))+
  geom_freqpoly(color='tan2',bins = 30, binwidth = 0.2)+
  scale_x_log10()+
  xlab("Number of Ratings")+
  ggtitle("Number of Rating for Movie")+
  theme_bw() #As seen in the above, number of ratings substantially vary among different movies. Some movies have much greater number of rating than others.

#graph of movieId vs mean rating
train_edx%>%group_by(movieId)%>%
  summarize(mean_rating=mean(rating))%>%ggplot(aes(mean_rating))+
  geom_freqpoly(color='tan2',bins = 30, binwidth = 0.2)+
  xlab("Mean Ratings")+
  ggtitle(" Mean Ratings for Movies")+
  theme_bw() #In the graph above, it is shown that average ratings are changed significantly among movies. Therefore, the movie effect should be considered in the model development.


#graph of userId vs the number of ratings 
train_edx%>%group_by(userId)%>%
  summarize(n=n())%>%ggplot(aes(n))+
  geom_freqpoly(color='tan2',bins = 30, binwidth = 0.2)+
  scale_x_log10()+
  xlab("Number of Ratings")+
  ggtitle("Number of Rating for Users")+
  theme_bw() #As shown in the graph above, some users are more involved in rating than others. Therefore, there is a substantial difference between users in terms of the number of ratings.

#graph of userId vs the mean ratings 
train_edx%>%group_by(userId)%>%
  summarize(mean_rating=mean(rating))%>%ggplot(aes(mean_rating))+
  geom_freqpoly(color='tan2',bins = 30, binwidth = 0.2)+
  xlab("Number of Ratings")+
  ggtitle("Mean Ratings for Users")+
  theme_bw() # Therefore, there is also a substantial difference between users in terms of the average rating. The user's effect also should be included in the model development.


#graph of time (month) vs number of ratings, first the timestamp need to be converted date-time format.
train_edx$timestamp=as_datetime(train_edx$timestamp)
train_edx %>% mutate(year=year(timestamp))%>%
  group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point(color="blue") +
  geom_smooth(color="red") #time(year) does not significantly change over the years, therefore i do not include time as an effect to perivous model.

#graph of genres and number of ratings
train_edx%>%group_by(genres)%>%
  summarize(n=n())%>%
  filter(n>1000)%>%
  ggplot(aes(n))+
  geom_freqpoly(color='tan2',bins = 30)+
  scale_x_log10(labels=scales::comma)+
  xlab("Number of Ratings")+
  ggtitle("Number of Rating for Genres")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60,hjust=1))

#graph of genres and mean of rating
train_edx%>%group_by(genres)%>%
  filter(n()>1000)%>%
  summarize(mean_rating=mean(rating))%>%
  ggplot(aes(mean_rating))+
  geom_freqpoly(color='tan2',bins = 30)+
  scale_x_log10(labels=scales::comma)+
  xlab("Mean Rating")+
  ggtitle("Mean Rating for Genres")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60,hjust=1))#genres show that genre also has an important effect on ratings' pattern (average and number of rating). Then, the genres' effect will be placed on the model. 


#Model Deveploment#### 

#creating RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##Model 1####
#This model predicts the rating by simply averaging movie ratings regardless of user.
mu<-mean(train_edx$rating)
mu


RMSE1=RMSE(test_edx$rating,mu) 
RMSE1  # 1.05904

#since the rmse greater than 1, the error is larger than on star.I will built new model to improve RMSE to make better prediction


all_rmse=tibble(Model="Model_1", RMSE=RMSE1)
all_rmse%>%knitr::kable()


##Model 2 - Movie Effect####
#Since different movies are rated at  different rates, I add b_i (movie effects) to our model. 
b_i=train_edx%>%
  group_by(movieId)%>%
  summarize(b_i=mean(rating-mu))
b_i


#calculate the RMSE for model 2
predicted_ratings2 <-test_edx %>% 
left_join(b_i, by='movieId') %>%
mutate(pred=mu+b_i)%>%.$pred 
  
RMSE2= RMSE(predicted_ratings2, test_edx$rating) 
RMSE2

all_rmse=tibble(Model=c("Model_1","Model_2"), RMSE=c(RMSE1,RMSE2))
all_rmse%>%knitr::kable()  #rmse= 0.9437429,  there is an improvement.but,new models need to be developed to achieve desired RMSE value



##Model 3 - User Effects####

#adding b_u (user effects) to the previous model and creating model 3
b_u<-train_edx%>%
  left_join(b_i,by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=mean(rating-mu-b_i))


#making prediction with model 3
predicted_ratings3<-test_edx%>%
  left_join(b_i,by="movieId")%>%
  left_join(b_u,by="userId")%>%
  mutate(pred=mu+b_i+b_u)%>%
  .$pred


#calculating rmse of model 3
RMSE3=RMSE(predicted_ratings3,test_edx$rating) 
RMSE3 # 0.865932


all_rmse<-data.frame(method=c("Model_1","Model_2","Model_3"),RMSE=c(RMSE1,RMSE2,RMSE3))
all_rmse%>%knitr::kable() #There is an improvement in the RMSE.


##Model- Time Effects
#since rating does not change significantly, I do not include time effect to the previous model.


##Model 4- Genres Effects
#adding genres effect to the previous model
b_g<-train_edx%>%
  left_join(b_i,by="movieId")%>%
  left_join(b_u,by="userId")%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating-mu-b_i-b_u))

#making prediction with model 4
predicted_ratings4<-test_edx%>%
  left_join(b_i,by="movieId")%>%
  left_join(b_u,by="userId")%>%
  left_join(b_g,by="genres")%>%
  mutate(pred=mu+b_i+b_u+b_g)%>%
  .$pred

#calculating rmse of model 4
RMSE4=RMSE(predicted_ratings4,test_edx$rating) 
RMSE4  #The genre effect improved the RMSE to 0.8655941.

all_rmse<-data.frame(method=c("Model_1","Model_2","Model_3","Model_4"),RMSE=c(RMSE1,RMSE2,RMSE3,RMSE4))
all_rmse%>%knitr::kable()


#Regularization####
#constraining the uncertainties from effect size
#choosing the best tuning parameter (alpha) with cross-validation

lambdas<-seq(0,10,0.25)

rmses_final<-sapply(lambdas,function(x){
  
  mu<-mean(train_edx$rating)  
  
  b_i<-train_edx%>%
    group_by(movieId)%>%
    summarize(b_i=sum(rating-mu)/(n()+x))
  
  b_u<-train_edx%>%
    left_join(b_i,by="movieId")%>%
    group_by(userId)%>%
    summarize(b_u=sum(rating-b_i-mu)/(n()+x))
  
  b_g<-train_edx%>%
    left_join(b_i,by="movieId")%>%
    left_join(b_u,by="userId")%>%
    group_by(genres)%>%
    summarize(b_g=sum(rating-mu-b_i-b_u)/(n()+x))
  
  
  predicted_rating_reg<-test_edx%>%
    left_join(b_i,by="movieId")%>%
    left_join(b_u,by="userId")%>%
    left_join(b_g,by="genres")%>%
    mutate(pred=mu+b_i+b_u+b_g)%>%
    pull(pred)
  
  return(RMSE(predicted_rating_reg,test_edx$rating))
})



qplot(lambdas,rmses_final,geom=c("point","line"))

min(rmses_final) #0.8649406

data.frame(lambdas,rmses_final)%>%filter(rmses_final==min(rmses_final))%>%.$lambdas # best lambda with minimum RMSE is 4.75.


#################################################

#FINAL MODEL AND VALIDATION DATASET

#final model is built with lambda 4.75.
#Edx was used to train model and validation was used to test the model.
mu<-mean(edx$rating)  

b_i<-edx%>%
  group_by(movieId)%>%
  summarize(b_i=sum(rating-mu)/(n()+4.75))

b_u<-edx%>%
  left_join(b_i,by="movieId")%>%
  group_by(userId)%>%
  summarize(b_u=sum(rating-b_i-mu)/(n()+4.75))

b_g<-edx%>%
  left_join(b_i,by="movieId")%>%
  left_join(b_u,by="userId")%>%
  group_by(genres)%>%
  summarize(b_g=sum(rating-mu-b_i-b_u)/(n()+4.75))


predicted_rating_reg<-validation%>%
  left_join(b_i,by="movieId")%>%
  left_join(b_u,by="userId")%>%
  left_join(b_g,by="genres")%>%
  mutate(pred=mu+b_i+b_u+b_g)%>%
  pull(pred)

RMSE_final=RMSE(predicted_rating_reg,validation$rating)
RMSE_final

#The project's ultimate aim is to reduce RMSE to **0.86490** or below; the latest model calculated the RMSE as **0.8644514**, which exceeds the target RMSE and provides better prediction.

#As the conclusion, Regularized Movie,User and Genres Effect (Model 5) linear model is suggested to predict movie rating since this model produces the minimum RMSE.


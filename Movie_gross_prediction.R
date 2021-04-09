# The dataset has been taken from kaggle.
# Dataset Description - IMDB 5000 Movie Dataset
# Meta data - rows/instances: 5044, columns/attributes: 28

# 1 Introduction
# 1.1 Background
#   We all watch movies who doesn't. It is an art form unlike any other. We all have seen some movies 
#   which have impacted us. Now we have movie making industry like Hollywood, Bollywood etc. It has become 
#   a form of business. The success of a movie depends on its domestic gross and International gross surpassing
#   the budget of the movie. In this project we will be estimating gross of a movie depending on the various 
#   factors related to the movie using appropriate Machine Learning algorithms. 

# 1.2 Data Description
#   The dataset is from kaggle. It contains 28 variables for 5043 rows. There are 2398 unique director names.
#   4917 unique movie titles. We are trying to predict gross while the other attributes are predictors.

# 1.3 Problem Statement

#   Based on the massive movie information, it would be interesting to understand what are the important factors 
#   that make a movie more successful than others. So, we would like to analyze what kind of movies are grossing
#   more and getting higher profits. We also want to show the results of this analysis in an intuitive way by 
#   visualizing outcome using ggplot2 in R.
#   In this project, we take gross as response variable and focus on operating predictions by analyzing 
#   the rest of variables in the IMDB 5000 movie data.

# 2 Data Exploration

# 2.1 Load Data

# Load packages
library(ggplot2) # visualization
library(ggrepel) # contains extra geoms for ggplot2
library(ggthemes) # visualization
library(data.table) # data.frame
library(dplyr) # data manipulation
library(stringr) # character manipulation
library(readr) # read data

IMDB_Movies <- read_csv("Semester 5/IDS/Project folder/movie_metadata.csv")
View(IMDB_Movies)
head(IMDB_Movies)
summary(IMDB_Movies)

#   We have 5043 observations of 28 variables. The response variable is "gross" and other predictor variables 
#   contain character, strings and numerical values.

# 2.2 Remove Duplicates

#   In this data, we will check for duplicate rows and delete them.

#   checking for duplicate rows
sum(duplicated(IMDB_Movies))
#   delete duplicate rows
IMDB_Movies <- IMDB_Movies[!duplicated(IMDB_Movies), ]

#   We have 4998 rows left.

#   We are working on effect of other attributes on gross of movie. The gross of movie is numeric variable

# 2.3 Remove rows containing NA values

#   checking for rows not containing any NA value
complete.cases(IMDB_Movies)
which(complete.cases(IMDB_Movies))
#   storing rows containing NA values in new vector
no_NA <- which(!complete.cases(IMDB_Movies))
no_NA
#   removing rows from dataset
IMDB_Movies <- IMDB_Movies[-no_NA, ]

#   3,719 rows left

# 2.4 Split Genres

#   we want to see relationship between genres and gross as one movie is having multiple genres it is best to
#   if they are having any impact on gross or we can remove genres attribute.

genre_type <- IMDB_Movies %>% select(movie_title,genres,contains('name'))
genre_type <- data.frame(lapply(genre_type, as.character), stringsAsFactors=FALSE)

#   Separating our Genre variable
library(reshape)
break_genre <- colsplit(IMDB_Movies$genres,split="\\|",names=c("n1","n2","n3","n4","n5","n6","n7","n8"))
break_genre <- data.frame(lapply(break_genre, as.character), stringsAsFactors=FALSE)

for (i in 1: nrow(break_genre)) { 
  break_genre[i,duplicated(as.character(break_genre[i,]))] <- ""
}

#   In the meanwhile lets just look at genre based movie gross from boxplot visualization.

gross_in_million <- IMDB_Movies$gross/1000000
IMDB_Movies <- mutate(IMDB_Movies, gross_in_million)
movie_gross <- IMDB_Movies %>% select (movie_title,gross_in_million) 
genre_gross <- cbind(movie_gross,break_genre)
genre_gross <- melt(genre_gross,id.vars=1:2)
genre_gross <- genre_gross[-3]
genre_gross <- genre_gross %>% filter(value!="") %>% droplevels() #filtering out unknown genres

ggplot(aes(y = gross_in_million, x = value, fill=value), data=genre_gross) + geom_boxplot() + theme(axis.text.x = element_text(angle=70,hjust=1))

#   As we can see from the plot the Animation and Adventure genre movies are the highest grossing movies
#   whereas Film-Noir and Documentary are the least grossing genre.

# 3 Redundant Data Cleaning

# 3.1.1 Analyze aspect ratio

table(IMDB_Movies$aspect_ratio)

#   The most common aspect ratios are 1.85 and 2.35. For analyzing purpose, we group other ratios together.

#   In order to compute the mean of gross for different aspect_ratio.

mean(IMDB_Movies$gross[IMDB_Movies$aspect_ratio == 1.85])
mean(IMDB_Movies$gross[IMDB_Movies$aspect_ratio == 2.35])
mean(IMDB_Movies$gross[IMDB_Movies$aspect_ratio != 1.85 & IMDB_Movies$aspect_ratio != 2.35])

#   From the means of gross for different aspect ratios, we can see there is not much difference.
#   For aspect ratio = 1.85, average gross is 44 Million$.
#   For aspect ratio = 2.35, average gross is 58 Million$.
#   Combining both ratios average is 51 Million$.

IMDB_Movies <- subset(IMDB_Movies, select = -c(aspect_ratio))

# 3.1.2 Sort out content ratings

#   According to the history of naming these different content ratings, we find M = GP = PG, 
#   X = NC-17. We want to replace M and GP with PG, replace X with NC-17, because these two are what we 
#   use nowadays.

#   assign 'PG' rating to 'M' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'M']   <- 'PG'
#   assign 'PG' rating to 'GP'
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'GP']  <- 'PG' 
#   assign 'NC-17' rating to 'X'
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'X']   <- 'NC-17'

#   We want to replace "Approved", "Not Rated", "Passed", "Unrated" with the most common rating "R".

IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Approved']  <- 'R' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Not Rated'] <- 'R' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Passed']    <- 'R' 
IMDB_Movies$content_rating[IMDB_Movies$content_rating == 'Unrated']   <- 'R' 
#   convert character to factor
IMDB_Movies$content_rating <- factor(IMDB_Movies$content_rating)
table(IMDB_Movies$content_rating)

#   Now we only have 5 different content ratings.

# 3.2 Add Columns

#   We have gross and budget information. So let's add two colums: profit and percentage return on 
#   investment for further analysis.

IMDB_Movies <- IMDB_Movies %>% mutate(profit = gross - budget, return_on_investment_perc = (profit/budget)*100)

# 3.3 Remove Columns

# 3.3.1 Is the color of a movie influential?

table(IMDB_Movies$color)

#   More than 96% movies are colored, which indicates that this predictor is nearly constant. 
#   Let's remove this predictor.

#   delete attribute color
IMDB_Movies <- subset(IMDB_Movies, select = -c(color))

# 3.3.2 Is language an important factor for imdb score? What about country?

table(IMDB_Movies$language)

#   Out of 3,719 movies 3,566 are in English, which indicates this predictor is nearly constant.

IMDB_Movies <- subset(IMDB_Movies, select = -c(language))

# 3.3.3 Let's take a look at predictor country.

table(IMDB_Movies$country)

#   Around 79% movies are from USA, 8% from UK, 13% from other countries. So we group other countries 
#   together to make this categorical variable with less levels: USA, UK, Others.

#   create a new vector having country names and character 'Others'
levels(IMDB_Movies$country) <- c(levels(IMDB_Movies$country), "Others")
IMDB_Movies$country[(IMDB_Movies$country != 'USA') & (IMDB_Movies$country != 'UK')] <- 'Others'
IMDB_Movies$country <- factor(IMDB_Movies$country)
table(IMDB_Movies$country)

# 4 Data Visualization

# 4.1 Top 20 grossing movies

IMDB_Movies %>% arrange(desc(gross_in_million)) %>% top_n(20, gross) %>%
  ggplot(aes(x=budget/1000000, y=gross_in_million)) + geom_point() + geom_smooth() +
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget in million$", y = "Gross in million$", title = "Top 20 Grossing Movies") +
  theme(plot.title = element_text(hjust = 0.5))

# 4.2 Top 20 profitable movies

IMDB_Movies %>% arrange(desc(profit)) %>% top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y=profit/1000000)) + geom_point() + geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget in million$", y = "Profit in million$", title = "Top 20 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

#   These are the top 20 movies based on the Profit earned. It can be inferred from this plot that high 
#   budget movies tend to earn more profit but there are some exceptions like Star Wars: Episode IV and 
#   E.T. the Extra-Terrestrial which have made huge profit on small budget.

# 4.3 Top 20 movies on its Return on Investment

IMDB_Movies %>% arrange(desc(profit)) %>% top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y = return_on_investment_perc)) + 
  geom_point() + geom_smooth() + geom_text_repel(aes(label = movie_title)) + 
  labs(x = "Budget in million$", y = "Percent Return on Investment", title = "20 Most Profitable Movies based on its Return on Investment") +
  theme(plot.title = element_text(hjust = 0.5))

#   These are top 20 movies based on the percentage return on investment. We conclude from this plot that movies
#   made on a high budget are low on returns percentage.

# 4.4 Top 20 directors with highest grossing movies

library(formattable)
IMDB_Movies %>% group_by(director_name) %>% summarise(Average_Gross = mean(gross_in_million)) %>% 
  arrange(desc(Average_Gross)) %>% top_n(20, Average_Gross) %>% formattable(list(Average_Gross = 
  color_bar("orange")), align = 'l')

# 4.5 Effect of imdb_score on gross

plot(IMDB_Movies$imdb_score,IMDB_Movies$gross,xlab="IMDb Rating", ylab="Gross in Million$", type = "h",
     main = "Plot of elationship between IMDb Rating and Gross") 
  

#   This is an analysis on Average Gross earnings by movies for a particular imdb score.

#   The highest grossing movies are int the range of imdb_score range of 6-9.

# 4.6 Relation between number of facebook likes and gross

library(plotly)
IMDB_Movies %>% plot_ly(x = ~movie_facebook_likes, y = ~gross, mode = "markers", alpha = 0.7, type = "scatter")


#   Most of the points are on the left bottom of the plot emphasising that facebook likes do not have very
#   much impact on movie gross.

# 4.7 Relation between number of voted users and gross

IMDB_Movies %>% plot_ly(x = ~num_voted_users, y = ~gross, mode = "markers", alpha = 0.7, type = "scatter")

#   Most of the movies in the range 500,000 user votes are grossing around 200 million$ or less.

# 4.8 Relation between content_rating and gross

IMDB_Movies %>% group_by(content_rating) %>% summarise(Average_Gross = mean(gross)) %>% 
  formattable(list(Average_Gross = color_bar("green")), align = 'l')

#   Movies with content rating "G" are highest grossing because any age group can see them. 

# 5 Data Pre-processing

# 5.1 Remove Names

#   number of directors
sum(uniqueN(IMDB_Movies$director_name))
#   number of actors
sum(uniqueN(IMDB_Movies[, c("actor_1_name", "actor_2_name", "actor_3_name")]))

#   Since all the names are so different for the whole dataset, there is no point to use names to predict gross.
#   Same with plot keywords, they are too diverse to be used in the prediction.
#   And movie link is also a redundant variable.

IMDB_Movies <- subset(IMDB_Movies, select = -c(director_name, actor_2_name, actor_1_name,
                                 movie_title, actor_3_name, plot_keywords, 
                                 movie_imdb_link))

# 5.2 Remove Linear Dependent Variables

#   For the purpose of data exploration, we added two variables based on existing variables: profit and 
#   return_on_investment_perc. In order to avoid multicollinearity, here we remove these two added variables.

IMDB_Movies <- subset(IMDB_Movies, select = -c(profit, return_on_investment_perc))
IMDB_Movies <- subset(IMDB_Movies, select = -c(gross_in_million))

# 5.3 Remove Highly Correlated Variables

#   First we plot the correlation heatmap for our data.
library(GGally)
ggcorr(IMDB_Movies, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#   Based on the heatmap, we can see some high correlations (greater than 0.7) between predictors.

#   According to the highest correlation value 0.95, we find actor_1_facebook_likes is highly 
#   correlated with the cast_total_facebook_likes, and both actor2 and actor3 are also somehow 
#   correlated to the total. So we want to modify them into two variables: actor_1_facebook_likes 
#   and other_actors_facebook_likes.

#   There are high correlations among num_voted_users, num_user_for_reviews and num_critic_for_reviews. 
#   We want to keep num_voted_users and take the ratio of num_user_for_reviews and num_critic_for_reviews.

#   add up actor 2 and 3 facebook likes into other actors facebook likes
IMDB_Movies$other_actors_facebook_likes <- IMDB_Movies$actor_2_facebook_likes + IMDB_Movies$actor_3_facebook_likes
#   use the ratio of critical reviews amount to total reviews amount
IMDB_Movies$critic_review_ratio <- IMDB_Movies$num_critic_for_reviews / IMDB_Movies$num_user_for_reviews
#   delete columns
IMDB_Movies <- subset(IMDB_Movies, select = -c(cast_total_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes))
IMDB_Movies <- subset(IMDB_Movies, select = -c(num_critic_for_reviews, num_user_for_reviews))

#   Now the correlation heatmap becomes like this.

ggcorr(IMDB_Movies, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

#   We don't see any strong correlation (absolute value greater than 0.7) any more.

# 5.4 Bin Response Variable

#   Our goal is to build a model, which can help us predict if a movie is going to do good box office collection
#   or not. So we don't really want an exact score to be predicted, we only want to know how good or how bad 
#   is the total gross. Therefore, we bin the score into 4 buckets: less than 6,000,000, 6,000,000~25,000,000,
#   25,000,000~65,000,000 and 65,000,000~800,000,000, which represents bad, ok, good and excellent respectively.

IMDB_Movies$binned_score <- cut(IMDB_Movies$gross, breaks = c(0,6000000,25000000,65000000,800000000))

# 5.6 Split Data

#   Here we split data into training, validation and test sets with the ratio of 6:2:2.
set.seed(45)
train.index <- sample(row.names(IMDB_Movies), dim(IMDB_Movies)[1]*0.6)
valid.index <- sample(setdiff(row.names(IMDB_Movies), train.index), dim(IMDB_Movies)[1]*0.2)
test.index <- setdiff(row.names(IMDB_Movies), union(train.index, valid.index))
train <- IMDB_Movies[train.index, ]
valid <- IMDB_Movies[valid.index, ]
test <- IMDB_Movies[test.index, ]


## 6 Implement Algorithm

# 6.1 Classification Tree

# 6.1.1 Full-grown Tree

library(rpart)
library(rpart.plot)
# Full grown tree
class.tree <- rpart(binned_score ~ duration + director_facebook_likes + actor_1_facebook_likes + 
                    num_voted_users + facenumber_in_poster + country + content_rating + budget + 
                    title_year + imdb_score + movie_facebook_likes + other_actors_facebook_likes + 
                    critic_review_ratio , data = train, method = "class")
## plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0) 


# 6.1.2 Best-pruned Tree

#   cross-validation procedure
#   argument cp sets the smallest value for the complexity parameter.
set.seed(51)
cv.ct <- rpart(binned_score ~ num_voted_users + imdb_score + duration + movie_facebook_likes + title_year +
                critic_review_ratio + director_facebook_likes + actor_1_facebook_likes + facenumber_in_poster + 
                country + content_rating + budget + other_actors_facebook_likes, data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)

#   The 13th tree tree has the lowest cross-validation error (xerror): 0.62923.

#   prune by lowest cp
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

# 6.1.3 Apply Model

library(caret)
#   apply model on training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
#   generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$binned_score)

#   Accuracy is 0.6333 for training set.


#   apply model on validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
#   generate confusion matrix for validation data
confusionMatrix(tree.pred.valid, valid$binned_score)

#   Accuracy is 0.5397 for validation set.

#   apply model on test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
#   generate confusion matrix for test data
confusionMatrix(tree.pred.test, test$binned_score)

#   Accuracy is 0.5611 for test set.

# 6.2 Random Forest

# 6.2.1 Build Model

library(randomForest)
set.seed(53)
rf <- randomForest(binned_score ~ num_voted_users + imdb_score + duration + movie_facebook_likes + title_year +
                    critic_review_ratio + director_facebook_likes + actor_1_facebook_likes + facenumber_in_poster + 
                    country + content_rating + budget + other_actors_facebook_likes, data = train, mtry = 5)
# Show model error
plot(rf)
legend('topright', colnames(rf$err.rate), col=1:5, fill=1:5)

#   The black line shows the overall error rate which is slightly larger than 50% and then decreasing and 
#   becoming constant eventually. The red, green, blue and aqua lines show the error rate for lowest, ok, good 
#   and excellent box-office collection  or gross respectively.
#   Let's look at relative variable importance by plotting the mean decrease in Gini calculated across 
#   all trees.
#   Get importance
importance <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

#  From the plot, we see "num_voted_users" is a very important variable, while "face_number_in_poster", 
#  "content_rating" and "country" are not so important.

# 6.2.2 Apply Model

set.seed(632)
# apply model on validation set
rf.pred.valid <- predict(rf, valid)
# generate confusion matrix for validation data
confusionMatrix(rf.pred.valid, valid$binned_score)

#   Accuracy is 0.5989 for validation set.

set.seed(633)
# apply model on test set
rf.pred.test <- predict(rf, test)
# generate confusion matrix for test data
confusionMatrix(rf.pred.test, test$binned_score)

# Accuracy is 0.6295 for test set.


# 7 Conclusion

# Accuracy table for different models:
  
#  Dataset    |Decision Tree| Random Forest
#-------------|-------------|----------------
#  Training   | 0.6333      |                
#  Validation | 0.5397      |   0.5989  
#   Test      | 0.5611      |   0.6295   

#   For Decision tree model, we have a higher accuracy for training data because the tree was built based 
#   on the training data.

#   Based on the overall performance, we find the best model is Random Forest, which gives a high accuracy 
#   around 0.63.












library(recommenderlab)
library(ggplot2)                       #Author DataFlair
library(data.table)
library(reshape2)
library(pacman)
library(maps) 
getwd()
setwd("/Users/armanshah/Desktop/R Movie") #for Mac 

DataMovie <- read.csv("movies.csv",stringsAsFactors=FALSE)
DataRating <- read.csv("ratings.csv")
str(DataMovie)  #to display information od the DataMovie frame

summary(DataMovie)    #get overview of summary of movies
head(DataMovie)  #print 1st 6 lines of the movie data
summary(DataRating)   #get overview of summary of rating
head(DataRating) #print 1st 6 lines of the ratingdata

#spliting genres into in its own genre
GenreMovie <- as.data.frame(DataMovie$genres, stringsAsFactors=FALSE)
library(data.table)
GenreMovie2 <- as.data.frame(tstrsplit(GenreMovie[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) #DataFlair
colnames(GenreMovie2) <- c(1:10)
GenreList <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
GenreMatrix1 <- matrix(0,10330,18)
GenreMatrix1[1,] <- GenreList
colnames(GenreMatrix1) <- GenreList
for (index in 1:nrow(GenreMovie2)) {
  for (col in 1:ncol(GenreMovie2)) {
    gen_col = which(GenreMatrix1[1,] == GenreMovie2[index,col]) #Author DataFlair
    GenreMatrix1[index+1,gen_col] <- 1
  }
}
GenreMatrix2 <- as.data.frame(GenreMatrix1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(GenreMatrix2)) {
  GenreMatrix2[,col] <- as.integer(GenreMatrix2[,col]) #convert from characters to integers
} 
str(GenreMatrix2)

#creating a search matrix  to perform a search of films by specifying  the genres in the list
SearchMatrix <- cbind(DataMovie[,1:2], GenreMatrix2[])
head(SearchMatrix)    #DataFlair

#converted matrix into sparse one
MatrixRating <- dcast(DataRating, userId~movieId, value.var = "rating", na.rm=FALSE)
MatrixRating <- as.matrix(MatrixRating[,-1]) #remove userIds

#Convert rating matrix into a recommenderlab sparse matrix
MatrixRating <- as(MatrixRating, "realRatingMatrix")
MatrixRating

#getting parameters for the recommending system
Recommend_Model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(Recommend_Model)
lapply(Recommend_Model, "[[", "description")

#explore similar data and show similarties between 2 users
SimilarMatrix <- similarity(MatrixRating[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(SimilarMatrix)
image(as.matrix(SimilarMatrix), main = "User's Similarities")

SimilarMovie <- similarity(MatrixRating[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(SimilarMovie)
image(as.matrix(SimilarMovie), main = "Movies similarity")

#creating a table of ratings
Ratings <- as.vector(MatrixRating@data)
unique(Ratings) # extracting unique ratings
RatingsTable <- table(Ratings) # creating a count of movie ratings
RatingsTable

#exploring most viewed movies in the dataset and then organize it in a table that group them in descending order
library(ggplot2)
MovieCount <- colCounts(MatrixRating) # count views for each movie
TableCount <- data.frame(movie = names(MovieCount),
                          views = MovieCount) # create dataframe of views
TableCount <- TableCount[order(TableCount$views,
                                 decreasing = TRUE), ] # sort by number of views
TableCount$title <- NA
for (index in 1:10325){
  TableCount[index,3] <- as.character(subset(DataMovie,
                                              DataMovie$movieId == TableCount[index,1])$title)
}
TableCount[1:6,]

#visualtion of total number of view for the top movies
ggplot(TableCount[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films")+ 
  theme(plot.title = element_text(hjust = 0.5))+xlab("Movie Title")+ylab("Number of Views")

#heatmap of movies ratings
image(MatrixRating[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

#to get useful data we set the threshold for minimum number of users who rate the film as a 50
ratings <- MatrixRating[rowCounts(MatrixRating) > 50,
                              colCounts(MatrixRating) > 50]
ratings

#showing the matrix of relevant users
MoviesMin<- quantile(rowCounts(ratings), 0.98)
UsersMin <- quantile(colCounts(ratings), 0.98)
image(ratings[rowCounts(ratings) > MoviesMin,
                    colCounts(ratings) > UsersMin],
      main = "Heatmap of the top users and movies")

#show distribution of average ratings per user
AVG_ratings <- rowMeans(ratings)
qplot(AVG_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user") +xlab("Average Ratings") +ylab("Count of Movies that have ratings")+ theme(plot.title = element_text(hjust = 0.5))

#normalizing the data
NormalizeRatings <- normalize(ratings)
sum(rowMeans(NormalizeRatings) > 0.00001)
image(NormalizeRatings[rowCounts(NormalizeRatings) > MoviesMin,
                         colCounts(NormalizeRatings) > UsersMin],
      main = "Normalized Ratings of the Top Users")

#binarize the data which allows the recommendation system to work efficeintly. If rating above 3 it is 1 and if not set to 0.
MoviesMinBinary <- quantile(rowCounts(ratings), 0.95)
UsersMinBinary <- quantile(colCounts(ratings), 0.95)
#movies_watched <- binarize(ratings, minRating = 1)
GoodFilms <- binarize(ratings, minRating = 3)
image(GoodFilms[rowCounts(ratings) > MoviesMinBinary,
                       colCounts(ratings) > UsersMinBinary],
      main = "Heatmap of the top users and movies")

#find similarites based on the people rating of them
DataSampled<- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
DataTraining <- ratings[DataSampled, ]
DataTesting <- ratings[!DataSampled, ]


#building the recommendation system using R
SystemRecommend <- recommenderRegistry$get_entries(dataType ="realMatrixRating")
SystemRecommend$IBCF_realMatrixRating$parameters
RecommendModel <- Recommender(data = DataTraining,
                              method = "IBCF",
                              parameter = list(k = 30))
RecommendModel
class(RecommendModel)

#generate a heat map and see the similarities shared betwn them
InfoModel <- getModel(RecommendModel)
class(InfoModel$sim)
dim(InfoModel$sim)
NumItems <- 20
image(InfoModel$sim[1:NumItems, 1:NumItems],
      main = "Heatmap of the first rows and columns")

#generate  sum of columns
RowSum <- rowSums(InfoModel$sim > 0)
table(RowSum)
ColSum <- colSums(InfoModel$sim > 0)
qplot(ColSum, fill=I("steelblue"), col=I("red"))+ ggtitle("Distribution of the column count")+theme(plot.title = element_text(hjust = 0.5)) +xlab("Column Sums") +ylab("Count")

#create top recommendations, the predict function identifies similar items and rank them accordingly
TopMoviePicks <- 20 # the number of films recommended to each user
Recommendations_Predicted<- predict(object = RecommendModel,
                                     newdata = DataTesting,
                                     n = TopMoviePicks)
Recommendations_Predicted

user1 <- Recommendations_Predicted@items[[1]] # movie recommendation to the 1st user
User1_Movie <- Recommendations_Predicted@itemLabels[user1]
User2_Movie <- User1_Movie
for (index in 1:20){
  User2_Movie[index] <- as.character(subset(DataMovie,
                                             DataMovie$movieId == User1_Movie[index])$title)
}
User2_Movie

MatrixRecommend <- sapply(Recommendations_Predicted@items,
                                function(x){ as.integer(colnames(ratings)[x]) }) # a matrix with  recommendations for each user

MatrixRecommend[,1:4]

NumOfItems <- factor(table(MatrixRecommend))
ChartTitle <- "Distribution of the Number of Items for IBCF"
qplot(NumOfItems, fill=I("steelblue"), col=I("red")) + ggtitle(ChartTitle) +theme(plot.title = element_text(hjust = 0.5))
+ xlab("Number of Items") +ylab("Count")

NumOfItems_sorted <- sort(NumOfItems, decreasing = TRUE)
NumOfItems_top <- head(NumOfItems_sorted, n = 4)
TopTable <- data.frame(as.integer(names(NumOfItems_top)),
                        NumOfItems_top)
for(i in 1:4) {
  TopTable[i,1] <- as.character(subset(DataMovie,
                                        DataMovie$movieId == TopTable[i,1])$title)
}

colnames(TopTable) <- c("Movie Title", "No. of Items")
head(TopTable)


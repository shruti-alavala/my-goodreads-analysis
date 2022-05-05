# Shruti Alavala
# Foundations of Analytics with R - Final Project

### Preparing the Data
 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

my.books.orig <- read.csv("goodreads_library_export_2021_Oct.csv", header=TRUE)
my.books <- my.books.orig

head(my.books)

colnames(my.books)

# delete blank/unnecessary columns
my.books <- subset(my.books, select = -c(Author.l.f, Additional.Authors, ISBN, ISBN13, 
                                         Bookshelves, Original.Publication.Year,
                                         Bookshelves.with.positions, 
                                         My.Review, Spoiler, Private.Notes, 
                                         Recommended.For, Recommended.By,
                                         Owned.Copies, Original.Purchase.Date,
                                         Original.Purchase.Location, Condition, 
                                         Condition.Description, BCID))

# naming columns
colnames(my.books) <- c("BookID", "Title", "Author", "Genre", "RatingsCount", 
                        "ReviewsCount", "MyRating", "AverageRating", "Publisher", 
                        "Format", "NumberOfPages", "YearPublished", 
                        "DateRead", "DateAdded", "Status", "ReadCount")

colnames(my.books)
head(my.books)

# check data types of each column
str(my.books)

# change date read column to Date data type
head(my.books$DateRead)
my.books$DateRead <- as.Date(my.books$DateRead, "%m/%d/%y")
head(my.books$DateRead)

# change date added column to Date data type
head(my.books$DateAdded)
my.books$DateAdded <- as.Date(my.books$DateAdded, "%m/%d/%y")
head(my.books$DateAdded)

# remove rows with NA number of pages
v <- which(is.na(my.books$NumberOfPages))
my.books <- my.books[-v, ]

# new column: review to rating ratio
my.books$ReviewRatingRatio <- round((my.books$ReviewsCount/my.books$RatingsCount), 2)
head(my.books)

### Analyzing the Data

#install.packages("RColorBrewer")
library(RColorBrewer)

# plot one categorical variable
# genres pie chart

par(mfrow = c(1,1))

genre.data <- table(my.books$Genre)
slice.labels <- names(genre.data)
slice.percents <- round(genre.data/sum(genre.data)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste0(slice.labels, "%")

pie(genre.data,
    labels = slice.labels,
    col = brewer.pal(9, "Spectral"))

# plot one categorical variable
# top 5 authors barplot

author.freq.df <- as.data.frame(table(my.books$Author))
colnames(author.freq.df) <- c("Author", "Frequency")
authors.ordered <- author.freq.df[order(-author.freq.df$Frequency), ]
top.5.freq <- authors.ordered[seq(1,5),]

top.5.authors <- subset(my.books, Author %in% top.5.freq$Author)
t <- table(top.5.authors$Author)
t

par(mfrow = c(1,1))

barplot(sort(t),
        main = "Most Logged Authors",
        xlab = "Author",
        ylab = "Number of Books",
        ylim = c(0, max(t)*1.1),
        col = brewer.pal(5, "Purples"))
axis(side = 2, at = seq(0,max(t)*1.1))

# plot one numerical variable - number of pages boxplot

my.books.unread <- subset(my.books, Status == "to-read")
my.books.read <- subset(my.books, Status == "read")

par(mfrow = c(1,1))

boxplot(my.books.unread$NumberOfPages, 
        my.books.read$NumberOfPages,
        names = c("Want To Read", "Read"),
        col = c("midnightblue", "mediumpurple1"),
        xlab = "Number of Pages",
        horizontal = TRUE, 
        xaxt = "n")
axis(side = 1, seq(0,1200,100))

fivenum(my.books.read$NumberOfPages)
fivenum(my.books.unread$NumberOfPages)

# one set of two or more variables

# two categorical variables - genre and status
genre.status.freq <- table(my.books$Status, my.books$Genre)
genre.status.freq

par(mfrow = c(1,1))
barplot(genre.status.freq, 
        beside = TRUE,
        legend.text = c("Read", "Want To Read"),
        args.legend = c(x="topright"),
        main = "Books Logged by Genre",
        xlab = "Genre",
        ylab = "Count",
        ylim = c(0, 200),
        yaxt = "n",
        col = c("mediumpurple1", "midnightblue"))
axis(side = 2, seq(0,200,25))

# two numerical variables - rating and review count

par(mfrow = c(1,1))
plot(my.books$RatingsCount, 
     my.books$ReviewsCount,
     main = "Correlation of Rating & Review Counts",
     xlab = "Ratings Count",
     ylab = "Review Count",
     xaxt = "n",
     yaxt = "n")
axis(side = 1, seq(0,8000000,500000), labels = TRUE)
axis(side = 2, seq(0,200000,25000), labels = TRUE)
abline(v = mean(my.books$RatingsCount), col = "blue")
abline(h = mean(my.books$ReviewsCount), col = "blue")

# linear regression
rating.review.lm <- lm(my.books$ReviewsCount~my.books$RatingsCount, my.books)
summary(rating.review.lm)

abline(rating.review.lm, col = "deeppink")

# distribution of numerical variable
# ratings count

hist(my.books$RatingsCount,
     main = "Distribution of Ratings Count",
     xlab = "Count of Ratings",
     breaks = seq(0,8000000,500000),
     xaxt = "n",
     col = "plum3")
axis(side = 1, seq(0,8000000,500000), labels = TRUE)

median(my.books$RatingsCount)
mean(my.books$RatingsCount)
sd(my.books$RatingsCount)

# Draw various random samples of the data and show the applicability of the 
# Central Limit Theorem for this variable - ratings count

set.seed(0417)
samples <- 1000
xbar <- numeric(samples)

par(mfrow = c(2,2))

for (size in c(10, 50, 100, 200)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(my.books$RatingsCount, size = size, 
                           replace = FALSE))
  }

  hist(xbar, prob = TRUE, 
       main = paste("Sample Size =", size),
       xaxt = "n",
       col = "plum2")
  axis(side=1, seq(0,1500000,250000), labels=TRUE)
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

# Show how various sampling methods can be used on your data. What are your 
# conclusions if these samples are used instead of the whole dataset. 

# install.packages("sampling")
library(sampling)

set.seed(0417)
sample.size <- 40

# simple random sample without replacement 

s <- srswor(sample.size, nrow(my.books))
table(my.books[s != 0, ]$Genre)
prop.table(table(my.books[s != 0, ]$Genre))

simple.random.sample <- my.books[s != 0, ]
head(simple.random.sample)

# systematic sampling, equal probabilities
set.seed(0417)

N <- nrow(my.books)
N
n <- sample.size
n
k <- ceiling(N / n)
k
r <- sample(k, 1)
r

s.sys.eq <- seq(r, by = k, length = n)
s.sys.eq

sys.sample.eq <- my.books[s.sys.eq, ]
tail(sys.sample.eq)

table(sys.sample.eq$Genre)
prop.table(table(sys.sample.eq$Genre))

# systematic sampling, proportional probabilities
set.seed(0417)

pik <- inclusionprobabilities(my.books$NumberOfPages, sample.size)
length(pik)
sum(pik)

s.sys.uneq <- UPsystematic(pik)
sys.sample.uneq <- my.books[s.sys.uneq != 0, ]

head(sys.sample.uneq)

table(sys.sample.uneq$Genre)
prop.table(table(sys.sample.uneq$Genre))

# stratified sampling
set.seed(0417)

my.books.genres.ordered <- my.books[order(my.books$Genre),]
freq <- table(my.books.genres.ordered$Genre)
freq
sizes <- round(sample.size * freq / sum(freq))
sizes

st <- strata(my.books.genres.ordered, stratanames = c("Genre"),
             size = sizes, method = "srswor")
head(st)

strat.sample <- getdata(my.books.genres.ordered, st)
head(strat.sample)

table(strat.sample$Genre)
prop.table(table(strat.sample$Genre))

# conclusions 

# comparing the means of Number of Pages
# overall data
mean(my.books$NumberOfPages)

# simple random sample, without replacement
mean(simple.random.sample$NumberOfPages)

# systematic sampling, equal probabilities
mean(sys.sample.eq$NumberOfPages)

# systematic sampling, unequal probabilities
mean(sys.sample.uneq$NumberOfPages)

# stratified sampling
mean(strat.sample$NumberOfPages)

# why is mean of number of pages with systematic sampling so much higher than
# mean of number of pages in original dataset
# is Genre having an effect?
# hypothesis - fantasy books are longer and they were oversampled in systematic 
# sampling with eq probabilities method

# function to calculate the mean number of pages per genre
mean.by.genre <- function(df) {
  
  genres <- c("Non-Fiction", "Contemporary", "Romance", "Fantasy", "Mystery",
              "Historical", "Science Fiction", "Classics", "Children's")
  
  mean.df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(mean.df) <- c("Genre", "Mean Number of Pages")  
  
  for(i in genres){
    subset.df <- subset(df, Genre == i)
    mean.pages <- round(mean(subset.df$NumberOfPages), 2)
    mean.df[nrow(mean.df) + 1, ] = c(i, mean.pages)
  }
  
  mean.df[nrow(mean.df) + 1, ] = c("Overall", round(mean(df$NumberOfPages)), 2)
  return(mean.df)
}

mean.by.genre(my.books)
mean.by.genre(sys.sample.eq)
mean.by.genre(strat.sample)

# second longest book in the whole dataset is in the systematic sampling with
# equal probabilities sample
sorted.by.pages <- my.books[order(-my.books$NumberOfPages),]
sorted.by.pages[seq(1:5), c(2,11)]

sys.sample.eq[sys.sample.eq$NumberOfPages == max(sys.sample.eq$NumberOfPages), c(2,11)]

# third longest science fiction book in the whole dataset is in the systematic sampling 
# with equal probabilities sample
sci.fi.overall <- subset(my.books, Genre == "Science Fiction")
sorted.sci.fi.overall <- sci.fi.overall[order(-sci.fi.overall$NumberOfPages),]
sorted.sci.fi.overall[seq(1:5), c(2,11)]

sci.fi.sys.sample.eq <- subset(sys.sample.eq, Genre == "Science Fiction")
sci.fi.sys.sample.eq[,c(2,11)]

# extra graph
# number of days it took between adding and reading

# new column
my.books.read$DaysToRead <- as.numeric(my.books.read$DateRead - 
                                         my.books.read$DateAdded, unit = "days")
head(my.books.read)

par(mfrow = c(1,1))
boxplot(my.books.read$DaysToRead, 
        horizontal = TRUE,
        col = "deepskyblue4",
        xlab = "Days To Read",
        xaxt = "n")
axis(side = 1, fivenum(my.books.read$DaysToRead), labels = TRUE)

fivenum(my.books.read$DaysToRead)

# Book that was shelfed for the longest period
subset(my.books.read, DaysToRead == max(my.books.read$DaysToRead, na.rm = TRUE))

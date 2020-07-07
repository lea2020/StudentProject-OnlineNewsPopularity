#Predicting Online News Popularity based on Pre-publication Features
#Initial Results and Code

#Link to dataset: https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity

#Link to data: https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip

#Sections:
#Step 1 - Data Processing
#Step 2 - Exploratory Analysis
#Step 3 - Dimensionality Reduction
#Step 4 - Experimental Design
#Step 5 - Prediction & Step 6 - Performance Evaluation


#Step 1 - Data Processing

#The “Online News Popularity Data Set” (Fernandes et al., 2015) is retrieved from the UCI Machine Learning Repository, 
#and data processing procedures are implemented. At the start of the univariate analysis, the target variable is 
#determined to be the number of shares; and articles are classified as popular or unpopular based on the number of 
#shares. Various graphs are plotted such as boxplots and histograms to detect  outliers, illustrate whether the data is 
#normally distributed, and whether the data is balanced. To determine whether there are any variables with low variance, 
#a near-zero variance filter is used to filter out the attributes with near-zero variance and help to reduce the 
#dimensionality of the dataset. In bivariate analysis, pairwise relations are examined between the input variables as 
#well as the output and input variables. Scatterplots provide visualisations to better understand the data; and 
#correlation analysis is done to help reduce the dimensionality of the dataset when there is high correlation among the
#independent variables.


#Initial Analysis - Univariate Analysis

#Read the data
temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip",temp)
pop <- read.table(unz(temp, "OnlineNewsPopularity/OnlineNewsPopularity.csv"), header = TRUE, sep = ",", 
                  stringsAsFactors = FALSE, na.strings = c("", "NA"))
unlink(temp)

#View the first and last few rows of the dataset
head(pop)
tail(pop)

#Number of rows
nrow(pop)
#39644 rows

#Number of attributes
length(pop)
#61 variables

#Data type of attributes
sapply(pop, class)
#The "shares" column is of type integer, "url" is of type character, and the remainder of the variables are of type
#numeric.

#Structure of the dataset
str(pop)

#Number of missing values
sum(is.na(pop) == TRUE)
#There are no missing values in the dataset.

#Remove the non-predictive columns from the dataset
pop_new <- pop[3:61]

#Dimensions of the new dataset
dim(pop_new)
#39644 rows and 59 columns

#Add a column that categorizes articles into binary levels of popularity according to the number of shares
#If the number of shares is less than or equal to 1400, label the article as unpopular ("0"), whereas if the number of 
#shares is greater than 1400, label the article as popular ("1").
pop_new$shares_cat <- NA
pop_new$shares_cat <- ifelse(pop_new$shares <= 1400, 0, 1)

#Change categorical variables from numeric to factor
pop_new$data_channel_is_lifestyle <- as.factor(pop_new$data_channel_is_lifestyle)
pop_new$data_channel_is_entertainment <- as.factor(pop_new$data_channel_is_entertainment)
pop_new$data_channel_is_bus <- as.factor(pop_new$data_channel_is_bus)
pop_new$data_channel_is_socmed <- as.factor(pop_new$data_channel_is_socmed)
pop_new$data_channel_is_tech <- as.factor(pop_new$data_channel_is_tech)
pop_new$data_channel_is_world <- as.factor(pop_new$data_channel_is_world)
pop_new$weekday_is_monday <- as.factor(pop_new$weekday_is_monday)
pop_new$weekday_is_tuesday <- as.factor(pop_new$weekday_is_tuesday)
pop_new$weekday_is_wednesday <- as.factor(pop_new$weekday_is_wednesday)
pop_new$weekday_is_thursday <- as.factor(pop_new$weekday_is_thursday)
pop_new$weekday_is_friday <- as.factor(pop_new$weekday_is_friday)
pop_new$weekday_is_saturday <- as.factor(pop_new$weekday_is_saturday)
pop_new$weekday_is_sunday <- as.factor(pop_new$weekday_is_sunday)
pop_new$is_weekend <- as.factor(pop_new$is_weekend)
pop_new$shares_cat <- as.factor(pop_new$shares_cat)

#Check the structure of the new dataset
str(pop_new)

#Five number summary for the numeric attributes; and levels and frequency tables for factors
summary(pop_new)
#Note the dataset is fairly balanced in terms of the popularity level of an article:
#0     1 
#20082 19562

#Standard deviation for a selection of features
sd(pop_new$n_tokens_title)
#Standard deviation for the number of words in the title is 2.114037

sd(pop_new$n_tokens_content)
#Standard deviation for the number of words in the content is 471.1075

sd(pop_new$num_hrefs)
#Standard deviation for the number of links is 11.33202

sd(pop_new$num_self_hrefs)
#Standard deviation for the number of links to other articles published by Mashable is 3.855141

sd(pop_new$num_imgs)
#Standard deviation for the number of images is 8.309434

sd(pop_new$num_videos)
#Standard deviation for the number of videos is 4.107855

sd(pop_new$num_keywords)
#Standard deviation for the number of keywords in the metadata is 1.90913

sd(pop_new$shares)
#Standard deviation for the number of shares is 11626.95

#Change categorical variables back to numeric to conduct further analysis
pop_new$data_channel_is_lifestyle <- as.numeric(levels(pop_new$data_channel_is_lifestyle))[as.integer(pop_new$data_channel_is_lifestyle)]
pop_new$data_channel_is_entertainment <- as.numeric(levels(pop_new$data_channel_is_entertainment))[as.integer(pop_new$data_channel_is_entertainment)]
pop_new$data_channel_is_bus <- as.numeric(levels(pop_new$data_channel_is_bus))[as.integer(pop_new$data_channel_is_bus)]
pop_new$data_channel_is_socmed <- as.numeric(levels(pop_new$data_channel_is_socmed))[as.integer(pop_new$data_channel_is_socmed)]
pop_new$data_channel_is_tech <- as.numeric(levels(pop_new$data_channel_is_tech))[as.integer(pop_new$data_channel_is_tech)]
pop_new$data_channel_is_world <- as.numeric(levels(pop_new$data_channel_is_world))[as.integer(pop_new$data_channel_is_world)]
pop_new$weekday_is_monday <- as.numeric(levels(pop_new$weekday_is_monday))[as.integer(pop_new$weekday_is_monday)]
pop_new$weekday_is_tuesday <- as.numeric(levels(pop_new$weekday_is_tuesday))[as.integer(pop_new$weekday_is_tuesday)]
pop_new$weekday_is_wednesday <- as.numeric(levels(pop_new$weekday_is_wednesday))[as.integer(pop_new$weekday_is_wednesday)]
pop_new$weekday_is_thursday <- as.numeric(levels(pop_new$weekday_is_thursday))[as.integer(pop_new$weekday_is_thursday)]
pop_new$weekday_is_friday <- as.numeric(levels(pop_new$weekday_is_friday))[as.integer(pop_new$weekday_is_friday)]
pop_new$weekday_is_saturday <- as.numeric(levels(pop_new$weekday_is_saturday))[as.integer(pop_new$weekday_is_saturday)]
pop_new$weekday_is_sunday <- as.numeric(levels(pop_new$weekday_is_sunday))[as.integer(pop_new$weekday_is_sunday)]
pop_new$is_weekend <- as.numeric(levels(pop_new$is_weekend))[as.integer(pop_new$is_weekend)]
pop_new$shares_cat <- as.numeric(levels(pop_new$shares_cat))[as.integer(pop_new$shares_cat)]


#Outliers - Boxplots
#Number of shares
boxplot(pop_new[c("shares")], xlab = "Number of Shares", ylab = "Count")

#Number of images and videos
boxplot(pop_new[c("num_imgs", "num_videos")], names = c("Number of Images", "Number of Videos"), ylab = "Count")

#Number of Keywords; Number of Words in the Title; and Number of Words in the Content
par(mfrow=c(1,3))
boxplot(pop_new$num_keywords, xlab = "Number of Keywords", ylab = "Count")
boxplot(pop_new$n_tokens_title, xlab = "Number of Words in the Title", ylab = "Count")
boxplot(pop_new$n_tokens_content, xlab = "Number of Words in the Content", ylab = "Count")

#Visualizing the data using boxplots shows that there are outliers present in the data.


#Distributions of input and output variables - Histograms
#Popularity of an Article
par(mfrow=c(1,1))
hist(pop_new$shares_cat, xlab = "Popularity of an Article", ylab = "Count", breaks = c(-1,0,1), main = NULL, 
     labels = c("Unpopular", "Popular"))

#The dataset is fairly balanced between unpopular and popular articles.

#Type of data channel that the article was published
library(ggplot2)
tab = data.frame(Count=colSums(pop_new[c("data_channel_is_lifestyle", "data_channel_is_entertainment", 
                                           "data_channel_is_bus", "data_channel_is_socmed",
                                           "data_channel_is_tech", "data_channel_is_world")]), 
                 Channel = names(pop_new[c("data_channel_is_lifestyle", "data_channel_is_entertainment", 
                                            "data_channel_is_bus", "data_channel_is_socmed",
                                            "data_channel_is_tech", "data_channel_is_world")]))

ggplot(data = tab, aes(x = Channel, y = Count)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = c("data_channel_is_lifestyle", "data_channel_is_entertainment", 
                              "data_channel_is_bus", "data_channel_is_socmed",
                              "data_channel_is_tech", "data_channel_is_world"),
                   labels = c("Lifestyle", "Entertainment", "Business", "Social Media",
                              "Technology", "World"))

#Most articles are published to "World" followed by "Technology," "Entertainment," "Business," "Social Media," and
#finally "Lifestyle".

#Day of the Week that the article was published
tab2 = data.frame(Count=colSums(pop_new[c("weekday_is_monday", "weekday_is_tuesday", 
                                            "weekday_is_wednesday", "weekday_is_thursday",
                                            "weekday_is_friday", "weekday_is_saturday",
                                            "weekday_is_sunday")]),
                  Day = names(pop_new[c("weekday_is_monday", "weekday_is_tuesday", 
                                             "weekday_is_wednesday", "weekday_is_thursday",
                                             "weekday_is_friday", "weekday_is_saturday",
                                             "weekday_is_sunday")]))

tab2$Day <- factor(tab2$Day, levels = c("weekday_is_monday", "weekday_is_tuesday", 
                                                "weekday_is_wednesday", "weekday_is_thursday",
                                                "weekday_is_friday", "weekday_is_saturday",
                                                "weekday_is_sunday"))

ggplot(data = tab2, aes(x = Day, y = Count)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = c("weekday_is_monday", "weekday_is_tuesday", 
                              "weekday_is_wednesday", "weekday_is_thursday",
                              "weekday_is_friday", "weekday_is_saturday",
                              "weekday_is_sunday"),
                   labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                              "Saturday", "Sunday"))

#Most articles are published on Wednesday, followed by Tuesday, Thursday, Monday, Friday, Sunday, and finally Saturday.

#Initial Analysis - Bivariate Analysis

#Pairwise Visualizations - Scatterplots
#Scatterplots between Shares and Text Subjectivity, Text Sentiment Polarity, Title Subjectivity and Title Polarity
par(mfrow = c(2,2))
plot(pop_new$global_subjectivity, pop_new$shares, xlab = "Text Subjectivity", ylab = "Shares")
plot(pop_new$global_sentiment_polarity, pop_new$shares, xlab = "Text Sentiment Polarity", ylab = "Shares")
plot(pop_new$title_subjectivity, pop_new$shares, xlab = "Title Subjectivity", ylab = "Shares")
plot(pop_new$title_sentiment_polarity, pop_new$shares, xlab = "Title Polarity", ylab = "Shares")

#Shares tend to cluster around the center of the distribution suggesting that the most popular articles tend to be 
#more neutral.

#Scatterplot matrix
par(mfrow=c(1,1))
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  #borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

#Assign a value of Selected Features that represents the relevant columns of interest between Shares and Text Subjectivity,
#Text Sentiment Polarity, Title Subjectivity and Title Polarity 
pop_new_Selected_Features <- pop_new[c("global_subjectivity", "global_sentiment_polarity",
                                         "title_subjectivity", "title_sentiment_polarity", "shares")]

#Show the results
pairs(pop_new_Selected_Features, lower.panel=panel.smooth, upper.panel=panel.cor)


#Low Variance Filter
library(caret)

#Near-zero variance may be applied to the data to ensure variables are used that help distinguish/determine the 
#popularity of an article.

#Dataframe with predictor information including whether variables have one unique value, or have very few unique values 
#compared to the number of samples, or whether the ratio of the frequency of the the most common value to the second 
#most common value is large.
nzv <- nearZeroVar(pop_new, saveMetrics= TRUE)
print(nzv)

#                               freqRatio percentUnique zeroVar   nzv
#n_tokens_title                  1.057184    0.05044900   FALSE FALSE
#n_tokens_content               11.693069    6.06901423   FALSE FALSE
#n_unique_tokens               118.100000   68.81495308   FALSE FALSE
#n_non_stop_words                8.557971    3.66007466   FALSE FALSE
#n_non_stop_unique_tokens       90.846154   57.83977399   FALSE FALSE
#num_hrefs                       1.011367    0.33548582   FALSE FALSE
#num_self_hrefs                  1.123159    0.14882454   FALSE FALSE
#num_imgs                        2.593245    0.22954293   FALSE FALSE
#num_videos                      2.639038    0.13368984   FALSE FALSE
#average_token_length           15.337662   76.01654727   FALSE FALSE
#num_keywords                    1.076606    0.02522450   FALSE FALSE
#data_channel_is_lifestyle      17.887089    0.00504490   FALSE FALSE
#data_channel_is_entertainment   4.617685    0.00504490   FALSE FALSE
#data_channel_is_bus             5.334931    0.00504490   FALSE FALSE
#data_channel_is_socmed         16.065863    0.00504490   FALSE FALSE
#data_channel_is_tech            4.396678    0.00504490   FALSE FALSE
#data_channel_is_world           3.704403    0.00504490   FALSE FALSE
#kw_min_min                      1.929795    0.06558369   FALSE FALSE
#kw_max_min                      1.268519    2.71415599   FALSE FALSE
#kw_avg_min                      8.567901   42.88921400   FALSE FALSE
#kw_min_max                     45.139842    2.57542125   FALSE  TRUE
#kw_max_max                      7.577521    0.08828574   FALSE FALSE
#kw_avg_max                      1.385965   77.77721723   FALSE FALSE
#kw_min_avg                    111.051948   40.31379276   FALSE FALSE
#kw_max_avg                      1.795455   49.03137928   FALSE FALSE
#kw_avg_avg                      9.875000   99.13227727   FALSE FALSE
#self_reference_min_shares       4.154024    3.16567450   FALSE FALSE
#self_reference_max_shares       9.366841    2.86802543   FALSE FALSE
#self_reference_avg_sharess     12.585965   21.75865200   FALSE FALSE
#weekday_is_monday               4.951659    0.00504490   FALSE FALSE
#weekday_is_tuesday              4.364547    0.00504490   FALSE FALSE
#weekday_is_wednesday            4.332078    0.00504490   FALSE FALSE
#weekday_is_thursday             4.455346    0.00504490   FALSE FALSE
#weekday_is_friday               5.953868    0.00504490   FALSE FALSE
#weekday_is_saturday            15.161435    0.00504490   FALSE FALSE
#weekday_is_sunday              13.484472    0.00504490   FALSE FALSE
#is_weekend                      6.638536    0.00504490   FALSE FALSE
#LDA_00                          3.000000   99.22560791   FALSE FALSE
#LDA_01                          1.416667   98.62274241   FALSE FALSE
#LDA_02                          8.500000   99.69982847   FALSE FALSE
#LDA_03                          1.137255   98.28221168   FALSE FALSE
#LDA_04                          2.684211   99.30884875   FALSE FALSE
#global_subjectivity            32.888889   87.02704066   FALSE FALSE
#global_sentiment_polarity      39.900000   87.51639592   FALSE FALSE
#global_rate_positive_words     10.141667   33.19291696   FALSE FALSE
#global_rate_negative_words     29.310345   25.90808193   FALSE FALSE
#rate_positive_words             1.178886    5.76127535   FALSE FALSE
#rate_negative_words             1.585821    5.76127535   FALSE FALSE
#avg_positive_polarity           6.054726   68.86540208   FALSE FALSE
#min_positive_polarity           2.086363    0.08324084   FALSE FALSE
#max_positive_polarity           2.238484    0.09585309   FALSE FALSE
#avg_negative_polarity           5.110220   34.91322773   FALSE FALSE
#min_negative_polarity           1.241491    0.13621229   FALSE FALSE
#max_negative_polarity           1.128019    0.12360004   FALSE FALSE
#title_subjectivity              6.817941    1.69760872   FALSE FALSE
#title_sentiment_polarity        9.428977    2.05075169   FALSE FALSE
#abs_title_subjectivity          7.771093    1.34194330   FALSE FALSE
#abs_title_sentiment_polarity    7.492099    1.64715972   FALSE FALSE
#shares                          1.143707    3.66764201   FALSE FALSE
#shares_cat                      1.026582    0.00504490   FALSE FALSE

#Column names of the zero- or near-zero predictors
nzv2 <- nearZeroVar(pop_new, names = TRUE)
print(nzv2)
#The results indicate one predictor - the minimum shares of the best keywords - has low variance.

#Return column index
nzv3 <- nearZeroVar(pop_new)
print(nzv3)

#Drop column with low variance
filtered_pop <- pop_new[, -nzv3]

#Dimensions of new data frame
dim(filtered_pop)
#39644 rows and 59 columns.


#Remove the number of shares since the analysis will focus on predicting the popularity of online news articles as a 
#binary classification task.
#Check the column names and remove the shares (integer) column
names(filtered_pop)
filtered_pop <- filtered_pop[-58]


#Correlation Analysis
library(corrplot)
library(MASS)

#Use Spearman correlation coefficient because the data is not normally distributed and has categorical variables
pop_new_cor <- cor(filtered_pop, use = "complete.obs", method = "spearman")

#Since the dataset has many variables, it may be informative to reduce the size of the correlation matrix
#Drop duplicate correlations
pop_new_cor[lower.tri(pop_new_cor, diag=TRUE)] <- NA

#Create a table
pop_new_cor <- as.data.frame(as.table(pop_new_cor))

#Remove NA values
pop_new_cor <- na.omit(pop_new_cor)

#Select the correlations that have at least moderate (moderate correlation > 0.4)
pop_new_cor <- subset(pop_new_cor, abs(Freq) > 0.4)

#Sort values by the highest correlation
pop_new_cor <- pop_new_cor[order(-abs(pop_new_cor$Freq)), ]

#Print the table
print(pop_new_cor)

#                              Var1                         Var2       Freq
#176               n_tokens_content             n_non_stop_words  0.9949854
#1593     self_reference_max_shares   self_reference_avg_sharess  0.9731616
#235                n_unique_tokens     n_non_stop_unique_tokens  0.8813053
#3302            title_subjectivity abs_title_sentiment_polarity  0.8394189
#2714           rate_positive_words          rate_negative_words -0.8320259
#2713    global_rate_negative_words          rate_negative_words  0.8243605
#1416                    kw_max_avg                   kw_avg_avg  0.8070733
#1121                    kw_max_min                   kw_avg_min  0.8066579
#2653     global_sentiment_polarity          rate_positive_words  0.8032534
#1592     self_reference_min_shares   self_reference_avg_sharess  0.8020695
#3009         avg_negative_polarity        min_negative_polarity  0.7791302
#1178                    kw_min_min                   kw_max_max -0.7493595
#118               n_tokens_content              n_unique_tokens -0.7204282
#177                n_unique_tokens             n_non_stop_words -0.7111355
#2065             weekday_is_sunday                   is_weekend  0.7016478
#1534     self_reference_min_shares    self_reference_max_shares  0.6808056
#2221         data_channel_is_world                       LDA_02  0.6758580
#3304        abs_title_subjectivity abs_title_sentiment_polarity -0.6748752
#3244            title_subjectivity       abs_title_subjectivity -0.6739325
#2711     global_sentiment_polarity          rate_negative_words -0.6632427
#2064           weekday_is_saturday                   is_weekend  0.6617074
#2655    global_rate_negative_words          rate_positive_words -0.6563685
#2336          data_channel_is_tech                       LDA_04  0.6152844
#2890         avg_positive_polarity        max_positive_polarity  0.6004516
#2537     global_sentiment_polarity   global_rate_positive_words  0.5922442
#2102           data_channel_is_bus                       LDA_00  0.5920692
#2654    global_rate_positive_words          rate_positive_words  0.5638215
#236               n_non_stop_words     n_non_stop_unique_tokens -0.5587888
#1415                    kw_min_avg                   kw_avg_avg  0.5539081
#1515                num_self_hrefs    self_reference_max_shares  0.5423319
#234               n_tokens_content     n_non_stop_unique_tokens -0.5370046
#294               n_non_stop_words                    num_hrefs  0.5334565
#1239                    kw_max_max                   kw_avg_max  0.5277993
#3003    global_rate_negative_words        min_negative_polarity -0.5228170
#2960              n_tokens_content        min_negative_polarity -0.5158222
#2962              n_non_stop_words        min_negative_polarity -0.5150513
#1414                    kw_avg_max                   kw_avg_avg  0.5094611
#1236                    kw_min_min                   kw_avg_max -0.5067561
#292               n_tokens_content                    num_hrefs  0.5061764
#2769     global_sentiment_polarity        avg_positive_polarity  0.4910328
#1238                    kw_avg_min                   kw_avg_max -0.4872756
#2157                  num_keywords                       LDA_01 -0.4793513
#2846              n_non_stop_words        max_positive_polarity  0.4691861
#1180                    kw_avg_min                   kw_max_max -0.4689681
#2099                  num_keywords                       LDA_00 -0.4679866
#2844              n_tokens_content        max_positive_polarity  0.4677236
#2287                    kw_avg_avg                       LDA_03  0.4644048
#2768           global_subjectivity        avg_positive_polarity  0.4547775
#3005           rate_negative_words        min_negative_polarity -0.4499243
#411       n_non_stop_unique_tokens                     num_imgs -0.4448630
#2595     global_sentiment_polarity   global_rate_negative_words -0.4439893
#3303      title_sentiment_polarity abs_title_sentiment_polarity  0.4405846
#1573                num_self_hrefs   self_reference_avg_sharess  0.4381096
#2159 data_channel_is_entertainment                       LDA_01  0.4295302
#1120                    kw_min_min                   kw_avg_min  0.4234541
#2787               n_unique_tokens        min_positive_polarity  0.4224206
#2885     global_sentiment_polarity        max_positive_polarity  0.4195723
#410               n_non_stop_words                     num_imgs  0.4145083
#2886    global_rate_positive_words        max_positive_polarity  0.4056356

#Transform the table to a matrix to use corrplot
matrix_cor <- reshape2::acast(pop_new_cor, Var1~Var2, value.var="Freq")

#Plot the correlations
corrplot(matrix_cor, method = c("circle"), is.corr = FALSE, tl.col="black", na.label=" ")


#Remove strongly correlated attributes
library(mlbench)

#Calculate correlation
pop_correlation <- cor(filtered_pop, method = "spearman")

#Find attributes that are highly correlated (greater than 0.7 correlation coefficient) and sort them
correlated <- findCorrelation(pop_correlation, cutoff=0.7)
correlated <- sort(correlated)

#Print the indexes of the highly correlated attributes
print(correlated)

#Check the names of the correlated data
correlated_pop = filtered_pop[,c(correlated)]
names(correlated_pop)

#[1] "n_tokens_content"             "n_unique_tokens"              "n_non_stop_words"            
#[4] "kw_min_min"                   "kw_avg_min"                   "kw_avg_avg"                  
#[7] "self_reference_max_shares"    "self_reference_avg_sharess"   "is_weekend"                  
#[10] "global_sentiment_polarity"    "rate_positive_words"          "rate_negative_words"         
#[13] "min_negative_polarity"        "abs_title_sentiment_polarity"

#There are 14 variables that may be removed due to being strongly correlated: number of words in the content, rate of
#unique words in the content, rate of non-stop words in the content, minimum shares of the worst keyword, average
#shares of the worst keyword, average shares of the average keyword, maximum shares of referenced articles in 
#Mashable, average shares of referenced articles in Mashable, whether the article was published on the weekend,
#text sentiment polarity, rate of positive words among non-neutral tokens, rate of negative words among non-neutral
#tokens, minimum polarity of negative words, and absolute polarity level.

#Reduce the data, not including variables that have high correlation
reduced_pop = filtered_pop[,-c(correlated)]

#Check the names of the remaining variables
names(reduced_pop)
#There are 44 remaining variables to be included in the analysis.

#[1] "n_tokens_title"                "n_non_stop_unique_tokens"      "num_hrefs"                    
#[4] "num_self_hrefs"                "num_imgs"                      "num_videos"                   
#[7] "average_token_length"          "num_keywords"                  "data_channel_is_lifestyle"    
#[10] "data_channel_is_entertainment" "data_channel_is_bus"           "data_channel_is_socmed"       
#[13] "data_channel_is_tech"          "data_channel_is_world"         "kw_max_min"                   
#[16] "kw_max_max"                    "kw_avg_max"                    "kw_min_avg"                   
#[19] "kw_max_avg"                    "self_reference_min_shares"     "weekday_is_monday"            
#[22] "weekday_is_tuesday"            "weekday_is_wednesday"          "weekday_is_thursday"          
#[25] "weekday_is_friday"             "weekday_is_saturday"           "weekday_is_sunday"            
#[28] "LDA_00"                        "LDA_01"                        "LDA_02"                       
#[31] "LDA_03"                        "LDA_04"                        "global_subjectivity"          
#[34] "global_rate_positive_words"    "global_rate_negative_words"    "avg_positive_polarity"        
#[37] "min_positive_polarity"         "max_positive_polarity"         "avg_negative_polarity"        
#[40] "max_negative_polarity"         "title_subjectivity"            "title_sentiment_polarity"     
#[43] "abs_title_subjectivity"        "shares_cat"

#Change the categorical variables back to factor to conduct exploratory analysis and dimensionality reduction
reduced_pop$data_channel_is_lifestyle <- as.factor(reduced_pop$data_channel_is_lifestyle)
reduced_pop$data_channel_is_entertainment <- as.factor(reduced_pop$data_channel_is_entertainment)
reduced_pop$data_channel_is_bus <- as.factor(reduced_pop$data_channel_is_bus)
reduced_pop$data_channel_is_socmed <- as.factor(reduced_pop$data_channel_is_socmed)
reduced_pop$data_channel_is_tech <- as.factor(reduced_pop$data_channel_is_tech)
reduced_pop$data_channel_is_world <- as.factor(reduced_pop$data_channel_is_world)
reduced_pop$weekday_is_monday <- as.factor(reduced_pop$weekday_is_monday)
reduced_pop$weekday_is_tuesday <- as.factor(reduced_pop$weekday_is_tuesday)
reduced_pop$weekday_is_wednesday <- as.factor(reduced_pop$weekday_is_wednesday)
reduced_pop$weekday_is_thursday <- as.factor(reduced_pop$weekday_is_thursday)
reduced_pop$weekday_is_friday <- as.factor(reduced_pop$weekday_is_friday)
reduced_pop$weekday_is_saturday <- as.factor(reduced_pop$weekday_is_saturday)
reduced_pop$weekday_is_sunday <- as.factor(reduced_pop$weekday_is_sunday)
reduced_pop$shares_cat <- as.factor(reduced_pop$shares_cat)



#Step 2 - Exploratory Analysis

#Normalization is applied to the dataset to scale the values of the features. Next, the data may be explored by 
#checking subsequences using association rules to find any patterns that may explain what makes an article popular, and 
#learn its characteristics, without making predictions yet.

#Normalization - normalize the numeric features in the data set
#Use a function to normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Determine the categorical variables
categorical = apply(reduced_pop, 2, function(x){all(x %in% 0:1)})
print(categorical)

#Apply the normalize function to the numeric variables
pop_norm <- as.data.frame(lapply(reduced_pop[!categorical], normalize))

#Combine the numeric and categorical variables back into one data set 
pop_norm <- cbind(pop_norm, reduced_pop[categorical])

#Check the five number summary and frequency tables for the normalized dataset
summary(pop_norm)


#Check subsequences - Association rules
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("plyr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("arulesCBA")
library(arulesCBA)

#Extract rules using apriori algorithm, setting minimum support and confidence.

#First, subset the data to only those that are popular
mostpopular <- subset(pop_norm, pop_norm$shares_cat == 1)

#Remove the popularity level of shares column since all the observations are for popular articles
mostpopular <- mostpopular[-44]

#Change the dataframe into transaction data
transaction_object <- as(mostpopular, "transactions")

#Check the transaction data
glimpse(transaction_object)
summary(transaction_object)

#Create an item frequency plot to view the distributions for the top 20 items
if (!require("RColorBrewer")) {
  #Install the color package of R
  install.packages("RColorBrewer")
  #Use the library RColorBrewer
  library(RColorBrewer)
}

#Absolute Item Frequency Plot
itemFrequencyPlot(transaction_object, topN=20, type="absolute", col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")

#Relative Item Frequency Plot
itemFrequencyPlot(transaction_object, topN=20, type="relative", col=brewer.pal(8,'Pastel2'), 
                  main="Relative Item Frequency Plot")

#The most frequently occurring items are the maximum shares of the best keyword followed by whether the data channel is
#lifestyle, the data channel is social media, the article was posted on Saturday, the article was posted on Sunday, the 
#data channel is entertainment, the article was posted on Friday, data channel is world, data channel is business, the 
#article was posted on Monday, and so on.

#Create association rules using the apriori algorithm
#Use minimum support = 0.5, and minimum confidence = 0.9
association.rules <- apriori(transaction_object, parameter = list(supp=0.5, conf=0.9, minlen=2))

summary(association.rules)

#Check the first 20 rules
inspect(association.rules[1:20])

#Sort by rules with high lift
rules_lift <- sort(association.rules, by = "lift", decreasing = TRUE)

#Show the support, lift, and confidence for the top 20 rules with high lift
inspect(rules_lift[1:20])

#lhs                                   rhs                                 support confidence  coverage     lift count
#[1]  {num_videos=[0,0.011),                                                                                               
#weekday_is_sunday=0}              => {data_channel_is_entertainment=0} 0.5106329  0.9129044 0.5593498 1.055576  9989
#[2]  {num_videos=[0,0.011),                                                                                               
#kw_max_max=[0,1],                                                                                                   
#weekday_is_sunday=0}              => {data_channel_is_entertainment=0} 0.5106329  0.9129044 0.5593498 1.055576  9989
#[3]  {num_videos=[0,0.011)}             => {data_channel_is_entertainment=0} 0.5625192  0.9116063 0.6170637 1.054075 11004
#[4]  {num_videos=[0,0.011),                                                                                               
#kw_max_max=[0,1]}                 => {data_channel_is_entertainment=0} 0.5625192  0.9116063 0.6170637 1.054075 11004
#[5]  {num_videos=[0,0.011),                                                                                               
#weekday_is_saturday=0}            => {data_channel_is_entertainment=0} 0.5066967  0.9111132 0.5561292 1.053505  9912
#[6]  {num_videos=[0,0.011),                                                                                               
#kw_max_max=[0,1],                                                                                                   
#weekday_is_saturday=0}            => {data_channel_is_entertainment=0} 0.5066967  0.9111132 0.5561292 1.053505  9912
#[7]  {num_videos=[0,0.011),                                                                                               
#data_channel_is_lifestyle=0}      => {data_channel_is_entertainment=0} 0.5152336  0.9042706 0.5697781 1.045593 10079
#[8]  {num_videos=[0,0.011),                                                                                               
#kw_max_max=[0,1],                                                                                                   
#data_channel_is_lifestyle=0}      => {data_channel_is_entertainment=0} 0.5152336  0.9042706 0.5697781 1.045593 10079
#[9]  {num_videos=[0,0.011),                                                                                               
#data_channel_is_socmed=0}         => {data_channel_is_entertainment=0} 0.5008690  0.9017948 0.5554136 1.042730  9798
#[10] {num_videos=[0,0.011),                                                                                               
#kw_max_max=[0,1],                                                                                                   
#data_channel_is_socmed=0}         => {data_channel_is_entertainment=0} 0.5008690  0.9017948 0.5554136 1.042730  9798
#[11] {min_positive_polarity=[0.1,1]}    => {data_channel_is_socmed=0}        0.5544423  0.9516539 0.5826091 1.039840 10846
#[12] {kw_max_max=[0,1],                                                                                                   
#min_positive_polarity=[0.1,1]}    => {data_channel_is_socmed=0}        0.5544423  0.9516539 0.5826091 1.039840 10846
#[13] {min_positive_polarity=[0.1,1],                                                                                      
#weekday_is_sunday=0}              => {data_channel_is_socmed=0}        0.5051120  0.9509191 0.5311829 1.039037  9881
#[14] {kw_max_max=[0,1],                                                                                                   
#min_positive_polarity=[0.1,1],                                                                                      
#weekday_is_sunday=0}              => {data_channel_is_socmed=0}        0.5051120  0.9509191 0.5311829 1.039037  9881
#[15] {min_positive_polarity=[0.1,1],                                                                                      
#weekday_is_saturday=0}            => {data_channel_is_socmed=0}        0.5071567  0.9506516 0.5334833 1.038745  9921
#[16] {kw_max_max=[0,1],                                                                                                   
#min_positive_polarity=[0.1,1],                                                                                      
#weekday_is_saturday=0}            => {data_channel_is_socmed=0}        0.5071567  0.9506516 0.5334833 1.038745  9921
#[17] {min_positive_polarity=[0.1,1],                                                                                      
#data_channel_is_lifestyle=0}      => {data_channel_is_socmed=0}        0.5157959  0.9482192 0.5439628 1.036087 10090
#[18] {kw_max_max=[0,1],                                                                                                   
#min_positive_polarity=[0.1,1],                                                                                      
#data_channel_is_lifestyle=0}      => {data_channel_is_socmed=0}        0.5157959  0.9482192 0.5439628 1.036087 10090
#[19] {abs_title_subjectivity=[0.467,1],                                                                                   
#data_channel_is_lifestyle=0,                                                                                        
#data_channel_is_entertainment=0}  => {weekday_is_sunday=0}             0.5025560  0.9206780 0.5458542 1.011758  9831
#[20] {kw_max_max=[0,1],                                                                                                   
#abs_title_subjectivity=[0.467,1],                                                                                   
#data_channel_is_lifestyle=0,                                                                                        
#data_channel_is_entertainment=0}  => {weekday_is_sunday=0}             0.5025560  0.9206780 0.5458542 1.011758  9831

#Some interesting rules found that may help determine whether an article will be popular include:

#51% of the transactions show that 91% of popular articles with the number of videos between 0 and the bottom 1% of the 
#distribution, maximum shares of the best keyword, and not published on Sunday (or Saturday), are also not entertainment.

#50-52% of the transactions show that 90% of popular articles with the number of videos between 0 and the bottom 1% of 
#the distribution, maximum shares of the best keyword, and not social media (or lifestyle), are also not entertainment.

#51% of the transactions show that 95% of popular articles with maximum shares of the best keyword, with the top 90%
#of the minimum polarity of positive words, and not published on Saturday (or Sunday), are also not social media.

#52% of the transactions show that 95% of popular articles with maximum shares of the best keyword, with the top 90%
#of the minimum polarity of positive words, and not lifestyle, are also not social media.

#50% of transactions show that 92% of popular articles with maximum shares of the best keyword, absolute subjectivity
#level in the top 53% of the distribution, and not lifestyle, or entertainment, are also not published on Sunday.

#Plot the rules
plot(rules_lift, main = "Scatterplot for Association Rules")
#This plot shows that rules with high lift have low support.

#Two-key plot
#The order shows the number of items in a rule
plot(rules_lift, method = "two-key plot")
#There are more items for rules with lower support.

#Interactive scatter-plot
#Hover over each rule to show the items and quality measures (support, confidence, lift)
plotly_arules(rules_lift)

#Interactive graph-based visualization
#Filter the top 20 rules with highest lift
subset_rules <- head(association.rules, n=20, by="lift", decreasing = TRUE)
plot(subset_rules, method = "graph",  engine = "htmlwidget")

#Individual rule representation - parallel coordinates plot
#Visualize what items cause other items in the set
#The right-hand side (RHS)/consequent is the item the set is proposed to have, and on the left-hand side are the 
#most recent additions to the set.
plot(subset_rules, method="paracoord")



#Step 3 - Dimensionality Reduction

#The Random Forest feature selection method, which uses the Gini index to assign a score and rank features, may be used 
#to help select the features to be used in the analysis

#Feature selection with Random Forest
library(randomForest)

#Train Random Forest
set.seed(123)
rf_class <- randomForest(shares_cat ~., data=pop_norm, importance=TRUE)
print(rf_class)

#Number of trees: 500
#No. of variables tried at each split: 6

#OOB estimate of  error rate: 33.71%
#Confusion matrix:
#      0     1 class.error
#0 13401  6681   0.3326860
#1  6682 12880   0.3415806

#Plot the results that show the change in errors as the number of trees increases
plot(rf_class)
#The errors decrease until around 300 trees and then flatten to a rate of around 34%

#Evaluate variable importance according to mean decrease in accuracy and mean decrease in gini
#Mean Decrease in Accuracy
imp_class = importance(rf_class, type=1)
imp_class <- data.frame(predictors=rownames(imp_class),imp_class)

#Mean Decrease Gini
imp_gini = importance(rf_class, type=2)
imp_gini <- data.frame(predictors=rownames(imp_gini),imp_gini)

#Order the predictors by importance according to Mean Decrease in Accuracy
imp_class.sort <- arrange(imp_class, desc(MeanDecreaseAccuracy))
imp_class.sort$predictors <- factor(imp_class.sort$predictors,levels=imp_class.sort$predictors)

#Check the importance of the predictors sorted by Mean Decrease in Accuracy
print(imp_class.sort)

#                      predictors MeanDecreaseAccuracy
#1                     kw_max_avg           61.9394440
#2      self_reference_min_shares           55.0765011
#3                         LDA_00           44.4673117
#4                     kw_min_avg           43.7175089
#5                         LDA_02           42.5568508
#6            weekday_is_saturday           40.1188224
#7                         LDA_04           36.3968977
#8         data_channel_is_socmed           35.5173088
#9  data_channel_is_entertainment           35.2841318
#10                        LDA_01           35.2136206
#11                        LDA_03           33.3672259
#12      n_non_stop_unique_tokens           30.5891593
#13                     num_hrefs           30.4651134
#14                    kw_max_max           30.4153065
#15                      num_imgs           30.3795300
#16                    kw_avg_max           29.0897650
#17             weekday_is_sunday           27.9019685
#18          average_token_length           27.0670120
#19    global_rate_positive_words           26.3063715
#20           global_subjectivity           24.6449979
#21         data_channel_is_world           23.7709442
#22          data_channel_is_tech           22.7403111
#23         min_positive_polarity           22.5604469
#24                  num_keywords           20.0040417
#25                num_self_hrefs           19.8729009
#26                    kw_max_min           19.3566300
#27    global_rate_negative_words           17.1153821
#28         max_positive_polarity           16.6575253
#29           data_channel_is_bus           16.3356759
#30                    num_videos           16.1676228
#31         avg_negative_polarity           15.5759165
#32         avg_positive_polarity           15.5735591
#33         max_negative_polarity           14.9521841
#34      title_sentiment_polarity           12.1364370
#35            title_subjectivity           11.6057996
#36        abs_title_subjectivity            8.3462881
#37                n_tokens_title            5.8054314
#38          weekday_is_wednesday            3.0249403
#39             weekday_is_friday            1.4720977
#40            weekday_is_tuesday            1.4080527
#41     data_channel_is_lifestyle            1.3611512
#42             weekday_is_monday           -0.1673653
#43           weekday_is_thursday           -0.5224005

#The top predictors in terms of Mean Decrease in Accuracy include maximum shares of the average keyword, minimum shares
#of referenced articles in Mashable, closeness to LDA topic 0, minimum shares of the average keyword, closeness to LDA
#topic 2, whether the article was published on Saturday, closeness to LDA topic 4, whether the data channel is social
#media, whether the data channel is entertainment, closeness to LDA topic 1, and so on.

#Note about LDA topics: natural language features were extracted from the Mashable news service using the Latent 
#Dirichlet Allocation algorithm which were applied to Mashable texts before publication to identify the top five 
#relevant topics and measure the closeness of the current article to those topics.

#Order the predictors by importance according to Mean Decrease in Gini
imp_gini.sort <- arrange(imp_gini, desc(imp_gini$MeanDecreaseGini))
imp_gini.sort$predictors <- factor(imp_gini.sort$predictors,levels=imp_gini.sort$predictors)

#Check the predictors sorted by Mean Decrease in Gini
print(imp_gini.sort)

#                      predictors MeanDecreaseGini
#1                     kw_max_avg       1153.92287
#2      self_reference_min_shares        946.62975
#3                         LDA_02        907.79724
#4                     kw_avg_max        814.85549
#5       n_non_stop_unique_tokens        806.63610
#6                         LDA_01        803.72210
#7                         LDA_04        797.00683
#8                         LDA_00        794.83394
#9            global_subjectivity        774.46316
#10          average_token_length        756.70179
#11    global_rate_positive_words        754.00867
#12                    kw_max_min        753.72227
#13                        LDA_03        751.77275
#14                    kw_min_avg        728.77415
#15         avg_positive_polarity        723.53471
#16    global_rate_negative_words        716.73448
#17         avg_negative_polarity        694.98664
#18                     num_hrefs        609.62104
#19                n_tokens_title        445.94421
#20      title_sentiment_polarity        442.60076
#21         max_negative_polarity        411.15707
#22                      num_imgs        410.05621
#23                num_self_hrefs        393.12895
#24            title_subjectivity        379.42032
#25         min_positive_polarity        363.90795
#26        abs_title_subjectivity        329.79344
#27         max_positive_polarity        306.06629
#28                  num_keywords        272.23733
#29                    num_videos        235.01009
#30                    kw_max_max        212.02053
#31 data_channel_is_entertainment        198.25991
#32           weekday_is_saturday        162.87000
#33         data_channel_is_world        155.49323
#34        data_channel_is_socmed        116.90543
#35             weekday_is_sunday        108.09454
#36          data_channel_is_tech        104.43794
#37          weekday_is_wednesday         81.20075
#38            weekday_is_tuesday         79.40986
#39           weekday_is_thursday         74.68520
#40             weekday_is_monday         70.99114
#41             weekday_is_friday         70.29596
#42           data_channel_is_bus         50.91741
#43     data_channel_is_lifestyle         37.07451

#The top predictors in terms of Mean Decrease in Gini include maximum shares of the average keyword, minimum shares of
#referenced articles in Mashable, closeness to LDA topic 2, average shares of the best keyword, rate of unique non-stop
#words in the content, closeness to LDA topic 1, closeness to LDA topic 4, closeness to LDA topic 0, text subjectivity,
#average length of the words in the content, and so on.

#Plot Important Variables
varImpPlot(rf_class, type=1, main = "Variable Importance (Mean Decrease in Accuracy)")
varImpPlot(rf_class, type=2, main = "Variable Importance (Mean Decrease in Gini")
varImpPlot(rf_class, main = "Variable Importance")

#Select the top predictors
imp_class.top <- imp_class.sort[1:20,]
imp_gini.top <- imp_gini.sort[1:20,]

#Print the top predictors according to Mean Decrease in Accuarcy
print(imp_class.top)

#                      predictors MeanDecreaseAccuracy
#1                     kw_max_avg             61.93944
#2      self_reference_min_shares             55.07650
#3                         LDA_00             44.46731
#4                     kw_min_avg             43.71751
#5                         LDA_02             42.55685
#6            weekday_is_saturday             40.11882
#7                         LDA_04             36.39690
#8         data_channel_is_socmed             35.51731
#9  data_channel_is_entertainment             35.28413
#10                        LDA_01             35.21362
#11                        LDA_03             33.36723
#12      n_non_stop_unique_tokens             30.58916
#13                     num_hrefs             30.46511
#14                    kw_max_max             30.41531
#15                      num_imgs             30.37953
#16                    kw_avg_max             29.08977
#17             weekday_is_sunday             27.90197
#18          average_token_length             27.06701
#19    global_rate_positive_words             26.30637
#20           global_subjectivity             24.64500

#Print the top predictors in terms of Mean Decrease in Gini
print(imp_gini.top)

#                   predictors MeanDecreaseGini
#1                  kw_max_avg        1153.9229
#2   self_reference_min_shares         946.6298
#3                      LDA_02         907.7972
#4                  kw_avg_max         814.8555
#5    n_non_stop_unique_tokens         806.6361
#6                      LDA_01         803.7221
#7                      LDA_04         797.0068
#8                      LDA_00         794.8339
#9         global_subjectivity         774.4632
#10       average_token_length         756.7018
#11 global_rate_positive_words         754.0087
#12                 kw_max_min         753.7223
#13                     LDA_03         751.7727
#14                 kw_min_avg         728.7742
#15      avg_positive_polarity         723.5347
#16 global_rate_negative_words         716.7345
#17      avg_negative_polarity         694.9866
#18                  num_hrefs         609.6210
#19             n_tokens_title         445.9442
#20   title_sentiment_polarity         442.6008

#Subset the data with the required independent and dependent variables
#The top variables in terms of the mean decrease in accuracy and mean decrease in gini are kept for the model.
pop_norm_subset <- pop_norm[,c("kw_max_avg", "self_reference_min_shares", "LDA_00", "kw_min_avg", "LDA_02", 
                               "weekday_is_saturday", "LDA_04", "data_channel_is_socmed",
                               "data_channel_is_entertainment", "LDA_01", "LDA_03", "n_non_stop_unique_tokens",
                               "num_hrefs", "kw_max_max", "num_imgs", "kw_avg_max", "weekday_is_sunday",
                               "average_token_length", "global_rate_positive_words", "global_subjectivity", 
                               "kw_max_min", "avg_positive_polarity", "global_rate_negative_words", 
                               "avg_negative_polarity", "n_tokens_title", "title_sentiment_polarity", "shares_cat")]

#Structure of the dataset to be used for predictive analysis
str(pop_norm_subset)

#'data.frame':	39644 obs. of  27 variables:
#$ kw_max_avg                   : num  0 0 0 0 0 0 0 0 0 0 ...
#$ self_reference_min_shares    : num  0.000588 0 0.001089 0 0.000646 ...
#$ LDA_00                       : num  0.5397 0.8627 0.2349 0.0308 0.0309 ...
#$ kw_min_avg                   : num  0.000277 0.000277 0.000277 0.000277 0.000277 ...
#$ LDA_02                       : num  0.0435 0.0545 0.0363 0.5377 0.0311 ...
#$ weekday_is_saturday          : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ LDA_04                       : num  0.0433 0.0539 0.7358 0.0308 0.955 ...
#$ data_channel_is_socmed       : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ data_channel_is_entertainment: Factor w/ 2 levels "0","1": 2 1 1 2 1 1 1 1 1 1 ...
#$ LDA_01                       : num  0.4085 0.054 0.036 0.4528 0.0311 ...
#$ LDA_03                       : num  0.0445 0.0541 0.036 0.0312 0.0308 ...
#$ n_non_stop_unique_tokens     : num  0.001254 0.001218 0.001021 0.001024 0.000832 ...
#$ num_hrefs                    : num  0.01316 0.00987 0.00987 0.02961 0.0625 ...
#$ kw_max_max                   : num  0 0 0 0 0 0 0 0 0 0 ...
#$ num_imgs                     : num  0.00781 0.00781 0.00781 0.00781 0.15625 ...
#$ kw_avg_max                   : num  0 0 0 0 0 0 0 0 0 0 ...
#$ weekday_is_sunday            : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#$ average_token_length         : num  0.582 0.611 0.546 0.548 0.582 ...
#$ global_rate_positive_words   : num  0.294 0.277 0.366 0.266 0.48 ...
#$ global_subjectivity          : num  0.522 0.341 0.702 0.43 0.514 ...
#$ kw_max_min                   : num  0 0 0 0 0 0 0 0 0 0 ...
#$ avg_positive_polarity        : num  0.379 0.287 0.496 0.386 0.411 ...
#$ global_rate_negative_words   : num  0.0741 0.0848 0.0513 0.112 0.0656 ...
#$ avg_negative_polarity        : num  0.65 0.881 0.533 0.63 0.78 ...
#$ n_tokens_title               : num  0.476 0.333 0.333 0.333 0.524 ...
#$ title_sentiment_polarity     : num  0.406 0.5 0.5 0.5 0.568 ...
#$ shares_cat                   : Factor w/ 2 levels "0","1": 1 1 2 1 1 1 1 1 2 1 ...

#Check the five number summary for numeric variables and levels and frequency tables for factor variables
summary(pop_norm_subset)

#kw_max_avg      self_reference_min_shares     LDA_00          kw_min_avg            LDA_02        weekday_is_saturday
#Min.   :0.00000   Min.   :0.0000000         Min.   :0.00000   Min.   :0.0000000   Min.   :0.00000   0:37191            
#1st Qu.:0.01194   1st Qu.:0.0007577         1st Qu.:0.02702   1st Qu.:0.0002767   1st Qu.:0.03106   1: 2453            
#Median :0.01460   Median :0.0014230         Median :0.03602   Median :0.2835153   Median :0.04348                      
#Mean   :0.01896   Mean   :0.0047418         Mean   :0.19914   Mean   :0.3093897   Mean   :0.23513                      
#3rd Qu.:0.02017   3rd Qu.:0.0030831         3rd Qu.:0.25993   3rd Qu.:0.5693853   3rd Qu.:0.36328                      
#Max.   :1.00000   Max.   :1.0000000         Max.   :1.00000   Max.   :1.0000000   Max.   :1.00000                      
#LDA_04        data_channel_is_socmed data_channel_is_entertainment     LDA_01            LDA_03       
#Min.   :0.00000   0:37321                0:32587                       Min.   :0.00000   Min.   :0.00000  
#1st Qu.:0.03082   1: 2323                1: 7057                       1st Qu.:0.02701   1st Qu.:0.03084  
#Median :0.04393                                                        Median :0.03601   Median :0.04317  
#Mean   :0.25241                                                        Mean   :0.15255   Mean   :0.24151  
#3rd Qu.:0.43140                                                        3rd Qu.:0.16289   3rd Qu.:0.40556  
#Max.   :1.00000                                                        Max.   :1.00000   Max.   :1.00000  
#n_non_stop_unique_tokens   num_hrefs         kw_max_max        num_imgs          kw_avg_max     weekday_is_sunday
#Min.   :0.0000000        Min.   :0.00000   Min.   :0.0000   Min.   :0.000000   Min.   :0.0000   0:36907          
#1st Qu.:0.0009627        1st Qu.:0.01316   1st Qu.:1.0000   1st Qu.:0.007812   1st Qu.:0.2050   1: 2737          
#Median :0.0010623        Median :0.02632   Median :1.0000   Median :0.007812   Median :0.2900                    
#Mean   :0.0010603        Mean   :0.03580   Mean   :0.8921   Mean   :0.035501   Mean   :0.3075                    
#3rd Qu.:0.0011610        3rd Qu.:0.04605   3rd Qu.:1.0000   3rd Qu.:0.031250   3rd Qu.:0.3925                    
#Max.   :1.0000000        Max.   :1.00000   Max.   :1.0000   Max.   :1.000000   Max.   :1.0000                    
#average_token_length global_rate_positive_words global_subjectivity   kw_max_min       avg_positive_polarity
#Min.   :0.0000       Min.   :0.0000             Min.   :0.0000      Min.   :0.000000   Min.   :0.0000       
#1st Qu.:0.5569       1st Qu.:0.1825             1st Qu.:0.3962      1st Qu.:0.001491   1st Qu.:0.3062       
#Median :0.5800       Median :0.2510             Median :0.4535      Median :0.002212   Median :0.3588       
#Mean   :0.5656       Mean   :0.2548             Mean   :0.4434      Mean   :0.003867   Mean   :0.3538       
#3rd Qu.:0.6037       3rd Qu.:0.3234             3rd Qu.:0.5083      3rd Qu.:0.003351   3rd Qu.:0.4114       
#Max.   :1.0000       Max.   :1.0000             Max.   :1.0000      Max.   :1.000000   Max.   :1.0000       
#global_rate_negative_words avg_negative_polarity n_tokens_title   title_sentiment_polarity shares_cat
#Min.   :0.00000            Min.   :0.0000        Min.   :0.0000   Min.   :0.0000           0:20082   
#1st Qu.:0.05199            1st Qu.:0.6716        1st Qu.:0.3333   1st Qu.:0.5000           1:19562   
#Median :0.08294            Median :0.7467        Median :0.3810   Median :0.5000                     
#Mean   :0.08983            Mean   :0.7405        Mean   :0.3999   Mean   :0.5357                     
#3rd Qu.:0.11755            3rd Qu.:0.8131        3rd Qu.:0.4762   3rd Qu.:0.5750                     
#Max.   :1.00000            Max.   :1.0000        Max.   :1.0000   Max.   :1.0000                     



#Step 4 - Experimental Design

#In this step, the processed data may be split into 70% training, 15% validation, and 15% test sets in order to tune the 
#parameters of the models that will be studied; as well as split into training and test sets using 10-fold 
#cross-validation to improve the performance of the models, and these results may be used to compare each model.

#First, create train, test, and validation sets for tuning parameters

#Index for 70% training set
set.seed(123)
train_index <- sample(1:nrow(pop_norm_subset), 0.7 * nrow(pop_norm_subset))

#Subset the data using the training index to create the training dataset
train.set <- pop_norm_subset[train_index,]

#The remaining data will be used for 15% validation and 15% test datasets
test_valid.set  <- pop_norm_subset[-train_index,]

#Index for 15% validation and 15% testing dataset
set.seed(123)
testvalid_index <- sample(1:nrow(test_valid.set), 0.5 * nrow(test_valid.set))

#Validation set
valid.set <- test_valid.set[testvalid_index,]

#Test set
test.set  <- test_valid.set[-testvalid_index,]

#Check that the shares categories are balanced
summary(train.set$shares_cat)
#0     1 
#14051 13699 

summary(valid.set$shares_cat)
#0    1 
#3042 2905 

summary(test.set$shares_cat)
#0    1 
#2989 2958 

#The popular and unpopular levels of shares are fairly balanced.


#Second, create train and test sets using 10-fold cross-validation
set.seed(123)
folds <- createFolds(pop_norm_subset$shares_cat)
str(folds)
for (f in folds){
  train <- pop_norm_subset[-f,]
  test <- pop_norm_subset[f,]
}

#Check that the shares categories are balanced
summary(train$shares_cat)
#0     1 
#18073 17606

summary(test$shares_cat)
#0    1 
#2009 1956

#The popular and unpopular levels of shares are fairly balanced.



#Step 5 - Prediction & Step 6 - Performance Evaluation

#Prediction: classification models - Logistic Regression, K-Nearest Neighbours, Decision Trees, Random Forest, and 
#Gradient Boosting Machine - are tested to find the algorithm with the best accuracy. 

#Performance Evaluation: at this stage, the machine learning algorithms that have been run are evaluated using various 
#methods. Several metrics are computed including Accuracy, Precision, Recall/Sensitivity, Specificity, and F1 score, 
#depending on the Confusion Matrix. 

#Note the results found are similar to what has been found in the literature.


#Logistic Regression

#For logistic regression, there are no tuning parameters
#Fit the logistic regression model on the training set using 10-fold cross-validation
set.seed(123)
glm_model <- glm(shares_cat ~ ., family = binomial(link='logit'), data = train)

#Print the output of the model
summary(glm_model)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.7418  -1.0827  -0.6992   1.1054   1.9157  

#Coefficients:
#                                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       601.44697  109.72008   5.482 4.21e-08 ***
#  kw_max_avg                        7.80835    1.00111   7.800 6.20e-15 ***
#  self_reference_min_shares         6.37848    1.00096   6.372 1.86e-10 ***
#  LDA_00                         -557.22283  101.70299  -5.479 4.28e-08 ***
#  kw_min_avg                        0.41822    0.03977  10.515  < 2e-16 ***
#  LDA_02                         -554.06224  100.93737  -5.489 4.04e-08 ***
#  weekday_is_saturday1              0.93981    0.05003  18.785  < 2e-16 ***
#  LDA_04                         -557.20049  101.72319  -5.478 4.31e-08 ***
#  data_channel_is_socmed1           0.81527    0.05225  15.603  < 2e-16 ***
#  data_channel_is_entertainment1   -0.56514    0.04034 -14.010  < 2e-16 ***
#  LDA_01                         -556.97269  101.58393  -5.483 4.18e-08 ***
#  LDA_03                         -557.03894  101.64952  -5.480 4.25e-08 ***
#  n_non_stop_unique_tokens       -591.38563   82.41525  -7.176 7.19e-13 ***
#  num_hrefs                         2.79933    0.37072   7.551 4.32e-14 ***
#  kw_max_max                       -0.25513    0.05708  -4.469 7.85e-06 ***
#  num_imgs                          0.44273    0.20944   2.114  0.03453 *  
#  kw_avg_max                       -0.23634    0.10530  -2.245  0.02480 *  
#  weekday_is_sunday1                0.73161    0.04520  16.187  < 2e-16 ***
#  average_token_length             -0.63080    0.19441  -3.245  0.00118 ** 
#  global_rate_positive_words        0.10389    0.11911   0.872  0.38311    
#global_subjectivity               1.47690    0.15444   9.563  < 2e-16 ***
#  kw_max_min                       -2.52485    1.44210  -1.751  0.07998 .  
#avg_positive_polarity            -0.27197    0.14544  -1.870  0.06149 .  
#global_rate_negative_words       -0.10257    0.20993  -0.489  0.62514    
#avg_negative_polarity            -0.03974    0.10384  -0.383  0.70194    
#n_tokens_title                   -0.11805    0.11385  -1.037  0.29980    
#title_sentiment_polarity          0.43254    0.08683   4.981 6.32e-07 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 49455  on 35678  degrees of freedom
#Residual deviance: 45911  on 35652  degrees of freedom
#AIC: 45965

#Number of Fisher Scoring iterations: 8

#The coefficients of the predictors indicate that every one unit change in the independent variable produces a
#specific unit change in the log odds of the dependent variable i.e. the log odds of an article being popular.
#The largest impact is from the rate of unique non-stop words in the content, followed by closeness to LDA topics 0,
#1, 2, 3, and 4 which all have a negative relationship with the log odds of an article being popular.
#Most of the estimates are statistically significant and the model produces the following relationship between the
#log odds and the predictors (only including the independent variables that have a significant effect on the dependent
#variable):
#logit(p) = 601.45 + 7.81*(maximum shares of average keyword) + 6.38*(minimum shares of referenced articles in Mashable)
#           - 557.22*(closeness to LDA topic 0) + 0.42*(minimum shares of the average keyword)
#           - 554.06*(closeness to LDA topic 2) + 0.94*(article published on Saturday)
#           - 557.2*(closeness to LDA topic 4) + 0.82*(data channel is social media)
#           - 0.56*(data channel is entertainment) - 556.97*(closeness to LDA topic 1)
#           - 557.04*(closeness to LDA topic 3) - 591.39*(rate of unique non-stop words in the content)
#           + 2.8*(number of links) - 0.26*(maximum shares of best keyword) + 0.44*(number of images)
#           - 0.24*(average shares of the best keyword) + 0.73*(article published on Sunday)
#           - 0.63*(average length of the words in the content) + 1.48*(text subjectivity) + 0.43*(title polarity)

#Furthermore, the residual deviance is found to be better than the null deviance.

#Plot the results
plot(glm_model)

#Analyze the table of deviance
anova(glm_model, test = "Chisq")

#Analysis of Deviance Table

#Model: binomial, link: logit

#Response: shares_cat

#Terms added sequentially (first to last)


#                                Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#NULL                                            35678      49455              
#kw_max_avg                       1   265.89     35677      49190 < 2.2e-16 ***
#  self_reference_min_shares      1   108.83     35676      49081 < 2.2e-16 ***
#  LDA_00                         1   166.84     35675      48914 < 2.2e-16 ***
#  kw_min_avg                     1   230.89     35674      48683 < 2.2e-16 ***
#  LDA_02                         1   549.65     35673      48133 < 2.2e-16 ***
#  weekday_is_saturday            1   397.77     35672      47736 < 2.2e-16 ***
#  LDA_04                         1   247.32     35671      47488 < 2.2e-16 ***
#  data_channel_is_socmed         1   384.11     35670      47104 < 2.2e-16 ***
#  data_channel_is_entertainment  1   385.15     35669      46719 < 2.2e-16 ***
#  LDA_01                         1    40.67     35668      46678 1.801e-10 ***
#  LDA_03                         1     1.60     35667      46677 0.2058005    
#n_non_stop_unique_tokens       1   108.87     35666      46568 < 2.2e-16 ***
#  num_hrefs                      1   147.18     35665      46421 < 2.2e-16 ***
#  kw_max_max                     1    56.83     35664      46364 4.761e-14 ***
#  num_imgs                       1    11.72     35663      46352 0.0006167 ***
#  kw_avg_max                     1    12.63     35662      46340 0.0003796 ***
#  weekday_is_sunday              1   272.29     35661      46067 < 2.2e-16 ***
#  average_token_length           1     0.00     35660      46067 0.9597446    
#global_rate_positive_words     1    23.08     35659      46044 1.551e-06 ***
#  global_subjectivity            1   101.37     35658      45943 < 2.2e-16 ***
#  kw_max_min                     1     2.95     35657      45940 0.0856799 .  
#avg_positive_polarity          1     2.22     35656      45938 0.1360054    
#global_rate_negative_words     1     1.32     35655      45936 0.2507089    
#avg_negative_polarity          1     0.01     35654      45936 0.9422567    
#n_tokens_title                 1     0.89     35653      45935 0.3452956    
#title_sentiment_polarity       1    24.88     35652      45911 6.099e-07 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#For most of the variables in the model, there is a drop in deviance when adding each variable at a time.
#Among the variables that significantly reduce the residual deviance are closeness to LDA topic 2, followed by
#whether the article was published on Saturday, whether the data channel is entertainment, whether the data channel
#is social media, whether the article was published on Sunday, maximum shares of the average keyword, closeness to
#LDA topic 4, minimum shares of the average keyword, closeness to LDA topic 0, minimum shares of referenced articles in
#Mashable, and so on.

#The McFadden R2 index may be used to assess the model fit
install.packages("pscl")
library(pscl)
glm_r2 <- pR2(glm_model)
print(glm_r2)
#alternative format:
print(format(glm_r2, scientific = FALSE))

#fitting null model for pseudo-r2
#          llh       llhNull            G2      McFadden          r2ML          r2CU 
#-2.295526e+04 -2.472774e+04  3.544969e+03  7.168000e-02  9.458085e-02  1.261150e-01 

#McFadden R2 = 0.07168 may not be a good model fit.

#Make a prediction on the test set
glm_prediction<-predict(glm_model, test, type = "response")
glm_pred_results <- as.factor(ifelse(glm_prediction > 0.5,1,0))

#Confusion Matrix and Statistics
glm_ConfusionMatrix_stats = confusionMatrix(glm_pred_results, test$shares_cat, mode = "everything", positive = "1")
print(glm_ConfusionMatrix_stats)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1334  742
#         1 675 1214

#Accuracy : 0.6426          
#95% CI : (0.6275, 0.6576)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.2848          

#Mcnemar's Test P-Value : 0.07955         
                                          
#            Sensitivity : 0.6207          
#            Specificity : 0.6640          
#         Pos Pred Value : 0.6427          
#         Neg Pred Value : 0.6426          
#              Precision : 0.6427          
#                 Recall : 0.6207          
#                     F1 : 0.6315          
#             Prevalence : 0.4933          
#         Detection Rate : 0.3062          
#   Detection Prevalence : 0.4764          
#      Balanced Accuracy : 0.6423

#Note for statistics found based on the confusion matrix:

#For a 2x2 confusion matrix where

#           Reference	
#Predicted	Event	  No Event
#Event	      A	       B
#No Event   	C	       D

#The formulas used are:
  
#Sensitivity = A/(A+C)

#Specificity = D/(B+D)

#Prevalence = (A+C)/(A+B+C+D)

#PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))

#NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))

#Detection Rate = A/(A+B+C+D)

#Detection Prevalence = (A+B)/(A+B+C+D)

#Balanced Accuracy = (sensitivity+specificity)/2

#Precision = A/(A+B)

#Recall = A/(A+C)

#F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)
#where beta = 1 for this function.


#AUC Score
#Note: a model with good predictive ability should have an Area Under the curve (AUC) closer to 1 than to 0.5.
library(pROC)
glmauc <- roc(test$shares_cat, glm_prediction)
print(glmauc)

#Area under the curve is 0.6883

#Plot AUC
plot(glmauc)

#Plot ROC curve
library(ROCR)
glmroc_pred <- prediction(glm_prediction, test$shares_cat)
glm_performance <- performance(glmroc_pred, "tpr", "fpr")
plot(glm_performance, colorize=TRUE)


#K-Nearest Neighbours
library("class")
#install.packages("gmodels")
library("gmodels")

#Remove the share categories column from the training and test datasets
train.set_new <- train.set[-27]
test.set_new <- test.set[-27]
valid.set_new <- valid.set[-27]

train_new <- train[-27]
test_new <- test[-27]

#Store the labels
train.set_labels <- train.set$shares_cat
test.set_labels <- test.set$shares_cat
valid.set_labels <- valid.set$shares_cat

train_labels <- train$shares_cat
test_labels <- test$shares_cat

#Tune the model for different levels of k (number of neighbours)
set.seed(123)
knnfit <- train(x = train.set_new, y = train.set_labels, method = "knn", tuneLength = 10, 
                trControl = trainControl(method = "cv", search = "grid"))

#Print the training model to see the accuracy
print(knnfit)

#Resampling results across tuning parameters:

#k   Accuracy   Kappa    
#5  0.6020179  0.2032993
#7  0.6069909  0.2132239
#9  0.6144868  0.2282005
#11  0.6195678  0.2383678
#13  0.6232789  0.2457662
#15  0.6232428  0.2456625
#17  0.6267745  0.2527195
#19  0.6275317  0.2542411
#21  0.6261626  0.2514936
#23  0.6281806  0.2555366

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was k = 23.

#Plot the results
plot(knnfit)
#The most accurate predictions are made at k = 23

#Validate and test the model with k = 23
knn_prediction <- knn(train = valid.set_new, test = test.set_new, cl= valid.set_labels, k = 23)

#Confusion Matrix and Statistics
knn_ConfusionMatrix_stats = confusionMatrix(knn_prediction, test.set_labels, mode = "everything", positive = "1")
print(knn_ConfusionMatrix_stats)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1937 1207
#         1 1052 1751

#Accuracy : 0.6201          
#95% CI : (0.6077, 0.6325)
#No Information Rate : 0.5026          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.2401          

#Mcnemar's Test P-Value : 0.001195          

#            Sensitivity : 0.5920          
#            Specificity : 0.6480          
#         Pos Pred Value : 0.6247          
#         Neg Pred Value : 0.6161          
#              Precision : 0.6247          
#                 Recall : 0.5920          
#                     F1 : 0.6079          
#             Prevalence : 0.4974          
#         Detection Rate : 0.2944          
#   Detection Prevalence : 0.4713          
#      Balanced Accuracy : 0.6200

#AUC Score
knnauc <- roc(test.set$shares_cat, as.numeric(levels(knn_prediction))[as.integer(knn_prediction)])
print(knnauc)

#Area under the curve: 0.62

#Plot AUC
plot(knnauc)

#Plot ROC curve
knnroc_pred <- prediction(as.numeric(levels(knn_prediction))[as.integer(knn_prediction)], test.set$shares_cat)
knn_performance <- performance(knnroc_pred, "tpr", "fpr")
plot(knn_performance, colorize=TRUE)


#Using 10-fold Cross-Validation
#For k = 23, make a prediction on the test set.
knn_prediction2 <- knn(train = train_new, test = test_new, cl= train_labels, k = 23)

#Confusion Matrix and Statistics
knn_ConfusionMatrix_stats2 = confusionMatrix(knn_prediction2, test_labels, mode = "everything", positive = "1")
print(knn_ConfusionMatrix_stats2)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1335  813
#         1  674 1143

#Accuracy : 0.625           
#95% CI : (0.6097, 0.6401)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.2491          

#Mcnemar's Test P-Value : 0.0003453 
      
#            Sensitivity : 0.5844          
#            Specificity : 0.6645          
#         Pos Pred Value : 0.6291          
#         Neg Pred Value : 0.6215          
#              Precision : 0.6291          
#                 Recall : 0.5844          
#                     F1 : 0.6059          
#             Prevalence : 0.4933          
#         Detection Rate : 0.2883          
#   Detection Prevalence : 0.4583          
#      Balanced Accuracy : 0.6244

#AUC Score
knnauc2 <- roc(test$shares_cat, as.numeric(levels(knn_prediction2))[as.integer(knn_prediction2)])
print(knnauc2)

#Area under the curve: 0.6244

#Plot AUC
plot(knnauc2)

#Plot ROC curve
knnroc_pred2 <- prediction(as.numeric(levels(knn_prediction2))[as.integer(knn_prediction2)], test$shares_cat)
knn_performance2 <- performance(knnroc_pred2, "tpr", "fpr")
plot(knn_performance2, colorize=TRUE)


#Decision Trees
#install.packages("party")
library("party")

#Tune the model based on mincriterion - the minimum value of the test statistic (1 - p-value)
set.seed(123)
ctreefit <- train(shares_cat ~., data = train.set, method = "ctree", tuneLength = 6, 
                  trControl = trainControl(method = "cv", search = "grid"))

#Print the training model to see the accuracy
print(ctreefit)

#mincriterion  Accuracy   Kappa    
#0.010         0.5997473  0.1989281
#0.206         0.6216938  0.2433777
#0.402         0.6246488  0.2493046
#0.598         0.6266667  0.2531889
#0.794         0.6278202  0.2552605
#0.990         0.6263789  0.2528885

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mincriterion = 0.794.

#Plot the results
plot(ctreefit)

#The most accurate predictions were made with mincriterion = 0.794
#Since mincriterion = 0.99 is very close in accuracy, this value may be chosen for the model since mincriterion 
#is determined by the p-value

#Prune the tree changing the control parameter to mincriterion = 0.99
#Run the model on the validation set
set.seed(123)
ctree_pruned <- ctree(shares_cat ~ ., data = valid.set, controls = ctree_control(mincriterion = 0.99))

#Make a prediction on the test set
ctree_pruned_prediction <- predict(ctree_pruned, test.set)

#Print the decision tree
print(ctree_pruned)

#1) LDA_02 <= 0.6314686; criterion = 1, statistic = 138.984
#  2) data_channel_is_entertainment == {1}; criterion = 1, statistic = 91.715
#    3) weekday_is_sunday == {1}; criterion = 1, statistic = 19.241
#      4)*  weights = 86 
#    3) weekday_is_sunday == {0}
#      5) weekday_is_saturday == {1}; criterion = 0.999, statistic = 17.112
#        6)*  weights = 69 
#      5) weekday_is_saturday == {0}
#        7) self_reference_min_shares <= 0.002964544; criterion = 0.998, statistic = 16.028
#          8)*  weights = 770 
#        7) self_reference_min_shares > 0.002964544
#          9)*  weights = 194 
#  2) data_channel_is_entertainment == {0}
#    10) weekday_is_saturday == {0}; criterion = 1, statistic = 65.636
#       11) num_hrefs <= 0.04276316; criterion = 1, statistic = 53.82
#          12) data_channel_is_socmed == {1}; criterion = 1, statistic = 34.097
#             13) LDA_00 <= 0.2259519; criterion = 0.996, statistic = 14.382
#                14)*  weights = 63 
#             13) LDA_00 > 0.2259519
#                15)*  weights = 169 
#          12) data_channel_is_socmed == {0}
#             16) n_non_stop_unique_tokens <= 0.0009482759; criterion = 1, statistic = 26.93
#                17)*  weights = 430 
#             16) n_non_stop_unique_tokens > 0.0009482759
#                18) weekday_is_sunday == {1}; criterion = 0.999, statistic = 16.896
#                   19)*  weights = 135 
#                18) weekday_is_sunday == {0}
#                   20) kw_min_avg <= 0.4634059; criterion = 0.997, statistic = 15.029
#                      21)*  weights = 1353 
#                   20) kw_min_avg > 0.4634059
#                      22)*  weights = 642 
#       11) num_hrefs > 0.04276316
#          23)*  weights = 873 
#      10) weekday_is_saturday == {1}
#         24)*  weights = 252 
#1) LDA_02 > 0.6314686
#  25) weekday_is_saturday == {1}; criterion = 0.999, statistic = 18.202
#     26)*  weights = 54 
#  25) weekday_is_saturday == {0}
#     27) weekday_is_sunday == {0}; criterion = 0.995, statistic = 14.076
#        28)*  weights = 801 
#     27) weekday_is_sunday == {1}
#        29)*  weights = 56

#Plot the decision tree
plot(ctree_pruned, type = "simple")

#At the root node of the tree is closeness to LDA topic 2. This variable will help decide the classes of popularity.
#For instance, based on the decision tree plotted, if closeness to LDA topic 2 is greater than 0.631, check if the day
#the article was published is Saturday.
#If it is Saturday, then 42.6% of the articles are unpopular, and 57.4% of the articles are popular; which is the case
#for 54 samples.
#If the day the article was published is not Saturday, then check if it is Sunday. If it is not Sunday then 71.9% of the
#articles are not popular wherease 28.1% of the articles are popular, and this is the case for 801 samples.
#If it is Sunday, then 48.2% of the articles are unpopular whereas 51.8% of the articles are popular, and this 
#is the case for 56 samples.

#Important variables found to help determine whether an article will be popular are closeness to LDA topic 2, whether
#data channel is entertainment, whether the day the article was published is Sunday, whether the day the article was 
#published is Saturday, minimum shares of referenced articles in Mashable, number of links, whether the data channel is
#social media, closeness to LDA topic 0, rate of unique non-stop words in the content, minimum shares of the average
#keyword, and so on.

#Confusion Matrix and Statistics
ctree_pruned_ConfusionMatrix_stats = confusionMatrix(ctree_pruned_prediction, test.set$shares_cat, mode = "everything", 
                                                     positive = "1")
print(ctree_pruned_ConfusionMatrix_stats)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1829 1141
#         1 1160 1817

#Accuracy : 0.6131          
#95% CI : (0.6006, 0.6255)
#No Information Rate : 0.5026          
#P-Value [Acc > NIR] : <2e-16          

#Kappa : 0.2262          

#Mcnemar's Test P-Value : 0.7075
         
#            Sensitivity : 0.6143          
#            Specificity : 0.6119          
#         Pos Pred Value : 0.6103          
#         Neg Pred Value : 0.6158          
#              Precision : 0.6103          
#                 Recall : 0.6143          
#                     F1 : 0.6123          
#             Prevalence : 0.4974          
#         Detection Rate : 0.3055          
#   Detection Prevalence : 0.5006          
#      Balanced Accuracy : 0.6131

#AUC Score
ctreeauc <- roc(test.set$shares_cat, as.numeric(levels(ctree_pruned_prediction))[as.integer(ctree_pruned_prediction)])
print(ctreeauc)

#Area under the curve: 0.6131

#Plot AUC
plot(ctreeauc)

#Plot ROC curve
ctreeroc_pred <- prediction(as.numeric(levels(ctree_pruned_prediction))[as.integer(ctree_pruned_prediction)], test.set$shares_cat)
ctree_performance <- performance(ctreeroc_pred, "tpr", "fpr")
plot(ctree_performance, colorize=TRUE)


#Using 10-fold cross-validation
#Create a decision tree model on the training set using the best parameters
set.seed(123)
ctree_model <- ctree(shares_cat ~. , data = train, controls = ctree_control(mincriterion = 0.99))

#Print the decision tree
print(ctree_model)

#1) LDA_02 <= 0.5968011; criterion = 1, statistic = 869.307
#  2) data_channel_is_entertainment == {0}; criterion = 1, statistic = 736.054
#    3) weekday_is_saturday == {0}; criterion = 1, statistic = 279.203
#      4) kw_min_avg <= 0.6040266; criterion = 1, statistic = 216.583
#        5) data_channel_is_socmed == {0}; criterion = 1, statistic = 161.159
#          6) num_hrefs <= 0.04605263; criterion = 1, statistic = 149.883
#            7) weekday_is_sunday == {1}; criterion = 1, statistic = 113.296
#              8) LDA_01 <= 0.04319949; criterion = 0.999, statistic = 16.598
#                9)*  weights = 466 
#              8) LDA_01 > 0.04319949
#                10)*  weights = 279 
#            7) weekday_is_sunday == {0}
#              11) LDA_00 <= 0.2589749; criterion = 1, statistic = 54.745
#                 12) LDA_02 <= 0.3881332; criterion = 1, statistic = 54.484
#                    13) n_non_stop_unique_tokens <= 0.001315496; criterion = 1, statistic = 18.327
#                       14)*  weights = 5574 
#                    13) n_non_stop_unique_tokens > 0.001315496
#                       15) global_rate_negative_words <= 0.1430531; criterion = 1, statistic = 22.144
#                          16) n_tokens_title <= 0.4285714; criterion = 1, statistic = 20.052
#                             17)*  weights = 256 
#                          16) n_tokens_title > 0.4285714
#                             18)*  weights = 137 
#                       15) global_rate_negative_words > 0.1430531
#                          19)*  weights = 114 
#                 12) LDA_02 > 0.3881332
#                    20) global_rate_positive_words <= 0.1843732; criterion = 0.997, statistic = 14.723
#                       21)*  weights = 407 
#                    20) global_rate_positive_words > 0.1843732
#                       22)*  weights = 739 
#              11) LDA_00 > 0.2589749
#                 23) global_rate_positive_words <= 0.2371013; criterion = 1, statistic = 21.81
#                    24)*  weights = 1739 
#                 23) global_rate_positive_words > 0.2371013
#                    25) global_subjectivity <= 0.4190236; criterion = 1, statistic = 25.692
#                       26)*  weights = 856 
#                    25) global_subjectivity > 0.4190236
#                       27)*  weights = 1687 
#           6) num_hrefs > 0.04605263
#             28) global_subjectivity <= 0.4068627; criterion = 1, statistic = 23.36
#                29)*  weights = 485 
#             28) global_subjectivity > 0.4068627
#                30) weekday_is_sunday == {1}; criterion = 1, statistic = 21.034
#                   31) LDA_01 <= 0.6242477; criterion = 0.993, statistic = 13.16
#                      32)*  weights = 249 
#                   31) LDA_01 > 0.6242477
#                      33)*  weights = 17 
#                30) weekday_is_sunday == {0}
#                   34) kw_max_avg <= 0.0121861; criterion = 0.994, statistic = 13.527
#                      35) LDA_04 <= 0.7096185; criterion = 0.994, statistic = 13.438
#                         36)*  weights = 280 
#                      35) LDA_04 > 0.7096185
#                         37)*  weights = 193 
#                   34) kw_max_avg > 0.0121861
#                      38)*  weights = 1965 
#        5) data_channel_is_socmed == {1}
#          39) LDA_00 <= 0.1358852; criterion = 1, statistic = 31.932
#             40)*  weights = 266 
#          39) LDA_00 > 0.1358852
#             41) n_non_stop_unique_tokens <= 0.001144902; criterion = 1, statistic = 21.664
#                42)*  weights = 647 
#             41) n_non_stop_unique_tokens > 0.001144902
#                43)*  weights = 224 
#      4) kw_min_avg > 0.6040266
#        44) kw_avg_max <= 0.5277778; criterion = 1, statistic = 92.725
#           45) LDA_00 <= 0.7223975; criterion = 1, statistic = 38.217
#              46) LDA_01 <= 0.03611333; criterion = 0.999, statistic = 18.082
#                 47) data_channel_is_socmed == {1}; criterion = 0.994, statistic = 13.595
#                    48) n_non_stop_unique_tokens <= 0.001164241; criterion = 0.997, statistic = 14.789
#                       49)*  weights = 128 
#                    48) n_non_stop_unique_tokens > 0.001164241
#                       50)*  weights = 33 
#                 47) data_channel_is_socmed == {0}
#                    51)*  weights = 1719 
#             46) LDA_01 > 0.03611333
#                52) kw_min_avg <= 0.6597951; criterion = 0.996, statistic = 14.259
#                   53)*  weights = 263 
#                52) kw_min_avg > 0.6597951
#                   54)*  weights = 1195 
#           45) LDA_00 > 0.7223975
#              55)*  weights = 475 
#        44) kw_avg_max > 0.5277778
#           56)*  weights = 1565 
#    3) weekday_is_saturday == {1}
#      57) LDA_00 <= 0.3962087; criterion = 1, statistic = 27.009
#         58) LDA_01 <= 0.04320616; criterion = 0.998, statistic = 16.015
#            59)*  weights = 725 
#         58) LDA_01 > 0.04320616
#            60)*  weights = 391 
#       57) LDA_00 > 0.3962087
#          61)*  weights = 355 
#  2) data_channel_is_entertainment == {1}
#    62) weekday_is_sunday == {1}; criterion = 1, statistic = 113.347
#       63)*  weights = 483 
#    62) weekday_is_sunday == {0}
#       64) weekday_is_saturday == {0}; criterion = 1, statistic = 72.14
#          65) kw_max_avg <= 0.01303062; criterion = 1, statistic = 38.531
#             66) kw_max_max <= 0.7327167; criterion = 1, statistic = 28.103
#                67)*  weights = 308 
#             66) kw_max_max > 0.7327167
#                68)*  weights = 1572 
#          65) kw_max_avg > 0.01303062
#             69) self_reference_min_shares <= 0.003320289; criterion = 1, statistic = 24.623
#                70)*  weights = 2859 
#             69) self_reference_min_shares > 0.003320289
#                71)*  weights = 691 
#      64) weekday_is_saturday == {1}
#         72)*  weights = 325 
#1) LDA_02 > 0.5968011
#  73) data_channel_is_socmed == {1}; criterion = 1, statistic = 88.108
#     74)*  weights = 223 
#  73) data_channel_is_socmed == {0}
#     75) weekday_is_saturday == {0}; criterion = 1, statistic = 57.96
#        76) num_imgs <= 0.0546875; criterion = 1, statistic = 58.131
#           77) weekday_is_sunday == {0}; criterion = 1, statistic = 24.364
#              78) kw_max_max <= 0.7327167; criterion = 1, statistic = 18.694
#                 79)*  weights = 439 
#              78) kw_max_max > 0.7327167
#                 80) num_hrefs <= 0.08881579; criterion = 1, statistic = 21.495
#                    81) LDA_02 <= 0.7970923; criterion = 0.997, statistic = 15.097
#                       82)*  weights = 1387 
#                    81) LDA_02 > 0.7970923
#                       83) self_reference_min_shares <= 0.002253053; criterion = 0.996, statistic = 14.514
#                          84)*  weights = 2128 
#                       83) self_reference_min_shares > 0.002253053
#                          85)*  weights = 474 
#                 80) num_hrefs > 0.08881579
#                    86)*  weights = 129 
#           77) weekday_is_sunday == {1}
#              87)*  weights = 337 
#        76) num_imgs > 0.0546875
#           88)*  weights = 515 
#     75) weekday_is_saturday == {1}
#        89) kw_max_max <= 0.8186885; criterion = 0.999, statistic = 17.163
#           90)*  weights = 43 
#        89) kw_max_max > 0.8186885
#           91)*  weights = 337

#Plot the decision tree
plot(ctree_model, type = "simple")

#Important variables found to help determine whether an article will be popular are closeness to LDA topic 2
#(statistic = 869.307), followed by whether the data channel is entertainment (statistic = 736.054), whether the day the
#article was published is Saturday (statistic = 279.203), minimum shares of the average keyword (statistic = 216.583),
#whether the data channel is social media (statistic = 161.159), number of links (statistic = 149.883), whether the day
#the article was posted is Sunday (statistic = 113.296), average shares of the best keyword (statistic = 92.725),
#number of images (statistic = 58.131), closeness to LDA topic 0 (statistic = 54.745), and so on.

#Make a prediction on the test set.
ctree_prediction <- predict(ctree_model, test)

#Confusion Matrix and Statistics
ctree_model_ConfusionMatrix_stats = confusionMatrix(ctree_prediction, test$shares_cat, mode = "everything", 
                                                    positive = "1")
print(ctree_model_ConfusionMatrix_stats)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1212  679
#         1  797 1277

#Accuracy : 0.6277          
#95% CI : (0.6125, 0.6428)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.2559          

#Mcnemar's Test P-Value : 0.002324         

#            Sensitivity : 0.6529          
#            Specificity : 0.6033          
#         Pos Pred Value : 0.6157          
#         Neg Pred Value : 0.6409          
#              Precision : 0.6157          
#                 Recall : 0.6529          
#                     F1 : 0.6337          
#             Prevalence : 0.4933          
#         Detection Rate : 0.3221          
#   Detection Prevalence : 0.5231          
#      Balanced Accuracy : 0.6281

#AUC Score
ctreeauc2 <- roc(test$shares_cat, as.numeric(levels(ctree_prediction))[as.integer(ctree_prediction)])
print(ctreeauc2)

#Area under the curve: 0.6281

#Plot AUC
plot(ctreeauc2)

#Plot ROC curve
ctreeroc_pred2 <- prediction(as.numeric(levels(ctree_prediction))[as.integer(ctree_prediction)], test$shares_cat)
ctree_performance2 <- performance(ctreeroc_pred2, "tpr", "fpr")
plot(ctree_performance2, colorize=TRUE)


#Random Forest

#Tune the model with different mtry parameters, i.e. the number of variables randomly sampled as candidates at each split
set.seed(123)
rffit <- train(shares_cat ~., data = train.set, method = "rf", tuneLength = 3, 
               trControl = trainControl(method = "cv", search = "grid"))

#Print the training model to see the accuracy
print(rffit)

#mtry  Accuracy   Kappa    
# 2    0.6622345  0.3242408
#14    0.6541983  0.3082772
#26    0.6520358  0.3039446

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 2.

#Plot the results
#Visualize the accuracy based on the value of mtry.
plot(rffit)

#Create a Random Forest model using the validation set with mtry = 2
set.seed(123)
rf_model <- randomForest(shares_cat ~., data = valid.set, mtry = 2, importance = TRUE)

#Print the model
print(rf_model)

#Number of trees: 500
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 34.81%
#Confusion matrix:
#     0    1 class.error
#0 2060  982   0.3228139
#1 1088 1817   0.3745267

#Plot the results
plot(rf_model)
#The errors from the model decrease until around 200 trees and then flatten.

#Make a prediction on the test set
rf_predict <- predict(rf_model, test.set, type = "class")

#Confusion Matrix and Statistics
rf_confusionMatrix_stats <- confusionMatrix(rf_predict, test.set$shares_cat, mode = "everything", positive = "1")
print(rf_confusionMatrix_stats)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1990 1120
#         1  999 1838

#Accuracy : 0.6437          
#95% CI : (0.6314, 0.6559)
#No Information Rate : 0.5026          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.2872          

#Mcnemar's Test P-Value : 0.009138 
        
#            Sensitivity : 0.6214          
#            Specificity : 0.6658          
#         Pos Pred Value : 0.6479          
#         Neg Pred Value : 0.6399          
#              Precision : 0.6479          
#                 Recall : 0.6214          
#                     F1 : 0.6343          
#             Prevalence : 0.4974          
#         Detection Rate : 0.3091          
#   Detection Prevalence : 0.4770          
#      Balanced Accuracy : 0.6436 

#AUC Score
rfauc <- roc(test.set$shares_cat, as.numeric(levels(rf_predict))[as.integer(rf_predict)])
print(rfauc)

#Area under the curve: 0.6436

#Plot AUC
plot(rfauc)

#Plot ROC curve
rfroc_pred <- prediction(as.numeric(levels(rf_predict))[as.integer(rf_predict)], test.set$shares_cat)
rf_performance <- performance(rfroc_pred, "tpr", "fpr")
plot(rf_performance, colorize=TRUE)

#Check the important variables
importance(rf_model)

#                                       0           1 MeanDecreaseAccuracy MeanDecreaseGini
#kw_max_avg                    12.7310120 14.75961893            20.702820        174.01898
#self_reference_min_shares     17.0067342 13.77551416            20.478818        155.88233
#LDA_00                         8.7836745  7.25108839            12.232652        140.88378
#kw_min_avg                    20.4511226  0.80879881            17.378091        123.40070
#LDA_02                        18.6280357  2.52206351            17.332176        156.80113
#weekday_is_saturday           21.3061769 11.56706968            21.518318         31.44492
#LDA_04                        10.8319983  6.07636732            14.161131        142.02218
#data_channel_is_socmed         7.9607130 13.33131301            14.927426         19.59069
#data_channel_is_entertainment 13.1381832  2.26272992            13.196795         23.36260
#LDA_01                        11.7698979  3.00109954            12.526222        136.59978
#LDA_03                        13.4850624 -0.74903440            11.002887        134.73511
#n_non_stop_unique_tokens      11.4177800  3.82414586            12.006379        145.02502
#num_hrefs                      6.8585193  5.83000026            10.415695        116.62004
#kw_max_max                    -0.1260238  7.04914180             5.672202         39.92993
#num_imgs                      10.3045425  0.96437227             9.694474         79.16993
#kw_avg_max                     8.2897306  0.05420312             6.925046        136.95709
#weekday_is_sunday              8.7984839  6.78159179            10.237964         18.43724
#average_token_length           8.4711551 -2.06198892             5.163225        129.97954
#global_rate_positive_words     6.4144414  5.95345343             9.270298        131.54903
#global_subjectivity            3.5315145  5.46060541             6.911155        135.10063
#kw_max_min                     9.4879264  3.51314322             9.712763        137.69209
#avg_positive_polarity         -0.8552206  6.95723819             4.330493        128.60078
#global_rate_negative_words     4.9363931  1.39243881             4.714351        128.23493
#avg_negative_polarity          2.8657762  2.33514021             3.765739        125.90124
#n_tokens_title                 2.3541071  1.97842684             3.008126         83.84260
#title_sentiment_polarity       3.5005870  2.85885534             4.599657         87.11326

#Evaluate variable importance according to mean decrease in accuracy and mean decrease in gini
#Mean Decrease in Accuracy
rf_imp_class = importance(rf_model, type=1)
rf_imp_class <- data.frame(predictors=rownames(rf_imp_class),rf_imp_class)

#Mean Decrease Gini
rf_imp_gini = importance(rf_model, type=2)
rf_imp_gini <- data.frame(predictors=rownames(rf_imp_gini),rf_imp_gini)

#Order the predictors by importance according to Mean Decrease in Accuracy
rf_imp_class.sort <- arrange(rf_imp_class, desc(MeanDecreaseAccuracy))
rf_imp_class.sort$predictors <- factor(rf_imp_class.sort$predictors,levels=rf_imp_class.sort$predictors)

#Print the predictors sorted by Mean Decrease in Accuracy
print(rf_imp_class.sort)

#                      predictors MeanDecreaseAccuracy
#1            weekday_is_saturday            21.518318
#2                     kw_max_avg            20.702820
#3      self_reference_min_shares            20.478818
#4                     kw_min_avg            17.378091
#5                         LDA_02            17.332176
#6         data_channel_is_socmed            14.927426
#7                         LDA_04            14.161131
#8  data_channel_is_entertainment            13.196795
#9                         LDA_01            12.526222
#10                        LDA_00            12.232652
#11      n_non_stop_unique_tokens            12.006379
#12                        LDA_03            11.002887
#13                     num_hrefs            10.415695
#14             weekday_is_sunday            10.237964
#15                    kw_max_min             9.712763
#16                      num_imgs             9.694474
#17    global_rate_positive_words             9.270298
#18                    kw_avg_max             6.925046
#19           global_subjectivity             6.911155
#20                    kw_max_max             5.672202
#21          average_token_length             5.163225
#22    global_rate_negative_words             4.714351
#23      title_sentiment_polarity             4.599657
#24         avg_positive_polarity             4.330493
#25         avg_negative_polarity             3.765739
#26                n_tokens_title             3.008126

#The top predictors in terms of Mean Decrease in Accuracy include whether the article was published on Saturday, maximum
#shares of the average keyword, minimum shares of referenced articles in Mashable, minimum shares of the average keyword,
#closeness to LDA topic 2, whether the data channel is social media, closeness to LDA topic 4, whether the data channel 
#is entertainment, closeness to LDA topic 1, closeness to LDA topic 0, and so on.

#Order the predictors by importance according to Mean Decrease in Gini
rf_imp_gini.sort <- arrange(rf_imp_gini, desc(rf_imp_gini$MeanDecreaseGini))
rf_imp_gini.sort$predictors <- factor(rf_imp_gini.sort$predictors,levels=rf_imp_gini.sort$predictors)

#Print the predictors sorted by Mean Decrease in Gini
print(rf_imp_gini.sort)

#                      predictors MeanDecreaseGini
#1                     kw_max_avg        174.01898
#2                         LDA_02        156.80113
#3      self_reference_min_shares        155.88233
#4       n_non_stop_unique_tokens        145.02502
#5                         LDA_04        142.02218
#6                         LDA_00        140.88378
#7                     kw_max_min        137.69209
#8                     kw_avg_max        136.95709
#9                         LDA_01        136.59978
#10           global_subjectivity        135.10063
#11                        LDA_03        134.73511
#12    global_rate_positive_words        131.54903
#13          average_token_length        129.97954
#14         avg_positive_polarity        128.60078
#15    global_rate_negative_words        128.23493
#16         avg_negative_polarity        125.90124
#17                    kw_min_avg        123.40070
#18                     num_hrefs        116.62004
#19      title_sentiment_polarity         87.11326
#20                n_tokens_title         83.84260
#21                      num_imgs         79.16993
#22                    kw_max_max         39.92993
#23           weekday_is_saturday         31.44492
#24 data_channel_is_entertainment         23.36260
#25        data_channel_is_socmed         19.59069
#26             weekday_is_sunday         18.43724

#The top predictors in terms of Mean Decrease in Gini include maximum shares of the average keyword, closeness to LDA
#topic 2, minimum shares of referenced articles in Mashable, rate of unique non-stop words in the content, closeness to
#LDA topic 4, closeness to LDA topic 0, maximum shares of the worst keyword, average shares of the best keyword,
#closeness to LDA topic 1, text subjectivity, and so on.

#Plot Important Variables
varImpPlot(rf_model, type=1, main = "Variable Importance (Mean Decrease in Accuracy)")
varImpPlot(rf_model, type=2, main = "Variable Importance (Mean Decrease in Gini")
varImpPlot(rf_model, main = "Variable Importance")
#install.packages("vip")
library(vip)
#Variable Importance (Mean Decrease in Accuracy)
vip(rf_model, type=1, num_features = 26L)
#Variable Importance (Mean Decrease in Gini)
vip(rf_model, type=2, num_features = 26L)


#Using 10-fold cross-validation
#Create a random forest model with parameter mtry = 2
set.seed(123)
rf_model2 <- randomForest(shares_cat ~., data = train, mtry = 2, importance = TRUE)

#Print the model
print(rf_model2)

#Number of trees: 500
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 33.84%
#Confusion matrix:
#      0     1 class.error
#0 12210  5863   0.3244066
#1  6209 11397   0.3526639

#Plot the results
plot(rf_model2)
#The errors from the model decrease until around 200 trees and then flatten.

#Test the model
rf_predict2 <- predict(rf_model2, test)

#Confusion Matrix and Statistics
rf_confusionMatrix_stats2 <- confusionMatrix(rf_predict2, test$shares_cat, mode = "everything", positive = "1")
print(rf_confusionMatrix_stats2)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1394  712
#         1  615 1244

#Accuracy : 0.6653        
#95% CI : (0.6504, 0.68)
#No Information Rate : 0.5067        
#P-Value [Acc > NIR] : < 2.2e-16     

#Kappa : 0.3301        

#Mcnemar's Test P-Value : 0.008405
       
#            Sensitivity : 0.6360        
#            Specificity : 0.6939        
#         Pos Pred Value : 0.6692        
#         Neg Pred Value : 0.6619        
#              Precision : 0.6692        
#                 Recall : 0.6360        
#                     F1 : 0.6522        
#             Prevalence : 0.4933        
#         Detection Rate : 0.3137        
#   Detection Prevalence : 0.4689        
#      Balanced Accuracy : 0.6649

#AUC Score
rfauc2 <- roc(test$shares_cat, as.numeric(levels(rf_predict2))[as.integer(rf_predict2)])
print(rfauc2)

#Area under the curve: 0.6649

#Plot AUC
plot(rfauc2)

#Plot ROC curve
rfroc_pred2 <- prediction(as.numeric(levels(rf_predict2))[as.integer(rf_predict2)], test$shares_cat)
rf_performance2 <- performance(rfroc_pred2, "tpr", "fpr")
plot(rf_performance2, colorize=TRUE)

#Check the important variables
importance(rf_model2)

#                                      0         1 MeanDecreaseAccuracy MeanDecreaseGini
#kw_max_avg                    29.371215 34.970232            51.190404        1048.0162
#self_reference_min_shares     33.307007 34.013098            46.813368         917.2052
#LDA_00                        22.184025 21.376196            35.448471         806.8836
#kw_min_avg                    38.957265  1.066518            39.472782         710.9007
#LDA_02                        37.135689  6.597171            43.750978         930.2219
#weekday_is_saturday           38.198417 20.948111            37.403561         148.8817
#LDA_04                        19.936763 20.492122            36.847525         833.7366
#data_channel_is_socmed        19.662243 32.743766            35.476738         130.1875
#data_channel_is_entertainment 29.736828 10.271871            34.176360         169.3873
#LDA_01                        24.424007 11.732300            32.296765         812.4145
#LDA_03                        21.912461  7.127194            29.180045         778.7597
#n_non_stop_unique_tokens      26.616667  6.499903            27.847808         805.7213
#num_hrefs                     18.582652 13.607931            26.458185         644.3131
#kw_max_max                    17.516106 15.750448            26.899394         237.4172
#num_imgs                      29.580147  5.876901            30.205263         451.3926
#kw_avg_max                    27.298916  2.245783            29.179395         806.7945
#weekday_is_sunday             24.716104 15.189719            25.542122         114.3049
#average_token_length          23.556573  1.884194            22.760132         763.7803
#global_rate_positive_words    11.139501 14.678262            21.486219         775.4221
#global_subjectivity           10.654194 18.558555            23.593209         798.0035
#kw_max_min                    13.705562 11.878612            18.255461         764.1800
#avg_positive_polarity          3.789470 12.216383            13.816956         742.6808
#global_rate_negative_words    10.396472  8.772201            15.219374         734.1997
#avg_negative_polarity          7.652149  8.834470            14.198962         715.4689
#n_tokens_title                 3.952239  4.255027             5.866347         480.0319
#title_sentiment_polarity      10.220610  3.092586             9.694414         505.5709

#Evaluate variable importance according to mean decrease in accuracy and mean decrease in gini
#Mean Decrease in Accuracy
rf_imp_class2 = importance(rf_model2, type=1)
rf_imp_class2 <- data.frame(predictors=rownames(rf_imp_class2),rf_imp_class2)

#Mean Decrease Gini
rf_imp_gini2 = importance(rf_model2, type=2)
rf_imp_gini2 <- data.frame(predictors=rownames(rf_imp_gini2),rf_imp_gini2)

#Order the predictors by importance according to Mean Decrease in Accuracy
rf_imp_class2.sort <- arrange(rf_imp_class2, desc(MeanDecreaseAccuracy))
rf_imp_class2.sort$predictors <- factor(rf_imp_class2.sort$predictors,levels=rf_imp_class2.sort$predictors)

#Print the predictors sorted by Mean Decrease in Accuracy
print(rf_imp_class2.sort)

#                      predictors MeanDecreaseAccuracy
#1                     kw_max_avg            51.190404
#2      self_reference_min_shares            46.813368
#3                         LDA_02            43.750978
#4                     kw_min_avg            39.472782
#5            weekday_is_saturday            37.403561
#6                         LDA_04            36.847525
#7         data_channel_is_socmed            35.476738
#8                         LDA_00            35.448471
#9  data_channel_is_entertainment            34.176360
#10                        LDA_01            32.296765
#11                      num_imgs            30.205263
#12                        LDA_03            29.180045
#13                    kw_avg_max            29.179395
#14      n_non_stop_unique_tokens            27.847808
#15                    kw_max_max            26.899394
#16                     num_hrefs            26.458185
#17             weekday_is_sunday            25.542122
#18           global_subjectivity            23.593209
#19          average_token_length            22.760132
#20    global_rate_positive_words            21.486219
#21                    kw_max_min            18.255461
#22    global_rate_negative_words            15.219374
#23         avg_negative_polarity            14.198962
#24         avg_positive_polarity            13.816956
#25      title_sentiment_polarity             9.694414
#26                n_tokens_title             5.866347

#The top predictors in terms of Mean Decrease in Accuracy include maximum shares of the average keyword, minimum shares
#of referenced articles in Mashable, closeness to LDA topic 2, minimum shares of the average keyword, whether the article
#was posted on Saturday, closeness to LDA topic 4, whether the data channel is social media, closeness to LDA topic 0,
#whether the data channel is entertainment, closeness to LDA topic 1, and so on.

#Order the predictors by importance according to Mean Decrease in Gini
rf_imp_gini2.sort <- arrange(rf_imp_gini2, desc(rf_imp_gini2$MeanDecreaseGini))
rf_imp_gini2.sort$predictors <- factor(rf_imp_gini2.sort$predictors,levels=rf_imp_gini2.sort$predictors)

#Print the predictors sorted by Mean Decrease in Gini
print(rf_imp_gini2.sort)

#                      predictors MeanDecreaseGini
#1                     kw_max_avg        1048.0162
#2                         LDA_02         930.2219
#3      self_reference_min_shares         917.2052
#4                         LDA_04         833.7366
#5                         LDA_01         812.4145
#6                         LDA_00         806.8836
#7                     kw_avg_max         806.7945
#8       n_non_stop_unique_tokens         805.7213
#9            global_subjectivity         798.0035
#10                        LDA_03         778.7597
#11    global_rate_positive_words         775.4221
#12                    kw_max_min         764.1800
#13          average_token_length         763.7803
#14         avg_positive_polarity         742.6808
#15    global_rate_negative_words         734.1997
#16         avg_negative_polarity         715.4689
#17                    kw_min_avg         710.9007
#18                     num_hrefs         644.3131
#19      title_sentiment_polarity         505.5709
#20                n_tokens_title         480.0319
#21                      num_imgs         451.3926
#22                    kw_max_max         237.4172
#23 data_channel_is_entertainment         169.3873
#24           weekday_is_saturday         148.8817
#25        data_channel_is_socmed         130.1875
#26             weekday_is_sunday         114.3049

#The top predictors in terms of Mean Decrease in Gini include maximum shares of the average keyword, closeness to LDA
#topic 2, minimum shares of referenced articles in Mashable, closeness to LDA topic 4,  closeness to LDA topic 1, 
#closeness to LDA topic 0, average shares of the best keyword, rate of unique non-stop words in the content, text 
#subjectivity, closeness to LDA topic 3, and so on.

#Comparing important variables found using mean decrease in accuracy with mean decrease in gini suggests the most
#important variables are maximum shares of the average keyword, minimum shares of referenced articles in Mashable, 
#closeness to LDA topic 2, closeness to LDA topic 4, closeness to LDA topic 0, and closeness to LDA topic 1.

#Plot Important Variables
varImpPlot(rf_model2, type=1, main = "Variable Importance (Mean Decrease in Accuracy)")
varImpPlot(rf_model2, type=2, main = "Variable Importance (Mean Decrease in Gini)")
varImpPlot(rf_model2, main = "Variable Importance")
#Variable Importance (Mean Decrease in Accuracy)
vip(rf_model2, type=1, num_features = 26L)
#Variable Importance (Mean Decrease in Gini)
vip(rf_model2, type=2, num_features = 26L)


#Gradient Boosting Machine
#install.packages("gbm")
library(gbm)

#Train the model with different tuning parameters
set.seed(123)
gbmfit <- train(shares_cat ~., data = train.set, method = "gbm", tuneLength = 5, 
                trControl = trainControl(method = "cv", search = "grid"), verbose = FALSE, distribution = "bernoulli")

#Print the training model to see the accuracy
print(gbmfit)

#interaction.depth  n.trees  Accuracy   Kappa    
#1                   50      0.6430993  0.2857176
#1                  100      0.6496583  0.2988222
#1                  150      0.6515318  0.3026774
#1                  200      0.6538017  0.3072533
#1                  250      0.6560001  0.3116892
#2                   50      0.6504868  0.3006533
#2                  100      0.6546308  0.3089990
#2                  150      0.6580904  0.3159702
#2                  200      0.6587030  0.3171902
#2                  250      0.6596037  0.3189818
#3                   50      0.6538740  0.3075327
#3                  100      0.6590271  0.3178898
#3                  150      0.6593154  0.3184361
#3                  200      0.6603244  0.3204782
#3                  250      0.6593874  0.3185842
#4                   50      0.6561444  0.3120997
#4                  100      0.6608653  0.3215290
#4                  150      0.6602887  0.3203715
#4                  200      0.6613697  0.3225514
#4                  250      0.6617299  0.3232823
#5                   50      0.6575858  0.3150339
#5                  100      0.6610454  0.3219121
#5                  150      0.6620181  0.3238679
#5                  200      0.6618379  0.3234836
#5                  250      0.6618741  0.3235657

#Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning parameter 'n.minobsinnode' was held constant at
#a value of 10
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were n.trees = 150, interaction.depth = 5, shrinkage = 0.1 and n.minobsinnode = 10.

#Plot the results
plot(gbmfit)

#Important variables that have the largest influence on popularity
summary(gbmfit)
vip(gbmfit, num_features = 26L)

#                                                          var    rel.inf
#kw_max_avg                                         kw_max_avg 12.6571144
#self_reference_min_shares           self_reference_min_shares 11.8560679
#kw_min_avg                                         kw_min_avg  7.5172750
#LDA_02                                                 LDA_02  7.3604733
#data_channel_is_entertainment1 data_channel_is_entertainment1  6.3975418
#weekday_is_saturday1                     weekday_is_saturday1  5.2460129
#LDA_04                                                 LDA_04  4.6223684
#n_non_stop_unique_tokens             n_non_stop_unique_tokens  4.5386342
#kw_max_max                                         kw_max_max  3.6010261
#weekday_is_sunday1                         weekday_is_sunday1  3.5225634
#data_channel_is_socmed1               data_channel_is_socmed1  3.1720619
#num_hrefs                                           num_hrefs  3.0009931
#LDA_01                                                 LDA_01  2.8673100
#LDA_00                                                 LDA_00  2.8246000
#kw_avg_max                                         kw_avg_max  2.8200655
#global_subjectivity                       global_subjectivity  2.5722805
#kw_max_min                                         kw_max_min  2.4256271
#global_rate_positive_words         global_rate_positive_words  2.2880355
#num_imgs                                             num_imgs  2.1627667
#LDA_03                                                 LDA_03  2.1558032
#title_sentiment_polarity             title_sentiment_polarity  1.7554967
#avg_positive_polarity                   avg_positive_polarity  1.1621387
#global_rate_negative_words         global_rate_negative_words  1.1050324
#avg_negative_polarity                   avg_negative_polarity  1.0281309
#average_token_length                     average_token_length  0.9962160
#n_tokens_title                                 n_tokens_title  0.3443644

#The most important variables found are maximum shares of the average keyword, minimum shares of referenced articles in
#Mashable, minimum shares of the average keyword, closeness to LDA topic 2, whether the data channel is entertainment,
#whether the article was published on Saturday, closeness to LDA topic 4, rate of unique non-stop words in the content,
#maximum shares of the best keyword, whether the article was published on Sunday, and so on.

#Since the response only has 2 unique values (0,1), a bernoulli distribution should be specified.

#Change dependent variable to numeric instead of factor to get (0,1) observations in order to use the bernoulli 
#distribution in the gbm() function.
train2 <- train
test2 <- test
train.set2 <- train.set
valid.set2 <- valid.set
test.set2 <- test.set

train2$shares_cat <- as.numeric(levels(train2$shares_cat))[as.integer(train2$shares_cat)]
train2$weekday_is_saturday <- as.numeric(levels(train2$weekday_is_saturday))[as.integer(train2$weekday_is_saturday)]
train2$weekday_is_sunday <- as.numeric(levels(train2$weekday_is_sunday))[as.integer(train2$weekday_is_sunday)]
train2$data_channel_is_socmed <- as.numeric(levels(train2$data_channel_is_socmed))[as.integer(train2$data_channel_is_socmed)]
train2$data_channel_is_entertainment <- as.numeric(levels(train2$data_channel_is_entertainment))[as.integer(train2$data_channel_is_entertainment)]

test2$shares_cat <- as.numeric(levels(test2$shares_cat))[as.integer(test2$shares_cat)]
test2$weekday_is_saturday <- as.numeric(levels(test2$weekday_is_saturday))[as.integer(test2$weekday_is_saturday)]
test2$weekday_is_sunday <- as.numeric(levels(test2$weekday_is_sunday))[as.integer(test2$weekday_is_sunday)]
test2$data_channel_is_socmed <- as.numeric(levels(test2$data_channel_is_socmed))[as.integer(test2$data_channel_is_socmed)]
test2$data_channel_is_entertainment <- as.numeric(levels(test2$data_channel_is_entertainment))[as.integer(test2$data_channel_is_entertainment)]

train.set2$shares_cat <- as.numeric(levels(train.set2$shares_cat))[as.integer(train.set2$shares_cat)]
train.set2$weekday_is_saturday <- as.numeric(levels(train.set2$weekday_is_saturday))[as.integer(train.set2$weekday_is_saturday)]
train.set2$weekday_is_sunday <- as.numeric(levels(train.set2$weekday_is_sunday))[as.integer(train.set2$weekday_is_sunday)]
train.set2$data_channel_is_socmed <- as.numeric(levels(train.set2$data_channel_is_socmed))[as.integer(train.set2$data_channel_is_socmed)]
train.set2$data_channel_is_entertainment <- as.numeric(levels(train.set2$data_channel_is_entertainment))[as.integer(train.set2$data_channel_is_entertainment)]

test.set2$shares_cat <- as.numeric(levels(test.set2$shares_cat))[as.integer(test.set2$shares_cat)]
test.set2$weekday_is_saturday <- as.numeric(levels(test.set2$weekday_is_saturday))[as.integer(test.set2$weekday_is_saturday)]
test.set2$weekday_is_sunday <- as.numeric(levels(test.set2$weekday_is_sunday))[as.integer(test.set2$weekday_is_sunday)]
test.set2$data_channel_is_socmed <- as.numeric(levels(test.set2$data_channel_is_socmed))[as.integer(test.set2$data_channel_is_socmed)]
test.set2$data_channel_is_entertainment <- as.numeric(levels(test.set2$data_channel_is_entertainment))[as.integer(test.set2$data_channel_is_entertainment)]

valid.set2$shares_cat <- as.numeric(levels(valid.set2$shares_cat))[as.integer(valid.set2$shares_cat)]
valid.set2$weekday_is_saturday <- as.numeric(levels(valid.set2$weekday_is_saturday))[as.integer(valid.set2$weekday_is_saturday)]
valid.set2$weekday_is_sunday <- as.numeric(levels(valid.set2$weekday_is_sunday))[as.integer(valid.set2$weekday_is_sunday)]
valid.set2$data_channel_is_socmed <- as.numeric(levels(valid.set2$data_channel_is_socmed))[as.integer(valid.set2$data_channel_is_socmed)]
valid.set2$data_channel_is_entertainment <- as.numeric(levels(valid.set2$data_channel_is_entertainment))[as.integer(valid.set2$data_channel_is_entertainment)]

#Create a GBM model with the validation set using the best parameters
#The final values found for the model were n.trees = 150, interaction.depth = 5, shrinkage = 0.1, and n.minobsinnode = 10
#where n.trees is the number of trees,
#interaction.depth is is the maximum depth of each tree,
#shrinkage is the learning rate applied to each tree in the expansion,
#and n.minobsinnode is the number of observations in the terminal nodes of the trees.
set.seed(123)
gbm_model <- gbm(shares_cat ~., data = valid.set2, n.trees = 150, interaction.depth = 5, shrinkage = 0.1, 
                 n.minobsinnode = 10, distribution = "bernoulli")

#Print the model
print(gbm_model)

#A gradient boosted model with bernoulli loss function.
#150 iterations were performed.
#There were 26 predictors of which 26 had non-zero influence.

#Plot the results (marginal plot of fitted GBM objects)
plot(gbm_model)
#The marginal plot shows the marginal effect between the maximum shares of the average keyword and the dependent
#variable, popularity of an article.
#This plot suggests there is a postive effect at the bottom 20% of the distribution of maximum shares of the average 
#keyword.

#Important variables
summary(gbm_model)
vip(gbm_model, num_features = 26L)

#                                                        var    rel.inf
#kw_max_avg                                       kw_max_avg 10.3582377
#self_reference_min_shares         self_reference_min_shares  8.6723327
#n_non_stop_unique_tokens           n_non_stop_unique_tokens  7.3430337
#LDA_04                                               LDA_04  6.0129047
#kw_min_avg                                       kw_min_avg  5.5262958
#LDA_02                                               LDA_02  5.1670884
#LDA_00                                               LDA_00  4.4950009
#kw_max_min                                       kw_max_min  4.4593690
#LDA_01                                               LDA_01  4.0357731
#weekday_is_saturday                     weekday_is_saturday  3.9389358
#global_rate_negative_words       global_rate_negative_words  3.6805618
#average_token_length                   average_token_length  3.5220553
#global_subjectivity                     global_subjectivity  3.5032973
#kw_avg_max                                       kw_avg_max  3.4919890
#num_hrefs                                         num_hrefs  3.1470426
#LDA_03                                               LDA_03  3.1283730
#avg_positive_polarity                 avg_positive_polarity  2.9307724
#avg_negative_polarity                 avg_negative_polarity  2.6840701
#title_sentiment_polarity           title_sentiment_polarity  2.5734181
#kw_max_max                                       kw_max_max  2.2448493
#global_rate_positive_words       global_rate_positive_words  2.2418899
#weekday_is_sunday                         weekday_is_sunday  2.0075215
#num_imgs                                           num_imgs  1.5013011
#data_channel_is_socmed               data_channel_is_socmed  1.2729848
#data_channel_is_entertainment data_channel_is_entertainment  1.2144479
#n_tokens_title                               n_tokens_title  0.8464541

#The most important variables found are maximum shares of the average keyword, minimum shares of referenced articles in
#Mashable, rate of unique non-stop words in the content, closeness to LDA topic 4, minimum shares of the average keyword, 
#closeness to LDA topic 2, closeness to LDA topic 0, maximum shares of the worst keyword, closeness to LDA topic 1, 
#whether the article was published on Saturday, and so on.

#Make predictions using the test data
gbm_predict <- predict(object = gbm_model, newdata = test.set2, n.trees = 150, type = "response")

#Create binary observations from the predictions
#Use the ifelse function to predict if an article is popular with a percentage/probability greater than 50%
gbm_predict_binary <- as.factor(ifelse(gbm_predict > 0.5, 1, 0))
test.set2$shares_cat <- as.factor(test.set2$shares_cat)

#Confusion Matrix and Statistics
gbm_confusionMatrix_stats <- confusionMatrix(gbm_predict_binary, test.set2$shares_cat, positive = "1", 
                                             mode = "everything")
print(gbm_confusionMatrix_stats)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1968 1119
#         1 1021 1839

#Accuracy : 0.6402          
#95% CI : (0.6278, 0.6524)
#No Information Rate : 0.5026          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.2802          

#Mcnemar's Test P-Value : 0.03601
        
#            Sensitivity : 0.6217          
#            Specificity : 0.6584          
#         Pos Pred Value : 0.6430          
#         Neg Pred Value : 0.6375          
#              Precision : 0.6430          
#                 Recall : 0.6217          
#                     F1 : 0.6322          
#             Prevalence : 0.4974          
#         Detection Rate : 0.3092          
#   Detection Prevalence : 0.4809          
#      Balanced Accuracy : 0.6401

#AUC Score
gbmauc <- roc(test.set2$shares_cat, gbm_predict)
print(gbmauc)

#Area under the curve: 0.6937

#Plot AUC
plot(gbmauc)

#Plot ROC curve
gbmroc_pred <- prediction(gbm_predict, test.set2$shares_cat)
gbm_performance <- performance(gbmroc_pred, "tpr", "fpr")
plot(gbm_performance, colorize=TRUE)


#Using 10-fold cross-validation
#Define the GBM model and use the training data to fit the model
set.seed(123)
gbm_model2 <- gbm(shares_cat ~., data = train2, n.trees = 150, interaction.depth = 5, shrinkage = 0.1, 
                  n.minobsinnode = 10, distribution = "bernoulli")

#Print the model
print(gbm_model2)

#A gradient boosted model with bernoulli loss function.
#150 iterations were performed.
#There were 26 predictors of which 26 had non-zero influence.

#Plot the results (marginal plot of fitted GBM objects)
plot(gbm_model2)
#The marginal plot shows the marginal effect between the maximum shares of the average keyword and the dependent
#variable, popularity of an article.
#This plot suggests there is a postive effect at the bottom 20% of the distribution of maximum shares of the average 
#keyword.

#Important variables
summary(gbm_model2)
vip(gbm_model2, num_features = 26L)

#                                                        var    rel.inf
#kw_max_avg                                       kw_max_avg 14.2237351
#self_reference_min_shares         self_reference_min_shares 12.8510400
#kw_min_avg                                       kw_min_avg  7.5303339
#LDA_02                                               LDA_02  6.7311093
#data_channel_is_entertainment data_channel_is_entertainment  5.3914263
#weekday_is_saturday                     weekday_is_saturday  5.3243059
#LDA_04                                               LDA_04  5.1569159
#n_non_stop_unique_tokens           n_non_stop_unique_tokens  4.7221051
#kw_max_max                                       kw_max_max  3.6840664
#weekday_is_sunday                         weekday_is_sunday  3.4819010
#data_channel_is_socmed               data_channel_is_socmed  3.2867379
#num_hrefs                                         num_hrefs  2.9878556
#LDA_00                                               LDA_00  2.7808518
#num_imgs                                           num_imgs  2.4352571
#global_subjectivity                     global_subjectivity  2.3983491
#kw_avg_max                                       kw_avg_max  2.3016619
#LDA_01                                               LDA_01  2.2997932
#kw_max_min                                       kw_max_min  2.2293580
#LDA_03                                               LDA_03  1.7798341
#global_rate_negative_words       global_rate_negative_words  1.7362134
#title_sentiment_polarity           title_sentiment_polarity  1.4552712
#global_rate_positive_words       global_rate_positive_words  1.4354622
#average_token_length                   average_token_length  1.3087644
#avg_negative_polarity                 avg_negative_polarity  1.0602890
#avg_positive_polarity                 avg_positive_polarity  1.0575778
#n_tokens_title                               n_tokens_title  0.3497847

#The most important variables found are maximum shares of the average keyword, minimum shares of referenced articles in
#Mashable, minimum shares of the average keyword, closeness to LDA topic 2, whether the data channel is entertainment,
#whether the article was published on Saturday, closeness to LDA topic 4, rate of unique non-stop words in the content,   
#maximum shares of the best keyword, whether the article was published on Sunday, and so on.

#The important variables obtained based on the Gradient Boosting Machine model may be compared with the ones found in 
#the Random Forest model (using both mean decrease in accuracy and mean decrese in gini).
#Across all models, maximum shares of the average keyword is found to be the most important variable.
#This is followed by minimum shares of referenced articles in Mashable, and closeness to LDA topic 2.

#Recall the important variables in the Random Forest model for comparison:
vip(rf_model2, type=1, num_features = 26L)
vip(rf_model2, type=2, num_features = 26L)

#Importance of predictors based on Random Forest sorted by Mean Decrease in Accuracy
print(rf_imp_class2.sort)

#                      predictors MeanDecreaseAccuracy
#1                     kw_max_avg            51.190404
#2      self_reference_min_shares            46.813368
#3                         LDA_02            43.750978
#4                     kw_min_avg            39.472782
#5            weekday_is_saturday            37.403561
#6                         LDA_04            36.847525
#7         data_channel_is_socmed            35.476738
#8                         LDA_00            35.448471
#9  data_channel_is_entertainment            34.176360
#10                        LDA_01            32.296765
#11                      num_imgs            30.205263
#12                        LDA_03            29.180045
#13                    kw_avg_max            29.179395
#14      n_non_stop_unique_tokens            27.847808
#15                    kw_max_max            26.899394
#16                     num_hrefs            26.458185
#17             weekday_is_sunday            25.542122
#18           global_subjectivity            23.593209
#19          average_token_length            22.760132
#20    global_rate_positive_words            21.486219
#21                    kw_max_min            18.255461
#22    global_rate_negative_words            15.219374
#23         avg_negative_polarity            14.198962
#24         avg_positive_polarity            13.816956
#25      title_sentiment_polarity             9.694414
#26                n_tokens_title             5.866347

#Importance of predictors based on Random Forest sorted by Mean Decrease in Gini
print(rf_imp_gini2.sort)

#                      predictors MeanDecreaseGini
#1                     kw_max_avg        1048.0162
#2                         LDA_02         930.2219
#3      self_reference_min_shares         917.2052
#4                         LDA_04         833.7366
#5                         LDA_01         812.4145
#6                         LDA_00         806.8836
#7                     kw_avg_max         806.7945
#8       n_non_stop_unique_tokens         805.7213
#9            global_subjectivity         798.0035
#10                        LDA_03         778.7597
#11    global_rate_positive_words         775.4221
#12                    kw_max_min         764.1800
#13          average_token_length         763.7803
#14         avg_positive_polarity         742.6808
#15    global_rate_negative_words         734.1997
#16         avg_negative_polarity         715.4689
#17                    kw_min_avg         710.9007
#18                     num_hrefs         644.3131
#19      title_sentiment_polarity         505.5709
#20                n_tokens_title         480.0319
#21                      num_imgs         451.3926
#22                    kw_max_max         237.4172
#23 data_channel_is_entertainment         169.3873
#24           weekday_is_saturday         148.8817
#25        data_channel_is_socmed         130.1875
#26             weekday_is_sunday         114.3049

#Make predictions using the test data
gbm_predict2 <- predict.gbm(gbm_model2, newdata = test2, type = "response", n.trees = 150)

#Create binary variables from predictions
gbm_predict_binary2 <- as.factor(ifelse(gbm_predict2 > 0.5, 1, 0))
test2$shares_cat <- as.factor(test2$shares_cat)

#Confusion Matrix and Stats
gbm_ConfusionMatrix_stats2 <- confusionMatrix(gbm_predict_binary2, test2$shares_cat, positive = "1", 
                                              mode = "everything")
print(gbm_ConfusionMatrix_stats2)

#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1377  704
#         1  632 1252

#Accuracy : 0.6631          
#95% CI : (0.6481, 0.6778)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.3257          

#Mcnemar's Test P-Value : 0.05208 
         
#            Sensitivity : 0.6401          
#            Specificity : 0.6854          
#         Pos Pred Value : 0.6645          
#         Neg Pred Value : 0.6617          
#              Precision : 0.6645          
#                 Recall : 0.6401          
#                     F1 : 0.6521          
#             Prevalence : 0.4933          
#         Detection Rate : 0.3158          
#   Detection Prevalence : 0.4752          
#      Balanced Accuracy : 0.6627 

#AUC Score
gbmauc2 <- roc(test2$shares_cat, gbm_predict2)
print(gbmauc2)

#Area under the curve: 0.7203

#Plot AUC
plot(gbmauc2)

#Plot ROC curve
gbmroc_pred2 <- prediction(gbm_predict2, test2$shares_cat)
gbm_performance2 <- performance(gbmroc_pred2, "tpr", "fpr")
plot(gbm_performance2, colorize=TRUE)


#The accuracy of all the models tested may be compared to find the best model overall to solve the problem of predicting
#Online News Popularity based on pre-publication features.
#Overall, Random Forest performs the best with Accuracy = 66.53%, Specificity = 69.39%, Precision = 66.92%, and 
#F1 score = 65.22%
#Gradient Boosting Machine closely follows Random Forest and has a close F1 score to Random Forest with F1 = 65.21%, 
#and the best Area Under the curve = 72.03%
#Finally, the Decision Tree model has the best Sensitivity/Recall = 65.29%

#Remember the accuracy found for the models using 10-fold cross-validation:

#Logistic Regression
#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1334  742
#         1 675 1214

#Accuracy : 0.6426          
#95% CI : (0.6275, 0.6576)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.2848          

#Mcnemar's Test P-Value : 0.07955         

#            Sensitivity : 0.6207          
#            Specificity : 0.6640          
#         Pos Pred Value : 0.6427          
#         Neg Pred Value : 0.6426          
#              Precision : 0.6427          
#                 Recall : 0.6207          
#                     F1 : 0.6315          
#             Prevalence : 0.4933          
#         Detection Rate : 0.3062          
#   Detection Prevalence : 0.4764          
#      Balanced Accuracy : 0.6423

#Area under the curve: 0.6883


#K-Nearest Neighbours
#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1335  813
#         1  674 1143

#Accuracy : 0.625           
#95% CI : (0.6097, 0.6401)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.2491          

#Mcnemar's Test P-Value : 0.0003453 

#            Sensitivity : 0.5844          
#            Specificity : 0.6645          
#         Pos Pred Value : 0.6291          
#         Neg Pred Value : 0.6215          
#              Precision : 0.6291          
#                 Recall : 0.5844          
#                     F1 : 0.6059          
#             Prevalence : 0.4933          
#         Detection Rate : 0.2883          
#   Detection Prevalence : 0.4583          
#      Balanced Accuracy : 0.6244

#Area under the curve: 0.6244


#Decision Trees
#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1212  679
#         1  797 1277

#Accuracy : 0.6277          
#95% CI : (0.6125, 0.6428)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2.2e-16       

#Kappa : 0.2559          

#Mcnemar's Test P-Value : 0.002324         

#            Sensitivity : 0.6529          
#            Specificity : 0.6033          
#         Pos Pred Value : 0.6157          
#         Neg Pred Value : 0.6409          
#              Precision : 0.6157          
#                 Recall : 0.6529          
#                     F1 : 0.6337          
#             Prevalence : 0.4933          
#         Detection Rate : 0.3221          
#   Detection Prevalence : 0.5231          
#      Balanced Accuracy : 0.6281

#Area under the curve: 0.6281


#Random Forest
#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1394  712
#         1  615 1244

#Accuracy : 0.6653        
#95% CI : (0.6504, 0.68)
#No Information Rate : 0.5067        
#P-Value [Acc > NIR] : < 2.2e-16     

#Kappa : 0.3301        

#Mcnemar's Test P-Value : 0.008405

#            Sensitivity : 0.6360        
#            Specificity : 0.6939        
#         Pos Pred Value : 0.6692        
#         Neg Pred Value : 0.6619        
#              Precision : 0.6692        
#                 Recall : 0.6360        
#                     F1 : 0.6522        
#             Prevalence : 0.4933        
#         Detection Rate : 0.3137        
#   Detection Prevalence : 0.4689        
#      Balanced Accuracy : 0.6649

#Area under the curve: 0.6649


#Gradient Boosting Machine
#Confusion Matrix and Statistics

#          Reference
#Prediction    0    1
#         0 1377  704
#         1  632 1252

#Accuracy : 0.6631          
#95% CI : (0.6481, 0.6778)
#No Information Rate : 0.5067          
#P-Value [Acc > NIR] : < 2e-16         

#Kappa : 0.3257          

#Mcnemar's Test P-Value : 0.05208 

#            Sensitivity : 0.6401          
#            Specificity : 0.6854          
#         Pos Pred Value : 0.6645          
#         Neg Pred Value : 0.6617          
#              Precision : 0.6645          
#                 Recall : 0.6401          
#                     F1 : 0.6521          
#             Prevalence : 0.4933          
#         Detection Rate : 0.3158          
#   Detection Prevalence : 0.4752          
#      Balanced Accuracy : 0.6627 

#Area under the curve: 0.7203


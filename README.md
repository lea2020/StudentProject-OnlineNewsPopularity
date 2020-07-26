# StudentProject-PredictingOnlineNewsPopularity

## Predicting Online News Popularity based on Pre-publication Features

Due to the large number of news articles published online, predicting the popularity of online news is an important problem to determine features that may enable news publishers to stand out from the competition and reach a large audience. This problem will be examined through the theme of classification based on features known before publication. The main research questions will be what are the significant variables that help determine if an article will be popular; and how to estimate and enhance the popularity of an article, ultimately improving on its delivery. The data being used to explore these questions is the “Online News Popularity Data Set” from the UCI Machine Learning Repository (Fernandes et al., 2015). This dataset includes features based on 2 years of statistics for over 39,000 articles collected from the Mashable news service - 58 predictor variables, such as keywords, number of links, and shares of referenced articles, and 1 output variable, the number of article shares (popularity). For classification, the output may be a binary variable, where articles are labelled as popular for shares greater than a threshold, and unpopular otherwise. Feature selection may be explored to improve prediction accuracy; and classification models will be tested for prediction, including Logistic Regression, K-Nearest Neighbours, Decision Trees, Random Forest, and Gradient Boosting Machine. Finally, to solve these problems, R is utilized for its extensive machine learning libraries.


## Approach

### Step 1: Data Processing

The “Online News Popularity Data Set” (Fernandes et al., 2015) is retrieved from the UCI Machine Learning Repository, and data processing procedures are implemented. At the start of the univariate analysis, the target variable is determined to be the number of shares; and articles are classified as popular or unpopular based on the number of shares. Various graphs are plotted such as boxplots and histograms to detect outliers, illustrate whether the data is normally distributed, and whether the data is balanced. To determine whether there are any variables with low variance, a near-zero variance filter is used to filter out the attributes with near-zero variance and help to reduce the dimensionality of the dataset. In bivariate analysis, pairwise relations are examined between the input variables as well as the output and input variables. Scatterplots provide visualisations to better understand the data; and correlation analysis is done to help reduce the dimensionality of the dataset when there is high correlation among the independent variables.

#### Source code: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_01-DataProcessing.Rmd

#### Dataset: https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity

#### Data: https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip


### Step 2: Exploratory Analysis

Normalization is applied to the dataset to scale the values of the features. Next, the data may be explored by checking subsequences using association rules to find any patterns that may explain what makes an article popular, and learn its characteristics, without making predictions yet.

#### Source code: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_02-ExploratoryAnalysis.Rmd


### Step 3: Dimensionality Reduction

The Random Forest feature selection method, which uses the Gini index to assign a score and rank features, may be used to help select the features to be used in the analysis.

#### Source code: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_03-DimensionalityReduction.Rmd


### Step 4: Experimental Design

In this step, the processed data may be split into 70% training, 15% validation, and 15% test sets in order to tune the parameters of the models that will be studied; as well as split into training and test sets using 10-fold cross-validation to improve the performance of the models, and these results may be used to compare each model.

#### Source code: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_04-ExperimentalDesign.Rmd


### Step 5: Prediction

Classification models - Logistic Regression, K-Nearest Neighbours, Decision Trees, Random Forest, and Gradient Boosting Machine - are tested to find the algorithm with the best accuracy. 

#### Source code:

Logistic Regression: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-01-LogisticRegression.Rmd

K-Nearest Neighbours: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-02-K-NearestNeighbours.Rmd

Decision Trees: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-03-DecisionTrees.Rmd

Random Forest: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-04-RandomForest.Rmd

Gradient Boosting Machines: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-05-GradientBoostingMachine.Rmd


### Step 6: Performance Evaluation

At this stage, the machine learning algorithms that have been run are evaluated using various methods. Several metrics are computed including Accuracy, Precision, Recall/Sensitivity, Specificity, and F1 score, depending on the Confusion Matrix.

#### Source code (see code after Prediction for Confusion Matrix and Statistics):

Logistic Regression: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-01-LogisticRegression.Rmd

K-Nearest Neighbours: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-02-K-NearestNeighbours.Rmd

Decision Trees: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-03-DecisionTrees.Rmd

Random Forest: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-04-RandomForest.Rmd

Gradient Boosting Machines: https://github.com/lea2020/StudentProject-PredictingOnlineNewsPopularity/blob/master/FinalResults_05-Prediction_Evaluation-05-GradientBoostingMachine.Rmd

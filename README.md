# Predicting Houly Wage of the National Longitudinal Survey of Youth 1997 Dataset

Author: John Carlo Maula

## Introduction


### Background

The **National Longitudinal Survey of Youth in 1997 (NLSY97)** is a cohort study of men and women born during the years 1980 - 1984 and living in the United States. Interviews were conducted annually from 1997 to 2011. The dataset contains information about the participants' family background, work, education, etc. This project will focus on predicting the hourly wage of the respondents based on these features. 

For more information about the dataset, you can visit the U.S. Bureau of Labor Statistics website [here](https://www.bls.gov/nls/nlsy97.htm).

This project comes from an online course, *100 Days of Python Coding*. However, I decided to use this dataset to mimic my methodology for the Data Analytics Capstone Project I did in my final semester of college.

### Objectives
- Clean the dataset to prepare for analysis
- Perform exploratory data analysis
- Build a predictive model for the response variable
- Utilize a clustering method for feature engineering
- Perform appropriate model testing and validation

### Description of the Dataset

The provided dataset is a subset of 2,000 observations. A list of variable names and descriptions can be found in [NLSY97_Variable_Names_and_Descriptions.csv](https://github.com/johncarlomaula/nlsy-earnings-project/blob/main/data/NLSY97_Variable_Names_and_Descriptions.csv). 

For more details about the variables, you can view the official documentation [here](https://www.nlsinfo.org/content/cohorts/nlsy97/topical-guide).


## Project Overview

### Part 1: [Data Cleaning and Preparation](https://github.com/johncarlomaula/nlsy-earnings-project/blob/main/nlsy_clean.md)

This section of the project focuses on preparing the dataset for exploratory data analysis.

### Part 2: [Exploratory Data Analysis](https://github.com/johncarlomaula/nlsy-earnings-project/blob/main/nlsy_eda.md)

This section focuses on exploring and visualizing the NLSY97 dataset.


### Part 3: [Data Modeling](https://github.com/johncarlomaula/nlsy-earnings-project/blob/main/nlsy_model.md)

This section focuses on building and testing models to predict the hourly wage of the NLSY97 respondents based on their features.


### Part 4: [Feature Engineering with Clustering](https://github.com/johncarlomaula/nlsy-earnings-project/blob/main/nlsy_cluster.md)

This section focuses on feature engineering using a clustering method to see if it can improve the performance of the previous models.


### Conclusion

The main goal of this project was to apply the principles of statistical learning to build a model that predicts the hourly wage of individuals in the US born in the years between 1980 - 1984 based on their features. After cleaning and exploring the data, I built several models and compared their performance. The best performing model was a boosting model with a RMSE of 10.48. This means that on average, a prediction made by this model is about $10.48 off the true hourly wage. I also explored the concept of feature engineering and hierarchical clustering to see if they could be used to improve the performance of the models.  

---

### Repository Guide

**Note:** Each .Rmd file has a corresponding .md file with the same name.

<details>
  <summary>Click to view list of files and folders:</summary>

**Main Files**:
    
1. **nlsy_clean.Rmd** - contains code for data cleaning and preparation
2. **nlsy_eda.Rmd** - contains code for exploratory data analysis
3. **nlsy_model.Rmd** - contains code for model building and analysis
4. **nlsy_cluster.Rmd** - contains code for feature engineering using clustering

**Main Folders**:
    
1. **nlsy_clean_files/** - folder containing R visualizations for *nlsy_clean.md*    
2. **nlsy_eda_files/** - folder containing R visualizations for *nlsy_eda.md*  
3. **nlsy_model_files/** - folder containing R visualizations for *nlsy_model.md*  
4. **nlsy_cluster_files/** - folder containing R visualizations for *nlsy_cluster.md*  
5. **data/** - folder containing the data
    
**Data Folder**:

1. **NLSY97_Variable_Names_and_Descriptions.csv** - csv file containing the variable descriptions of the dataset
2. **NLSY97_subset.csv** - csv file containing the raw data
3. **NLSY97_train.csv** - csv file containing the training set; output of *nlsy_clean.Rmd*
4. **NLSY97_test.csv** - csv file containing the testing set; output of *nlsy_clean.Rmd*
   
    
</details>

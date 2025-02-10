#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Paulina Harder
find all the scripts to run the main script
"""

#%% Load packages and modules
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
import seaborn as sns
%matplotlib inline
from sklearn.feature_selection import RFE, f_regression
from sklearn.preprocessing import MinMaxScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import learning_curve
from sklearn.svm import SVC
from sklearn.datasets import load_digits
from sklearn.model_selection import GridSearchCV




#%% for loafing and preprocessing data 


def load_preprocess_data(file_path: str, average_days: int = 7) -> pd.DataFrame:
    """
    Usage: Loading data from a .csv file and creating neew columns averaging values for the last 7 days

    Input:
    - file_path (string): path to the input .csv file. make sure it is semicolon seperated.
    - average_days (integer): amount of days for calculating the averages.

    Output:
    pd.DataFrame: Preprocessed Dataframe.
    """
    # Read the data:
    df = pd.read_csv(file_path, sep=";")
    
    # Calculate weekly averages:
    columns_to_average = ['Temp_mean', 'Temp_max', 'BFGL01_AG', 'BFGL02_AG', 'BFGL03_AG', 'Prec', 'VPGFAO', 'VRGS_AG']
    for col in columns_to_average:
        avg_col_name = f"avg_{col}"
        df[avg_col_name] = df[col].rolling(window=average_days).mean()
    
    # Convert "Datum" column and extract the individual features:
    df["Datum"] = pd.to_datetime(df["Datum"], errors='coerce')
    df['doy'] = df["Datum"].dt.dayofyear
    df['week'] = df["Datum"].dt.isocalendar().week
    df['month'] = df["Datum"].dt.month
    
    # Return the df
    return df



#%% function which stores the feature importance rankings to the ranks dictionary


def ranking(ranks, names, order=1):
    minmax = MinMaxScaler()
    ranks = minmax.fit_transform(order*np.array([ranks]).T).T[0]
    ranks = map(lambda x: round(x,2), ranks)
    return dict(zip(names, ranks))


#%% function for NSE

def nse(obs, pred):
    
    """
    function to compute the Nash-Sutcliffe model efficiency
    input:
    - obs (array): observations of the target variable (y_test)
    - pred (array): modelled values of the target variable y using the test parameters (X_test)
    output: 
    Nash-Sutcliffe model efficiency     
    """
    
    return 1 - (np.sum((obs - pred) ** 2) / np.sum((obs - np.mean(obs)) ** 2))


#%% function for k fold analysis
def evaluate_cv_mse(model, X, y, Scoring, cv=10):
    """
    It conducts a cross validation and calculate the mean score and standard devaition of of the chosen evaluation
    metric of the model.    

    Parameter:
    -----------
    model : sklearn-odel
        the model that should be evaluated.in my case it would be random forest regressor 'rfr'
    X (array): feature data used to predict the target value  
        
    y (array): target variable  
    Scoring (string): evaluation metrics that is used e.g.'neg_mean_squared_error'  
    cv (integer): amount of folds for the cross validation. 
         per default: 10     
    

    Returns:
    --------
    mean_score : average score of the chosen metric (float)
        
    std_score : standard deviation score of the chosen metric (float)
        
    """
    # cross validation
    scores = cross_val_score(model, X, y, cv=cv, scoring=Scoring)
    
    # Da neg_mean_squared_error verwendet wird, wird das Vorzeichen umgedreht, 
    final_scores = -scores
    
    # Berechnung des Durchschnitts und der Standardabweichung
    mean_score = np.mean(final_scores)
    std_score = np.std(final_scores)
    
    # Ergebnisse ausgeben
    print(f"average score: {mean_score}")
    print(f"standard deviaton of score: {std_score}")
    
    return mean_score, std_score

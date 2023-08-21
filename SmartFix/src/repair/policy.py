import numpy as np
from sklearn.tree import DecisionTreeRegressor
from sklearn.linear_model import LinearRegression
import matplotlib.pyplot as plt

def tree_train(X, y):
    X = np.array(X)
    y = np.array(y)
    # model = DecisionTreeRegressor(max_depth=5)
    model = DecisionTreeRegressor()
    model.fit(X,y)
    return model

def train(X, y):
    X = np.array(X)
    y = np.array(y)

    model = LinearRegression().fit(X, y)
    model.fit(X,y)

    score = model.score(X, y)
    coef = model.coef_
    bias = model.intercept_

    return (model, score, coef, bias)

def predict(model, X):
    X = np.array(X)
    y = model.predict(X)
    assert(len(y) == 1)

    return (y[0])

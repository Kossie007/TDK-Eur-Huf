import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import datetime
from copy import deepcopy

from sklearn.metrics import mean_absolute_error, mean_squared_error
from math import sqrt

from tensorflow.keras.models import Sequential
from tensorflow.keras import layers, callbacks
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.utils import plot_model


def str_to_datetime(s):
    split = s.split('-')
    year, month, day = int(split[0]), int(split[1]), int(split[2])
    return datetime.datetime(year=year, month=month, day=day)


def df_to_windowed_df(dataframe, first_date_str, last_date_str, n=3):
    first_date = str_to_datetime(first_date_str)
    last_date = str_to_datetime(last_date_str)

    target_date = first_date

    dates = []
    X, Y = [], []

    last_time = False
    while True:
        df_subset = dataframe.loc[:target_date].tail(n + 1)

        if len(df_subset) != n + 1:
            print(f'Error: Window of size {n} is too large for date {target_date}')
            return

        values = df_subset.to_numpy()
        x, y = values[:-1], values[-1]

        dates.append(target_date)
        X.append(x)
        Y.append(y)

        next_week = dataframe.loc[target_date:target_date + datetime.timedelta(days=7)]
        next_datetime_str = str(next_week.head(2).tail(1).index.values[0])
        next_date_str = next_datetime_str.split('T')[0]
        year_month_day = next_date_str.split('-')
        year, month, day = year_month_day
        next_date = datetime.datetime(day=int(day), month=int(month), year=int(year))

        if last_time:
            break

        target_date = next_date

        if target_date == last_date:
            last_time = True

    ret_df = pd.DataFrame({})
    ret_df['Target Date'] = dates

    X = np.array(X)
    for i in range(0, n):
        X[:, i]
        ret_df[f'Target-{n - i}'] = X[:, i]

    ret_df['Target'] = Y

    return ret_df


def windowed_df_to_date_X_y(windowed_dataframe):
    df_as_np = windowed_dataframe.to_numpy()

    dates = df_as_np[:, 0]

    middle_matrix = df_as_np[:, 1:-1]
    X = middle_matrix.reshape((len(dates), middle_matrix.shape[1], 1))

    Y = df_as_np[:, -1]

    return dates, X.astype(np.float32), Y.astype(np.float32)


def merge_with_dummies(X, dummies, date):
    X_df = X.reshape(X.shape[0], X.shape[1])
    X_df = pd.DataFrame(data=X_df, index=date)

    X_df = X_df.join(dummies, how='left')
    X_with_dummies = X_df.to_numpy()
    X_with_dummies = X_with_dummies.reshape(X_with_dummies.shape[0], X_with_dummies.shape[1], 1)

    return X_with_dummies


def fit_lstm(model, X_train, y_train, X_val, y_val):
    lstm = model.fit(X_train, y_train, validation_data=(X_val, y_val), epochs=100,
                     callbacks=callbacks.EarlyStopping(patience=10, monitor='val_loss'))

    return lstm


def sim_lstm(df, y_name, dummy_names, n_sim):
    # splitting up the data
    windowed_df = df_to_windowed_df(df[y_name], '2018-06-10', '2024-09-14', 6)
    dates, X, y = windowed_df_to_date_X_y(windowed_df)

    # merging the data with the dummies
    if 0 < len(dummy_names):
        X = merge_with_dummies(X, df[dummy_names], dates)

    # train, validation, test split
    q_80 = int(len(dates) * .8)
    q_90 = int(len(dates) * .9)

    dates_train, X_train, y_train = dates[:q_80], X[:q_80], y[:q_80]

    dates_val, X_val, y_val = dates[q_80:q_90], X[q_80:q_90], y[q_80:q_90]
    dates_test, X_test, y_test = dates[q_90:], X[q_90:], y[q_90:]

    # model
    model = Sequential([layers.Input((len(X[1]), 1)),
                        layers.LSTM(32),
                        layers.Dense(16, activation='relu'),
                        layers.Dense(16, activation='relu'),
                        layers.Dense(1)])

    model.compile(loss='mse',
                  optimizer='adam',
                  metrics=['mean_absolute_error'])

    # model fitting and calculating mse
    sim_rmse = []
    errors = pd.DataFrame()


    for i in range(n_sim):
        # fitting the model
        lstm = fit_lstm(model, X_train, y_train, X_val, y_val)

        # prediction
        test_predictions = model.predict(X_test).flatten()

        # errors
        errors[f'sim_{i}'] = y_test - test_predictions

        # rmse
        sim_rmse.append(sqrt(mean_squared_error(y_test, test_predictions)))



    return sim_rmse, errors


def sim_lstm_custom_lag(df, y_name, dummy_names, lag, n_sim):
    # splitting up the data
    windowed_df = df_to_windowed_df(df[y_name], df['row.names.eurhuf.'][lag], '2024-09-14', lag)
    dates, X, y = windowed_df_to_date_X_y(windowed_df)

    # merging the data with the dummies
    if 0 < len(dummy_names):
        X = merge_with_dummies(X, df[dummy_names], dates)

    # train, validation, test split
    q_80 = int(len(dates) * .8)
    q_90 = int(len(dates) * .9)

    dates_train, X_train, y_train = dates[:q_80], X[:q_80], y[:q_80]

    dates_val, X_val, y_val = dates[q_80:q_90], X[q_80:q_90], y[q_80:q_90]
    dates_test, X_test, y_test = dates[q_90:], X[q_90:], y[q_90:]

    # model
    model = Sequential([layers.Input((len(X[1]), 1)),
                        layers.LSTM(32),
                        layers.Dense(16, activation='relu'),
                        layers.Dense(16, activation='relu'),
                        layers.Dense(1)])

    model.compile(loss='mse',
                  optimizer='adam',
                  metrics=['mean_absolute_error'])

    # model fitting and calculating mse
    sim_rmse = []

    for i in range(n_sim):
        # fitting the model
        lstm = fit_lstm(model, X_train, y_train, X_val, y_val)

        # prediction
        test_predictions = model.predict(X_test).flatten()

        # mae
        sim_rmse.append(sqrt(mean_squared_error(y_test, test_predictions)))

    return sim_rmse

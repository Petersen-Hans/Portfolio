import numpy as np
import pandas as pd
from keras.src.saving.saving_api import load_model
from keras_tuner.src.backend.io import tf
from sklearn.preprocessing import StandardScaler
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, Bidirectional, Dense
import keras_tuner as kt
from sklearn.metrics import mean_squared_error
import math

# Load and preprocess  data
#timeframe = [6,12]
#batch_sizes = []
predicted = []

def create_dataset(data, time_steps=1):
    X, y = [], []
    for i in range(len(data) - time_steps):
        a = data[i:(i + time_steps), 0]
        X.append(a)
        y.append(data[i + time_steps, 0])
    return np.array(X), np.array(y).reshape(-1, 1)


# Define the model-building function for Keras Tuner


def build_model(hp):
    model = Sequential()
    neurons = hp.Int('neurons', min_value=1, max_value=1, step=1)

    if option == 'L':
        model.add(LSTM(neurons, stateful=False, batch_input_shape=(1, time_steps, 1), return_sequences=False))
    elif option == 'B':
        model.add(Bidirectional(LSTM(neurons, stateful=False, return_sequences=True), batch_input_shape=(1, time_steps, 1)))
        model.add(Bidirectional(LSTM(neurons, stateful=False), return_sequences=False))

    model.add(Dense(1))
    model.compile(loss='mean_squared_error', optimizer='adam')
    return model


djM = pd.read_excel("DJIAmonthly.xlsx")
djW = pd.read_excel("DJIAweekly.xlsx")
djD = pd.read_excel("DJIAdaily.xlsx")

df = [djM,djW, djD]
keys = ["month", "week", "day"]
for dj, key in zip(df, keys):

    series = dj["Adj Close**"].to_numpy()
    series = series[::-1]  # Reverse the series
    dates = dj["Date"].to_numpy()
    dates = dates[::-1]  # Reverse dates

    option = "L"

    # Split data into training and testing
    size = int(len(series) * 0.70)
    train, test = series[:size], series[size:]
    scaler = StandardScaler()
    train_scaled = scaler.fit_transform(train.reshape(-1, 1))
    test_scaled = scaler.transform(test.reshape(-1, 1))

    timeframe = [6,12,32,64]  # This is the number of time steps you're using in your LSTM model

    batch_sizes = [4,8,16,32,64]  # Define your batch sizes here
    best_model = None
    best_score = float('inf')
    best_batch_size = None

    for batch_size in batch_sizes:
        for time_steps in timeframe:
            train_x, train_y = create_dataset(train_scaled, time_steps)
            test_x, test_y = create_dataset(test_scaled, time_steps)
            tuner = kt.RandomSearch(
                build_model,
                objective='val_loss',
                max_trials=15,
                executions_per_trial=6,
                directory=f'allem_{key}',
                project_name=f'stock_lstm_tf{batch_size}'
            )
            print(train_x.shape, train_y.shape, test_x.shape, test_y.shape)
            tuner.search(train_x, train_y, epochs=50, batch_size=batch_size, validation_data=(test_x, test_y))

            # Get the best score for the current batch size
            best_hps = tuner.get_best_hyperparameters(num_trials=1)[0]
            model = tuner.hypermodel.build(best_hps)
            model.fit(train_x, train_y, epochs=50, batch_size=batch_size, validation_data=(test_x, test_y))
            val_loss = model.evaluate(test_x, test_y, batch_size=batch_size)

            # Check if this model is better than the previous models
            if val_loss < best_score:
                best_score = val_loss
                best_model = model
                best_time_step = time_steps
                best_batch_size = batch_size

        print(f"The best batch size is {best_time_step} with a validation loss of {best_score}")


    # Train the model with the optimal hyperparameters

    # Run the hyperparameter search
        model = tuner.hypermodel.build(best_hps)
        history = model.fit(train_x, train_y, epochs=50, validation_data=(test_x, test_y))


    # Get optimal hyperparameters
        best_hps = tuner.get_best_hyperparameters(num_trials=1)[0]
        best_model = build_model(best_hps)


    # Make predictions
        predictions = best_model.predict(test_x)

        predictions_original_scale = scaler.inverse_transform(predictions)
        test_original_scale = scaler.inverse_transform(test_y)

    # Evaluate the model
        tmp = predictions_original_scale
        predicted.append(tmp)
        mse = mean_squared_error(test_original_scale, predictions_original_scale)
        rmse = math.sqrt(mse)
        print("RMSE:", rmse)
        best_model.save(f'best_model_{key}.keras')

Monthly_model = load_model("best_model_month.keras")
Weekly_model = load_model("best_model_week.keras")
Daily_model = load_model("best_model_day.keras")

predictions_df = pd.DataFrame(predicted)
predictions_df.to_csv('predictions.csv', index=False, header=False)

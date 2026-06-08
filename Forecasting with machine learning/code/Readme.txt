Our paper looked at a subset of the data used in Siami-Namini et al.(2019). We analyzed the Dow Jones value from 1992 through July 2018, on a daily, weekly, and monthly scale. The data was sourced from Yahoo finance.

There are 3 code files, buildModel is a Python file that primarily uses the keras package to train the LSTM model. The hyperparameters are optimized for each dataset, the time step and batch size is done using for loops, and the neurons are optimized using the keras tuner. The model for each series is saved in this program as well. The results Python file uses the saved models to calculate the forecasted values, and saves them as well. Finally the R file graphs was used to generate the figures used in the report. 


We were not able to replicate the results of the paper, due to many factors fully explained in the paper. In short, the architecture of the model used in Siami-Namini et al. was not defined. With the limited computing power available, we were not able to run enough complex models to replicate their architecture iteratively. 

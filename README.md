# RShiny

The Project is about an interactive dashboard that depicts the Economic Stability of a country considering its GDP and also to predict the GDP of these countries by using statistical methods. This dashboard emphasis on implementing a Prediction system that takes the previous data into consideration and predicts the value of the given input. The data that is dealt here is a Time Series Data of Various Countries. The project focuses on predicting the GDPs of those countries in a more convenient and graphical model such that it would be easy to understand by any common person. We are following a Time Series Approach to predict the values by using ARIMA model as the Dataset is considerably small. It consists data of GDP values of 164 countries ranging from 1994 to 2017.


The main focus is on representing the country and it’s GDP. Also there is prediction of GDP of those respective countries. The functions that are used are:
A)	Homepage
B)	Data
C)  GDP
D)	Top 10 GDP
E)	Prediction-Search


A) Homepage: It contains some basic information about the GDP and how it helps economists and investors in understanding the economic situation of countries and thereby helping them in their business or work.
                                    

B) Data- When clicked it directs the users to a new page (View info) where the user are provided with a view of the data and are also provided with a search option in which the users can search either country-wise or year-wise depending on their interest. And they are provided with an option of selecting the number of entries to be displayed.


C) GDP: When the users clicks this option they are directed to a new page titled “GDP of a country” and they are provided with select option to select the country of their choice. When a country is selected the corresponding line plot consisting of GDP of that country is displayed from 1994 to 2017 and this gives a clear idea about where the GDP of that country is heading towards.


D) Top 10 GDP: When the users click this they are directed to a new page in which they are able to view the top 10 GDP based on years. They are given an option to select the country they want and the corresponding year’s Top 10 GDP based on highest values will be displayed.


E) Prediction-search: When the users click this option they are directed to a new page where the users are able to see the predicted GDP values of their respective country. On the top, the method used for prediction (forecast) will be displayed. And the users are given a select option to select a country of their choice and when entered the predicted GDP of that particular country will be displayed with a line plot consisting of the GDP values along with its predicted values, summary of that particular country’s GDP and the forecasted values underneath that for next 10 years by default. The users can change the number of years in the below option (number of years). How-ever the maximum number of years that can be predicted is 30.

Description of App

User Interface:

The user has three options when interacting with my app: One, the frequency of returns, which are either monthly or daily. Two, the number of factors that stock return data is regressed on, either three or five factors. And three, the stock they seek to estimate an expected return for. They do this by typing the ticker of the stock they wish to analyze. Any stock that is publicly traded on an exchange (any stock that yahoo finance has return data on) can be analyzed.

On the first tab, on the top, there are two rates that are outputted: The expected market return and the expected rate of return. The expected market return does not vary by stock choice as this is a market return. This number is read in from online and updates monthly. The expected rate of return, on the other hand, updates when the user types in a different ticker and is also dependent on the frequency and number of factors. There are two tables below displaying financial information. Both tables are read in from Yahoo Finance and are dependent solely on the stock being analyzed.

The second tab outputs the coefficients, standard errors, t-statistics, and p-values. The standard errors are heteroskedastic robust. This information is dependent on the ticker, frequency, and the number of factors used in the regression. 

Under the hood:

There are a number of different packages and functions involved in the creation of this app. The XML package is used to scrape data dynamically off of the web. Table and price data are read in using the readHTMLTable function. For one instance, the quantmod package is used to read in one piece of information off the web, the risk-free rate (rf). Other packages include tidyverse, lubridate, stringr, and broom for helping filter and organize price and factor data once read in. The AER package was used for estimating standard errors that are heteroskedastic robust (The vcovHC function was used).

The code is composed of four if statements that read in different data sets depending on the frequency and number of factors in the model. This data is read in from dropbox. The code within each if statement differs depending on the format of the data. 

Directions: 

Type in the ticker of the stock you want to analyze under "stock ticker:" then choose the frequency and number of factors you want in your model under "Frequency" and "Number of Factors," respectively. Results can be analyzed thereafter.

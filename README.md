# Machine Learning Project

Implementation of machine learning classifiers in R and their usage in a real project

## Project Description

We were given a dataset of protein corona fingerprints which includes 122 proteins with gold or silver cores and we were asked to predict the toxicity of a new protein using machine learning algorithms. In this project we precisely used naïve Bayes classifier, k-nearest neighbors algorithm and multiple linear regression.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

R Programming language 
There is no need to install any package.

## Running the program

To run the program on your computer you have to change the location in the first line of each program with the location of data.txt which is the data file given in the zip file.
for example:
	setwd("Location/of/datafile/")

### for naive Bayes
Open the file NB.R
There are 5 training and test datasets you could run the program on any of them you want by changing the number of arguments for example train.2 and test.2

### for KNN
Open the file KNN.R
Set the K value to the value you want and run the program

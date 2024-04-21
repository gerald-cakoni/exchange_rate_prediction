# Exchange Rate - Shiny App

This application is designed to make an detailed analysis in time series. Starting from Statistic Analysis of the time series and going further into prediction of the possible trends of exchange rate between EURO and LEK (Albanian National Currency).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

In the file `time_series_web_version` is an HTML version of the project.
### Prerequisites

Before setting up the project, you need to have R Studion installed on your computer.
Download it from: [R Studio](https://www.r-project.org/).


#### Environment Setup

1. Open a script in **R Studio.**
    
    - Run the following commant. It will run this github repository on your local machine.

            library(shiny)
            runGitHub(repo = "exchange_rate_prediction", username = "gerald-cakoni", ref = "main")
            
    - Or, alternativeely: Run the file `run_shiny.R`




### Usage

Once the web application is running, you can see all the analysis I made and models I built for forcatsting time series..
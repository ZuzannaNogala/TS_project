# Statistical Analysis and Forecasting of Air Passenger Traffic in Poland in the Face of the Pandemic Shock

$\textbf{Authors}$: Zuzanna Nogala, Joanna Pokora

This project was completed as the final assignment for the 'Time Series' course between December 2025 and January 2026.

## Overview

This project conducts a statistical analysis of the number of passengers using air transport in Poland over a 21-year period (2004–2025). 

The primary goal is to examine the evolution of the Polish aviation market—from its dynamic growth after joining the EU to the drastic collapse caused by the COVID-19 pandemic and the subsequent recovery phase.  The study evaluates how different data preparation strategies impact the ability of time series models to accurately reflect market dynamics and forecast the industry's condition for the next 5 years.  

For more details, please refer to the full report (in Polish), $\texttt{Nogala_Pokora_TS_project.pdf}$, included in this repository.

## Dataset

* The analysis utilizes monthly data sourced from the Eurostat database (identifier: [avia_paoc](https://ec.europa.eu/eurostat/databrowser/view/avia_paoc/default/table?lang=en&category=avia.avia_pa.avia_pao)). 

* The dataset includes statistics on both domestic and international (intra- and extra-EU) passenger traffic handled in Poland. 

![](/Users/zuza/Downloads/data_rplot.png)

## Methodology

The time series was divided into three key periods: 

* pre-pandemic (Jan 2004 – Feb 2020), 
* during the pandemic (Mar 2020 – May 2022), 
* and post-pandemic recovery (May 2022 – Aug 2025). 

To evaluate the predictive capabilities of different models in the face of structural shocks, the study tests three distinct modeling scenarios:  

* $\textbf{Scenario 1}$: Ignoring the COVID-19 period completely (models will be trained only on pre-pandemic data). 
* $\textbf{Scenario 2}$: Including the original data from the COVID-19 period (models will be trained on pre-pandemic and pandemic data). 
* $\textbf{Scenario 3}$: Imputing the COVID-19 period data based on historical trends using STL decomposition and ETS models (models will be trained on pre-pandemic data and the imputed pandemic data).   

### Models Used

* $\textbf{SARIMA}$: Both manual (expert) parameter selection and automatic selection (using auto.arima) were tested to handle the non-stationary, highly seasonal data.  
* $\textbf{Prophet}$: Meta's forecasting tool was utilized to model the multiplicative trend and seasonality without requiring data stationarity. Both an automatic configuration and a manually calibrated approach were tested. The calibration involved defining special events as holidays (to account for the pandemic period), manually specifying changepoints to capture the structural breaks, and incorporating a yearly seasonality cycle.


## Key Findings

### Trend and Seasonality Changes

* Trend Shift: The pandemic pushed the market off its previous growth path onto a new, parallel trajectory shifted downwards, effectively setting the market's development back by a few years. However, the growth dynamic remains strong, indicating that the pandemic did not permanently change consumers' flying habits.  

* Seasonality Amplification: The classic seasonality shape remains, but the amplitude between the summer peak and winter low has drastically increased. Additionally, the peak earning season for airlines has extended to include May, June, and September.  

### Model Performance:

* Scenario 1 (Ignoring COVID): Models trained strictly on pre-2020 data failed to account for the structural shock, leading to significant overestimation of future passenger numbers. Relying on this approach could lead to oversupply and major financial losses.  

* Scenario 2 (Including COVID): Automatic models struggled heavily with the pandemic shock. However, the manually calibrated Prophet model achieved the lowest error on the test set among all models in this scenario.  

* Scenario 3 (Imputation): Replacing the shock period with imputed values simplified the series, allowing simpler models, like the fully automatic SARIMA, to perform exceptionally well. 

### Final Conclusion & Future Outlook

* Best Model: The manually calibrated Prophet model (from Scenario 2) proved to be the most robust overall, though the automatically selected SARIMA with imputed data (Scenario 3) achieved very comparable results with less model complexity.  

| Scenario | Model | RMSE |
| :---: | :--- | ---: |
| Scenario 1 (Ignoring COVID) | Prophet (automatic) | 1,421,368 |
| Scenario 2 (Including COVID) | Prophet (calibrated) | **427,322** |
| Scenario 3 (Imputation) | SARIMA (automatic) | 540,244 |

* Forecast: Predictions for the next five years are optimistic, showing steady growth and an increasing number of passengers. Airlines must, however, implement flexible resource planning to handle the widening gap between the summer peaks and the rest of the year.  

![](/Users/zuza/Downloads/prophet_rplot2.png)
# received_forecast
 Client forecasting problem for live Power BI dashboard (demo_data replaces original dataset).

To run:
1. Run generate_demo_data.R to create the demonstration data.
The original data is currently loaded, cleaned and prepped (with governance meta data records) in Power BI.
It is not used here and the model card (for AI governance) is not provided - to protect client anonymity.
You could save this (e.g. as a csv) and load into Power BI for the full experience.

2. Run forecasting_powerbi.r
This creates the forecast and plot.
Additionally the model is packaged.
If running in Power BI, press the R button and paste the code in (Power BI will refer to the table as dataset - uncomment line 15 and comment out line 16. Comment out line 79 onwards.

3. forecast_plot.png allows you to see the plot that the above code creates.

4. To demonstrate, run load_model.r which loads the saved model and forecasts tomorrow's 'Received' value with lower and upper bounds.
Example: 
print(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
          ds   yhat yhat_lower yhat_upper
1 2024-03-01 60.504   54.74227   66.64765
Original margins were within tolerance accepted by the client as useful. No inference can be made from the above example as the data used to demonstrate was simulated.

For automated monitoring and versioning of the model (as data is updated over time) use vetiver or MLFlow.

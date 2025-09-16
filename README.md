# Growth-Detective
This repository contains R scripts and documentation for the "Growth Detective" project which can be categorized under two main sections. In the first section, three countries at different development stages are taken into consideration and analyzes economic growth from 1980 to 2025. In the second section, taking key variables from all available OECD countries into consideration and the relationships between GDP growth are analyzed. All content aligns with the assignment requirements and best practices for reproducibility, clarity, and data transparency.


Project Overview

This project examines the macroeconomic growth of Canada (advanced), Paraguay (emerging), and Lesotho (low-income), focusing on key growth determinants through comparative and time-series analysis. The workflow addresses data collection, cleaning, growth rate computations, visualization and economic reasoning.



Data Sources

World Bank World Development Indicators (WDI): GDP, GDP per capita, population growth, investment rates and inflation rates.




Data Collection and Preparation

Countries were chosen to represent varying development stages: Canada, Paraguay, and Lesotho. Variables include GDP per capita, population growth, investment rates and inflation rates and they are justified in the report for their relevance to growth. Data ranges cover multiple decades (as available), ensuring meaningful cross-country and trend analysis. Missing data handled using interpolation (averaging adjacent years) documented in script comments.



Analysis Workflow


Growth Calculation: Annual growth rates of GDP per capita are computed for each country.

Visualization: Well-labeled time series and comparative growth graphs are generated.

Country Deep-Dive: A focused analysis of Canada divides growth trends into pre-1990, 1990–2007, and 2008 onward, highlighting major economic events.

Cross-Country OECD Analysis: Key macroeconomic variables are examined for OECD members through scatterplots and correlation visualizations.



Project Team - Clif Clemotte, Ravindu Weerasekara

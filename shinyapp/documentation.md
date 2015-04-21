# Distribution simulator

This Shiny application allows you to create, configure and display dynamic distributions.
You can choose the kind of distribution you want to simulate, its main parameters, 
the number of observations to simulate and the type of plot to be displayed.

The left frame allows to configure the distribution, while the right frame shows the 
results.

## Simulation kind

In order to select the type of distribution to be simulated, chose the appropiate 
option in the __Plot type__ section.

After that, configure the required distribution parameters, which depend on the 
simulation type.

The available distributions and parameters are the following:

1. Normal: normal distribution, its mean and standard deviation can be modified.
2. Uniform: uniform distribution, its lower and upper limits can be modified.
3. Log-normal: log normal distribution, its mean and standard deviation logs 
can be modified.
4. Exponential: exponential distribution, its rate can be modified.

## Plot type

The type of plot to be displayed can be chosen in the __Plot type section__.
Two different plots can be represented:

* Histogram: distribution histogram. x are distribution values and y is the 
frequency of the x value.
* Scatter plot: distribution scatter plot with smooth line. x is the index along 
the distribution values and y are the values corresponding to each index.

## Number of observations

The size of distribution data can be chosen in the __Number of observations__ section.

## Data display

The generated distribution data is displayed in the right frame, and organized in 
three different tabs:

* Plot: shows the chosen plot for the distribution data.
* Summary: shows a small summary of the distribution data: mean, median and quantiles.
* Table: shows the generated distribution values in an interactive data table.

Data display tabs are refreshed each time a parameter is changed in the configuration.

## Source code

Application source code can be found at:

https://github.com/gmcorral/shinyapp


# fp_final-project-group-5

COVID19 Shiny app analysis

## Purpose 
Purpose is to present a visual appealing and easily manipulative data involved the dataset from the CDC

## Data Source 
Data set can obtained from the following [link]: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4

## The app consists of 4 tabs:
1. A US map with a clickable interface allowing you to click on a certain state of interest and display pertinent information
2. A plot analysis which allows you to select certain x variables and display select statistics
3. A plot analysis tab that allows for a user to select x and y variables and conduct certain statistical analysis
4. A Ranking tab which shows an overall summary and spreadsheet of the dataset from the CDC that has been cleaned and tidied. 

## Installation

To install shiny packages from CRAN:

``` r
install.packages("shiny")
```
## Example

You can clone or download the repository from GitHub by asking us for authorization, and use run

```{r example}
library(shiny)
shiny::runApp("Covid-19 Data Analysis")
```

## Help
- The packages used can be viewed in the Vignette which will also show the versions used.
- Other important information can be viewed in the vignette as well. 



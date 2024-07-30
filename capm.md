
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
#fill the code

# Take the data frame
df <- df %>% 
  # Create a new column calculating the daily return for AMD and GSPC
  mutate(amd_daily_return = (AMD - lag(AMD)) / lag(AMD),
         gspc_daily_return = (GSPC - lag(GSPC)) / lag(GSPC))

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
#fill the code

# Calculate daily risk free rate from the annual RF column given
df <- df %>%
  mutate(daily_rf = (1 + RF/100)^(1/360) - 1)

```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
#fill the code

df <- df %>%
  # Calculate the excess returns for AMD and GSPC
  mutate(amd_excess_returns = amd_daily_return - daily_rf,
         gspc_excess_returns = gspc_daily_return - daily_rf)
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
#fill the code

# Create a linear regression model using gspc excess returns as the predictor variable for amd excess returns
Model<-lm(amd_excess_returns~gspc_excess_returns, df)
summaryModel<-summary(Model)
summaryModel

beta <- summaryModel$coefficients[2,1]
cat("Beta is ", beta, "\n")
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The \(\beta\) I obtained is 1.5691696 this means that for every 1 unit change in the excess returns of the S&P 500, the excess returns of AMD are expected to change by approximately 1.5691696 units. Since \(\beta\) > 1, we consider AMD as more volatile than the market. If the market's excess returns increase or decrease by 1 unit, AMD's excess returns are expected to increase or decrease by approximately 1.57 units, making AMD more sensitive to market movements.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
#fill the code

# Create the scatter plot and CAPM regression line
plot<-ggplot(df,aes(x=gspc_excess_returns, y=amd_excess_returns)) +
  geom_point() + # Adds Scatterplot
  geom_smooth(method = "lm", col = "blue") + # Adds a regression line
  labs(title = "AMD excess returns vs. GSPC excess returns", x = "GSPC excess returns", y = "AMD excess returns")

print(plot)
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
#fill the code

# Calculate the residual standard error from the regression model output
s_f <- summaryModel$sigma

# Convert the Daily Standard Error to an Annual Standard Error
s_annual <- s_f * sqrt(252)

# Calculate the Annual Expected Return for AMD using the CAPM formula
rf <- 0.05
GSPC_expected_return <- 0.133
AMD_expected_return = rf + beta * (GSPC_expected_return - rf)

# Calculate degrees of freedom
n<-length(df$gspc_excess_returns)
k<-1
df<-n-k-1

# Calculate t-value for 90% confidence
t = qt(0.95, df)

# Calculate prediction interval
upper = AMD_expected_return + t * s_annual
lower = AMD_expected_return - t * s_annual

upper
lower

```


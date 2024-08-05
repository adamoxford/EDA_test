## Note - "commented"text is indicated with a double has "##" at the start of the line. This means that it will not be processed when the script is run

##install the necessary packages (comment out if already installed)
#install.packages("tidyverse")

## Note - tidyverse is a collection of packages ideal for most data exploration, including dplyr, ggplot and more.
## load the libraries required

library(tidyverse)

## read our csv file containing life expectancy and happiness coefficients as a dataframe called "lifeHappy"

lifeHappy <- read_csv("le_v_h.csv")

## Use dplyr to tidy the data by cleaning up the column names and removing spaces, then removing NAs

lifeHappyclean <- lifeHappy %>%
  rename(cantrilScore = "Cantril ladder score", lifeExpectancy = "Life expectancy - Sex: all - Age: 0 - Variant: estimates") %>%
  drop_na()

## Do a simple scatterplot showing Cantril score versus Life expectancy using our cleaned dataset

plot(lifeHappyclean$cantrilScore, lifeHappyclean$lifeExpectancy)


## Let's use ggplot to make that a bit nicer to look at

e <- lifeHappyclean %>%
  ggplot(aes(cantrilScore, lifeExpectancy)) +
  geom_point() + 
  ## add a line of best fit
  geom_smooth(aes(cantrilScore, lifeExpectancy), method="lm", se=F) +
  ## add axis labels
  ggtitle("Life expectancy versus happiness coefficient") +
  xlab("Cantril ladder score") +
  ylab("Average life expectancy") +
  geom_text(aes(label=Entity))


## plot the graph
e

## Or, we can calculate outliers using the inter-quartile range (IQR) method

model <- lm(lifeExpectancy ~ cantrilScore, data = lifeHappyclean)
lifeHappyclean$residuals <- resid(model)
Q1 <- quantile(lifeHappyclean$residuals, 0.25)
Q3 <- quantile(lifeHappyclean$residuals, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- lifeHappyclean %>% filter(residuals < lower_bound | residuals > upper_bound)

## now plot the chart with only outliers shown

e <- lifeHappyclean %>%
  ggplot(aes(cantrilScore, lifeExpectancy)) +
  geom_point() + 
  ## add a line of best fit
  geom_smooth(aes(cantrilScore, lifeExpectancy), method="lm", se=F) +
  ## add axis labels
  ggtitle("Life expectancy versus happiness coefficient") +
  xlab("Cantril ladder score") +
  ylab("Average life expectancy") +
  geom_point(data = outliers, aes(x = cantrilScore, y = lifeExpectancy), color = 'red', size = 3) +
  geom_text(data = outliers, aes(label=Entity))

e

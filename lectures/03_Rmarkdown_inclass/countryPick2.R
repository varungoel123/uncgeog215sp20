## Required Libraries 
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

## Read in Data
gapMinder <- read.delim("gapminderDataFiveYear.tsv") 

### Check data 
head(gapMinder) #First 10 lines of dataset
dim(gapMinder) #number of rows and columns in data set

# check country names in the dataset
levels(gapMinder$country)

### Pick 2 Countries
countryName1 <- "United States" # Replace with your Country 1
countryName2 <- "China" # Replace with your Country 2


### Subset Country One records
country1 <- subset(gapMinder, country == countryName1)

# Plot year vs population
ggplot(country1, aes(year, pop)) + 
  geom_path() +
  ggtitle(countryName1) +
  theme(plot.title = element_text(size = 15, face = "bold"))

# Plot gdp vs lifeExpectancy
ggplot(country1, aes(gdpPercap, lifeExp, size = pop, label = year)) + 
  geom_point() +
  geom_text(hjust = 1.3, vjust = 0, size = 3) +
  ggtitle(countryName1) +
  theme(plot.title = element_text(size = 15, face = "bold"))

### Country Two
country2 <- subset(gapMinder, country == countryName2)

# Plot year vs population
ggplot(country2, aes(year, pop)) + 
  geom_path() +
  ggtitle(countryName2) +
  theme(plot.title = element_text(size = 15, face = "bold"))

# Plot gdp vs lifeExpectancy
ggplot(country2, aes(gdpPercap, lifeExp, size = pop, label = year)) + 
  geom_point() +
  geom_text(hjust = 1.3, vjust = 0, size = 3) +
  ggtitle(countryName2) +
  theme(plot.title = element_text(size = 15, face = "bold"))


# Plot both countries

# Add subsetted data together
allCountries <- rbind(country1, country2)

#Plot Year vs pop size
ggplot(allCountries, aes(year, pop, color=country)) + 
  geom_path() +
  xlab("Year") + ylab("Population Size") +
  ggtitle("Both countries") +
  theme(plot.title = element_text(lineheight=.8, face = "bold"))

# plot all the years at once also:
ggplot(allCountries,
       aes(x = gdpPercap, y = lifeExp, color = country, size = pop)) + 
  scale_x_log10(limits = c(500, 90000)) +
  ylim(c(30, 90)) +
  geom_point(alpha = 0.8) + 
  scale_size_area(max_size = 14) +
  theme_bw() +  # black grid on white background
  xlab("GDP per capita") + ylab("Life Expectancy") +
  ggtitle("Both countries") +
  theme(plot.title = element_text(size = 15, face = "bold"))

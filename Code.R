```{r include = FALSE}
load("predictors.Rdata")
library(knitr)
library(fields)
library(stringr)
library(lubridate)
library(stats)
library(stats)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tibble)
```
```{r include = FALSE}
my_year<-as.numeric(substr(dates, 1, 4))
unique(my_year)

my_month<-as.numeric(substr(dates, 5, 6))
sort(unique(my_month))

# Check unique values in the my_year variable
unique_years <- unique(my_year)
print("Unique years in the my_year variable:")
print(unique_years)
```

```{r include = FALSE}
#*Plotting some of the temperature vertical profiles*
dim(Twb.prof)
head(Twb.prof)
step.size <- seq(0, 3000, by = 100)
xlims <- range(Twb.prof[1:100, ])
#Ice Pellet Profiles
pellet <- which(ptype == "IP")
xlims <- range(Twb.prof[pellet,])
```
```{r echo = FALSE}
df <- data.frame(
  ptype = ptype,
  Twb.prof = Twb.prof[,1],
  station.ind = station.ind,
  date.ind = date.ind
)
```
```{r echo = FALSE}
df <- data.frame(
  ptype = ptype,
  Twb.prof = Twb.prof[,1],
  station.ind = station.ind,
  date.ind = date.ind
)
```


Analyzing the ice_pellet plot
```{r echo = FALSE}
ice_pellet_df <- df %>%
  filter(ptype == "IP")
ice_pellet_df$pellet <- pellet

stray_ice_pellet_df <- ice_pellet_df %>%
  filter(Twb.prof > 280)

#Ice Pellet Profiles
pellet <- which(ptype == "IP")
xlims <- range(Twb.prof[pellet,])
plot(Twb.prof[1,], step.size, xlab = "Temperature (K)", ylab = "Meters AGL", type = "n", xlim = xlims, col = rgb(0,0,0,.1))
title("Ice Pellet Profiles",cex.main = 2)
abline(v = 273.15, col = 2, lwd = 2)
for(i in 1:length(pellet)){
	lines(Twb.prof[pellet[i],],step.size,col=rgb(0,0,0,.1))
}
abline(v=273.15,col=5,lwd=3)
lines(Twb.prof[pellet[694],],step.size,col=4,lwd=2) ## Normal
lines(Twb.prof[pellet[31],],step.size,col=2,lwd=2) ## Coldest
lines(Twb.prof[pellet[445],],step.size,col=2,lwd=2) ## Warmest
lines(Twb.prof[pellet[555],],step.size,col=2,lwd=2) ## Strange
text(278.5, 2500, "Points Above Freezing", col = "steelblue", font = 4, cex = 0.8)
text(280, 1250, "Strange", col = "black", font = 1, cex = 0.8)
text(253, 350, "Coldest recorded landing point", col = "black", font = 1, cex = 0.8)
text(280, 500, "Warmest recorded landing point", col = "black", font = 1, cex = 0.8)
text(267, 2500, "Normal line", col = "black", font = 1, cex = 0.8)


sorted_df <- ice_pellet_df %>%
  arrange(desc(Twb.prof))
top_10_pellets <- sorted_df %>%
  select(Twb.prof, station.ind) %>%
  slice(1:10)
coldest_10_pellets <- sorted_df %>%
  select(Twb.prof, station.ind) %>%
  slice(752:743)

# Select the desired columns and the top 10 rows
# Add color to the Twb.prof column



top_10_pellets$Twb.prof <- cell_spec(top_10_pellets$Twb.prof, "html", color = "white", bold = T, background = "darkblue")

# Add color to the station.ind column
top_10_pellets$station.ind <- cell_spec(top_10_pellets$station.ind, "html", color = "black", bold = T, background = "lightblue")

coldest_10_pellets$Twb.prof <- cell_spec(coldest_10_pellets$Twb.prof, "html", color = "white", bold = T, background = "darkblue")

coldest_10_pellets$station.ind <- cell_spec(coldest_10_pellets$station.ind, "html", color = "black", bold = T, background = "lightblue")

combined_table <-cbind(top_10_pellets, coldest_10_pellets)

# Style the table

styled_table <- combined_table %>%
  kable("html", escape = F) %>%  # Set escape = F to allow HTML in the table
  kable_styling("striped", full_width = F) %>%
  add_header_above(c("Warmest 10 Pellets by station" = 2, "Coldest 10 Pellets by station" = 2))


styled_table


```

```{r echo = FALSE}
set.seed(1)
snow <- which(ptype=="SN")
xlims <- range(Twb.prof[snow,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters Above Ground Level",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Snow Profiles",cex.main=2)
for(i in 1:length(snow)){
	lines(Twb.prof[snow[i],],step.size,col=rgb(0,0,0,.1))
}
text(287, 2500, "Points Above Freezing", col = "steelblue", font = 4)
abline(v=275,col=2,lwd=2)
```

```{r echo = FALSE}
set.seed(1)
snow <- which(ptype=="SN")
xlims <- range(Twb.prof[snow,])
plot(Twb.prof[1,],step.size,xlab="Temperature (K)",ylab="Meters Above Ground Level",type="n",xlim=xlims,col=rgb(0,0,0,.1))
title("Snow Profiles",cex.main=2)
for(i in 1:length(snow)){
	lines(Twb.prof[snow[i],],step.size,col=rgb(0,0,0,.1))
}
text(287, 2500, "Points Above Freezing", col = "steelblue", font = 4)
abline(v=275,col=2,lwd=2)
```


Observing Snow when temperature is above freezing
```{r echo = FALSE}
snow_temp_df <- df %>%
  filter(ptype == "SN" & Twb.prof > 275)

station_counts <- snow_temp_df %>%
  group_by(station.ind) %>%
  summarize(count = n())

# Assuming station_counts is your data frame with station counts
# Arrange the data frame in descending order based on the count
station_counts_sorted <- station_counts %>%
  arrange(desc(count))

# Plot the bar chart
ggplot(station_counts_sorted, aes(x = factor(station.ind), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Station Counts",
       x = "Station Index",
       y = "Count of snow recorded above 275 kelvin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(breaks = seq(1, max(station_counts_sorted$station.ind), by = 5))


top_n_value <- 10

# Select the top N station indexes
top_stations <- station_counts %>%
  top_n(top_n_value, wt = count) %>%
  arrange(desc(count))

# Add an index column for row color
top_stations$index <- 1:nrow(top_stations)

# Create a formatted table with alternating row colors
top_stations_table <- top_stations %>%
  kable("html", caption = "Top Station Indexes") %>%
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(c(" ", "Top Stations" = 2), background = "#F8766D", color = "black") %>%  # Header row color
  row_spec(top_stations$index, background = "steelblue")  # Alternating row color

# Display the table
top_stations_table

count_condition <- sum(df$ptype == "SN" & df$Twb.prof > 275)
print(paste("Number of times snow is recorded above freezing temperatures"))
count_condition

top_10_station_names <- c(stations[550], stations[551], stations[530], stations[545],
                          stations[529], stations[535], stations[546], stations[531],
                          stations[549], stations[25])
top_10_station_names
```


```{r include = FALSE}
#*Proportion of observations of each type*
head(ptype)
summary(ptype)
tail(ptype)
unique(ptype)
class(ptype)
table(ptype)

nn <- length(ptype)
```

```{r echo = FALSE}
library(maps)
lon.new <- lon - 360
plot(lon.new,lat,pch=19, xlab = "Longitude", ylab = "Latitude", main = "NOAA Precipitation recording stations")


data_new_points <- data.frame(
  latitude = c(60.1167, 59.5167, 60.5, 70.2, 55.2208, 58.3667, 57.0667, 57.75, 62.3, 41.25),
  longitude = c(-149.45, -139.6667, -145.5, -148.4667, -162.7278, -134.5833,-135.35, -152.5167, -150.1, -70.0667))
points(data_new_points$longitude, data_new_points$latitude, pch = 19, col = "red", cex = 1.5)  


map("world",add=T)
map("state",add=T)
```

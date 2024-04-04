library(tidyverse)
install.packages("plotly")
install.packages("countrycode")
library(plotly)
library(countrycode)
unicef_indicator_1_5_ <- read_csv("C:/Users/rissu/Downloads/unicef_indicator_1 (5).csv")
unicef_metadata_5_ <- read_csv("C:/Users/rissu/Downloads/unicef_metadata (5).csv")
Data_join <- full_join(unicef_indicator_1_5_, unicef_metadata_5_, by = join_by(country==country, time_period==year))
install.packages("maps")
library(maps)
map_world <- map_data("world")
map_data_join <- full_join(Data_join, map_world, by = c("country" = "region"))
install.packages("ggplot2")
library(ggplot2)

 #map 1 Life expectancy
install.packages("viridis")
library(ggplot2)
library(viridis)
ggplot(map_data_join, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "black") +  
  scale_fill_viridis(name = "Life Expectancy", option = "plasma", na.value = "gray50", guide = "legend") + 
  labs(title = "World Map: Life Expectancy", x = NULL, y = NULL) + 
  theme_minimal()


 #map 2 life expectancy in 1960
data_join_1960 <- Data_join %>%
  filter(time_period == "1960")
map_data_join_1960 <- full_join(data_join_1960, map_world, by = c("country" = "region"))
ggplot(map_data_join_1960, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "black") +  # Add black borders to polygons
  scale_fill_gradient(name = "Life Expectancy", low = "lightblue", high = "darkblue") +  # Use gradient fill colors
  labs(title = "World Map: Life Expectancy in 1960", x = NULL, y = NULL) +  # Add title and remove axis labels
  theme_minimal()



 #time series
timeseries_plot_1 <- Data_join %>%
  ggplot() +
  aes(x = time_period, y = `Life expectancy at birth, total (years)`, color = country) +
  geom_line() +
  labs(title = "**Time Series of Life Expectancy by Country**",
       x = "Time Period",
       y = "Life Expectancy at Birth (years)") +
  scale_color_viridis_d() +  # Using Viridis color palette for countries
  theme_minimal()  # Use a minimal theme for better readability

# Convert country names to continent names
Data_join$continent <- countrycode(Data_join$country, "country.name", "continent")

# Revised time series plot with continents
timeseries_plot_2 <- Data_join %>%
  ggplot() +
  aes(x = time_period, y = `Life expectancy at birth, total (years)`, color = continent) +
  geom_line() +
  labs(title = "**Time Series of Life Expectancy by Continent**",
       x = "Time Period",
       y = "Life Expectancy at Birth (years)") +
  scale_color_brewer(palette = "Set1") +  # Using Set1 color palette for continents
  theme_minimal()  # Use a minimal theme for better readability

# Print the plots
print(timeseries_plot_1)
print(timeseries_plot_2)

ggplotly(timeseries_plot_1)


  


# scatter plot
ggplot(Data_join) +
  aes(x = obs_value, y = `Life expectancy at birth, total (years)`, size = `Population, total`) +
  geom_point(alpha = 0.6, color = "darkblue") +  # Increase transparency and set point color
  labs(
    x = "Observed Value",
    y = "Life Expectancy (Years)",
    title = "Life Expectancy vs. Observed Value",
    size = "Population"
  ) + 
  scale_x_continuous(labels = scales::comma) +  # Format x-axis labels with commas
  scale_size_continuous(range = c(2, 10)) +  # Adjust size range for better visibility
  theme_minimal() +  # Use minimal theme for simplicity
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.title = element_text(size = 10),  # Adjust axis title size
    legend.position = "none"  # Remove legend
  )







 #bar chart
  
  
  
  library(ggplot2)
  library(dplyr)
  
  # Group by continent and time_period, then calculate average life expectancy
  Data_join %>%
    group_by(continent, time_period) %>%
    summarise(avg_life_exp = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE), .groups = "drop") %>%
    
    # Plotting the data as a bar chart
    ggplot(aes(x = reorder(continent, avg_life_exp), y = avg_life_exp, fill = continent)) +
    geom_bar(stat = "identity") +
    
    # Facet by time period
    facet_wrap(~time_period) +
    
    # Adjusting aesthetics and labels
    labs(
      x = "",
      y = "Average life expectancy",
      fill = "Continent",
      title = "Evolution of average life expectancy per continent from 1960 to 2021 per continent"
    ) +
    
    # Applying a classic theme and serif font
    theme_classic() +
    theme(
      text = element_text(family = "serif"),
      axis.text.x = element_blank()
    )
  
  
  library(ggplot2)
  library(dplyr)
  library(countrycode)
  
  # Create a function to assign continents based on countries
  assign_continent <- function(country) {
    # Use countrycode package to get continents from countries
    continent <- countrycode(country, "country.name", "continent")
    return(continent)
  }
  
  # Apply the function to create a new column 'continent'
  Data_join <- Data_join %>%
    mutate(continent = assign_continent(country))
  
  # Now, let's group by the 'continent' column
  Data_join %>%
    group_by(continent, time_period) %>%
    summarise(avg_life_exp = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE), .groups = "drop") %>%
    
    # Plotting the data as a bar chart
    ggplot(aes(x = reorder(continent, avg_life_exp), y = avg_life_exp, fill = continent)) +
    geom_bar(stat = "identity") +
    
    # Facet by time period
    facet_wrap(~time_period) +
    
    # Adjusting aesthetics and labels
    labs(
      x = "",
      y = "Average Life Expectancy",
      fill = "Continent",
      title = "Evolution of Average Life Expectancy per Continent from 1960 to 2021"
    ) +
    
    # Applying a classic theme and serif font
    theme_classic() +
    theme(
      text = element_text(family = "Helvetica", size = 12),
      axis.text.x = element_blank()
    )
  
  
  # pie chart
  ggplot(Data_join, aes(x = "", y = obs_value, fill = continent, label = paste0(continent, ": ", obs_value))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = scales::comma(obs_value)), position = position_stack(vjust = 0.5), color = "white", size = 4) +
    labs(title = "Distribution of unemployed adolescents by Continent", fill = "Continent") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_fill_brewer(palette = "Set3")
  
  
  
  

# Print the plots
print(timeseries_plot_1)
print(timeseries_plot_2)

  
  
  save.image("plots.RData")  
  load("~/plots.RData")
  load("/path/to/plots.RData")
  
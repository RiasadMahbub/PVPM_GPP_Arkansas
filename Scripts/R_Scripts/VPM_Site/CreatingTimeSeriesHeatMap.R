# Assuming you have a data frame called 'df' with a column named 'years'
# 'years' should be in character format

library(dplyr)
library(ggplot2)
# Sample data
df <- data.frame(years = c("2015-2020", "2016-2021"))

# Create a new data frame with individual years
year_counts <- data.frame(year = unlist(lapply(strsplit(df$years, "-"), function(x) as.numeric(x[1]:x[2]))))

# Create a bar chart
barplot(table(year_counts$year), 
        main = "Frequency of Years",
        xlab = "Year",
        ylab = "Frequency",
        col = "skyblue")
library(ggplot2)

# Generate the data and calculate frequencies
df <- data.frame(years = c(
  "2000-2005", "2001-2006", "2002-2007", "2003-2008", "2004-2009", 
  "2005-2010", "2006-2011", "2007-2012", "2008-2013", "2009-2014",
  "2010-2015", "2011-2016", "2012-2017", "2013-2018", "2014-2019",
  "2015-2020", "2016-2021", "2017-2022", "2018-2023", "2019-2024",
  "2020-2025", "2021-2026", "2022-2027", "2000-2006", "2001-2007",
  "2002-2008", "2003-2009", "2004-2010", "2005-2011", "2006-2012",
  "2007-2013", "2008-2014", "2009-2015", "2010-2016", "2011-2017",
  "2012-2018", "2013-2019", "2014-2020", "2015-2021", "2016-2022",
  "2017-2023", "2018-2024", "2019-2025", "2020-2026", "2021-2027",
  "2000-2007", "2001-2008", "2002-2009", "2003-2010", "2004-2011",
  "2005-2012", "2006-2013", "2007-2014", "2008-2015", "2009-2016"
))

year_counts <- data.frame(year = unlist(lapply(strsplit(df$years, "-"), function(x) as.numeric(x[1]:x[2]))))

# Calculate frequencies
year_freq <- table(year_counts$year)

# Create a data frame for the heatmap
heatmap_data <- data.frame(year = as.numeric(names(year_freq)), freq = as.numeric(year_freq))

# Create the heatmap
ggplot(heatmap_data, aes(x = year, y = 1, fill = freq)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") +  # Adjust the color palette as needed
  labs(
    x = "Year",
    y = "",
    fill = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text( vjust = 0.5, hjust = 1),
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.y = element_blank(),   # Remove y-axis labels
    panel.grid.major = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank()   # Remove grid lines
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) 


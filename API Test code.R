# Load library
library(httr)
library(jsonlite)

# The API URL
api_scb <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel"

# Make the API request
response <- GET(api_scb)

# Extract data
json_content <- content(response, as = "text")

# Convert JSON to a data frame
scb_data <- fromJSON(json_content, flatten = TRUE)

# Print the data
print(scb_data)


# View the data 
View(scb_data)
summary(scb_data)
str(scb_data)
dim(scb_data)
head(data)


head(data, 3)

print(data[1:3, ])
View(scb_data$variables)

head(scb_data$variables$code)

View(scb_data$variables$code)


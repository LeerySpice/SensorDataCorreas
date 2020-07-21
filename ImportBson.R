library(mongolite)
library(jsonlite)

# Import MongoDB
BD <- mongo(collection = Sys.Date(), db = "polin")
BD$import(file("Data/polin/measurements.bson"), bson = TRUE)
BD$export(file("Data/Data.json"))

# Data.R
data <- stream_in(file("Data/Data.json"),pagesize = 10)
save(data,file="Data/data.Rda")

# Clear
BD$drop()
BD$disconnect()
rm(BD)
file.remove("Data/Data.json")
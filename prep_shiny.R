library(fs)

#Used file_copy to copy the rds file into the 2020_Primary shinyapp file

file_copy(path = "raw-data/data.rds", new_path = "2020_Primary/data.rds")


# load required libraries
library("assertthat", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
library("bigrquery", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")

# specify project name id (as on Google Cloud Console)
project <- "bq-test1-143216"

# test query: extract 5 entries from a public dataset
sql <- "SELECT year, month, day, weight_pounds FROM [publicdata:samples.natality] LIMIT 5"

# execute query and return the data
query_exec(sql, project = project)

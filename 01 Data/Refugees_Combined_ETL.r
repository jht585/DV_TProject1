require(tidyr)
require(dplyr)
require(ggplot2)

setwd("~/DataVisualization/DV_TProject1/01 Data/csv")


file_path <- "Yearly_Pop_By_Country.csv"    # File 1
#file_path <- "UNHCR_pop_stats.csv"         # File 2

df <- read.csv(file_path, stringsAsFactors = FALSE)

# Replace "." (i.e., period) with "_" in the column names.
names(df) <- gsub("\\.+", "_", names(df))

# Check for measures
str(df)

# File 1
measures <- c("X1960","X1961","X1962","X1963","X1964","X1965","X1966","X1967","X1968","X1969","X1970","X1971","X1972","X1973","X1974","X1975","X1976","X1977","X1978","X1979","X1980","X1981","X1982","X1983","X1984","X1985","X1986","X1987","X1988","X1989","X1990","X1991","X1992","X1993","X1994","X1995","X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014")

# File 2
#measures <- c("Record_Year","Refugees","Asylum_seekers","Returned_refugees","IDPs","Returned_IDPs","Stateless_persons","Others_of_concern","Total_population")

# Get rid of special characters in each column.
# Google ASCII Table to understand the following:
for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

dimensions <- setdiff(names(df), measures)
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
  }
}


# Get rid of all characters in measures except for numbers, the - sign, and period.dimensions
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
  }
}

write.csv(df, paste(gsub(".csv", "", file_path), ".reformatted.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
if( length(measures) > 1 || ! is.na(dimensions)) {
  for(d in dimensions) {
    sql <- paste(sql, paste(d, "varchar2(4000),\n"))
  }
}
if( length(measures) > 1 || ! is.na(measures)) {
  for(m in measures) {
    if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
    else sql <- paste(sql, paste(m, "number(38,4)\n"))
  }
}
sql <- paste(sql, ");")
cat(sql)


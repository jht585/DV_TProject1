require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(tidyr)

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
"""select * from Refugee_Stats;"""')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_cjs2599', PASS='orcl_cjs2599', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE))); 

df <- df %>% select(ASYLUM_COUNTRY, RECORD_YEAR, REFUGEES, ORIGIN_COUNTRY) %>% filter (as.character(ORIGIN_COUNTRY) == "Viet Nam", as.numeric(as.character(REFUGEES)) != "null") %>% group_by(as.numeric(RECORD_YEAR)) %>% arrange(desc(as.character(ASYLUM_COUNTRY)))

View(df)

p1 <- ggplot(df, aes(x=as.character(RECORD_YEAR), y = as.numeric(as.character(REFUGEES)), fill = as.character(ASYLUM_COUNTRY))) + geom_bar(stat = "identity")

p1

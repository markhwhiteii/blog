## data pulled on 2018-01-01 14:05:00
library(tidyverse)
brands <- c("barclay", "belair", "capri", "carlton", "gpc", "lucky strike",
            "misty", "monarch", "now", "tareyton", "vantage", "viceroy")
brands <- paste(brands, "cigarettes")
dat <- data.frame(date = NA, keyword = NA, hits = NA)
for (b in brands) {
  dat <- gtrendsR::gtrends(
    keyword = b, 
    geo = "US", 
    time = "2008-10-16 2016-10-16"
  )$interest_over_time[, 1:3] %>% 
    bind_rows(dat, .)
}
dat <- dat[-1, ]
dat$keyword <- gsub(" cigarettes", "", dat$keyword)

datwide <- spread(dat, keyword, hits) %>% 
  as.data.frame() %>% 
  `rownames<-`(.$date)
datwide <- datwide[, !colnames(datwide) %in% "date"]
datwide <- datwide[, sort(names(datwide), decreasing = TRUE)]

write_csv(dat, "viceroy.csv")
write_csv(datwide, "viceroy_wide.csv")

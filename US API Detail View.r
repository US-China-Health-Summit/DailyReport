# US Hospitalize and Test Results Detail View

# folder where to save the output
folder = ""
setwd(folder)

library(httr)
library(jsonlite)
state_detail_url = 'https://covidtracking.com/api/states/daily'
r = GET(state_detail_url)
if (r$status != 200) stop(paste("BAD API RETURN!"))
data_us_states = fromJSON(paste(rawToChar(r$content), collapse=""))

data_us_states$date = as.Date(as.character(data_us_states$date), format = "%Y%m%d")
data_us_states$hospitalized_pct=data_us_states$hospitalized/data_us_states$positive
data_us_states$positive_rate=data_us_states$positive/data_us_states$totalTestResults
data_us_states$positive_rate_day=data_us_states$positiveIncrease/data_us_states$totalTestResultsIncrease
report_date = as.character( max(data_us_states$date))
write.csv(data_us_states, paste(report_date,"table_US_details_data_all.csv"), row.names = F)

data_us_states_latest = data_us_states[data_us_states$date == report_date,]
data_us_states_latest = data_us_states_latest[order(data_us_states_latest$positive, decreasing = T), ]
write.csv(data_us_states, paste(report_date,"table_US_details_data_today.csv"), row.names = F)

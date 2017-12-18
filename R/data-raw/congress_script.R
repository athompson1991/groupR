rollcalls <- read.csv("/users/alex/Desktop/HSall_rollcalls.csv", stringsAsFactors = F)
rollcalls$date <- as.Date(rollcalls$date)
rollcalls$first_of_month <- lubridate::floor_date(rollcalls$date, unit = "month")
rollcalls <- rollcalls[ ,c("date", "first_of_month", "congress",
                           "chamber", "rollnumber", "clerk_rollnumber", "session",
                           "bill_number", "vote_result", "vote_desc",
                           "vote_question", "dtl_desc")]

devtools::use_data(rollcalls, overwrite = T)

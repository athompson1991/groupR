########
# Core #
########

name <- c("Joe","Bob","Janet","Heather","John","Winston","Melissa","Monica")
company <- c("University of Fakesville" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"Coal Mine Incorporated" ,"University of Fakesville" ,"University of Fakesville")
party <- c("Whig" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Whig" ,"Libertarian" ,"Libertarian")
color <- c("Orange", "Blue", "Orange", "Green", "Green", "Blue", "Red", "Red")
salary <- c(100,70,80,80,60,100,120,80)

main_df <- data.frame(name, company, party, color, salary, stringsAsFactors = F)


###############
# Time series #
###############

set.seed(12345)

dates <- seq(as.Date("2017-01-01"), length.out = 100, by = "day")
companies <- c("company_a", "company_b", "company_c")
states <- c("WA", "CA")
department <- c("sales", "marketing")
budget <- abs(rnorm(1200))

time_df <- cbind(expand.grid(companies, states, department, dates), budget)
names(time_df) <- c("companies", "states", "department", "dates", "budget")


###############
# Kaggle Data #
###############

raw_download_data <- read.csv("~/.kaggle/datasets/aparnashastry/building-permit-applications-data/Building_Permits.csv", stringsAsFactors = F)
permits <- raw_download_data[ ,c("Permit.Number", "Permit.Type", "Permit.Type.Definition", "Current.Status", "Current.Status.Date", "Issued.Date", "Neighborhoods...Analysis.Boundaries", "Existing.Use", "Existing.Construction.Type.Description")]
permits <- permits[-(which(duplicated(permits$Permit.Number))), ]
colnames(permits) <- c("permit_number", "type", "type_desc", "status", "status_date", "issued_date", "location", "existing_use", "existing_const_type")
permits$issued_date <- as.Date(permits$issued_date, "%m/%d/%Y")
permits$status_date <- as.Date(permits$status_date, "%m/%d/%Y")
permits <- permits[!is.na(permits$issued_date), ]
permits$issued_month <- lubridate::floor_date(permits$issued_date, unit="month")
permits$existing_use[permits$existing_use==''] <- NA
permits <- permits[!is.na(permits$existing_use), ]


# Save the data

devtools::use_data(main_df, time_df, internal = T, overwrite = T)
devtools::use_data(permits, overwrite=T, compress="bzip2")
rm(list = ls())

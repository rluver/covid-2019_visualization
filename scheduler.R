require("taskscheduleR")




# taskscheduleR
# data load

taskscheduler_create(taskname = "",
                     rscript = "covid-2019_crawling.R",
                     schedule = "DAILY",
                     starttime = "12:00",
                     startdate = Sys.Date()
                     )

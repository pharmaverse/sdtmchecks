test_that("Returns true when no errors present", {
  
  ### identify current WHODrug version
  
  t <-  Sys.Date()
  y <-  substring(t,1,4)
  
  may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
  date1 <-  may[weekdays(may) == "Monday"]
  
  nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
  date2 <- nov[weekdays(nov) == "Monday"]
  
  # Work out the main WHODrug version of the year eg March 1, 2021
  
  # if today is before the first Monday of May, WHODrug version is the previous version, ie SEPTEMBER 1, 2020
  # if today is after the first Monday of November, WHODrug version is the next version, ie SEPTEMBER 1, 2021
  whodrug_ver <- case_when(t < date1 ~ paste0("SEPTEMBER 1, ", as.numeric(y)-1),
                           t >= date1 & t< date2 ~ paste0("MARCH 1, ", as.numeric(y)),
                           t>= date2 ~ paste0("SEPTEMBER 1, ", as.numeric(y))
                           
  )
  
  version <- paste0("WHODRUG GLOBAL B3 ",whodrug_ver)
  
  
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "CMDICT",
    TSVAL = version,
    TSVAL2 = ""
  )
  
  expect_true(check_ts_cmdict(TS1))
})

test_that("Returns false when errors present - 1", {
  
  t <-  Sys.Date()
  y <-  substring(t,1,4)
  
  may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
  date1 <-  may[weekdays(may) == "Monday"]
  
  nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
  date2 <- nov[weekdays(nov) == "Monday"]
  
  # Work out the main WHODrug version of the year eg March 1, 2021
  
  # if today is before the first Monday of May, WHODrug version is the previous version, ie SEPTEMBER 1, 2020
  # if today is after the first Monday of November, WHODrug version is the next version, ie SEPTEMBER 1, 2021
  whodrug_ver <- case_when(t < date1 ~ paste0("SEPTEMBER 1, ", as.numeric(y)-1),
                           t >= date1 & t< date2 ~ paste0("MARCH 1, ", as.numeric(y)),
                           t>= date2 ~ paste0("SEPTEMBER 1, ", as.numeric(y))
                           
  )
  
  version <- paste0("WHODRUG GLOBAL B3 ",whodrug_ver)
  
  TS2 <- data.frame(
    STUDYID = 2,
    TSPARMCD = "CMDICT",
    TSVAL = "",
    TSVAL1 = version
  )
  
  
  expect_false(check_ts_cmdict(TS2))
  
})


test_that("Returns false when errors present - 2", {
  
  TS3 <- data.frame(
    STUDYID = 3,
    TSPARMCD = "CMDICT",
    TSVAL = ""
  )
  
  expect_false(check_ts_cmdict(TS3))
  
})

test_that("Returns false when errors present - 3", {
  
  TS4 <-data.frame(
    STUDYID = 4,
    TSPARMCD = "AEDICT",
    TSVAL = ""
  )
   
  
  expect_false(check_ts_cmdict(TS4))
})

test_that("Returns false when errors present - 4", {
  
  TS5 <- data.frame(
    STUDYID = 5,
    TSPARMCD = "CMDICT",
    TSVAL = "meddra 24.0",
    TSVAL2 = ""
  )
  
  expect_false(check_ts_cmdict(TS5))
})


test_that("Returns false when errors present - 5", {
  t <-  Sys.Date()
  y <-  substring(t,1,4)
  
  may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
  date1 <-  may[weekdays(may) == "Monday"]
  
  nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
  date2 <- nov[weekdays(nov) == "Monday"]
  
  # Work out the main WHODrug version of the year eg March 1, 2021
  
  # if today is before the first Monday of May, WHODrug version is the previous version, ie SEPTEMBER 1, 2020
  # if today is after the first Monday of November, WHODrug version is the next version, ie SEPTEMBER 1, 2021
  whodrug_ver <- case_when(t < date1 ~ paste0("SEPTEMBER 1, ", as.numeric(y)-1),
                           t >= date1 & t< date2 ~ paste0("MARCH 1, ", as.numeric(y)),
                           t>= date2 ~ paste0("SEPTEMBER 1, ", as.numeric(y))
                           
  )
  
  version <- paste0("WHODRUG vGLOBAL B3 ",whodrug_ver)
  
  
  TS6 <- data.frame(
    STUDYID = 6,
    TSPARMCD = "CMDICT",
    TSVAL = version,
    TSVAL2 = ""
  )
  
  expect_false(check_ts_cmdict(TS6))
  
})


test_that("Returns false when errors present - 6", {
  
  ### identify current WHODrug version
  
  t <-  Sys.Date()
  y <-  substring(t,1,4)
  
  may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
  date1 <-  may[weekdays(may) == "Monday"]
  
  nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
  date2 <- nov[weekdays(nov) == "Monday"]
  
  # Work out the main WHODrug version of the year eg March 1, 2021
  
  # if today is before the first Monday of May, WHODrug version is the previous version, ie SEPTEMBER 1, 2020
  # if today is after the first Monday of November, WHODrug version is the next version, ie SEPTEMBER 1, 2021
  whodrug_ver <- case_when(t < date1 ~ paste0("SEPTEMBER 1, ", as.numeric(y)-1),
                           t >= date1 & t< date2 ~ paste0("MARCH 1, ", as.numeric(y)),
                           t>= date2 ~ paste0("SEPTEMBER 1, ", as.numeric(y))
                           
  )
  
  version <- paste0("WHODRUG GLOBAL B3 ",whodrug_ver)
  
  
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "CMDICT",
    TSVAL = version,
    TSVAL2 = ""
  )
  
  expect_false(check_ts_cmdict(rbind(TS1,TS1)))
  
})


test_that("Returns false when expected column not present", {
  
  ### identify current WHODrug version
  
  t <-  Sys.Date()
  y <-  substring(t,1,4)
  
  may <- seq(as.Date(paste0(y, "-05-01")),as.Date(paste0(y, "-05-07")),by="1 day")
  date1 <-  may[weekdays(may) == "Monday"]
  
  nov <- seq(as.Date(paste0(y, "-11-01")),as.Date(paste0(y, "-11-07")),by="1 day")
  date2 <- nov[weekdays(nov) == "Monday"]
  
  # Work out the main WHODrug version of the year eg March 1, 2021
  
  # if today is before the first Monday of May, WHODrug version is the previous version, ie SEPTEMBER 1, 2020
  # if today is after the first Monday of November, WHODrug version is the next version, ie SEPTEMBER 1, 2021
  whodrug_ver <- case_when(t < date1 ~ paste0("SEPTEMBER 1, ", as.numeric(y)-1),
                           t >= date1 & t< date2 ~ paste0("MARCH 1, ", as.numeric(y)),
                           t>= date2 ~ paste0("SEPTEMBER 1, ", as.numeric(y))
                           
  )
  
  version <- paste0("WHODRUG GLOBAL B3 ",whodrug_ver)
  
  
  
  TS1 <- data.frame(
    STUDYID = 1,
    TSPARMCD = "CMDICT",
    TSVAL = version,
    TSVAL2 = ""
  )
  
  TS1$TSVAL <- NULL
  
  expect_false(check_ts_cmdict(TS1))
  
})
 
 
 
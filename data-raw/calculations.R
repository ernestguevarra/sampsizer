child <- foreign::read.dta(file = "data-raw/MMKR71DT/MMKR71FL.DTA")

waz <- child$hw5

waz[waz == 9998] <- NA

waz <- waz / 100


summary(waz[child$v024 == "kayin"])
summary(waz[child$v024 == "kayah"])

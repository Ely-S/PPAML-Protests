clear
est clear
import delimited "/home/eli/progs/ppaml/PakData.csv"
gen period = date(start, "MDY")
replace period = period - 14974
drop start end
sum

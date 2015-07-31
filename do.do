// Allergies = DiscreteExpenonential(...) //  categories of allergies
Allergy_n = Flip(BaseRateOfAllergy) ... n
//time till event for each allergy
tte = Allergy_n * Normal(time_delay) 

clear
est clear
import delimited "/home/eli/progs/ppaml/PakData.csv"
gen period = date(start, "MDY")
replace period = period - 14974
drop start end
sum

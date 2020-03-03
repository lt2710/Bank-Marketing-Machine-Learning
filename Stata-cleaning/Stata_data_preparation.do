*set up working directory. all data is stored and updated here.
cd "C:\Users\Tianl\Downloads"

*code chunk below output a subset of claims with only IPPS/LTCH and count LTCH cases
*load claim data
use "pseudo_inpatient_claims.DTA", replace
* extract hospital type indicator
tostring  prvdr_num, replace
gen hospitals = regexs(1) if regexm(prvdr_num,"([0-9]?[0-9]?[0-9]?[0-9])$")
destring hospitals, replace
recode hospitals (0001/0879 = 1) (2000/2299 = 2) (else=0), gen(hospitals_type)
* change date (i can use a for loop but im lazy...)
gen adm_date = date( clm_admsn_dt , "DMY")
format adm_date %td
gen dsc_date = date( nch_bene_dschrg_dt , "DMY")
format dsc_date %td
* keep only relevant records (IPPS/LTCH)
drop if hospitals_type == 0
keep desy_sort_key claim_no hospitals_type adm_date dsc_date
*count total LTCH cases
count if hospitals_type==2
*So, there are total 8810 LTCH cases. 
*save data for now
save "pseudo_inpatient_claims_V2.DTA", replace

*the code chunk below output claim records with more than total 3 days stay in ICU
*load revenue data
use "pseudo_inpatient_rev_cntr.DTA", replace
recode rev_cntr (200/219 = 1) (else=0), gen(icu)
gen icu_days= icu * rev_cntr_unit_cnt
*keep only ICU-relevant revenue items
drop if icu==0
*count total ICU days for a given claim and patient
*here, if a given patient has more than once same-day stay in a claim, the stay time is regarded as only once
collapse (sum) icu_days_total = icu_days,by (desy_sort_key claim_no)
*keep only claims with more than 3 days stay in ICU
drop if icu_days_total<3
*save data for now
save "pseudo_inpatient_rev_cntr_V2.DTA", replace

*merge both datasets
use "pseudo_inpatient_claims_V2.DTA", replace
merge m:m desy_sort_key claim_no using "pseudo_inpatient_rev_cntr_V2.DTA"
*sort claims in timely order 
sort desy_sort_key claim_no
*compuate the gap between an admission and last IPPS discharge
gen gap = adm_date - dsc_date[_n-1]
*count LTCH cases with same day or next day admission from IPPS
count if  hospitals_type==2 & gap<=1
*So, eligible LTCH cases are 6611.
*75 percent of cases discharged from a LTCH in 2016 met the new criteria

*Potential bias/unaccounted cases: patients with consecutive LTCH admissions. if i have more time i can write more to improve. 

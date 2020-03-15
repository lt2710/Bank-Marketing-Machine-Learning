## Simulation and analysis of medical claim data
Use R to create three simulated tables: Patient, Encounter, and Diagnosis in a SQLite instance
Then use both R and SQL to achieve the same task: 
 - Code up a SQL query which returns the following: The number of inpatient admissions per patient_id for all patients between the ages of 18 and 64 who have an ICD-9 diagnosis code 290 - 319, and have an inpatient admission between 1/1/2017 and 12/31/2017.  
 - Create a person level table with the number of inpatient, outpatient, and ED visits per person, as well as flags for people who have the following diagnoses: bipolar, or schizophrenia.

See: 
[HTML report](https://github.com/lt2710/Pet-projects/blob/master/simulate-sql-claims/simulate-analysis-report.html)
[R codes to simulate and analyze data](https://github.com/lt2710/Pet-projects/blob/master/simulate-sql-claims/simulate-and-make-analysisanalysis.R)
[SQL](https://github.com/lt2710/Pet-projects/blob/master/simulate-sql-claims/make-analysis.sql)

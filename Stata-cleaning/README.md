## Background
In 2016, the Medicare program began to phase in patient level criteria for long-term acute care hospitals (LTCHs).  Under the new patient criteria, a LTCH would be eligible to receive its full prospective payment upon discharge of the patient if the case met the following criteria:

•	Immediately prior to admission to the LTCH, the patient was discharged from a hospital paid under the hospital Inpatient Prospective Payment System (IPPS); and 

•	While at the IPPS hospital immediately preceding LTCH admission, the patient spent 3 or more days in an intensive care unit (ICU).
For cases that do not meet these criteria, LTCH receives payment lower than its full prospective payment.

## Task  
Write a program in Stata that answers the following questions:

•	What percent of cases discharged from a LTCH in 2016 met the new criteria?
We provide a set of randomly-generated data files, consisting of a claims file and a revenue center file, designed to resemble actual Medicare claims data. The claims file includes information on treatment setting, admission dates, and discharge dates. A patient identifier uniquely identifies patients and a claim number uniquely identifies stays. The revenue center file includes information on utilization of various products and services for each hospital stay. The revenue center code identifies the type of product or service in question and the revenue center unit count identifies the quantity of the product or service consumed. The claim files and revenue center file can be linked using the patient identifier and claim number.  Please note that the claims file includes one claim per observation whereas the revenue center file may have multiple observations per claim.

## Relevant Information
•	LTCHs and IPPS can be identified in the claims file based on the provider number (see Table 1 below).

•	An IPPS hospital stay is considered to be “immediately prior admission to an LTCH” if the discharge date of the IPPS hospital stay is the same day as admission to the LTCH or next day.

•	ICU information is available on the claim based on revenue center code.  A revenue center code equal to 201 through 219 refers to an ICU service.  The revenue center units correspond to days with the corresponding revenue center. 

•	The contents of the claims and revenue center files are provided below.  

# experiment setup notes

# reporters (constant)

count susceptibles
count exposeds
count symptomatics
count asymptomatics
count recovereds
count_staying-at-home

num-contacts

lockdown-active?
shielding-active?
protections-active?

get-age-bracket-data "0-18" "infected"
get-age-bracket-data "19-39" "infected"
get-age-bracket-data "40-49" "infected"
get-age-bracket-data "50-59" "infected"
get-age-bracket-data "60-69" "infected"
get-age-bracket-data "70-79" "infected"
get-age-bracket-data "80+" "infected"

get-age-bracket-data "0-18" "deceased"
get-age-bracket-data "19-39" "deceased"
get-age-bracket-data "40-49" "deceased"
get-age-bracket-data "50-59" "deceased"
get-age-bracket-data "60-69" "deceased"
get-age-bracket-data "70-79" "deceased"
get-age-bracket-data "80+" "deceased"


# base parameters in logical order (ver 2.0 29/10/2020)

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

# new tests

## no-controls

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" true]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-lockdown-compliance-0100

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" true]
["lockdown-threshold" 0.1]
["lockdown-compliance" 0 10 20 30 40 50 60 70 80 90 100]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-shield-compliance-0100

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" true]
["shield-threshold" 0.1]
["shield-compliance" 0 10 20 30 40 50 60 70 80 90 100]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-protections-strength-0100

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 0 10 20 30 40 50 60 70 80 90 100]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-protections-compliance-0100

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 50]
["protections-compliance" 0 10 20 30 40 50 60 70 80 90 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-test-coverage-combo-0100

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" true]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 0 10 20 30 40 50 60 70 80 90 100]
["test-coverage-asym" 0 10 20 30 40 50 60 70 80 90 100]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-isolation-compliance-0100

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 4]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" true]
["isolation-sym-threshold" 0.1]
["isolation-compliance-sym" 0 10 20 30 40 50 60 70 80 90 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## vary-lockdown-threshold-01

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" true]
["lockdown-threshold" 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 3]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 1]
["protections-strength" 50]
["protections-compliance" 100]

["test-and-trace?" false]
["testtrace-threshold" 0]
["contact-history-length" 7]
["test-coverage-sym" 100]
["test-coverage-asym" 100]
["contacts-traced" 100]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 100]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 100]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## all-controls

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" true]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" true]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" true]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" true]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## ld-only

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" true]
["lockdown-threshold" 0.1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 0.1]
["protections-strength" 50]
["protections-compliance" 75]

["test-and-trace?" false]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## sv-only

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" true]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 0.1]
["protections-strength" 50]
["protections-compliance" 75]

["test-and-trace?" false]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## pp-only

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" false]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## tt-only

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 0.1]
["protections-strength" 50]
["protections-compliance" 75]

["test-and-trace?" true]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## is-only

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 0.1]
["protections-strength" 50]
["protections-compliance" 75]

["test-and-trace?" false]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" true]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## sv-is

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" true]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" false]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" true]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## sv-is-dld

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" true]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" true]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" false]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" false]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" true]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## pp-tt

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" true]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## pp-tt-dld

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" true]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" false]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" true]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]

## pp-tt-sv

["duration" 1]
["initial-infected" 0.1]
["allow-exogenous-infections?" true]
["exogenous-infection" 1]
["visual-elements?" false]

["min-radius" 2]
["daily-contacts" 50]

["p-infect-base" 10]
["asym-infectiousness" 30]

["lose-immunity?" false]
["min-immunity-duration" 180]

["recovery-mean" 20.5]
["recovery-stdev" 6.7]
["death-mean" 16]
["death-stdev" 8.21]
["incubation-mean" 1.6]
["incubation-stdev" 1.4]

["imposed-lockdown?" false]
["lockdown-threshold" 1]
["lockdown-compliance" 75]

["shield-vulnerable?" true]
["shield-threshold" 0.25]
["shield-compliance" 50]

["personal-protections?" true]
["protections-threshold" 0.1]
["protections-strength" 70]
["protections-compliance" 75]

["test-and-trace?" true]
["testtrace-threshold" 0.25]
["contact-history-length" 7]
["test-coverage-sym" 25]
["test-coverage-asym" 1]
["contacts-traced" 75]
["isolation-compliance-tested" 100]
["isolation-compliance-traced" 75]

["isolation-symptomatics?" false]
["isolation-sym-threshold" 0]
["isolation-compliance-sym" 75]

["isolation-duration-case" 7]
["isolation-duration-contact" 14]

["min-pxcor" 0]
["min-pycor" 0]
["max-pxcor" 99]
["max-pycor" 99]
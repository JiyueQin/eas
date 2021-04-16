
# eas

## Overview

Package eas for [Einstein Aging
Study](https://einsteinmed.org/departments/neurology/clinical-research-program/eas/)

The EAS is a longitudinal study of community-residing individuals, aged
70 and older, in the Bronx, New York, a racially and ethnically diverse
urban setting. In May 2017, the EAS added the neuropsychological battery
of the Uniform Data Set (UDSNB 3.0) to the in-person assessment battery.
Below is the list of UDSNB 3.0 tests, the corresponding variables and
item numbers on the UDSNB 3.0 form. [Click here for the defintion of
each
variable](https://files.alz.washington.edu/documentation/uds3-ivp-c2.pdf)

  - Montreal Cognitive Assessment(MoCA)

  - Craft Story 21 Recall(Immediate)

  - Benson Complex Figure Copy

  - Number Span Test: Forward

  - Number Span Test: Background

  - Category Fluency

  - Trail Making Test

  - Craft Story 21 Recall(Delayed)

  - Benson Complex Figure Recall

  - Multilingual Naming Test (MINT)

  - Verbal Fluency: Phonemic Test

Current package provides a tool to calculate demographically adjusted
z-scores and impairment indicators for UDS3 neuropsychological tests.
You can specify if you want to use the norms from Einstein Aging
Study(EAS) or National Alzheimer’s Coordinating Center(NACC).

  - EAS norms

obtained from 225 cognitively normal older adults in EAS

  - NACC norms

obtaied from comparable data of 5,031 participants in the NACC database

Use norms regression estimates to generate gender, age, education, black
race adjusted norms. The mean is calculated as:

\[ \hat{Y}=\hat{b_0}+ \hat{b_1}*female + \hat{b_2}*(age-77) + \hat{b3}*(educyrs-16)+ \hat{b4}*black\]

For trail A1 and trail B1, as a higher score indicates a worse
performance, z-score is calculated as

\(-(Y - \hat{Y})/\sigma\).

For all the other cognitive tests where a higher score indicates a
better performance, z-score is calculated as

\((Y - \hat{Y})/\sigma\).

Thus, an impairment can be defined as a z-score below a certain cutoff,
for example, -1 or -1.5 are often used. Using a cutoff of -1 means that
a subject is defined as impaired on this test if their performance is
1SD below the population mean.

The indicator of impairment is generated as:
\[\text{impair} = 1 \;\;\;\text{if z-score} \le \text{cutoff}\]

## Installation

``` r
install.packages("devtools")
devtools::install_github("JiyueQin/eas")
```

## Usage

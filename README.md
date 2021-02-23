---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
# "opitools"

An R-package for analyzing Opinions in Big Text Document

### Description

The `opitools` is an opinion analytical toolset designed for assessing cross-impacts of opinion expressed on multiple subjects in an opinion-based text documents (OTD) from a social media platform. An OTD (input as `textdoc`) should composed of individual text records on a specific subject (A). An example of an OTD is a collection of Twitter posts concerning a specific topic or hashtag. Any other subjects referenced in relation to the primary subject A can be referred to as secondary subjects, and they can be identified through the keywords used in the text records.  In order to download a Twitter OTD for a defined geographical area, please see the manual of `rtweet` package [(Kearney, 1019)](https://doi.org/10.21105/joss.01829) for details. In the article [adepeju, M. and Jimoh, F. (2021)](https://osf.io/preprints/socarxiv/c32qh/), we described how to deploy `opitools` in order to answer a real-life research question, such as 'what are the impacts of `COVID-19 pandemic` (secondary subject) on the public opinion concerning policing (primary subject) across England and Wales?' Please note: A freshly downloaded OTD generally requires some data cleaning exercises in order to remove unwanted texts, such as duplicates, punctuations, hashtags, emojis, and stop words.

### Installation from `CRAN`

From an R console, type:

```{r}
#{r, echo=TRUE, message=FALSE, eval=TRUE}
#install.packages("opitools")
library(opitools)

```

To install the development version of the package, type
`remotes::install_github("MAnalytics/opitools")`. Please, report any
installation problems in the issues.

### Example usage

Below is an example usage of the main `opitools` function, `opi_impact`. Given an `OTD` consisting of public tweets concerning the police/policing during the COVID-19 pandemic, across a geographical area, the following is an example of how `opitools` can be used to estimate the opinion score and also answer the afore-stated research question. In this example, I will use a fake OTD, namely `policing_otd`, available in the `data/` directory.

### Importing the dataset

As soon as the package is installed, the data will automatically be available, and can be assessed by typing: 

```r

> policing_otd

#to preview the text document, type:
head(policing_otd)

```

### Performing analysis

Assuming that we want to assess the impacts of a secondary subject B inherent in the document on the original subject A (for which OTD is downloaded), we need to first identify keywords that are used to reference the subject B in the OTD. A user can employ any relevant analytical approach in order to identify such keywords, e.g. using a frequency analysis. Alternatively, a user can define those keywords manually. For example, keywords that relate to the COVID-19 pandemic (as a secondary subject) of the `policing_otd` data include words, such as 'covid-19', 'coronavirus', 'pandemic' and their variations. We provide a full list of these keywords in the package, and can be accessed by typing:

```r

> covid_keys 

#          keys
#1     pandemic
#2    pandemics
#3     lockdown
#4    lockdowns
#5       corona
#6  coronavirus
#7        covid
#8      covid19
#9     covid-19
#10       virus
#11     viruses
#12  quarantine
#13      infect
#14     infects
#15   infecting
#16    infected

```

```r

#Running the analysis

results <- opi_impact(textdoc = policing_otd, sec_keywords=covid_keys, metric = 1,
                       fun = NULL, nsim = 99, alternative="two.sided",
                       pplot = TRUE, quiet=FALSE)
                       
print(results)

$test
[1] "Test of significance (Randomization testing)"

$criterion
[1] "two.sided"

$exp_summary
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -8.240  -5.880  -5.880  -5.028  -3.530  -1.180 

$p_table


|observed_score |S_beat |nsim |pvalue    |signif |
|:--------------|:------|:----|:----|:------|
|-5.88          |56     |99   |0.57 |'      |

$p_key
[1] "0.99'"   "0.05*"   "0.025**" "0.01***"

$p_formula
[1] "(S_beat + 1)/(nsim + 1)"

#......


```

### References
1. Adepeju, M. and Jimoh, F. (2021). An Analytical Framework for Measuring Inequality in the Public Opinions on Policing – Assessing the impacts of COVID-19 Pandemic using Twitter Data. [https://doi.org/10.31235/osf.io/c32qh](https://osf.io/preprints/socarxiv/c32qh/)

2. Kearney MW (2019). “rtweet: Collecting and analyzing Twitter data.” Journal of Open Source Software, 4(42), 1829. [doi: 10.21105/joss.01829](https://doi.org/10.21105/joss.01829)

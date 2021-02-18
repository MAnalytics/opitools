# "opitools"

An R-package for analyzing Opinions in Big Text Doc

### Description

The `opitools` is an opinion analysis toolset designed for analyzing opinion-based text documents (OTD), such as social media data. An OTD (input as `textdoc`) should comprised of individual text records on a specific subject. In essence, the central theme of an OTD is that it contains a collection of text records expressing certain sentiments concerning a subject (A). An example of an OTD is a collection of Twitter posts concerning a specific topic or hashtag. When downloaded, an OTD can be analyzed to assess public opinion concerning the tagged topic. In `adepeju, M. and Jimoh, F. (2021)`, we described how to download an OTD and analyze it in order to answer real-life questions. For instance, 'what are the impacts of `COVID-19 pandemic` on the public opinion concerning policing across England and Wales?' In other words, the `opitools` can allow a user to assess the impacts of an(other) underlying subject (B) on the original subject (A). Note: A freshly downloaded OTD generally requires some data cleaning exercises in order to remove unwanted texts, such as duplicates, punctuations, hashtags, emojis, and stop words.

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

Below is an example usage of the main `opitools` function, `opi_impact`. Given an `OTD` consisting of public tweets concerning the police/policing across a geographical area, the following is an example of how `opitools` can be used to estimate the opinion score and also answer the afore-stated question. We will use a fake OTD, namely `police_otd`, available in the `data/` directory.

### Importing the dataset

```r
#Upon installing the package, the dataset can be 
#assessed by typing:

> policing_otd

#preview the data
head(policing_otd)

```

### Performing analysis

Assuming that we want to assess the impacts of a secondary subject B on the original subject A (for which OTD is downloaded), we need to first identify keywords that relate to the former from the OTD. A user can employ any relevant analytical means in order to identify such keywords, e.g. using frequency analysis or `tf_idf` metric (Silge, J. and Robinson, D. 2016). Alternatively, a user can define those keywords manually. For example, keywords that relate to the COVID-19 pandemic (as a secondary subject) of the `policing_otd` data include words, such as 'covid-19', 'coronavirus', 'pandemic' and their variations. We provide a full list of these keywords in the package. They can be accessed by typing:

```r
#accessing covid-19 related keywords

covid_keys 

```

```r

#Running the analysis

results <- opi_impact(textdoc = polic, sec_keywords=covid_keys, metric = 1,
                       fun = NULL, nsim = 99, alternative="two.sided",
                       pplot = FALSE)
                       
#print the result
print(results)
#......

#run in the console and paste result here..

```

### References
1. Adepeju, M. and Jimoh, F. (2021). An Analytical Framework for Measuring Inequality in the Public Opinions on Policing – Assessing the impacts of COVID-19 Pandemic using Twitter Data. https://doi.org/10.31235/osf.io/c32qh


2. Silge, J. and Robinson, D. (2016). tidytext: Text mining and analysis using tidy data principles in R. Journal of Open Source Software, 1, 37.

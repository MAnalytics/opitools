# opitools

An R-package for opinion analysis of social media data

###Description

The `opitools` is an opinion analysis toolset designed to explore the opinions inherent in an opinion-based text documents (OTD), such as social media data. An input OTD (read as `textdoc`) should comprised of individual text records, such as tweets, Facebook post, or Youtube video comments. The central theme of an OTD is that they often express certain sentiments concerning a subject (A). An example of an OTD is the text of Twitter posts concerning an #hashtag or a topic. When downloaded, an OTD can be analyzed to assess public opinion on the tagged topic. In \code{Adepeju, M. and Jimoh, F. 2021}, we described how to download an OTD and analyze it in order to answer real-life questions. For example, 'what is the impacts of `COVID-19 pandemic` on the public opinion concerning the policing?' In essence, the `opitools` can allow a user to assess the impacts of an underlying subject (B) within OTD on the original subject (A). Note: most OTD often require that a user carry out some data cleaning exercise first for a better result. For Twitter data, as an example, a user needs to remove unwanted texts, such as duplicates, punctuations, hashtags, emojis, and stop words.

### Installation from `CRAN`

From an R console, type:

``` r
#install.packages("opitools")
library(opitools)

```

To install the development version of the package, type
`remotes::install_github("MAnalytics/opitools")`. Please, report any
installation problems in the issues.

### Example usage of `` function:

Given an `OTD` representing public tweets on policing, the following is an example of how `opitools` can be used to estimate the public opinion concerning policing and answer the aforementioned question. We will use a fake text document (OTD) for this demonstration as shown below. The dataset
(named `police_otd.rda`) stored in the `data/` directory.

### Importing the dataset

```r
#Modify and set data directory:

setwd("C:/Users/monsu/Documents/GitHub/opitools/data/")

load("./policing_otd.rda")

#preview
head(policing_otd)

```

### Perform analysis

Assuming that we want to assess the impact, we need to first define keywords that will enable us to identify tweets that express sentiments about COVID-19 pandemic in relation to policing. This is achieved by manually defining COVID-19 pandemic-related keywords. Alternatively, a user can determine such keywords using any analytical metric of choice, such as, `tf_idf` metric (Silge & Robinson, 2016). Here, we manually define the keywords as follows:

```r
#define covid-19 related keywords

covid_keys <- c("pandemic","pandemics","lockdown","lockdowns","corona","coronavirus",
          "covid", "covid19", "covid-19", "virus", "viruses", "quarantine", "infect",
          "infects","infecting", "infected')

#Run the analysis

impact_results <- opimpact_ba(textdoc = policing_otd,  keywords_b = covid_keys, nsim = 99)

#print results
print(impact_results)
```

The print out above shows the 

### Plotting

To visualize the proportion of opinions relating to each subject, a user can employ `plot_opi.R` function with `opimpact_b` object as input.

```r
plot_opi(impact_results)
```

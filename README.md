---
output:
  html_document: default
---
# "Opitools"

An R-package for analyzing Opinions in Big Digital Text Document (DTD)

## Description

The `opitools` is an R package for exploring a digital text document (DTD) as well as performing the impact analysis of the opinions expressed by the DTD. The package is particularly suited for opinion-based DTD, such as individual-level posts (e.g. commentaries on Twitter or Facebook) and comments (as in an online product reviews). First, an `exploratory` function `word_imp` can be used to identify words relating to different themes (or subjects) that are referenced in a DTD. The `opi_impact` function can then be utilized to investigate whether an identified/specified theme (or subject) impacts the overall opinion expressed by the document. The potentials of `opitools` for application across a wide range of domains is described briefly here (see the `vignette` for details). 

[Click here](https://manalytics.github.io/opitools/index.html) to visit the built website for the package.

## Installation from `CRAN`

From an R console, type:

```{r}
#install.packages("opitools")
library(opitools)

```

To install the developmental version of the package, type:
`remotes::install_github("MAnalytics/opitools")`. (Note: `remotes` is an extra package that needed to be installed prior to the running of this code). 

Please, report any installation problems in the issues.

## Example usage

Below is an example usage of how `opitools` can be employed to identify themes (or subjects) from a DTD and then deployed to perform opinion impact analysis. 


### Importing the dataset

The `policing_dtd` - a DTD comprising Twitter posts about police/policing in a neighbourhood, will be used in this demonstration.

```r
> data(policing_dtd)

```

### Identify themes (subjects)

Utilize `word_imp` function to highlights terms or words in accordance to their importance in the DTD. Through visual inspection, collate related terms or words that denote specific theme or subject from the generated wordcloud. Several words relating to the COVId-19 pandemic, including 'infect', 'pandemic', and 'lockdown' can be identified as been important in the document. These words are provided as `covid_theme` in the package. The function can be ran as follows (see the documentation for details): 

```r
> p1a <- word_imp(textdoc = policing_dtd, metric= "tf", 
                           words_to_filter=c("police","policing"))
                           
#Note: `policing_dtd` is a dataframe
```

### Impact analysis

The impact analysis can be conducted as follows:

```r
#Running the analysis

results <- opi_impact(textdoc = policing_dtd, theme_keys=covid_theme, metric = 1,
                       fun = NULL, nsim = 99, alternative="two.sided",
                       pplot = TRUE, quiet=FALSE)
                       
#Note: `policing_dtd` is a dataframe    

print(results)

$test
[1] "Test of significance (Randomization testing)"

$criterion
[1] "two.sided"

$exp_summary
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -8.240  -5.880  -5.880  -5.028  -3.530  -1.180 

$p_table


|observed_score |S_beat |nsim |pvalue  |signif |
|:--------------|:------|:----|:-------|:------|
|-5.88          |56     |99   |0.52    |'      |

$p_key
[1] "0.99'"   "0.05*"   "0.025**" "0.01***"

$p_formula
[1] "(S_beat + 1)/(nsim + 1)"

#......

```

The research question of the analysis above can be stated as follows:

***RQ1***: "Does COVID-19 pandemic influence public opinion on neighourhood policing?"

The output shows an overall negative opinion (`-5.88`) of the public on the neighbourhood policing, and that the pandemic has not had a significant impacts (`pvalue` = 0.52) on the opinion expressed by the public. (More detailed explanation can be found in the study [adepeju, M. and Jimoh, F. (2021)](https://www.scirp.org/journal/paperinformation.aspx?paperid=107836)). 

### Other applications

Table 1 summarizes the analysis using different example datasets provided in the package. The output from the law enforcement application (as in above) is entered in the first row of the table. Other research questions investigated are as follow:

***RQ2***: "How does the democratic candidate (Hillary Clinton) affects viewers’ opinion of the presidential debate?"

***RQ3a***: "Do the refreshment outlets/items impact customers’ opinion of the Piccadilly train services?"

***RQ4b***: "Do the signages influence customers’ opinion of the Piccadilly train services?"


***Table 1. Impact analysis results***

```r
|  RQs   | Primary data | Theme_keys        | Score function   | Observed Score (S) | P-value    |
|:-----: | :----------: | :---------------: | :---------------:| :-----------------:| :---------:| 
|  RQ1   | policing_dtd | covid_theme       | 'Polarity score' | -5.88              | 0.52       |
|  RQ2   | debate_dtd   | direct input      | 'Polarity score' | -0.33              | 0.93       |
|  RQ3a  | reviews_dtd  | refreshment_theme | 'Polarity score' | 67.92              | 0.01       |
|  RQ3b  | reviews_dtd  | signage_theme     | 'Polarity score' | 67.92              | 0.1        |

```

In each example, the same opinion score function is employed (`metric = 1`, i.e. the `polarity score = (P - N)/(P + N)*100`, where `P` and `N` represent `positive` and `negative` sentiments, respectively). See the documentation for details. Employing a threshold of `p=0.05`, any p-values less or equal to the threshold (e.g. RQ3a) represent a significant impact of the specified theme (i.e. `refreshment_theme`) on the overall opinion score computed based on the `reviews_dtd`.


### References
1. Adepeju, M. and Jimoh, F. (2021). An Analytical Framework for Measuring Inequality in the Public Opinions on Policing – Assessing the impacts of COVID-19 Pandemic using Twitter Data. [click here:](https://www.scirp.org/journal/paperinformation.aspx?paperid=107836)


## Code of Conduct

Please note that the `opitools` package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

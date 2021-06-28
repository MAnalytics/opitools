---
title: "R-Opitools – An Opinion Analytical Tool for Big Text Document"
date: "17th April 2021"
bibliography: paper.bib
output: pdf_document
affiliations:
- name: Crime and Well-being Big Data Centre, Manchester Metropolitan University
  index: 1
tags:
- digital text document
- sentiment analysis
- opinion mining
- randomization testing
authors:
- name: Monsuru Adepeju
  orcid: 0000-0002-9006-4934
  affiliation: 1
---

# Statement of Need

The recent proliferation of digital textual documents, across a wide range of application domains, has not only heralded enormous data opportunities, but also new advances in the opinion mining of natural language texts [@Liu:2012; @Tsapa:2019]. However, in spite of the availability of wide variety of opinion mining tools [@Tai:2015; @Kiomourtzis:2014; @Wawer:2016; @Munson:2019], there has been a lack of capability for assessing how an inherent theme or subject within a document may have impacted the overall opinion expressed by the document. The `opitools` package is designed in R language to address the aforementioned problem. We demonstrated the potentials of `opitools` for considerably wide domains by providing examples of its applications to different subject areas, including law enforcement [@Adepeju:2021], politics, entertainment and marketing (see all the examples under `opitools::opi_impact` function). The outcomes of our demonstrations show that `opitools` can be deployed by a user in order to identify the most important issues that may be driving the opinions of a given subject matter.


# Implementation

Given a text document comprising individual record texts, the goal is to computationally classify the sentiments expressed in each text record into positive, negative or a neutral sentiment, using a lexicon-based classification approach [@Nielsen:2011; @Adepeju:2021]. The resulting sentiment scores across records are combined in order to derive the overall opinion score of the text document. Then, to assess the impacts of an inherent theme (or subject), within the document, on the estimated opinion score, we simply ask the question, *If expected opinion scores were generated under the null hypothesis, how likely would we be to find a score higher than the estimated score?*. In order to answer the question, we employed a non-parametric randomization testing strategy [@Fisher:1935; @Good:2006], which involves generating an expectation distribution of the observed score through random re-assignment of sentiment labels of the original text records. The re-assignment is repeated a large number of times to obtain an expectation distribution. Lastly, the observed score is compared with the expectations in order to derive the statistical significance of the impacts.


# Key Functionalities

The key function of `opitools` package is the `opi_impact()`. The function draws from two other supporting functions, `opi_score()` and `opi_sim()`, to compute the observed score and its expectations, respectively. A user can choose from amongst four different opinion score functions or utilize the `'fun'` parameter in order to integrate a user-defined function. The latter provision enables the uptake of `opitools` across a wide variety of application domains.

# Acknowledgment

We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.

# References


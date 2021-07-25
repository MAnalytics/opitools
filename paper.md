---
title: "R-Opitools – An Opinion Analytical Tool for Big Digital Text Document (DTD)"
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

The recent proliferation of digital textual documents (DTD), across a wide range of application domains, has not only heralded enormous data opportunities, but also new advances in the opinion mining of natural language texts [@Liu:2012; @Tsapa:2019]. However, in spite of the availability of wide variety of opinion mining tools [@Tai:2015; @Kiomourtzis:2014; @Wawer:2016; @Munson:2019], there has been a lack of mechanism for assessing the extent to which a selected theme or subject from a DTD may have impacted the overall opinion expressed in the document. `Opitools` package is designed in R language to address the aforementioned problem. In other words, given a text document the package allows a user to assess how a selected a theme or subject from the digital text may have driven the overall opinion expressed in the text. The package can be applied to any DTD as long as text records represent individual-level posts, such as a tweets or comments (as in a review section of a product website). The demonstration of `opitools` for social research is shown with an application in the law enforcement domain [@Adepeju:2021]. The general usage of `opitools` for wider application, including marketing and politics, is presented in the documentation. 

# Implementation

Given a text document comprising individual record text records, the goal is to computationally classify the sentiments expressed in each text record into positive, negative or a neutral sentiment, using a lexicon-based classification approach [@Nielsen:2011; @Adepeju:2021]. The resulting sentiment scores across records are combined in order to estimate the overall opinion score of the text document. To assess the impacts of a selected theme (or a subject) on the estimated opinion score, we simply ask the question, *If expected opinion scores were generated under the null hypothesis, how likely would we be to find a score higher than the estimated score?*. In order to answer the question, we employed a non-parametric randomization testing [@Fisher:1935; @Good:2006]approach involving random re-assignment of sentiment labels of the original text document to derive the expectation distribution. Lastly, the observed score is compared with the expectation distribution in order to derive the statistical significance of the impacts.


# Key Functionalities

The key function of `Opitools` is the `opi_impact()` which draws from two supporting functions, `opi_score()` and `opi_sim()`, to compute the observed opinion score and its expectations, respectively. Four different opinion score functions are embedded in the package for user selection, with a `'fun'` parameter to allow a user-defined score function. These provisions enable is to enable the uptake of `opitools` package in a wide variety of application domains.

# Acknowledgment

We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.

# References


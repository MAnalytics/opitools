---
title: "R-Opitools – An Opinion Analytical Tool for Big Digital Text Document (DTD)"
date: "28th July 2021"
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

The recent proliferation of digital textual documents (DTD) across a wide range of application domains has not only heralded enormous data opportunities, but also new advances in the opinion mining of natural language texts [@Liu:2012; @Tsapa:2019]. However, in spite of the availability of wide variety of opinion mining tools [@Tai:2015; @Kiomourtzis:2014; @Wawer:2016; @Munson:2019], there has been a lack of mechanism for assessing the extent to which a selected theme or subject from within a DTD may have impacted the overall opinion expressed by the document. The `Opitools` package is designed in R language to address the aforementioned problem. In other words, given a digital text document, the package allows a user to test the statistical significance of the impacts produced by a selected theme or subject from the document on the overall opinion expressed by the document. The package can be applied to any DTD as long as the text records represent individual-level comments, such as posts (as on Twitter and Facebook) or comments (as in an online product reviews). The utility of `opitools` for social research is demonstrated in [@Adepeju:2021] for law enforcement application. Other applications, particularly in marketing and politics, are presented in the documentation.

# Implementation

For a text document comprising individual text records, the goal is to computationally classify the sentiments expressed in each text record into positive, negative or a neutral sentiment, using a lexicon-based classification approach [@Nielsen:2011; @Adepeju:2021]. The resulting sentiment scores are combined in order to estimate the overall opinion score of the document. To assess the impacts of a selected theme (or a subject) on the estimated opinion score, we simply ask the question; *If expected opinion scores were generated under the null hypothesis, how likely would we be to find a score higher than the estimated score?*. The question is answered by employing a non-parametric randomization testing [@Fisher:1935; @Good:2006] strategy involving random re-assignment of sentiment labels of the original text document to derive the expectation distribution. By comparing the observed score with the expectation distribution, the statistical significance of impacts is derived.


# Key Functionalities

The key function of `Opitools` is the `opi_impact()` which draws from two supporting functions, `opi_score()` and `opi_sim()`, to compute the observed opinion score and its expectations, respectively. Four different opinion score functions are embedded in the package, plus a `'fun'` parameter to allow a pre-defined user score function. Other functions, `word_distrib` and `word_imp` are provided for a prior exploratory analysis of a text document.

# Acknowledgment

We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.

# References


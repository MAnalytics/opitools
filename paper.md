---
title: "R-Opitools – An Opinion Analytical Tool for Big Digital Text Document (DTD)"
date: "30th July 2021"
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

Since the year 2000, various computational intelligence techniques have been developed for analyzing sentiments of users either in the field of natural language processing or computational linguistics. To date, the majority of the techniques as deployed across various fields, including Social science [@Somasundaran:2010; @Morency:2011; @Nikolovska:2020], Market research [@Feldman:2011; @Otaibi:2018], and Electoral politics [@Ansari:2020; @Connor:2010] have focused largely on detecting subjectivity, and/or extracting and classifying sentiments and opinions in a text document. Building on this existing work, the current paper advances an opinion impact analytical tool, namely; `Opitools`, that not only extracts inherent themes from within a digital text document (DTD), but also evaluates the extent to which a specified theme may have contributed to the overall opinions expressed by the document. 

`Opitools` has potential for wider applications. For example, in Law enforcement, the package can be deployed to understand factors (themes) that drive public perception of the police services [@Adepeju:2021]; in Market research, to identify factors that underlie customers satisfaction in a product; and, in Electoral politics, to identify factors that may be influencing electoral interests in an upcoming election.

# Implementation

Having extracted a set of thematic keywords from a digital text document, the goal is to computationally classify the sentiments expressed in each text record into positive, negative or a neutral sentiment, using a lexicon-based classification approach [@Nielsen:2011; @Adepeju:2021]. The resulting sentiment scores are combined in order to estimate the overall opinion score of the document. To assess the impacts of a selected theme (or a subject) on the estimated opinion score, we simply ask the question; *If expected opinion scores were generated under the null hypothesis, how likely would we be to find a score higher than the estimated score?*. The question is answered by employing a non-parametric randomization testing strategy [@Fisher:1935; @Good:2006] which involves random re-assignment of sentiment labels of the original text document to derive the expectation distribution, which is then compared with the observed score to obtain the statistical significance of the impacts.


# Key Functionalities

The package contains text exploratory functions to extract themes from a digital text document as well as functions to conduct impact analysis.

The package provides different types of opinion score functions, which can be used depending on the research question. Additional parameters also permit the integration of pre-defined user score functions, which provide flexibility for applications in other fields.

# Acknowledgment

We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.

# References


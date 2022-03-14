## This is a repository for a term project of the course "04  Introduction to replicable research practices via browser-based replication"

### About the experiment
This repository contains the data analysis part of a replication of the self-paced reading experiment laid out in [Bergen & Grodner (2012)](https://web.archive.org/web/20160508203939id_/http://web.mit.edu/bergen/www/papers/BergenGrodner%202012.pdf) using the [Magpie Framework](https://magpie-manual.netlify.app/).

A hosted version of this repository can be found at: https://keen-mahavira-61eb1c.netlify.app/

The repository for our online experiments can be found at : https://github.com/nmeisinger/self-paced-reading-magpie

### Repository structure

In current repository, the core part are one Python script to preprocess data and one R script to build linear mixed effect regression models.

### Data processing

- Global and local accuracies are checked.
- Reading Time (RT) are log-transformed.
- Extreme RTs are not excluded yet which will be dealed after discussion.

### Mixed effect regression models

The **linear mixed effect regression models** using analysis of variance(**ANOVA**) with:

- **Fixed effects**: Speaker knowledge (full vs. partial), trigger-type (scaler vs. focused)

- **Random effects**: Participants and items.


### Hypotheses:
- **H1**: full-knowledge caused longer RT over the scalar quantifier part of scalar trigger sentences than partial-knowledge

- **H2**: full-knowledge caused shorter RT over the anaphor part in complement sentences than partial-knowledge
 
- **H3**: full-knowledge showed no significant different effect over the "focus particle" in focused trigger sentences than partial-knowledge
 
- **H4**: No matter full-knowledge or partial knowledge, for scalar trigger sentences, RTs over all the regions are similar in complement sentences and cancellation sentences

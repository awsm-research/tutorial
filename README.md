# Software Analytics in Action

### A Hands-on Tutorial on Mining, Analyzing, Modelling, and Explaining Software Data


Presented by Dr. Chakkrit (Kla) Tantithamthavorn at the International Conference on Mining Software Repositories (MSR): [Education Track](https://2019.msrconf.org/track/msr-2019-Education?track=MSR%20%20Education#program) on May 26, 2019. 

<img src="./resources/software-analytics-tutorial-cover.jpg" width="500">



# Tutorial Abstract

Software analytics focuses on analyzing and modeling a rich source of software data using well-established data analytics techniques in order to glean actionable insights for improving development practices, productivity, and software quality. However, if care is not taken when analyzing and modeling software data, the predictions and insights that are derived from analytical models may be inaccurate and unreliable. The goal of this hands-on tutorial is to guide participants on how to (1) analyze software data using statistical techniques like correlation analysis, hypothesis testing, effect size analysis, and multiple comparisons, (2) develop accurate, reliable, and reproducible analytical models, (3) interpret the models to uncover relationships and insights, and (4) discuss pitfalls associated with analytical techniques including hands-on examples with real software data. R will be the primary programming language. Code samples will be available in a public GitHub repository. Participants will do exercises via either RStudio or Jupyter Notebook through Binder.

### How to run this tutorial in a live environment using Binder?

Jupyter+R: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?filepath=Modelling-Software-Data.ipynb)

RStudio: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?urlpath=rstudio)

### How to run this tutorial on your local machine using Docker?

To run this tutorial on Jupyter, please run the following command.

First, we need to build a docker container.

```docker build -t awsmdocker/binder .```

Then, we run the container. If the command is run successfully,  

```docker run -v `pwd`:/home/rstudio -p 8888:8888  awsmdocker/binder:latest```

Finally, the Jupyter can be accessed via the URL (http://localhost:8888/). The required token for Jupyter can be obtained from the Terminal.

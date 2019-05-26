# Software Analytics in Action [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?filepath=Software-Analytics-In-Action.ipynb)

### A Hands-on Tutorial on Mining, Analyzing, Modelling, and Explaining Software Data


Presented by Dr. Chakkrit (Kla) Tantithamthavorn at the International Conference on Mining Software Repositories (MSR): [Education Track](https://2019.msrconf.org/track/msr-2019-Education?track=MSR%20%20Education#program) on May 26, 2019. 

[![Slideshare](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRa8GqWGjjaJB2jz3t_HuNxaL_xdhGCFgVCqj9MqJMx07EUK77uKQ)](https://www.slideshare.net/klainfo/software-analytics-in-action-a-handson-tutorial-on-mining-analyzing-modelling-and-explaining-software-data/klainfo/software-analytics-in-action-a-handson-tutorial-on-mining-analyzing-modelling-and-explaining-software-data)

<img src="./resources/software-analytics-tutorial-cover.jpg" width="500">

# Tutorial Abstract

Software analytics focuses on analyzing and modeling a rich source of software data using well-established data analytics techniques in order to glean actionable insights for improving development practices, productivity, and software quality. However, if care is not taken when analyzing and modeling software data, the predictions and insights that are derived from analytical models may be inaccurate and unreliable. The goal of this hands-on tutorial is to guide participants on how to (1) analyze software data using statistical techniques like correlation analysis, hypothesis testing, effect size analysis, and multiple comparisons, (2) develop accurate, reliable, and reproducible analytical models, (3) interpret the models to uncover relationships and insights, and (4) discuss pitfalls associated with analytical techniques including hands-on examples with real software data. R will be the primary programming language. Code samples will be available in a public GitHub repository. Participants will do exercises via either RStudio or Jupyter Notebook through Binder.

## Download and Run This Tutorial

Begin by cloning or downloading the tutorial GitHub project [https://github.com/awsm-research/tutorial](https://github.com/awsm-research/tutorial).


### How to run this tutorial in a live environment ANYTIME and ANYWHERE?

To run this tutorial on Jupyter ANYTIME and ANYWHERE, please access Binder the following command:

Jupyter+R: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?filepath=Software-Analytics-In-Action.ipynb)

RStudio: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?urlpath=rstudio)

### How to run this tutorial on your local machine?

If you need to install Docker, follow the installation instructions at [docker.com](https://www.docker.com/products/overview) (the _community edition_ is sufficient).

Now we'll run the docker image. It's important to follow the next steps carefully. We're going to mount two local directories inside the running container, one for the data we want to use so and one for the notebooks.

* Open a terminal or command window
* Change to the directory where you expanded the tutorial project or cloned the repo
* To run this tutorial on Jupyter on your local machine, please run the following command:

First, we need to build a docker container.

```docker build -t awsmdocker/binder .```

Then, we run the container. If the command is run successfully,  

```docker run -v `pwd`:/home/rstudio -p 8888:8888  awsmdocker/binder:latest```

Finally, the Jupyter can be accessed via the URL (http://localhost:8888/). The required token for Jupyter can be obtained from the Terminal.

## BEST PRACTICES FOR ANALYTICAL MODELLING 

#### 7 DOs

1. Include control factors
2. Remove correlated factors
3. Build interpretable models
4. Explore different parameter settings
5. Use out-of-sample bootstrap
6. Summarize by a Scott-Knott test
7. Visualize the relationship

#### 3 DON'Ts

1. Don’t use ANOVA Type-I
2. Don’t optimize prob thresholds
3. Don’t solely use F-measure

## About the Instructor

Dr. Chakkrit (Kla) Tantithamthavorn is a lecturer in the Faculty of Information Technology, Monash University, Australia. His research aims to develop technologies that enable software practitioners to produce the highest quality software systems with the lowest costs. Currently, his research focused on inventing practical and explainable analytics to prevent future software defects. He is best known as a lead instructor at MSR Education 2019 about Guidelines and Pitfalls for Mining, Analyzing, Modelling, and Explaining Software Defects, and the author of the ScottKnott ESD R package (i.e., a statistical mean comparison test) with more than 5,000 downloads. More about him is available at http://chakkrit.com

## Reference for this tutorial

```tex
@inproceedings{tantithamthavorn2018pitfalls,
    Author={Tantithamthavorn, Chakkrit and Hassan, Ahmed E.},
    Title = {An Experience Report on Defect Modelling in Practice: Pitfalls and Challenges},
    Booktitle = {In Proceedings of the International Conference on Software Engineering: Software Engineering in Practice Track (ICSE-SEIP'18)},
    Pages = {286--295},
    Year = {2018}
}
```


## License

Anyone may contribute to our project. Submit a pull request or raise an issue.

## Final Thoughts

Thank you for working through this tutorial. Feedback and pull requests are welcome.

[Chakkrit (Kla) Tantithamthavorn](mailto:chakkrit.tantithamtahvorn@monash.edu)

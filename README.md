# Specifying an R environment with a runtime.txt file

Jupyter+R: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?filepath=demo.R)

RStudio: [![Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/awsm-research/mining-software-defects/master?urlpath=rstudio)


### How to run this tutorial on Docker

To run this tutorial on Jupyter, please run the following command.

First, we need to build a docker container.

```docker build -t awsmdocker/binder .```

Then, we run the container. If the command is run successfully,  

```docker run -v `pwd`:/home/rstudio -p 8888:8888  awsmdocker/binder:latest```

Finally, the Jupyter can be accessed via the URL (http://localhost:8888/). The required token for Jupyter can be obtained from the Terminal.
# Plumber application to use extReme library for distribution fitting

<b>Description</b>
API to run nonstationary distribution fit of annual flood peaks and return quantile estimates over time. Currently, it is set up for using the extRemes function evdfit and using a linear time dependent parameter for the location. The input is a json array [[year1, peak1],[year2,peak2],...,[yearn,peakn]] and the ouput is a json array the is the flood quantiles. It can be accessed on heroku currently. 

<h2>Credit</h2>
#Plumber template taken from https://github.com/virtualstaticvoid/heroku-plumber-app/tree/main

<h2>Deploy to heroku</h2>
#to deploy: https://heroku.com/deploy?template=https://github.com/atmichalek/gamlss-r-plumber/tree/main

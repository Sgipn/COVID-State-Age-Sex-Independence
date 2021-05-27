# COVID-State-Age-Sex-Independence
STA104 Project 2
Winter 2021
Professor Amy Kim
Kevin Xu, Radhika Kulkarni

A term project for my Nonparametric Statistics class in which I tested the largest US states' response-effectiveness against COVID-19 using the nonparametric Kruskal-Wallis test.
I also conducted nonparametric permutation tests with R=300 random permutations to determine if Age and Sex are independent variables regarding COVID-19 related deaths. We find that Age and Sex are dependent variables with regard to COVID-19 related deaths. Using Tukey's HSD inspired cutoff values at a family confidence level of 95%, we find that males tended to die more than females for ages 18-84 years old and females tended to die more often than males for ages 85+ years old.

# Nonparametric K-sample Test
Analyzing COVID-19 deaths by State to see if States (their infrastructures and response to COVID-19, resources, density, etc) have an effect on the number of deaths from COVID-19 using nonparametric Krustkal Wallis Test. States with the largest populations were chosen because their COVID-19 policies affect a large number of people and they would be forced into a similar situation in terms of COVID 19 deaths and thus are comparable. These states are California, Texas, New York, and Florida.

The goal of this paper was to determine whether State COVID-19 response strategies have an effect on the number of COVID 19 deaths for the four states with the largest populations in the United States, which are California, Texas, New York, and Florida.

The result of testing this claim is consequential because we would be able to better understand or at least solidify a starting point for understanding which of the State methods of dealing with COVID 19’s effects would result in lower number of deaths. In large population states this will be even more impactful if we find some difference because then public policy can be tailored to more effectively reduce the number of deaths due to COVID-19.

# Independence of Age and Sex for Number of Deaths due to COVID-19
Nonparametric Permutation Test for Independence
Study was done to determine if Age and Sex are independent variables regarding COVID-19 related deaths. The timeframe selected for analysis is March 2020 to the end of February 2021, and Age groups selected for analysis are those 18 years and older who have died from COVID-19.

Analysis of the independence of the variables Age and Sex regarding death as a result of COVID-19 has broader implications on whether age matters for how at risk a person is of dying from COVID-19, for a given sex, and whether sex matters for how at risk a person is of dying from COVID-19 for a given age

# Source:
Provisional Data on COVID-19 retrieved from CDC at https://www.cdc.gov/nchs/nvss/vsrr/covid19/index.htm

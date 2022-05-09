# 22SDaliChallenge
Project, Code and Visualizations of Dali Application Challenge 

### Part 1
#### N.B.: I did clicked the wrong challenge link, so this is based on the 22W challenge

For Part 1, I looked to create visualizations that would give the viewer a solid idea of what sorts of information the dataset contains. 

For the first (two) visualizations, I decided to look at two key variables, Gini Index and Quality of Life, and integrate them into a world map, to show how we have the following:
- Economic data about most countries and which ones don't 
- The types of economic data
- Emphasize that we have data from a number of years; that's why I used the medians for 21st century.

I put these two graphs together so that a viewer can visually look at the correlation between Gini index and quality of life around the world.

For the next visualization, I decided to choose a random row that didn't contain missing values, and pack as much information from the data into the visualization as possible, especially what I considered most valuable:
- Country Name
- Year
- Distribution of Wealth
- Population
- Gini Index
- Quality of life
- Income level

For the last visualization, I wanted to look at another important variable, GDP per Capita, and see the distribution of Wealth as well as the income equality of the top 10 richest countries per capita.

### Part 2

For part 2, I wanted to do some analysis of the superstore's profit model and make some predictions about their future, using current/past trends. 

1. I created a dataframe of combined past and future yearly sales numbers, quantities, and total profits, as well as a cumulative sum that is the tracks the lifetime profit of the store. The r<sup>2</sup> of these models were quite strong, all being above 0.86, so I would have a fair amount of confidence in these predictions being relatively accurate. I also visualized these trends to make them easier to understand on a basic level.
2. As a proof of concept, I narrowed the data to only appliance sales to see what the appliance profit model is (how sales, discounts, and quantity combine to determine profit). The r<sup>2</sup> of making a profit model on the whole dataset was something weak, between 0.3 and 0.4, so I scrapped that, figuring that is likely due to profits being composed differently for different types of products. We can see the r<sup>2</sup> of the appliance profit model is above 0.7, so we can make fairly accurate predictions of the profits from appliance sales given the sales, quanitity and discount values. We can follow a similar technique with other types of products to make further predicitons of individual profits from sales at the superstore.

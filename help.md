# East Link Trip Change Dashboard

## About

This dashboard shows the anticipated level of change in trip volumes for 2022 East Link network as compared to service levels in Fall 2021.  We chose Fall 2021 as the baseline comparison point because it includes the NorthLink service restructure. We also added back service that was suspended due to Covid 19. 
## Metrics

The dashboard has several metrics available for planners. All reference the same data and can be used together to develop an understanding of the scale and size of change in an area.

**Trips/Period:** The number of trips passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard.

**Avg Trips/Hour:** The number of trips per hour passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. For routes with spans of service that only cover a portion of a period, the dashboard assumes that the average should be calculated using the total number of hours in the period. In other words, if a route has its last trip at 7:10 PM, it will have records available in the "7 PM - 10 PM" period, but the average number of trips per hour will be less than 1.

**Change in Trips/Period:** The change in number of trips passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**Change in Avg Trips/Hour:** The change in number of trips per hour passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**% Change Trips/Period:** The percent change in number of trips passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**% Change Avg Trips/Hour:** The percent change in number of trips per hour passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**New Service:** This metric identifies areas that will have service in the selected Metro Connects 2030 scenario that did not have service in 2020. The goal is to identify areas that may need 100% new capital infrastructure or pathway improvements.

**Max Service:** This metric finds the highest amount of service that will be present in each hexagon across all scenarios and the Spring 2020 baseline. There are certain hexagons where the highest level of service is in Spring 2020. This metric is intended to provide an understanding of the maximum service that might serve each area.


## More Info

This app was developed in R using shinydashboard by Melissa Gaughan. For code and technical documentation see <https://github.com/melissa-gaughan/mc-2030-trips-by-hex>

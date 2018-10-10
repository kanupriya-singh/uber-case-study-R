# Uber Case Study using R
Analysis of Uber rides between City &amp; Airport to understand the pattern of ride cancellation/non-availability

## Business Objectives

The aim of analysis is to identify the root cause of the problem (i.e. cancellation and non-availability of cars) and recommend ways to improve the situation. As a result of your analysis, you should be able to present to the client the root cause(s) and possible hypotheses of the problem(s) and recommend ways to improve them.  

## Data Understanding
There are six attributes associated with each request made by a customer:

- Request id: A unique identifier of the request
- Time of request: The date and time at which the customer made the trip request
- Drop-off time: The drop-off date and time, in case the trip was completed 
- Pick-up point: The point from which the request was made
- Driver id: The unique identification number of the driver
- Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available

## Results Expected
Visually identify the most pressing problems for Uber. 
- Find out the gap between supply and demand and show the same using plots.
- Find the time slots when the highest gap exists
- Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
- What do you think is the reason for this issue for the supply-demand gap? 
- Recommend some ways to resolve the supply-demand gap.

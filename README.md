# PLS
PLS, standing for Peer Learning System, is a scheme at the University of Hong Kong for SCNC1112 Fundamentals of modern science.
This small, dynamic, and modular program helps with the interviewing process by selecting random words (from categories or not).

## Installation
To install this program, type the following in  **R**
```
devtools::install_github(repo = "CarolusKwok/PLS")
```
## Demo usage
Create a dataframe with columns "category" and "word", then run the following code.
```
PLS::interview_1n1(data = dataframe)
```

## Custom usage
Create a function with internal codes `PLS:::sample_categorized` and `PLS:::sample_random`. You should use the package `cli`, `tibble`, and `dplyr` too for easier control and better aesthetics.

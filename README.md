# Predictors of mental health among U.S. adult population <br/>STAT  471 Final Project<br/>Dung Pham<br/>Dec, 2021

Executive Summary<br/>

Problem<br/>
To date, most studies have examined predictors of mental health only among a specific subset of the population. Given the high prevalence of mental health problems among Americans and its profound consequences at both the individual and societal levels, I seek to investigate various predictors of mental health risks among the US adult population.

Data<br/>

Data was obtained from the National Health and Nutrition Examination (NHANES) Survey
2017-2020 by the Center for Disease Control and Prevention. NHANES survey combines
interviews and physical examinations to assess the health and nutritional status of adults and children in the United States. The explanatory variables come from 10 different areas: demographics, mental health/depression screening questions, physical activity, smoking, sleep, medical history, diabetes, blood pressure and cholesterol, occupation, and weight history. There are multiple items within each group of variables for a total of 41 features included in the analysis. The response variable (mental health/depression score) was created by summing up all ten items in the mental health/depression screening dataset. Each item has a value ranging from 0 to 3. Therefore, the response variable is on a scale from 0 to 30, with greater value indicative of higher mental health risks.

Analysis<br/>

Missing values were removed from the dataset, resulting in a total of 922 responses in the final data file. Data was split into training and test sets for a 80-20 ratio. First, I explored the relationships between different variables on the training data. Six different cross-validated predictive models were then built, including ordinary least squares, ridge regression, LASSO regression, elastic net regression, random forest, and boosting. For the tree-based models, the boosted model had the lowest test error as well as the lowest test error among the six methods.Within the regression models, OLS had the lowest test error, followed by ridge and elastic net regression (which had the same predictive performance).

Conclusions<br/>
All six models consider sleep and ratio of family income to poverty as important predictors of mental health risks. Gender, vigorous-intensity recreational activities and self-perception about weight are also shared by several predictive models. These findings hope to inform initiatives and policies to address the sleep deprivation epidemic among Americans, provide more support for women and low-income populations, allocate more resources for mental wellness, physical and recreational activities, as well as raise awareness about positive body image. I concluded by discussing several limitations and providing recommendations for future research directions.

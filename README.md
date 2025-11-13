# BostonHousing2
The original data are 506 observations on 14 variables, medv being the target variable:

# introduction：Original Data Description
The dataset includes **506 observations** and **14 variables**, where **`medv`** is the target variable:

| Variable Name | Description |
|---------------|-------------|
| `crim`        | Per capita crime rate by town. |
| `zn`          | Proportion of residential land zoned for lots over 25,000 sq. ft. |
| `indus`       | Proportion of non-retail business acres per town. |
| `chas`        | Charles River dummy variable (1 = tract bounds river; 0 = otherwise). |
| `nox`         | Nitric oxide concentration (in parts per 10 million). |
| `rm`          | Average number of rooms per dwelling. |
| `age`         | Proportion of owner-occupied units built prior to 1940. |
| `dis`         | Weighted distances to five Boston employment centers. |
| `rad`         | Index of accessibility to radial highways. |
| `tax`         | Full-value property tax rate per $10,000. |
| `ptratio`     | Pupil-teacher ratio by town. |
| `b`           | $ 1000(B - 0.63)^2 $, where \( B \) is the proportion of Black population by town. |
| `lstat`       | Percentage of lower status of the population. |

---

### Enhanced Dataset Description
The revised dataset includes the following additional columns:

| Added Column Name | Description |
|-------------------|-------------|
| `cmedv`           | Corrected median value of owner-occupied homes (in $1,000s). |
| `town`            | Name of the town. |
| `tract`           | Census tract (area). |
| `lon`             | Longitude of the census tract. |
| `lat`             | Latitude of the census tract. |


# References
1. Harrison, D. and Rubinfeld, D.L. (1978). Hedonic prices and the demand for clean air. Journal of Environmental Economics and Management, 5, 81--102.

2. Gilley, O.W., and R. Kelley Pace (1996). On the Harrison and Rubinfeld Data. Journal of Environmental Economics and Management, 31, 403--405. [Provided corrections and examined censoring.]

3. Newman, D.J. & Hettich, S. & Blake, C.L. & Merz, C.J. (1998). UCI Repository of machine learning databases [http://www.ics.uci.edu/~mlearn/MLRepository.html]. Irvine, CA: University of California, Department of Information and Computer Science.

4. Pace, R. Kelley, and O.W. Gilley (1997). Using the Spatial Configuration of the Data to Improve Estimation. Journal of the Real Estate Finance and Economics, 14, 333--340. [Added georeferencing and spatial estimation.]
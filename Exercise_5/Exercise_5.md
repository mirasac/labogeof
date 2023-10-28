# Exercise 5

## Compiling
Compilation is performed with SH script `compile.sh`.

## Pseudocode
The following is the pseudocode for the exercise.

```
05) Declare parameter for missing value, maximum number of consecutive missing values for interpolation.
10) Declare variables for filename, shortwave sum, infrared sum, counter for missing values, two temporary variables for interpolated radiance.
20) Insert filename.
30) Check existence of file.
33) If there are issues with the file, exit.
35) Skip header line.
38) Set to zero variables for infrared sum and shotwave sum and counter for missing values.
40) Read line.
45) If EOF or counter for missing values is greater than the maximum value, then exit.
50) If radiance is not missing, then.
53) If counter for missing values is zero then.
55) Set first temporary variable to read variable.
58) Else.
59) Set second temporary variable to read variable.
60) Evaluate interpolation for radiance using for loop.
65) Set first temporary variable to interpolated radiance.
70) Set to zero counter for missing values.
75) If first temporary variable is non-negative, sum it to variable for shortwave radiance sum, else sum it to infrared radiance sum variable.
80) Else increment counter for missing values.
85) Return to point 40.
88) Evaluate energies using the time span of a day.
90) Write sums as formatted string.
```

## Release
- Add controls and check on edge cases (e.g. trim filenames).
- Make pseudocode more readable and in LaTeX.
- Use package listings in LaTeX.
- Set real numbers to double precision.
- Remove debug lines.

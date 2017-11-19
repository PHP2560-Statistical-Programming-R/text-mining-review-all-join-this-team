# Team 1

Coding style is good and well-commented, but there are a few errors, such as in the `check_packages()` function, there were no brackets around the `if` statement, causing every single package to be reinstalled on my machine which created conflicts with updating packages vs installing packages.

Coding strategy breaks up project into smaller parts, but the divisions are not completly logical. I'm not sure why some of the questions are answered in separate files from `04_main_analysis.R`.

Many of the plots do answer the question being asked, however, many of the axis labels/ticks need to be resized because they are cut off because of plot size or too small to read, some of the plots could benefit from reordering bars by frequency, and sometimes a lot of colors are used when only one could be used, which distracts from the main point of the plot. In general, a lot of explaining has to be done below/above the plot in order for the reader to understand it, but ideally plots should speak for themselves.

The table in the project could benefit from better labels at the top so that the table can speak more for itself. 

Questions are answered well using the tools learned in the course.

`makefile.R` ran for me, but only after I fixed the issue in the `check_packages()` function.

# Team 2

Code did not run for me. There was a typo in the `check_packages()` function that caused the function to get imported as `heck_packages()`. Additionally, errors arose when the data created in `04_get_sentiment.R` was trying to be saved in line 60 because there was something not accounted for when the data was initially created.

None of the graphs were available in the folder on GitHub, so I wasn't able to see what the plots would have looked like if the code was able to run. 

The code isn't commented, making it difficult to read. Additionally, a lot of the code in `04_get_sentiment.R` is repetitive and could be condensed.

# Team 4

Comments are good but could be more evenly distributed throughout different lines of the code instead of just at the beginning of chunks.

The code ran well on my computer, but it took me awhile to figure out that the analysis code was in the Rmd. One downside of putting the code into the Rmd is that it takes much longer to knit and the code within the Rmd can't be separated into different files like it could if it was in an R script. 

Plots are well labeled and answer the questions being asked. One of the table's headers aren't labelled well, but most of the headings make sense.

One thing that I'm still confused about is what exactly is meant by the maximum rating of a Ted Talk and why it's a word instead of a number.

# Team 5

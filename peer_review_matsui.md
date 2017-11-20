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

The comments are good but could be more evenly distributed throughout different lines of the code instead of just at the beginning of chunks.

The code ran well on my computer, but it took me awhile to figure out that the analysis code was in the Rmd. One downside of putting the code into the Rmd is that it takes much longer to knit and the code within the Rmd can't be separated into different files like it could if it was in an R script. 

The plots are well labeled and answer the questions being asked. One of the table's headers aren't labelled well, but most of the headings make sense.

One thing that I'm still confused about is what exactly is meant by the maximum rating of a Ted Talk and why it's a word instead of a number.

# Team 5

The code ran smoothly on my computer and made sense. 

The code is commented, but the comments are sometimes too sparse for me to understand what is happening, but I'm able to get a general understanding. 

The plots are well-labelled and answer the questions being asked. However, many of the tables displayed could benefit from using `kagle` instead of just being displayed in raw R output form and the headers could be renamed to make sense to outside readers. Additionally, I'm not sure if spitting out model summaries from R is the best way of showing them to people. There could be some nice looking tables or visualizations that could be made instead, especially when a more general audience is interested in the results rather than someone who knows more about statistics. 

# Team 6

I'm not sure where exactly to find the `makefile.R`, but I found one in the folder called 'Lisha', so the feedback is based off of this folder. In the makefile, the path names are not correct, so it doesn't run. The code is commented well, has good style, and I understand what's happening, but I don't know where to find the other questions. Also, I would have removed the word 'tsingtao' from the word count, since that's the account that's being used and doesn't really tell us anything.

The plot and table are both labelled well so I can understand what they represent without additional explanation needed. 

You do a good job with breaking up the project into different parts in a logical manner, making it easier to understand the process.

# General

In all teams, the `check_packages()` function doesn't have brackets in the if statement, forcing my computer to reinstall packages that I already have. 

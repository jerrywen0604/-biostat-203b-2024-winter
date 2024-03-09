*Jinrui Wen*

### Overall Grade: 158/260

### Quality of report: 5/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.

-   Is the final report in a human readable format html?

    **No html submitted**

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succinct to grasp, or there are too many typos/grammar.

### Completeness, correctness and efficiency of solution: 145/210

-   Q1 (30/30)

    -   Q1.1 (15/15)

    -   Q1.2 (15/15) **X-axis should be equal lengths**

-   Q2 (10/10)

    -   Q2.1 (5/5)

    -   Q2.2 (5/5) A bar plot of similar suffices.

-   Q3 (15/25)

    -   Q3.1 (5/5)

    -   Q3.2 (10/20) Student must explain patterns in admission hour, admission minute, and length of hospital display. Just describing the pattern is not enough. There are no wrong or correct explanations; any explanation suffices.

        **Any explanation for the patterns?**

-   Q4 (10/15)

    -   Q4.1 (5/5)

    -   Q4.2 (5/10) There's not much pattern in gender. But some explanations are expected for anchor age: what are they and why the spike on the right.

        **Any explanation for the spike?**

-   Q5 (10/30) Check the final number of rows and the first few rows of the final data frame.

    **Incorrect number of rows despite memory problem**

-   Q6 (15/30) Check the final number of rows and the first few rows of the final data frame.

    **1 row off, points deducted for not having all cols**

-   Q7 (20/30) Check the final number of rows and the first few rows of the final data frame.

    Too many rows, need distinct subject ids and stay ids

-   Q8 (35/40) This question is open ended. Any graphical summaries are good. Since this question didn't explicitly ask for explanations, it's fine students don't give them. Students who give insights should be encouraged.

    **Axes should be cleaned up**

### Usage of Git: 0/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (\>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline.

-   Is the hw submission tagged?

    **No tag**

-   Are the folders (`hw1`, `hw2`, ...) created correctly?

-   Do not put a lot auxiliary files into version control.

-   If those gz data files or `pg42671` are in Git, take 5 points off.

### Reproducibility: 8/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`?

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    **Plots in Q8 referencing cols that don't exist**

### R code style: 0/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation.

**Only marking first 10 violations**

-   [Rule 2.5](https://style.tidyverse.org/syntax.html#long-lines) The maximum line length is 80 characters. Long URLs and strings are exceptions.

    **Line 116, 150, 238, 281, 309**

-   [Rule 2.4.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.

    **Line 217 (don't indent unnecessarily), 255**

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, \<-, etc.).

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.

    **Line 203, 452, 453**

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.

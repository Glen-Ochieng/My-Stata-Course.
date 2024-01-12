
**#SAMPLING**
*selecting a sample of size 10*
set obs 10 

*generating the sample from a normal distribution*
generate x = rnormal(12, 2)

**#SUMMARIZING** 
*summarizing the variable x*
summarize x 

*to get more comprehensive details aout a varaible, we use the option detail on the command summarize. This will display  4small values, 4 large values, perventiles, variance, skewness, kurtosis, mean, sd, Detail can also be wriiten in short as d
summarize age,detail

*Sometimes you wish to summarize a variable alongside another i.e how many female are in the category degree and and how many diploma
by edustatus,sort: summarize sex

*we include sort in order to sort the data first. A shortcut for the above code is 
bysort low:summarize -(here no comma is used)

**#CONTIGENCY TABLES**
*Univariate tables 

*The command table is very useful for displaying the frequency of observations in in a variable. Sometimes with a little coding it can get you some descriptive measurements you specifically want

-if you wish to tabulate even the missing values use the option missing 
tab age,missing 

-if you wish to get mean use the command tabstat
tabstat diff, by(Group) stats(mean min max)
*This will tabulate diffrences alongside group and will also display the mean, min, max

*Two-way tables

*To get a two way contigency cross tabulation table we can use the command tab2 var1 var2
tab2 sex casestatus 

*to bring insert row/column/cell percentages within the table use the option col/row/cell
tab low smoke,row
 
*Two-way tables for each level of sex
 table (agegrp) (race) (sex)
 
 *You may also want some statistics of a variable in relation to  another variable. Eg finding the mean and range of  the variable diff in all categories of the variable group. In this case the option will be by()not over
 tabstat diff, by(group) stats(mean min max)

*Three way tables
*Three-way table of frequencies
 table (agegrp)-confounder (sex race)

 *More tables 
 *Table with percentage of observations in each category of sex and race, for each category of region
table region, stat(fvpercent sex race) nformat(%5.2f)

 *Table with pairwise correlations
table (rowname) (colname), command(r(C): pwcorr age bmi weight)

*Table of regression coefficients
  table colname, command(regress bpsystol age bmi)



*TABULATING
*You may wish to get various descriptive statistics for different variables. The command to use becomes table with the option stats(freq /percent.)
table var1, stat(percent var2 var3 var4)
*This will bring a table of the percentages of var2, var3 and var4 alonngside the observations in var 1
tab race, stat(age, wbs, lwt)

*for a quick description we can use the command describe/des
*describe,simple - just brings variable names
*describe, short- brings variable names together with observations
*describe - contains variables and storage type/data types

*If you wish to count the total number of appeareances of entries beneath the variable name, use the command "tabulate/tab" 

use GOT1
tab epi

*to force stata to tabulate even the missing values we can use the option missing

tab age,missing

*To display a summary of the number of missing data for each of the variables in this data table, we can use the following command
misstable summarize

**#COUNTING**
*to count the number of observations use the function 'count' but it has to be followed with if since in stata 'count' is regarded as a condition * 

count if y <55 & z == 1

*or*
count if y < 55

 *the function count just counts the number of observations, however, you must give it specifications otherwise it will just count the number of rows 

count(no specifications)

count if id >1 (specifaction)  


**#ROUNDING OFF**
*in Stata rounding up is a bit more tedious as the function round doesn't exist. In ths situation, we must generate a new variable with round as an option

gen bmi = weight/(height/100)^2
gen bmi1 = round(bmi,.01)- rounds to two decimal places

*OR
*if you wish to set x to the 3 dp use the function format * 
summarize x, format 

*To get to 2 decimal places use the option format (%6.2f)
summarize x, format(%6.2f) 

**#LISTING/VIEWING** 
*If you wish to view the values in the variable x, use the list command*
list x 

*To view the 5 th value in the x,*
list x in 5

*To view the first 5 values,*
list x in 1/5

* it is possible to restrict the results you see using the if function and setitng conditions*

list y if y 2600

*or*    

list y if z == 82.7 

*or*

list y if y <= 82.7

* if I was using a dataset with more than two columns use the operator & for and and | for or*

list x if y < 55 & z == 1

**#DATA ENTRY/CREATING DATASETS**
*To input data into variable you use the command input but remember to cloe with the end command* 
input y z 
2357  82.7
2551 70.5
2557 47.7
2594 49.1
2600 48.6
2622 56.4
2337 53.6
2637 46.8
2633 55.9
2665 51.4
end 

**#NEW VARIABLES**
* to generate new variables from existing ones we use the function generate*
generate n = z/1000
list n in 1/3 

*to replace a value in a variable with another we use the command "replace". Let us replace 47.7 in the 3 rd row of z with 47.6*
replace z = 47.6 in 3 
list z in 1/3

*let us try substituting the y column with the log of the entire y column*
replace y= log(y)

*sometimes in generating new variables we can use = "" afterwards for string values and . for numerics

gen agecat = ""- *creates a string variable with 227 spaces
gen agecat = . - *creates a numeric variable with 227 spaces

*if you are creating a variable with a chain of conditions, you use generate only at the beginning of the code the rest of the code use replace

gen occup=1 if a2occupation=="2 informal"
replace occup=1 if a2occupation=="3 formal"
replace occup=2 if a2occupation=="1 unemployed"
replace occup=2 if a2occupation=="4 student"


**#DELETING VARIABLES**
* to drop a variable we use the command 'drop'*
drop n

*Just in the same breadth as the drop command you can also choose which variables to keep using the command keep
keep idnumber a1age agecat occcup
keep if sex=="Female"

**#RENAMING NEW VARIABLES**
*rename oldvar newvar
rename family families

**#MISSINGNESS**

* Missing data in Stata is represented with a dot(.)
*To see if the range of the missing data we use the command "misstable summarize "
mistable summarize x


* If you wish to use exclude data thats missing we can use the operation !(not equal to)* 
list x if !missing(x)
*that will display all values excluding the missing ones*

*we can also summarize the variable using the same opeeration*
summarize  x if !missing(x) 

*or*

summarize y if !missing (x)

**#IMPORTING**
*If you wish to import a certain section from a whole dataset consider using the following commands*

*infile- for files seperated by spaces
*insheet- for files seperated by commas (CSV),tabulations

*Consider the following code;
* infile str5 name age byte rep using "fichier.txt",clear

*NB- the specification of the variable type comes before the variable name 

* Stata is instructed to build from the file called fichier.txt a table containing three variables: name, age and rep. The variable name must be explicitily treated as a string of characters(max of 5) and the storage format of the variable rep must be limited to minimum 1 byte.(1 byte = values from -127 to -100)

**#LABELLING**
*In the case of using categorical variables we usually assign numerics to variables such as 0 for no and 1 to mean yes
*Examples

label define yesno 0 "No" 1 "Yes"
label define ethn 1 "White" 2 "Black" 3 "Other"

*NB yesno and ethn are just folder names that store those labels. Not variable names! 

*Next,you must apply the labels you have assigned to the variable column. Here the syntax is label values variablename foldername 

label values ht ui yesno
label values race ethn

* #to remove a label use label drop variable name 
label drop id

**#EGEN**
*The "e" in egen stands for extensions. Egen was specifically made when the generated variables should be generated under certain arguements/extensions
 
 *The arguements could be descriptive statistics e.g mean,mode,max... with the syntax egen newvariablename=arguement()
 *example
  egen Xm = median(10^X) if X > log10(50)
  
 *other arguements
 
        egen [type] newvar = fcn(arguments) [if] [in] [, options]

    by is allowed with some of the egen functions, as noted below.

    Depending on fcn, arguments refers to an expression, varlist, or numlist, and the
    options are also fcn dependent.  fcn and its dependencies are listed below.

        anycount(varlist), values(integer numlist)
            may not be combined with by.  It returns the number of variables in
            varlist for which values are equal to any integer value in a supplied
            numlist.  Values for any observations excluded by either if or in are set
            to 0 (not missing).  Also see anyvalue(varname) and anymatch(varlist).

        anymatch(varlist), values(integer numlist)
            may not be combined with by.  It is 1 if any variable in varlist is equal
            to any integer value in a supplied numlist and 0 otherwise.  Values for
            any observations excluded by either if or in are set to 0 (not missing).
            Also see anyvalue(varname) and anycount(varlist).

        anyvalue(varname), values(integer numlist)
            may not be combined with by.  It takes the value of varname if varname is
            equal to any integer value in a supplied numlist and is missing otherwise.
            Also see anymatch(varlist) and anycount(varlist).

        concat(varlist) [, format(%fmt) decode maxlength(#) punct(pchars)]
            may not be combined with by.  It concatenates varlist to produce a string
            variable.  Values of string variables are unchanged.  Values of numeric
            variables are converted to string, as is, or are converted using a numeric
            format under the format(%fmt) option or decoded under the decode option,
            in which case maxlength() may also be used to control the maximum label
            length used.  By default, variables are added end to end: punct(pchars)
            may be used to specify punctuation, such as a space, punct(" "), or a
            comma, punct(,).

        count(exp)                                                (allows by varlist:)  
            creates a constant (within varlist) containing the number of nonmissing
            observations of exp.  Also see rownonmiss() and rowmiss().

        cut(varname), {at(numlist)|group(#)} [icodes label]
            may not be combined with by.  Either at() or group() must be specified.
            When at() is specified, it creates a new categorical variable coded with
            the left-hand ends of the grouping intervals specified in the at() option.
            When group() is specified, groups of roughly equal frequencies are
            created.

            at(numlist) with numlist in ascending order supplies the breaks for the
            groups.  newvar is set to missing for observations with varname less than
            the first number specified in at() and for observations with varname
            greater than or equal to the last number specified in at().

            group(#) specifies the number of equal-frequency grouping intervals when
            breaks are now specified.  Specifying this option automatically invokes
            icodes.

            icodes requests that the codes 0, 1, 2, etc., be used in place of the
            left-hand ends of the intervals.

            label requests that the integer-coded values of the grouped variable be
            labeled with the left-hand ends of the grouping intervals.  Specifying
            this option automatically invokes icodes.

        diff(varlist)
            may not be combined with by.  It creates an indicator variable equal to 1
            if the variables in varlist are not equal and 0 otherwise.

        ends(strvar) [, punct(pchars) trim [head|last|tail]]
            may not be combined with by.  It gives the first "word" or head (with the
            head option), the last "word" (with the last option), or the remainder or
            tail (with the tail option) from string variable strvar.

            head, last, and tail are determined by the occurrence of pchars, which is
            by default one space (" ").

            The head is whatever precedes the first occurrence of pchars, or the whole
            of the string if it does not occur.  For example, the head of "frog toad"
            is "frog" and that of "frog" is "frog".  With punct(,), the head of
            "frog,toad" is "frog".

            The last word is whatever follows the last occurrence of pchars or is the
            whole of the string if a space does not occur.  The last word of "frog
            toad newt" is "newt" and that of "frog" is "frog".  With punct(,), the
            last word of "frog,toad" is "toad".

            The remainder or tail is whatever follows the first occurrence of pchars,
            which will be the empty string "" if pchars does not occur.  The tail of
            "frog toad newt" is "toad newt" and that of "frog" is "".  With punct(,),
            the tail of "frog,toad" is "toad".

            The trim option trims any leading or trailing spaces.

        fill(numlist)
            may not be combined with by.  It creates a variable of ascending or
            descending numbers or complex repeating patterns.  numlist must contain at
            least two numbers and may be specified using standard numlist notation;
            see numlist.  if and in are not allowed with fill().

        group(varlist) [, missing autotype label[(lblname[, replace truncate(#)])]]
            may not be combined with by.  It creates one variable taking on values 1,
            2, ... for the groups formed by varlist.  varlist may contain numeric
            variables, string variables, or a combination of the two.  The order of
            the groups is that of the sort order of varlist.

            missing indicates that missing values in varlist (either .  or "") are to
            be treated like any other value when assigning groups.  By default, any
            observation with a missing value is assigned to the group with newvar
            equal to missing (.).

            autotype specifies that newvar be the smallest [type] possible to hold the
            integers generated.  The resulting type will be byte, int, long, or
            double.

            label or label(lblname) creates a value label for newvar.  The integers in
            newvar are labeled with the values of varlist or their value labels, if
            they exist.  label(lblname) specifies lblname as the name of the value
            label.  If label alone is specified, the name of the value label is
            newvar.  label(..., replace) allows an existing value label to be
            redefined.  label(..., truncate(#)) truncates the values contributed to
            the label from each variable in varlist to the length specified by the
            integer argument #.

        iqr(exp)[, autotype]                                      (allows by varlist:)  
            creates a constant (within varlist) containing the interquartile range of
            exp.  autotype specifies that newvar be the smallest type possible to hold
            the result.  The resulting type will be byte, int, long, or double.  Also
            see pctile().

        kurt(exp)                                                 (allows by varlist:)  
            returns the kurtosis (within varlist) of exp.

        mad(exp)                                                  (allows by varlist:)  
            returns the median absolute deviation from the median (within varlist) of
            exp.

        max(exp) [, missing]                                      (allows by varlist:)  
            creates a constant (within varlist) containing the maximum value of exp.
            missing indicates that missing values be treated like other values.

        mdev(exp)                                                 (allows by varlist:)  
            returns the mean absolute deviation from the mean (within varlist) of exp.

        mean(exp)                                                 (allows by varlist:)  
            creates a constant (within varlist) containing the mean of exp.

        median(exp)[, autotype]                                   (allows by varlist:)  
            creates a constant (within varlist) containing the median of exp.
            autotype specifies that newvar be the smallest type possible to hold the
            result.  The resulting type will be byte, int, long, or double.  Also see
            pctile().

        min(exp) [, missing]                                      (allows by varlist:)  
            creates a constant (within varlist) containing the minimum value of exp.
            missing indicates that missing values be treated like other values.

        mode(varname) [, minmode maxmode nummode(integer) missing] 
                                                                  (allows by varlist:)  
            produces the mode (within varlist) for varname, which may be numeric or
            string.  The mode is the value occurring most frequently.  If two or more
            modes exist or if varname contains all missing values, the mode produced
            will be a missing value.  To avoid this, the minmode, maxmode, or
            nummode() option may be used to specify choices for selecting among the
            multiple modes.  minmode returns the lowest value, and maxmode returns the
            highest value.  nummode(#) returns the #th mode, counting from the lowest
            up.  missing indicates that missing values be treated like other values.

        pc(exp) [, prop]                                          (allows by varlist:)  
            returns exp (within varlist) scaled to be a percentage of the total,
            between 0 and 100.  The prop option returns exp scaled to be a proportion
            of the total, between 0 and 1.

        pctile(exp) [, p(#) autotype]                             (allows by varlist:)  
            creates a constant (within varlist) containing the #th percentile of exp.
            If p(#) is not specified, 50 is assumed, meaning medians.  autotype
            specifies that newvar be the smallest type possible to hold the result.
            The resulting type will be byte, int, long, or double.  Also see median().

        rank(exp) [, field|track|unique]                          (allows by varlist:)  
            creates ranks (within varlist) of exp; by default, equal observations are
            assigned the average rank.  The field option calculates the field rank of
            exp: the highest value is ranked 1, and there is no correction for ties.
            That is, the field rank is 1 + the number of values that are higher.  The
            track option calculates the track rank of exp: the lowest value is ranked
            1, and there is no correction for ties.  That is, the track rank is 1 +
            the number of values that are lower.  The unique option calculates the
            unique rank of exp: values are ranked 1, ..., #, and values and ties are
            broken arbitrarily.  Two values that are tied for second are ranked 2 and
            3.

        rowfirst(varlist)
            may not be combined with by.  It gives the first nonmissing value in
            varlist for each observation (row).  If all values in varlist are missing
            for an observation, newvar is set to missing for that observation.

        rowlast(varlist)
            may not be combined with by.  It gives the last nonmissing value in
            varlist for each observation (row).  If all values in varlist are missing
            for an observation, newvar is set to missing for that observation.

        rowmax(varlist)
            may not be combined with by.  It gives the maximum value (ignoring missing
            values) in varlist for each observation (row).  If all values in varlist
            are missing for an observation, newvar is set to missing for that
            observation.

        rowmean(varlist)
            may not be combined with by.  It creates the (row) means of the variables
            in varlist, ignoring missing values.  For example, if three variables are
            specified and, in some observations, one of the variables is missing, in
            those observations newvar will contain the mean of the two variables that
            do exist.  Other observations will contain the mean of all three
            variables.  If all values in varlist are missing for an observation, 
            newvar is set to missing for that observation.

        rowmedian(varlist)
            may not be combined with by.  It gives the (row) median of the variables
            in varlist, ignoring missing values.  If all values in varlist are missing
            for an observation, newvar is set to missing for that observation.  Also
            see rowpctile().

        rowmin(varlist)
            may not be combined with by.  It gives the minimum value in varlist for
            each observation (row).  If all values in varlist are missing for an
            observation, newvar is set to missing for that observation.

        rowmiss(varlist)
            may not be combined with by.  It gives the number of missing values in
            varlist for each observation (row).

        rownonmiss(varlist) [, strok]
            may not be combined with by.  It gives the number of nonmissing values in
            varlist for each observation (row).

            String variables may not be specified unless the strok option is also
            specified.  When strok is specified, varlist may contain a mixture of
            string and numeric variables.

        rowpctile(varlist) [, p(#)]
            may not be combined with by.  It gives the #th percentile of the variables
            in varlist, ignoring missing values.  If p() is not specified, p(50) is
            assumed, meaning medians.  If all values in varlist are missing for an
            observation, newvar is set to missing for that observation.  Also see
            rowmedian().

        rowsd(varlist)
            may not be combined with by.  It creates the (row) standard deviations of
            the variables in varlist, ignoring missing values.  If all values in
            varlist are missing for an observation, newvar is set to missing for that
            observation.

        rowtotal(varlist) [, missing]
            may not be combined with by.  It creates the (row) sum of the variables in
            varlist, treating missing as 0.  If missing is specified and all values in
            varlist are missing for an observation, newvar is set to missing for that
            observation.

        sd(exp)                                                   (allows by varlist:)  
            creates a constant (within varlist) containing the standard deviation of
            exp.  Also see mean().

        seq() [, from(#) to(#) block(#)]                          (allows by varlist:)  
            returns integer sequences.  Values start from from() (default 1) and
            increase to to() (the default is the maximum number of values) in blocks
            (default size 1).  If to() is less than the maximum number, sequences
            restart at from().  Numbering may also be separate within groups defined
            by varlist or decreasing if to() is less than from().  Sequences depend on
            the sort order of observations, following three rules: 1) observations
            excluded by if or in are not counted; 2) observations are sorted by
            varlist, if specified; and 3) otherwise, the order is that when called.
            No arguments are specified.

        skew(exp)                                                 (allows by varlist:)  
            returns the skewness (within varlist) of exp.

        std(exp) [, mean(#) sd(#)]                                (allows by varlist:)  
            creates the standardized values (within varlist) of exp.  The options
            specify the desired mean and standard deviation.  The default is mean(0)
            and sd(1), producing a variable with mean 0 and standard deviation 1
            (within each group defined by varlist).

        tag(varlist) [, missing]
            may not be combined with by.  It tags just one observation in each
            distinct group defined by varlist.  When all observations in a group have
            the same value for a summary variable calculated for the group, it will be
            sufficient to use just one value for many purposes.  The result will be 1
            if the observation is tagged and never missing, and 0 otherwise.  Values
            for any observations excluded by either if or in are set to 0 (not
            missing).  Hence, if tag is the variable produced by egen tag =
            tag(varlist), the idiom if tag is always safe.  missing specifies that
            missing values of varlist may be included.

        total(exp) [, missing]                                    (allows by varlist:)  
            creates a constant (within varlist) containing the sum of exp treating
            missing as 0.  If missing is specified and all values in exp are missing,
            newvar is set to missing.  Also see mean().

 
 *arguement cut, at()- at is used to specify the beginning of your classes. Assume you want three classes 0-10,11-20,21-30. you would use at(10,11,30) 0r at(10(10)30)
 
  egen lwt3 = cut(lwt), at(70,120,170,220,270)
  
**#HELP**
*using the help command. Shortform is h *
help graphs- brings help on graphs
h egen

**#CLOSING STATA**
* the function exit,clear closes stataafter saving memory

**#BROWSING**
*the br for browse displays the entire data which you wuold see if you went into the Data-editor

**#CODEBOOK**
*To get in depth info on all variables we can use the command codebook
*use dataset name 
codebook

**#SORTING;CLEANING
*To sort your data in ascending order use the command sort then variable name 

sort age 

*if you want to sort more than two variables 
sort age idnumber id 

* if there are two variables that you wish to sort but keep the odering of the adjacent variable. Say you want to sort names but you want the weights of those pple to carry forward after the odering use the option stable 

sort age,stable 
*Recall this odering from Non-parametric

*to get descending order use gsort - (hyphen)
gsort -age


**#DUPLICATES**
* to get duplicates we use the command duplicates report variablename
duplicates report age 

*to bring up the list of the duplicates use the command duplicates list variabklename 
duplicates list 

*to drop any duplicates in a varaible/column use duplicates drop variablename

duplicates drop age 

*This can work with many variables 
duplicates drop age sex weight height, force

*just to sipmlify work or for better accounting you can create a var that has the frequencies of the duplicates of a particular varaible

duplicates tag a2occupation, gen(dup)

**#DISPLAY**
*the counterpart of print in R here is dis to mean display which just displays a program you have coded. Examples

display 2 + 2
display as text "mean of mpg = " as result r(mean)

**#ARITHMETRICS**
*srqt() -returns the square root
sqrt(64)

*the function mean would also bring the same output(confidence interval) just without observations
mean(age)

**#CONFIDENCE INTERVAL**
*to obtain confidence interval use the command ci means varaiblename- for 95C.I of means and ci variances variablename for 95CI for variance
ci means(age)
ci variances(age )

* To set other intervals other than 95%, use the option level()
ci means age, level(90)

*If you wish to use a binomial distribution to build the confidence interval using the binomial distribution, you can use option binomial, now without stating means
ci low,binomial 

**#CONVERSION**
**String to numeric
*to create a  new numeric variable from a string variable, we use the command encode the variablename,gen(newvariablename)
encode gender, gen (gender_num)

*Or

*to convert numeric to string we use the command tostring,replace
tostring bio,replace

*to convert string to numeric we use the command destring,replace
destring bio,replace

**#RESHAPING
* reshape -- Convert data from wide to long form and vice versa
  
Syntax

    Overview

           long
        +------------+                  wide
        | i  j  stub |                 +----------------+
        |------------|                 | i  stub1 stub2 |
        | 1  1   4.1 |     reshape     |----------------|
        | 1  2   4.5 |   <--------->   | 1    4.1   4.5 |
        | 2  1   3.3 |                 | 2    3.3   3.0 |
        | 2  2   3.0 |                 +----------------+
        +------------+

        To go from long to wide:

                                            j existing variable
                                           /
                reshape wide stub, i(i) j(j)

        To go from wide to long:

                reshape long stub, i(i) j(j)
                                           \
                                            j new variable

        To go back to long after using reshape wide:

                reshape long

        To go back to wide after using reshape long:

                reshape wide


**#PLOTS AND GRAPHS**
*to draw a histogram we use the command histogram/hist. You can set the y-axis to use class /interval ile ya probs 1 by adding the option frequency 
hist variable,frequency

*You use filters to make the output more presentable. bin()-to set the number of bins, or width(). The two annot be used together. start() -sets the value of which the histogram starts with 
hist age, frequency,bin(12),start(3)
 
*The sub-option col(3) increases the column height thus making the histogram(s) taller
 histogram bwt, by(race, col(3)) freq

 *To plot a pie chart use the command graph pie , over(varname)
graph pie,over(sex)

*You can also add the percentages of the slices using the following code plabel which labels the slices 
graph pie, over(sex) plabel(_all percent) 
 graph pie, over(hand) plabel(1 "Left") plabel(2 "Right") legend(off)
*To plot a boxplot use graph box y-axis variable ,over(x-axis variable )
graph box bmi, over(sex)

*To plot a bargraph use the command plot bar y-axis var, over(x-axis var). The expression sum means in the variable frequency the plot will use the sum and not all the observations
graph bar (sum) freq, over(race) ytitle("Ethnicity")

graph bar, over(BMIcat,descending) ascategory asyvar bar(1,fcolor(teal)) bar(2, fcolor(navy)) bar(3, fcolor(maroon))
graph bar (median) price, over(foreign)

*You can also laterally invert the graph by setting the bars to display horizontally using the syntax hbar in stead of bar
graph hbar
*To plot a smooth curve use the command kdesnisty varname 
kdensity bmi (sum) freq, over(race) ytitle("Ethnicity")
*Alternatively you can plot a histogram and use the option discrete
hist bmi,discrete

*To overlay a normal density curve over a histogram use the option normal
hist volume,freq normal

*Try this
hist r , normal xline(-1.96, lcolor(purple) lpattern(dash)) xline  (1.96, lcolor(yellow) lpattern(dash))
*To draw a scatterplot use the command scatter yaxis -varname x-axis varname 
scatter a1age bmi 

*To draw a dot plot use the command graph dot dependentvar ,over(independentvar) 
graph dot lwt,over(low)

*By default, it is the average of the numeric variable (lwt) that is considered, but another statistic can be used by inserting the option (stats). For example, when the median weight must be displayed, the previous command will be replaced by:
. graph dot (median) lwt, over(low)

*To plot a matrix plot:graph matrix var1 var2 var3,half-if you wish to remove the mirror effect of  matrix plot
graph matrix mpg price weight,half

*JUXTAPOSING AND SUPERIMPOSING PLOTS
*To juxtapose(place side by side) use the option by(var3 var 4)
twoway  scatter mpg price,by(foreign age)

*To draw a regression line on the above plot
twoway(scatter mpg price) (lowess mpg price)

*If you wish to draw a scatter plot where the dots are conected use the command twoway connected var1 var2 
twoway  connected bwt lwt


*To superimpose(place one on top of another) use the command graph combine. The graphs had to be saved though.

*To save a graph 
graph save "myPlot.gph", replace

*Now after saving we can combine
graph combine plot1.gph plot2.pgh

*To export a graph 
graph export "myPlot.pdf", as(.pdf)

*(see options to set size and resolution)

**#APPENDING & MERGING**

* If you have dataset A and dataset B you can get dataset A+B(. i.e. B will join at the bottom of B.) using the command append.. But first the dataset has to be in memory. Hint: use the command use datasetA

*IT IS A MASSIVE HELP IF THE DATA IS SORTED

use set A
append using set B 
*To help you track of where each obs is coming from after appending you can use the option gen(filenum) 
append using set B,gen(filenum) 

*If a two datasets have the same variable observations e.g both have a varaiable id and in both id variables have the same the number of observations we can combine the datasets using the command merge(One-one merge). Just like the command append the command merge requires datasetA to be in memory.
*Merge ,however,  does more of  a columnwise merge 

use set A
merge 1:1 id using set B

*It also possible to merge one-many. This is whereby two datasets have a common variable though not necessarily with the same ordering. 
*To merge one to many 
use set A
merge 1:m using set B
*This means that A is the one with many variables

*To merge  many to one.
use setA
merge m:1 using set B
*the command tab_merge shows the number of successful and unsuccesful merges
* 1- shows observations from the master file that couldn' be merged
*2- shows observations from the using file(servant file is called usingfile) that couldn't be merged
*3- shows the observations that were successfully merged


**#SIGNIFICANCE TESTING**
*To perform a two sample test we use the command ttest independentvar, by dependentvar
 ttest height,by (sex)
 
 **#LOGS**
 *Using the log command*
 A full log is a file containing what you type and Stata's output that is shown in the Results window.  To begin logging your session, you type log using filename.
If filename contains embedded spaces, remember to enclose it in double quotation
 marks.
*Examples
*log using mylog
*log close -closes the log file
*log using mylog, append- adds to you current log

*log using "filename containing spaces"

 *log using firstfile, name(log1) text

*log using secondfile, name(log2) smcl

 *log using thirdfile, name(log3) smcl

 *log query _all

 *log close log1

 *log close _all

 *The default output of a log is a SCML file which of course Stata understands but will lead to some distortment if opened by lets say a word processor. Therefore we use the following two ways to solve this:
 
 *1. In stead of using log using session to start things of we use the type command* 
type session.scml 

*2. We can also translate it directly to a word text using the command translate
translate session.scml session.log

*If you wish to be more selective and not record absolutely everything you can use cmd as a prefix for each code. Example
.cmdlog using session
(cmdlog C:\example\session.txt opened)

 use http://www.stata-press.com/data/r13/census5
(Census Data)

. tabulate region [freq=pop]
(output omitted )

. summarize median_age
(output omitted )

. cmdlog close
(cmdlog C:\example\session.txt closed)

*You can view a saved log using 
view logfilename.scml

**#INCREASING MEMORY**
*In some cases you may get a huge dataset and I mean huge datset.. This may prompt an error that suggests obs must be between 1121 and 1121. 

*To increase memory size, type: 
set memory 1g where g=gigabyte, m=megabyte

* You may also need to compress the dataset using the command compress
compress datasetA

*To get the log to base 10 use the command log10(value)
log10(5)

*After installing the appropriate package, now I can find  mode usig the command modes(varname)
modes(CD4_5)

*To get range use the command range(varname)
range(CD4_5)

**#REGRESSION*
*regress dependentvar independentvar
regress bwt lwt
*This will display the Anova table, R squared , adjusted R -squared ...but if you plan is just to get the regression coeff use:
display _b[lwt]
*This will show just the regress coeff , however you have to had done the regression 

*A shorter way to do this would be 
regress bwt lwt,coefflegend
* just coeff wont work

*this is works in tandum with the command corr or pwcorr with  return correlation coefficients 

*The value of the coefficient of determination can also be displayed by making use of e(r2). In the following illustration, we insert the command display with a combination of text and numeric value (rounded to two decimal places):

 display "Coefficient of determination = " %3.2f e(r2)*100 " %"

*a useful command that displays everything post-regression
 ereturn list
 
 *Sometimes we may wish to display the amount variation of variables whose relationship we are trying to establish via a plot(scatterplot). However, under the control of a third confounding variable . Two ways exist on how to do this: either as a combination of plots or just one with the plotting differentiated either by colour or by shapes

*Combination
scatter bwt lwt, by(race)

*One plot
*download a package on sepscatter

sepscatter bwt lwt, separate(race)

 *american english-separate in stead of seperate , though the latter will work tho with a note 
 
*When we want to calculate the fitted values for the model (predicted values of bwt for the observed values of lwt), we will make use of the command predict after an estimation command such as regress. The predicted values will always correspond to the latest regression model:

 predict double p, xb

 *The option xb (which is the default) provides the fitted values of the previous model. It is important to remember that a variable name be specified, here p, to store the predictions! The option double makes it possible to restrict the size of the memory storage of the predicted values
 
*The previous command does not provide any confidence intervals. However, it is not difficult to obtain the standard error for the fitted values and calculate from predicted values the associated confidence intervals. Take the case of the fitted values:
. predict sep, stdp
. generate lci=p- 1.96*sep
. generate uci=p+ 1.96*sep

**#MODEL DIAGNOSTICS*
*The(downloaded from a package) command fitstat can be used. It displays log-lik, adjusted R2, AIC,BIC
fitstat

*If you want a more specific diagnosis, estat can help
estat ic -returns AIC and BIC
estat vif - returns the Variance Inflation Factor

**To obtain the residuals of the regression model (difference between the observedvalues and those predicted according to the model), we always make use of the command predict, specifying this time an option among: residuals (rawresiduals), rstandard (standardized residuals) or rstudent (studentized residuals)

**# MACROS
*This are basically small storage spaces for variables or sub-datasets
*There are two types of macros: global and local. Global is available anywhere in stata as it is stored in the global environment whereas local is only available in the do file they were created. 

*NB- global uses a dollar sign where as local uses the backward qoutation marks(button in front of 1)

*syntax
global nameofyourglobalfolder var1 var2 ,...
local nameofyourlocalfolder var1 var2,...
local nameofyourlocalfolder= 5+5
display `localnameichose'

* application  
regress var2 $globalnameichose
regress var2 `localnameichose'

*For string variables you would have to use quotation marks alongside the speacial characters( backwards quotation/ dollar sign)
local local1 "Abigael "
display "$local1"
*macros can also be used as an alternative to setting a working directory everytime you wish to save a dataset in a certain  folder. In stead of changing your directory to that folder you can just save the macro directly to that folder 

global nameiilke "filepath"
use "$globalnameichose"

*even when you clear the global and local macros would still remain in memory and work. This enables you to work with multiple datasets in stata. 
clear 
global a = 1
local b= 2 
display `a' + $b

*This still works . However, if i used clear all then the macros would then be deleted.

**# Something on strings

 substr(s,n1,n2)
*       Description:  the substring of s, starting at n1, for a length of n2
        
  *                   substr() is intended for use with only plain ASCII characters and for use by programmers who want to extract a subset of bytes from a string.
  *                   For those with plain ASCII text, n1 is the starting character, and n2 is the length of the string in characters.  For programmers, substr() is
  *                  technically a byte-based function.  For plain ASCII characters, the two are equivalent but you can operate on byte values beyond that range.
  *                 Note that any Unicode character beyond ASCII range (code point greater than 127) takes more than 1 byte in the UTF-8 encoding; for example, é
  *                   takes 2 bytes.
*
 *                   To obtain substrings of Unicode strings, see usubstr().
*
 *                    If n1 < 0, n1 is interpreted as the distance from the end of the string; if n2 = . (missing), the remaining portion of the string is returned.

*                     substr("abcdef",2,3) = "bcd"
  *                   substr("abcdef",-3,2) = "de"
*                     substr("abcdef",2,.) = "bcdef"
 *                    substr("abcdef",-3,.) = "def"
  *                   substr("abcdef",2,0) = ""
     *                substr("abcdef",15,2) = ""
       *Domain s:     strings
       *Domain n1:    integers >= 1 and <= -1
       *Domain n2:    integers >= 1
       *Range:        strings


Chat bot written in Prolog used for food recommendations

Course project details with proposal here: [CPSC 312 Course Project](http://wiki.ubc.ca/Course:CPSC312-2017-Restaurant-Recommendation)

Types of queries we will support:
```prolog
ask([i,do,not,like,to,eat,seafood],Res,Food).
```
"Res" is variable for Restaurant
"Food" is variable for food that can be found in the restaurant.

Supported query formats:  
```
I like to eat pizza.  
I like to eat ramen and sushi.  
I like to eat ramen or pizza.  
I like to eat fast food but not pizza.  
I don’t want to eat noodle, pizza, and sushi.  
I want to eat somewhere cheap.  
```
Also supports Dynamically adding to our knowledge base through user inputs:
```
A new place called Sausage in a Bun open and it serves hotdog.
Sausage in a Bun also sells burger.
Sausage in a Bun no longers sells hotdog.
Sausage in a Bun has by closed by food inspectors
```

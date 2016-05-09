# elm-graphing-calculator
A virtual graphing calculator for the web using FRP with Elm

Project Team: Benjamin Rohrer, Zakir Gowani

We built a basic graphing calculator application in pure Elm. Basic binary arithmetic operators are supported (+, -, *, / , ^), and a basic unary operator (~, negation). 

![Snapshot](/imgs/snapshot.jpg?raw=true)

This application was well-suited for a functional language. An example of an advantage: a number of dynamically generated features of the GUI come from mappings of functions onto lists of ranges. 
The graph ticks are generated on the fly, building lists of graphics elements from lists of numbers; the plot itself is the result of mapping a domain to a list of tuples, the second element of each tuple being the output of the function application. 
Another advantage: built-in case expressions made the design of the recursive evaluation logic very straightforward. 

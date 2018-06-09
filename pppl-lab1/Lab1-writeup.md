
#### Valquiria Lucero 
#### Vilok Krishnan


## Lab 1 Writeup

###### 2. Scala Basics: Binding and Scope. For each the following uses of names, give the line where that name is bound. Briefly explain your reasoning (in no more than 1–2 sentences).

###### (a) Consider the following Scala code. 

    1   val pi = 3.14 
    2   def circumference(r: Double): Double = {
    3      val pi = 3.14159
    4      2.0 * pi * r
    5   }
    6   def area(r: Double): Double =
    7   pi * r * r
    
###### The use of pi at line 4 is bound at which line? The use of pi at line 7 is bound at which line?

##### Answer: 
* The use of pi at line 4 is bound at line 3 because line 3 and line 4 exist within the scope of the circumference function. 
* The use of pi at line 7 is bound at line 1 because line 1 and line 7 exist outside of the scope of the circumference function. 

###### (b) Consider the following Scala code.
    1   val x = 3
    2   def f(x: Int): Int =
    3     x match {
    4       case 0 => 0
    5       case x => {
    6         val y = x + 1
    7         ({
    8            val x = y + 1
    9            y
    10        } * f(x - 1))
    11     }
    12   }
    13   val y = x + f(x)
    
###### The use of x at line 3 is bound at which line? The use of x at line 6 is bound at which line? The use of x at line 10 is bound at which line? The use of x at line 13 is bound at which line?

##### Answer: 
* Use of x at line 3 is bound at line 2 because this is the defining parameter, line 3 is a use site instance
* Use of x at line 6 is bound at line 2 because this is where the parameter was defined, line 6 is a use site instance
* Use of x at line 10 is bound at line 2 because this is where the parameter was defined, line 10 is a use site instance
* Use of x at line 13 is bound at line 1 because it exists outside of the function f scope and x was defined/bound as a global variable at line 1

###### 3. Scala Basics: Typing. In the following, I have left off the return type of function g. The body of g is well-typed if we can come up with a valid return type. Is the body of g well-typed?

    1   def g(x: Int) = {
    2      val (a, b) = (1, (x, 3))
    3      if (x == 0) (b, 1) else (b, a + 2)
    4   }

##### Answer: 
The body of g is well typed and has a valid return type of tuple.

    a: int because
        1: int
    b: tuple because 
        (x,3): tuple because 
            x: int
            3: int
    (b , 1): tuple because 
        b: tuple because
            (x, 3): tuple because 
                x: int
                3: int
        1: int
    (b , a + 2): tuple because
        b: tuple because 
            (x, 3): tuple because 
                x: int
                3: int
        a + 2 : int because
            a: int
            2: int
        
The return type appears to be a tuple that consists of another tuple and an int: ((tuple), int)
             

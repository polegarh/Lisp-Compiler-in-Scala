# lisp-compiler-in-scala
This project includes several key steps: 

### 1. Understanding of the project and its steps.
### 2. Basic understanding of coding in lisp
    It may be helpful to look at https://clisp.sourceforge.io/ or https://common-lisp.net/. 
### 3. Code several easy programs in lisp       
Hello World
    
    (quote "Hello World")    

Simple Addition
    
    (+ 1 3)
Fibonacci

    (defun fibonacci (n)
        (cond 
            ( (= n 0) 0)
            ( (= n 1) 1)
            (t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
These programs are used to test this mini compiler

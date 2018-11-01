# Lisp Compiler Written in Scala
This project includes several key steps: 

### 1. Understanding the steps of the compiler
Lexical analysis 

*This is the initial part of reading and analysing the program text: The text is read 
  and divided into tokens, each of which corresponds to a sym- bol in the programming 
  language, e.g., a variable name, keyword or number.
Syntax Analysis 
     
 *This phase takes the list of tokens produced by the lexical analysis and arranges 
     these in a tree-structure (called the syntax tree) that reflects the structure 
     of the program. This phase is often called parsing.
Type checking 
    
 *This phase analyses the syntax tree to determine if the program violates certain 
    consistency requirements, e.g., if a variable is used but not declared or 
    if it is used in a context that does not make sense given the type of the variable, 
    such as trying to use a boolean value as a function pointer.
Intermediate code generation 
    
  *The program is translated to a simple machine- independent intermediate language.
Register allocation 
    
  *The symbolic variable names used in the intermediate code are translated to numbers,
    each of which corresponds to a register in the target machine code.
Machine code generation 
    
 *The intermediate language is translated to assembly language 
    (a textual representation of machine code) for a specific machine architecture.
Assembly and linking 
    
 *The assembly-language code is translated into binary rep- resentation and addresses 
    of variables, functions, etc., are determined.

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

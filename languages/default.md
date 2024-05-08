# Default language
The default supported language is an imperative language without functions capable of representing affine probabilistic programs with non-determinism and invariants. See [examples](/tests/parsers/default).

## Statements
The default language uses five types of statements, some of which use semantics of common imperative languages:
    - assignment
    - if
    - while
    - odds
    - choose

### Assignment statements
Follow traditional semantics. They are used to modify the programs state.
```
<variable> = <affine_polynomial>
```

### If statements
Follow traditional semantics. They branch the execution based on a logical condition. They can use optional else and else-if branches.
```
if <condition> {}
else if <condition> {}
else {}
```

### While statements
Extend traditional semantics. They execute their content in a loop. There are three types of while loops:
    - logical
    - probabilistic
    - non-deterministic

#### Logical
Follow traditional semantics. They check provided condition at the start of every loop, if its true they execute, otherwise jump to the end of the block.
```
while <condition> {}
```

#### Probabilistic
At the start of a loop, a probability, given in the form of odds to execute its contents to skip, is evaluated. For example, 3:1 is interpreted as three to one, making the probability of execution 3/4 and the probability of skipping 1/4.
```
while <n>:<m> {}
```

#### Non-deterministic
At the start of a loop, it is non-deterministically chosen whether the content of the loop should execute or be skipped.
```
while * {}
```


### Odds statements
They are used for probabilistic branching, selecting a branch based on the odds. For example when given the odds 1:1 the probability of execution of each of its two branches is the same.
```
odds <n>:...:<m> {} ...
```

### Sampling statements
Sampling is used to assign a random value to the variable. The minimum, maximum and expected value are set as arguments.
```
<variable> = random(<min>, <max>, <E>)
```


### Choose statements
They non-deterministically choose exactly one of their branches to execute.
```
choose {} or ... or {}
```


## Invariants
Invariants restrict the set of values program variables can attain during execution. One can specify an invariant before the execution of a statement or at the end of the program. When not specified, there's no restriction to program variables. An invariant is a system of inequalities connected with the **and** keyword. Multiple inequalities invariants in a row are treated as if connected by an **or** operator. For example the following invariant constrains the variable **a** to the interval [0, 5) while restricting **b** to be less than **a**.
```
[a >= 0 and a > b]
[a < 5 and a > b]
```


## Affine programs
A program is affine when every arithmetic expression it contains is a linear polynomial. This restriction applies to invariants as well, since the programs state can only ever attain values resulting from affine expressions. 





# Haskell Interpreter

This project is a simple interpreter written in Haskell. It is designed to evaluate expressions and demonstrate the power of functional programming.

The language that is interpreted is called AHA, it's syntax is block based denoted with brackets and is pretty simple. 

## Features

- Basic arithmetic operations
- Variable assignments
- Simple conditional
- Printing values
- Looping (for loop, do loop, while loop)
- Conditional operators (<,>,>=,<=)
- Lists and basic operations (pop, append, remove and add)
- Comments
- Function definitions
- Basic logical operation
- Basic module handling
- Loop flow control (haltLoop, next, next if, next unless)

## Examples

Here are some examples of expressions you can evaluate with the interpreter:

### Basic arithmetic
```
1 + 2 * 3 / 2
```

### Variable assigment
```
x = 2
```

### Simple conditional
```
x = 10
y = 20
if x {
    result = x + y
} else {
    result = 0
}
```

### Printing values
```
x = 10
y = "Hello"
print x
print y
z = y + " World"
print z
```

### For loop
```
for (x = 0; x < 10; x = x + 1) {
    print x
}
```

### Conditional operators
```
print "Greater than"
print 5 > 1
print 5 > 6

print "Less than"
print 5 < 1
print 5 < 6

print "Equal to"
print 5 == 1
print 5 == 6

print "Not equal to"
print 5 != 1
print 5 != 6

print "Greater than or equal to"
print 5 >= 1
print 5 >= 6

print "Less than or equal to"
print 5 <= 1
print 5 <= 6
```

### Lists
```
x = [1, 2, 3]
y = x[1]
print y 

print x 
x = x << "hellows world" # appends value to the end of list
print x 

x = x >> 0 # Deletes the item at position 0 from the list
z = x =>> 0 # Pops the item at position 0, returning the element
print x 
print z

x = x << 1 <<= 5 # Add 5 to the list, placing it a the index 1 position 
print x
```

### Comments
```
# This is a comment

/*
    This is a
    multiline
    comment :)
*/
```
### Functions
```
def add(x, y) {
    return x + y
}

def sayHello(){
    print "Hello World"
}

x = 5
y = 10
z = add(x, y)

sayHello()
print z
```

### Logical operations
```
print true and false
print true and true

print true or true
print false or false
print false or true

print not false
print not true
```

### Modules
```
import ./example/funcs

print "calling a function of the funcs.aha file, inside imports.aha 👀"
print add(1,2)
```

### While loops
```
x = 1 

do {
    print x 
    x = x + 1
} loop (x < 0)

y = 1
loop (y < 5) {
    print y
    y = y + 1
} 
```

### Loop flow control

```
loop (true) {
    print "once"
    haltLoop
}

for (i = 0; i < 5; i = i + 1) {
    next if i < 4
    print i
}

for (i = 0; i < 5; i = i + 1) {
    next unless i < 3
    print i
}

for (i = 0; i < 5; i = i + 1) {
    next
    print i
}
```
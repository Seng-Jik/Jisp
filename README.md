# Jisp
Jisp Programming Language

## Examples

[HelloWorld.jisp](Examples/HelloWorld.jisp)
```
print-str-ln "Hello, world!"
```

[Fibonacci.jisp](Examples/Fibonacci.jisp)
```
($fibo (Y (λ self n 
    (? (| (= n 0) (= n 1)) 
        1
        (+ (self (- n 1)) (self (- n 2)))))))


generate 20 fibo
```

[PrintCommandLineArguments.jisp](Examples/PrintCommandLineArguments.jisp)
```
ignore (map print-str-ln argv)
```

[stdlib.jisp](Jisp/stdlib.jisp)

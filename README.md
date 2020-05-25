# Jisp
Jisp Programming Language

## Examples

[HelloWorld.jisp](Examples/HelloWorld.jisp)
```
print-str-ln "Hello, world!"
```

[Fibonacii.jisp](Examples/Fibonacii.jisp)
```
($fibo (Y (λ self n 
    (? (| (= n 1) (= n 2)) 
        1
        (+ (self (- n 1)) (self (- n 2)))))))

($fibo-count 20)

($make-fibo-list (Y (λ self n
    (? (<= n fibo-count)
        (cons (fibo n) (self (+ n 1)))
        () ))))

(make-fibo-list 1)
```

[stdlib.jisp](Jisp/stdlib.jisp)

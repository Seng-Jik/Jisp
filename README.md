# Jisp
Jisp Programming Language

## Examples

[HelloWorld.jisp](Examples/HelloWorld.jisp)
```
print-str-ln "Hello, world!"
```

[Call-CC.jisp](Examples/Call-CC.jisp)
```
;; Basiclly Call-CC
($_ (print (+ 1 (call-cc (λ cc (cc 1))))))

;; Count down by loop function
($_ (print-str-ln ""))
($_ 
	(loop 10 
		(λ break continue state 
			(	
				($_ (? (% state 2) () (continue (- state 1))))
				($_ (print state))
				($next-state (- state 1))
				(? (> next-state 0)
					next-state
					(break 0) 
				) 
			) 
		)
	)
)

()

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

# Jisp 编程语言

一个基于 λ-演算 的饥饿求值编程语言。    

## 工具链用法

##### 以交互式编程方式启动
```shell
jisp
```

##### 解释运行一个Jisp程序
```shell
jisp jisp程序源代码 [命令行参数]
```

##### 编译一个Jisp程序
```shell
jisp -c jisp程序源代码 输出
```

## 特性
* 使用Y-组合子实现递归
* Call-CC以及使用Call-CC实现循环、异常控制流
* 与F#编程语言具有良好的交互性
* 完全“纯”的计算环境
* 支持Eval
* 极小语言核心，使用库来充实功能

## 例子程序

[HelloWorld](Examples/HelloWorld.jisp)
```
print-str-ln "Hello, world!"
```

[Call-CC与控制流例子](Examples/Call-CC.jisp)    
[打印斐波那契数列](Examples/Fibonacci.jisp)     
[打印命令行参数](Examples/PrintCommandLineArguments.jisp)     
[生命游戏](Examples/LifeGame.jisp)      
[Brainf*ck语言解释器](Examples/Brainfxxk.jisp)    
[Monad例子](Examples/BoxMonad.jisp)        
[Jisp标准库](Jisp/stdlib.jisp)    

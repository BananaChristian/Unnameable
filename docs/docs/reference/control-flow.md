# Control flow

## If, elif, else

```unn
func if_test(i32 val):void {
    if(val == 1) {
        trace 1
    } elif(val == 2) {
        trace 2
    } else {
        trace "I dont know"
    }
}
```

Conditions are wrapped in parentheses. `elif` is used for additional branches rather than `else if`.

## While loop

```unn
func while_test:void {
    mut i32 x = 1
    while(x < 3) {
        x++
        trace x
    }
}
```

## For loop

```unn
func for_test:void {
    for(mut i32 i = 0; i < 3; i++) {
        trace i
    }
}
```

The loop variable is declared inline with a full type declaration. The variable must be `mut` since the loop increments it.

## Switch

```unn
func switch_test(i32 val):void {
    switch(val) {
        case 1:
            trace 1
        case 2:
            trace 2
        default:
            trace "No Num"
    }
}
```

Unlike C, the compiler always breaks after each case. Fallthrough is not possible as each case is isolated by design.
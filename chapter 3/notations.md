## Notations

| **Syntax**	       | **Description**                  |
|--------------------|----------------------------------|
| 𝜌                  | env                              |
| [ ]                | Empty env                        |
| [var = val] 𝜌      | (extend-env var val 𝜌)           |
| «exp»              | AST for expression exp           |
| ⌈n⌉                | (num-val n)                      |
| ⌊n⌋                | (expval->num val)                |

### Note: $⌈⌊n⌋⌉=n$ 
---

### Example without comments
```scheme
Let ρ = [i=1, v=5, x=10].

(value-of
  <<-(x,3), -(v,i)>>
  ρ)

= ⌈-(
      ⌊(value-of <<-(x,3)>> ρ)⌋ 
      ⌊(value-of <<-(v,i)>> ρ)⌋
  )⌉

= ⌈(-
    (-
      ⌊(value-of <<x>> ρ)⌋
      ⌊(value-of <<3>> ρ)⌋)
   ⌊(⌈(value-of <<-(v,i)>> ρ)⌋)⌉ 

= ⌈(-
    ⌈(-
       10
       ⌊(value-of <<3>> ρ)⌋)
      (value-of <<-(v,i)>> ρ))⌉

= ⌈(-
    ⌈(-
      10
       3)
      ⌊(value-of <<-(v,i)>> ρ)⌋)⌉ 

= ⌈(-
    ⌈(-
      10
       3)
      ⌊(value-of <<-(v,i)>> ρ)⌋)⌉ 
```


### Example with comments
```kotlin
Let ρ = [i=1, v=5, x=10].

(value-of
  <<-(x,3), -(v,i)>>
  ρ)

= ⌈-(
      ⌊(value-of <<-(x,3)>> ρ)⌋ 
      ⌊(value-of <<-(v,i)>> ρ)⌋
  )⌉
  // Evaluate the value-of <<-(x,3)>> and value-of <<-(v,i)>> separately.

= ⌈(-
    (-
      ⌊(value-of <<x>> ρ)⌋
      ⌊(value-of <<3>> ρ)⌋)
   ⌊(⌈(value-of <<-(v,i)>> ρ)⌋)⌉ 

  // x is 10 in the environment ρ. So, (value-of <<x>> ρ) evaluates to 10.
  // 3 is a constant, so (value-of <<3>> ρ) evaluates to 3.

= ⌈(-
    ⌈(-
       10
       ⌊(value-of <<3>> ρ)⌋)
      (value-of <<-(v,i)>> ρ))⌉

    // Compute 10 - 3 to get 7.

= ⌈(-
    ⌈(-
      10
       3)
      ⌊(value-of <<-(v,i)>> ρ)⌋)⌉ 

    // Simplify the inner expression to 7.

= ⌈(-
    ⌈(-
      10
       3)
      ⌊(value-of <<-(v,i)>> ρ)⌋)⌉ 
  // Now we need to evaluate <<-(v,i)>> with v=5 and i=1 in the environment ρ.
```

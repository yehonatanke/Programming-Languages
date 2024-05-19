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

//... 

= ⌈3⌉
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
  // Start by evaluating the expressions <<-(x,3)>> and <<-(v,i)>> separately.

= ⌈(-
    (-
      ⌊(value-of <<x>> ρ)⌋
      ⌊(value-of <<3>> ρ)⌋)
   ⌊(⌈(value-of <<-(v,i)>> ρ)⌋)⌉ 

  // Evaluate <<x>> and <<3>> within the environment ρ.
  // In ρ, x = 10 and 3 is a constant.

= ⌈(-
    ⌈(-
       10
       ⌊(value-of <<3>> ρ)⌋)
      (value-of <<-(v,i)>> ρ))⌉

  // Compute 10 - 3 and evaluate the inner expression <<-(v,i)>> within ρ.

= ⌈(-
    ⌈(-
      10
       3)
      ⌊(value-of <<-(v,i)>> ρ)⌋)⌉ 

  // Simplify to 7 and evaluate <<-(v,i)>> in the environment ρ.

= ⌈(-
    ⌈(-
      10
       3)
      ⌊(value-of <<-(v,i)>> ρ)⌋)⌉ 

//... 


= ⌈3⌉
```

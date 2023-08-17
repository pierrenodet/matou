---
layout: page
---

The following table is a cheat sheet of operators defined in `matou`, yet it is not exhaustive:

| Concept (Operator) | Category Syntax | Math Syntax |
|--------------------| :-------------: | :---------: |
| Binary Operations (hadamard) |  | a + b |
|  |  | a - b |
|  |  | a ⊙ b |
|  |  | a ⊘ b |
| Broadcasted Operations (broadcast) |  | a :+ b |
|  |  | a :- b |
|  |  | a :* b |
|  |  | a :/ b |
| Composition (matmul) | a >>> b | a * b |
|                      | a <<< b | a ∘ b |
| Norm (froebenius) |  | a ⋅ b | 
| Product (kroenecker) | a *** b | a ⊗ b |
| Khatri Rao Product (khatrirao) | a ### b | a ∗ b |
| Face Splitting Product (facesplitting) | a &&& b | a ∙ b |
| Diagonal (diagonal) | ∇ |  |
| Coproduct (directsum) | a +++ b | a ⊕ b |
| Horizontal Concatenation (hcat) | a \|\|\| b |  |
| Vertical Concatenation (vcat) | a === b |  |
| Codiagonal (codiagonal) | Δ |  |
| Convolution (convolve) |  | a ⋆ b |

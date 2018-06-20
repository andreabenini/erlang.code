# Multiple guards
It is possible to use multiple guards within the same function definition or expression.
When using multiple guards, a semicolon, `;` signifies a boolean "**OR**", while a comma, `,` signifies boolean "**AND**".
```
the_answer_is(N) when N == 42, is_integer(N) -> true;
geq_1_or_leq_2(N) when N >= 1; N =< 2 -> true;
```

---
title: C++—Composition de fonctions
date: 2013-05-30
logo: cpp
lang: fr
---

Lorsque l’on a gouté aux joies des langages fonctionnels, ou que
l’on vient du monde des mathématiques, ou les deux, on est souvent
habitué à "chainer" des appels de fonctions entre eux. Par exemple, si
nous disposons de `n` fonctions de prototypes suivants :

```haskell
f1 :: T1 -> T2
f2 :: T2 -> T3
...
fm :: Tm -> Tn
fn :: Tn -> To
```

Nous aimerions pouvoir les composer de la manière suivante :

```haskell
fn o fm o ... o f2 o f1
```

Ce qui dans un langage comme C++ nous obligerait à faire :

```c++
auto res1 = f1(arg);
auto res2 = f2(res1);
...
auto resn = fn(resm);
```

Ce qui est assez peu élégant. De plus, nous pourrions avoir envie
qu’il n’y ait pas de copie dans les différents passages de
paramètres (ce que nous pouvons faire en passant les arguments par
référence, ou en utilisant un *move semantic*). Puisque nous ne
faisons rien des résultats intermédiaires, nous aimerions disposer
d’une fonction, qui prendrait les fonctions en arguments, ainsi que
l’argument à donner à la première fonction, et s’occuperait de
faire les appels successifs, tout en passant les arguments par move
semantic ou référence. Une telle fonction peut-être créée en `c++11`,
voici une solution possible.

L’idée est assez simple, nous utilisons un template récursif qui
correspond a peu de choses près à la fonction suivante, le reste
n’est que du surplut visant à propager les types et déterminer le
type de retour des fonctions (avec `std::result_of`) :

```c++
pipeline(arg, f1, f_suivantes) = pipeline(f1(arg), f_suivantes)
```

Et enfin voici le code C++ correspondant :

```c++
#ifndef PIPELINE_HH_
# define PIPELINE_HH_

namespace
{
    //
    // Extends the behavior of std::result_of for pipelines of function,
    // setting field type to the type returned by the last function of
    // the pipeline.
    //
    template <typename In, typename F, typename ...Args>
    struct result_of : public result_of<typename std::result_of<F(In)>::type, Args...> {};

    template <typename In, typename F>
    struct result_of<In, F>
    {
        typedef typename std::result_of<F(In)>::type type;
    };
}


//
// Pipeline of function
// usage : pipeline(T arg, f1, f2, ..., fn) with:
// f1: T -> T1
// f2: T1 -> T2
// ...
// fn: Tn -> Tm
// returns the result of fn(...f2(f1(arg)))
//
template <typename In>
In pipeline(In&& a)
{
    return std::forward<In>(a);
}

template <
    typename In,
    typename Function>
auto pipeline(In&& a, Function f) -> decltype (f(std::forward<In>(a)))
{
    return f(std::forward<In>(a));
}

template <
    typename In,
    typename Function,
    typename ...Args>
auto pipeline(In&& a, Function f, Args... args) -> typename result_of<In, Function, Args...>::type
{
    return pipeline<decltype (f(std::forward<In>(a)))>(f(std::forward<In>(a)), args...);
}

#endif /* !PIPELINE_HH_ */
```

N’hésitez pas à me faire part de vos remarques.

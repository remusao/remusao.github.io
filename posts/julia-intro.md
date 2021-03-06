---
title: Julia
date: 2014-02-13
logo: julia
lang: fr
---

Aujourd'hui j'aimerais parler d'un langage de programmation que je viens de
découvrir : **Julia**. Je vais donc vous le présenter succinctement et donner
mon ressenti après quelques jours d'utilisation.

## Qu'est-ce que c'est ?

Le Julia est un langage :

* **Haut niveau** (_garbage collection_)
* **Dynamique**
* **Interprété**
* **Disposant d'un compilateur JIT** (_backend LLVM_)
* **Performant**

Il s'agit d'un subtile mélange entre les langage **Python**, **R** et
**Matlab**. Python pour une partie de la syntaxe et les constructions élégantes du
langage (_list comprehension_, boucles _for_, etc.), Matlab et R pour l'intégration
des vecteurs, matrices et opérations associées dans la bibliothèque standard,
en faisant un langage particulièrement adapté pour l'analyse numérique, le
_machine learning_, et toute autre application nécessitant des outils d'algèbre
linéaire, et des fonctions mathématiques diverses.

```julia
# Dynamic type
a = 42          # Int
a = "toto"      # String
a = [1, 2, 3]   # Vector
a = [1 2 3]     # 1x3 Matrix
a = [1:3]       # same 1x3 Matrix

for e in a
    println(test)
end

# Output
# 1
# 2
# 3
```


Julia dispose également des atouts suivants :

* **Multi-méthode** permettant un dispatch dynamique en fonction du type des arguments passés aux fonctions

```julia
# Method declaration
# foo is a function taking one argument of any type
function foo(bar)
    println(bar)
    end

# Shorter function declaration
baz(x) = println(x)

# Multimethod
# Takes an argument of type Int (Haskelish syntax)
myprint(x::Int) = println("Int")
# Takes an argument of type Matrix (of any type)
myprint(x::Matrix) = println("Matrix")
# Take an argument of type Matrix of Float64
myprint(x::Matrix{Float64}) = println("Matrix of Float64")

myprint(42)         # First one is called
myprint([1 2 3])    # Second one is called
myprint([0.5 0.42]) # Third one is called
```

* Un système de **template** pour créer du code générique

```julia
function printtype(m)
    println("Any type")
end

# Takes argument of type T, in julia a type
# is an object, so you can print it
function printtype{T}(m::T)
    println(T)
end

# Takes an argument of type T, with the constraint FloatingPoint
function printtype{T <: FloatingPoint}(m::T)
    println("Floating Point")
end

printtype(42)
printtype(0.42)
```

* Des **expressions rationnelles** compatible Perl
* Des **macros** à la Lisp grâce à l’auto-iconicité du langage
* La possibilité de lancer des **commandes Shell**, de piper, etc.
* **Interfaçage avec le C** sans bindings (simple utilisation de ccall comme une fonction)

```julia
# Simple function that wrap a call to libc clock function
clock() = ccall( (:clock, "libc"), Int32, ())

t = clock()

# Do some stuff

println(clock() - t)
```

* **Duck-typing** mais possibilité de spécifier les types
* **Parallélisation** du code aisée en multithreading sur une unique machine ou sur un cluster (tasks, parallel for, etc.)



Cet article n'a pas pour but d'être exhaustif à propos de Julia, pour plus
d'exemples je vous redirige vers le documentation officielle du langage :
[Documentation](http://docs.julialang.org/en/latest/manual/).



## Pourquoi l'utiliser ?

Julia est un langage jeune mais dynamique. Il est pour le moment peu utilisé
mais il gagne à être connu. Grâce aux avantages cités ci-dessus, c'est un
parfait candidat pour tous les projets nécessitant :

* **Performances** (compilation JIT),
* **Bibliothèque standard** très riche : _algèbre linéaire_, _fonctions mathématiques_ diverses, etc.
* Rassemble les qualités de plusieurs langages bien connus (Matlab, R, Python)


Il est très aisé de convertir du code Matlab ou Python vers du Julia car les
différences sont assez peu nombreuses. La manipulation de matrice est très
similaire à celle de Matlab.



## Défauts de jeunesse

Néanmoins le langage dispose de quelques points faibles, sans doute liés à sa
jeunesse et au manque de projets l'utilisant. Mis à part la richesse de sa
bibliothèque standard, les packages sont encore relativement peu nombreux et
donc selon le projet vous ne trouverez pas forcément les outils nécessaires
(par exemple pour du traitement d'image, il n'existe pas de binding ou
d'équivalent à OpenCV, des packages existent mais les fonctionnalités sont
encore limitées).

L'interpréteur met du temps à se lancer. Selon les ressources de la machine
que vous utilisez cela peut aller de 2-3 secondes à 30 secondes (sur mon
ordinateur portable peu puissant). Bien sûr il est possible de lancer une
session de l'interpréteur une bonne fois pour toutes et de travailler depuis
le prompt, mais cela reste gênant quand vous voulez simplement lancer un
script. Néanmoins, cela devrait s'améliorer à l'avenir, espérons-le, le
langage est encore en phase de release candidate.

Voilà pour cette rapide introduction du langage Julia, cet article n'avait
pas pour but d'être un tutoriel, mais juste de faire connaitre un projet qui
semble avoir de l'avenir. Il est possible que j'utilise ce langage dans mes
prochains articles pour les démonstrations de code.

Quelques ressources supplémentaires :

* [Github](https://github.com/JuliaLang/julia)
* [Documentation](http://docs.julialang.org/en/latest/manual/)
* [Site officiel](http://julialang.org/)

---
layout: post
title:  "Installation de Julia"
date:   2014-02-14 22:00:42
categories: julia
---


Suite au premier article sur le langage Julia, voici un guide rapide de mise en
route de votre environnement pour utiliser le langage. Voici le plan :

* Compiler Julia depuis les sources
* Coloration syntaxique et indentation sous vim
* Environnement IJulia (équivalement de IPython)



## Compilation

Tout d'abord compiler l'interpréteur Julia depuis les sources. Notez que si
votre distribution _linux_ dispose d'un paquet Julia dans ses dépôt ou si vous
ne désirez pas compiler Julia depuis les sources, il est toujours possible
d'installer une version pré-compilée depuis la
[page téléchargement](http://julialang.org/downloads/) du site officiel.

Commençons tout d'abord par récupérer les sources de Julia :

{% highlight sh %}
git clone https://github.com/JuliaLang/julia.git
cd julia
{% endhighlight %}

Ensuite, vérifiez que les dépendances suivantes sont bien disponibles sur votre
système :
* [GNU make](http://www.gnu.org/software/make/)
* [gcc et g++](http://gcc.gnu.org/) (ou [Clang](http://clang.llvm.org/))
* [gfortran](http://gcc.gnu.org/)
* [git](http://git-scm.com/)
* [perl](http://www.perl.org/)
* [wget](http://www.gnu.org/software/wget/) (ou [curl](http://curl.haxx.se/), ou _fetch_)
* [m4](http://www.gnu.org/software/m4/)
* [patch](http://www.gnu.org/software/patch/)

Si tout est installé, lancez la compilation (notez que **N** est à remplacer
par le nombre de cœurs dont dispose votre processeur, cela peut grandement
améliorer le temps de compilation) :

{% highlight sh %}
make -j N
{% endhighlight %}

La première fois que vous compilez Julia, des dépendances vont être téléchargées
puis compilées, c'est pourquoi cela peut prendre un certains temps. La
compilation peut consommer jusqu'à _700 Mo de mémoire_ et _1.5 Go d'espace
disque_. Les éventuelles compilations futures seront moins gourmandes.

Une fois la compilation terminée, vous disposez d'un
[lien symbolique](http://en.wikipedia.org/wiki/Symbolic_link) vers l'exécutable
Julia. Afin qu'il soit accessible depuis n'importe quel endroit, vous pouvez
le rajouter à votre `PATH` (vous pouvez le rajouter dans le fichier de
configuration de votre Shell favori en remplaçant `$(pwd)` par le chemin
absolu vers le dossier julia dans lequel se trouve le lien symbolique) :

{% highlight sh %}
export PATH="$(pwd):$PATH"
{% endhighlight %}

Julia intègre tout le nécessaire pour gérer l'installation, la mise à jour
et la création de packages. Ces fonctionnalités sont disponibles depuis un
prompt Julia. La première fois que vous lancez Julia, il est nécessaire
d'initialiser les packages :

{% highlight sh %}
$ julia
julia> Pkg.update()
{% endhighlight %}

Les principales commandes disponibles sont :

* **Pkg.update()** : met à jours les différents packages installés
* **Pkg.add("Package")** : installe le package _Package_ ainsi que ses éventuelles dépendances
* **Pkg.rm("Package")** : supprime le package _Package_



## Coloration syntaxique et indentation


Cette explication n'est valide que pour les utilisateurs de l'éditeur
[Vim](http://www.vim.org/). Voici des explications différentes en
fonction de la manière dont vous gérez les extensions.

#### Pathogen

{% highlight sh %}
cd ~/.vim
mkdir -p bundle && cd bundle
git clone git://github.com/JuliaLang/julia-vim.git
{% endhighlight %}


#### Vundle

Ajouter un nouveau Bundle à votre `.vimrc` :
{% highlight sh %}
Bundle 'JuliaLang/julia-vim'
{% endhighlight %}

Lancer Vim et mettre à jour vos Bundle :
{% highlight sh %}
:BundleInstall!
{% endhighlight %}


#### Manuel

{% highlight sh %}
git clone git://github.com/JuliaLang/julia-vim.git
cd julia-vim
cp -R * ~/.vim
{% endhighlight %}

Voilà qui devrait vous fournir la coloration syntaxique ainsi que l'indentation
pour les fichier dont l'extension et `.jl`.



## IJulia

IJulia permet d'interfacer Julia à l'environnement de développement interactif
**IPython**. Cela permet notamment d'utiliser le mode **notebook**, qui combine
du code, du texte, et des contenus multimédias (dessin, etc.) dans un même
environnement. Pour l'installer vous aurez besoin d'avoir sur votre système :

* **IPython** en version `1.0` ou supérieure
* [Jinja2](http://jinja.pocoo.org/docs/), [Tornado](http://www.tornadoweb.org/en/stable/), et [pyzmq](https://github.com/zeromq/pyzmq)

Ensuite il faut installer le package IJulia depuis un prompt julia :

{% highlight julia %}
$ julia
julia> Pkg.add("IJulia")
julia> Pkg.update()
{% endhighlight %}

Une fois ceci fait, vous n'avez plus qu'à lancer IPython de la manière
suivante. Pour le mode notebook :

{% highlight sh %}
ipython notebook --profile julia
{% endhighlight %}

Et voilà, le tour est joué. C'est tout pour cet article. Vous devriez
normalement avoir de quoi démarrer à programmer en Julia et exécuter
vos programmes. A bientôt pour de nouvelles aventures !


Afin de réaliser cet articles je me suis inspiré de la documentation présente
sur les dépôts officiels de [Julia](https://github.com/JuliaLang/julia),
[julia-vim](https://github.com/JuliaLang/julia-vim) et
[IJulia](https://github.com/JuliaLang/IJulia.jl).

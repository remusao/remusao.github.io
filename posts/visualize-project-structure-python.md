---
title: Visualiser la structure d’un projet python
date: 2012-07-31
logo: python
lang: fr
---

Lors d’une de mes dernières contributions à un projet écrit en
Python, j’ai voulu trouver un moyen de rapidement pouvoir visualiser
la structure d’un projet selon plusieurs critères (dépendances
entres les modules, héritages entre les objets, etc.). J’ai donc
écris un script Python qui permet de générer un graphe à partir de
différentes données.

[Project sur GitHub](https://github.com/remusao/PyProjectViewer)

J’ai essayé de faire en sorte de rentre le projet flexible afin de
pouvoir scanner d’autres types de données à l’avenir (et pourquoi
pas d’autres langages). Pour le moment on peut générer le graphe des
importations ainsi que le graphe de l’héritage.

Le projet est divisé en deux parties :

1. Les fichiers Scan, qui doivent respecter l’interface d’un scanner.
2. Un objet modelViewer qui s’occupe de parcourir tout votre projet et d’appeler son scanner sur chaque fichier.

Ainsi on peut tout à fait rajouter des Scanners à volonté. Pour
finir, voici un graphe de dépendance d’un de mes projets en Python.
Le graphe est coloré et chaque module (chaque dossier) possède sa
propre couleur.

N’hésitez pas à faire des remarques ou à proposer des améliorations !

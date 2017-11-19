---
title: Compiler Mediatomb sur archlinux ARM
date: 2012-07-31
logo: raspberry
lang: fr
---

En voulant installer le logiciel mediatomb sur le Raspberry Pi que
j’ai reçu récemment je me suis heurté à quelques problèmes
puisque la version dans les dépots de Archlinux ne compilait pas et
aucun paquet précompilé pour Arm n’était disponible. Je vais donc
exposer une solution pour pouvoir le compiler et l’installer. Mais
tout d’abord une petite description du logiciel trouvée sur le site
des créateurs :

> MediaTomb is an open source (GPL) UPnP MediaServer with a nice web user interface, it allows you to stream your digital media through your home network and listen to/watch it on a variety of UPnP compatible devices.

La marche à suivre pour arriver à compiler le paquet est assez simple,
il suffit de télécharger un patch supplémentaire et de remplacer le
PKGBUILD fourni par un autre que je vais vous donner. Voici comment
faire :

1. Premièrement vous devez télécharger le patch suivant : [patch](http://bugs.debian.org/cgi-bin/bugreport.cgi?msg=5;filename=libavformat_0.11_support.patch;att=1;bug=677959).
2. Téléchargez également le fichier PKGBUILD suivant : [PKGBUILD](http://www.pythux.com/exemples/PKGBUILD).
3. Exécutez les commandes suivantes :

```sh
$ cd /tmp
$ yaourt -G mediatomb
$ cd mediatomb
[Copier le patch ici]
[Copier le fichier PKGBUILD ici]
$ makepkg -si
```

Sur le raspberry Pi la compilation prend un certain temps, mais ça
vaut le coup. J’espère que ça servira à certains le temps que les
paquets dans les dépôts d’archlinux soient de nouveau compilables
sans modification.

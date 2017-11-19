---
title: Raspberry Pi
date: 2012-07-31
logo: raspberry
lang: fr
---

J’ai reçu il y a quelques jours le Raspberry Pi que j’avais
commandé sur le site Element 14. Voici une description que j’ai
honteusement recopiée depuis le site d’archlinux pour architecture
arm :

> The Raspberry Pi is a credit-card sized computer that plugs into
> your TV and a keyboard. It’s a capable little PC which can be used
> for many of the things that your desktop PC does, like spreadsheets,
> word-processing and games. It also plays high-definition video.

> The Raspberry Pi measures 85.60mm x 53.98mm x 17mm, with a little
> overlap for the SD card and connectors which project over the edges. The
> SoC is a Broadcom BCM2835. This contains an ARM1176JZFS with floating
> point running at 700Mhz, and a Videocore 4 GPU. The GPU is capable of
> BluRay quality playback, using H.264 at 40MBits/s.

C’est donc assez rigolo de bidouiller sur une ordinateur au format de
poche. Sa puissance limitée n’est pas faite pour tous les usages,
mais puisqu’il ne prend pas beaucoup de place, ne fait aucun bruit et
ne consomme pas beaucoup d’énergie, il est tout à fait adapté pour
servir de lecteur multimédia (avec mediatomb par exemple).

Comme vous vous en doutez j’ai installé archlinux (la version arm
dédiée au raspberry pi que vous pouvez télécharger gratuitement
ici : archlinux – arm). L’avantage c’est qu’ils fournissent
une image que vous avez juste à installer sur votre carte SD, et vous
n’avez pas à passer par la procédure d’installation classique des
distributions linux. J’ai donc pu me connecter directement en SSH
après avoir lancé mon raspberry Pi. Ce qui était plutôt pratique
puisque je n’avais pas de clavier usb sous la main.

Par contre, prévoyez au moins une carte SD de 4 Go pour faire tourner
votre système. J’en utilise une de 2 Go et en ayant installé
quelques paquets elle est pleine à 94%. Je l’utilise actuellement
pour streamer du contenu présent sur un de mes disques durs externe via
Mediatomb. Cela permet d’accéder à vos vidéos, photos, musiques
depuis n’importe quel périphérique supportant la technologie uPnP.

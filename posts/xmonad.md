---
title: XMonad
date: 2013-02-14
logo: xmonad
lang: fr
---

Étant utilisateur de GNU/Linux depuis quelques années maintenant,
j’ai eu l’occasion de tester quelques distributions et quelques
environnements de bureau. Parmi les distributions je suis passé par
Ubuntu, Debian, Fedora pour enfin essayer Archlinux, que je n’ai plus
quitté depuis. En terme d’environnement c’est un peu la même
chose, gnome, xfce, lxde, openbox puis Awesome que j’ai utilisé
pendant une année. Je n’ai jamais trop cherché à modifier la
configuration par défaut, qui me convenait plutôt bien. Puis je me
suis mis au Haskell, et avec lui des envies de refaire le monde, des
envies de pureté, des envies de paresse (enfin pas tant que ça !), et
j’ai ouï dire qu’il existait un gestionnaire de fenêtre écrit
avec ce fabuleux langage : XMonad !

## Présentation de XMonad

XMonad est un gestionnaire de fenêtre de style « tiling » (c’est
à dire qu’il est capable de gérer tout seul la disposition des
fenêtres afin d’optimiser l’espace). Il est léger, épuré, très
minimaliste. En fait la première fois qu’on le lance il n’y a rien,
à part un curseur de souri ! C’est un bon début. A la différence
d’Awesome qui propose tout de même un minimum syndical (une barre de
tâches, une vue des différents tags, l’heure…). Sur XMonad rien
de tout ça. J’ai vraiment eu l’impression qu’on me fournissait
des briques de base, et qu’ensuite c’était à moi de jouer pour me
construire mon propre gestionnaire de fenêtre, et c’est en quelque
sorte ce que j’ai fait. Heureusement les ressources se trouvent assez
facilement sur internet, à partir de la documentation, d’articles et
d’exemples, on parvient à créer un petit environnement douillet en
quelques heures (à condition de connaitre quelques bribes de Haskell,
car oui, XMonad se configure en Haskell !). Au final, il n’y a pas à
dire, c’est un gestionnaire de fenêtre fonctionnel.

## Installation

Pour les utilisateur de Archlinux il suffit d’exécuter la commande
suivante ! Pour les autres, un petit tour par la documentation de votre
distribution favorite devrait vous éguiller.

```sh
$ pacman -S xmonad xmonad-contrib
```

## Configuration

Venons-en aux sujets qui fâchent, à savoir, la configuration. Comme
dit un peu plus haut, ça se fait en utilisant le langage Haskell et
on place tout ça dans un fichier nommé xmonad.hs dans un dossier
~/.xmonad. Puisque c’est du Haskell, vous l’aurez deviné, ça
se compile. Heureusement cela se fait à chaud, sans avoir besoin de
quitter XMonad, ce qui est très appréciable. Grâce au raccourci Mod
+ q le fichier de configuration est recompilé, si il est correct il
est appliqué à chaud, sinon une fenêtre s’ouvre vous indiquant les
éventuelles erreurs, et l’ancienne configuration est conservée.
Comme ça, pas de risque de casser votre environnement en le modifiant
en cours d’utilisation.

Je vais vous présenter ma configuration actuelle, qui est assez
basique, mais comble amplement mes attentes. Ensuite je vais détailler
quelques points importants.

```haskell
{- Config of berson_r -}

import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map        as M
import qualified XMonad.StackSet as W


-- Main
main = do
    xmproc <- spawnPipe "xmobar /home/berson_r/.xmobarrc" -- lance xmobar avec le bon fichier de config
    spawn "nitrogen --restore" -- affiche le fond d'écran
    xmonad $ defaultConfig { -- redéfinition de certaines options de XMonad
    -- simple stuff
    terminal           = myTerminal,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    -- mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    logHook            = myLogHook xmproc,
    startupHook        = myStartupHook
    }


-- Binding for the mod key
myModMask :: KeyMask
myModMask = mod4Mask

-- Default terminal to use
myTerminal :: String
myTerminal = "urxvt"

-- Names of the workspaces
myWorkspaces :: [String]
myWorkspaces = ["term", "web", "mail", "music"] ++ (map show [5..9])

-- Color of normal borders
myNormalBorderColor :: String
myNormalBorderColor  = "#7c7c7c"

-- Color of the selected window's borders
myFocusedBorderColor :: String
myFocusedBorderColor = "#ffb6b0"

-- Color of current window title in xmobar.
xmobarTitleColor :: String
xmobarTitleColor = "#FFB6B0"

-- -- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Custom layout
myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}


myStartupHook = return ()

-- Custom rules
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "web"
    , className =? "Firefox"        --> doShift "web"
    , className =? "Uzbl-core"      --> doShift "web"
    , className =? "Thunderbird"    --> doShift "mail"
    , className =? "Spotify"        --> doShift "music"
    , className =? "Vlc"            --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Gimp"           --> doFloat
    , manageDocks]
--    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

myLogHook :: Handle -> X ()
myLogHook xmproc = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn xmproc,
    ppTitle = xmobarColor "green" "" . shorten 50
}


-- Key bindings --
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

  -- Custom key bindings

  [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- start term
  , ((modMask, xK_r), spawn "exe=`dmenu_path_c | yeganesh` && eval \"exec $exe\"") -- dmenu
  , ((modMask .|. controlMask, xK_m), spawn "amixer -q set Master toggle") -- mute volume
  , ((modMask .|. controlMask, xK_j), spawn "amixer -q set Master 10%-") -- dec volume
  , ((modMask .|. controlMask, xK_k), spawn "amixer -q set Master 10%+") -- inc volume
  , ((modMask, xK_g), goToSelected defaultGSConfig) -- display selection grid
  -- workspaces
  , ((modMask, xK_Right), nextWS)
  , ((modMask .|. shiftMask, xK_Right), shiftToNext)
  , ((modMask, xK_Left), prevWS)
  , ((modMask .|. shiftMask, xK_Left), shiftToPrev)

  -- "Standard" xmonad key bindings

  , ((modMask .|. shiftMask, xK_c), kill) -- close selected window
  , ((modMask, xK_space), sendMessage NextLayout) -- change layout
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- reset layout
  , ((modMask, xK_n), refresh) -- resize windows to the correct size
  , ((modMask, xK_Tab), windows W.focusDown) -- move focus to next window
  , ((modMask, xK_j),   windows W.focusDown) -- move focus to next window
  , ((modMask, xK_k),   windows W.focusUp)   -- move focus to previous window
  , ((modMask, xK_m),   windows W.focusMaster) -- move focus to master window
  , ((modMask, xK_Return), windows W.swapMaster) -- swap focused and master window
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown) -- swap focused and next window
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp) -- swap focused and previous window

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_p] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
```

Globalement, on peut résumer l’organisation du fichier de configuration comme ceci :

1. Les imports permettant de faire appel à des fonctionnalités de
XMonad, ou toute autre bibliothèque Haskell (car vous pouvez vraiment
mettre n’importe quoi dans votre fichier de configuration, si vous
le vouliez vous pourriez coder factorielle, Fibonacci et un crible
d’Ératosthène pour les afficher dans votre barre des tâches, aucun
problème !).
2. Des redéfinitions d’options ou de comportements sous forme de constantes.
3. Une fonction Main qui regroupe tout ceci et spécifie à XMonad
toutes les options, et les comportements que vous désirez. Globalement
vous pourrez quasiment tout modifier via un type enregistrement. Les
champs non spécifiés auront leur valeur par défaut, on se contente
donc de spécifier ce qui nous intéresse.

## Barre d’état

Par défaut, XMonad ne dispose pas d’une barre des tâches. Il en
existe une qui est également en Haskell et qui s’intègre très
simplement avec XMonad, il s’agit de XMobar. Elle est très légère
et permettra de répondre à la plupart des besoins. Il suffit pour la
mettre en place de :

1. Disposer d’un fichier de configuration situé ici : `~/.xmobarrc`
(vous trouverez le mien un peu plus bas)
2. Rajouter la ligne : `xmproc <- spawnPipe "xmobar /home/berson_r/.xmobarrc`
en haut de votre fonction main (cf mon fichier de configuration ci-dessus).
3. Indiquer quelles informations XMonad doit transmettre à XMobar
lors de l’exécution (les workspaces par exemple). Pour ceci il faut
redéfinir le myLogHook dans le main, je vous invite encore une fois à
regarder mon exemple donné un peu plus haut.

```haskell
Config { font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*"
, bgColor = "black"
, fgColor = "grey"
, position = TopW L 90
, lowerOnStart = True
, commands = [ Run Date "%a %b %_d %H:%M" "date" 10
, Run StdinReader
]
, sepChar = "%"
, alignSep = "}{"
, template = "%StdinReader% }{ <fc=#ee9a00>%date%</fc> =<<"
}
```

Attention, contrairement aux apparences, ce n’est pas du Haskell,
donc ne mettez pas de commentaires dans ce fichier. Une fois les deux
étapes décrites plus hauts effectuées, xmobar devrait s’afficher au
lancement de XMonad. Vous remarquerez qu’il affiche les workspaces,
l’application qui a le focus, l’heure, la date et … c’est tout.
En plus il y a un espace de 10% de la taille de l’écran sur la
droite. Il servira à mettre un trayer. C’est justement l’objet de
la section suivante.

## System Tray

Par défaut, ni XMonad ni xmobar ne proposent un « trayer », où vous
pourriez voir les icônes des applications en cours d’exécution.
Pour cela j’utilise trayer, qui est très léger et facile à mettre
en place. En fait, une fois qu’on lui a réservé une petite place
(ce qui est notre cas), il suffit de le lancer avec les bonnes options
depuis notre .xinitrc (ou .Xsessions). Voici la commande telle que je
l’utilise :

```sh
trayer --SetDockType true       \
       --SetPartialStrut true   \
       --align right            \
       --alpha 0                \
       --edge top               \
       --expand true            \
       --height 12              \
       --heighttype pixel       \
       --tint 0x000000          \
       --transparent true       \
       --width 10 &
```

## Fond d’écran

Vous l’aurez compris, XMonad ne propose pas non plus par défaut
d’utilitaire pour changer l’image ou la couleur du fond d’écran.
Mais il est relativement simple d’en mettre un en place. J’ai
personnellement utilisé nitrogen. Il est très simple à utiliser, la
première fois il suffit de le lancer à la main et d’aller chercher
le fond d’écran désiré. Ensuite, il suffira de le relancer à
chaque démmarage avec la commande :

```sh
nitrogen --restore &
```

Et c’est exactement le rôle de la ligne 23 dans la fonction main du
fichier `xmonad.hs` :

```haskell
spawn "nitrogen --restore"
```

## Raccourcies clavier utiles

Le seul raccourci qu’il me manquait afin que ce soit parfait, était
le `« mod + flèche »` afin de se déplacer sur le workspace de droite
ou de gauche. Pour ce faire, il suffit de rajouter les lignes suivantes
dans le tableau des raccourcis personnalisés :

```haskell
, ((modMask, xK_Right), nextWS)
, ((modMask .|. shiftMask, xK_Right), shiftToNext)
, ((modMask, xK_Left), prevWS)
, ((modMask .|. shiftMask, xK_Left), shiftToPrev)
```

Il y a d’autres options que je n’ai pas détaillées dans cet
article, mais j’ai laissé quelques commentaires dans le fichier de
configuration afin de guider la compréhension. Si jamais ils ne sont
pas suffisents, n’hésitez pas à laisser un commentaire pour poser
une question, ou faire une remarque.

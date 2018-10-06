---
title: Force VSYNC on Linux desktop for Nvidia graphic cards
---

If you are getting awful [tearing](https://en.wikipedia.org/wiki/Screen_tearing) in on your Linux desktop while trying to watch a video or play a game, then you need to force your desktop compositor to [VSYNC](https://en.wikipedia.org/wiki/Analog_television#Vertical_synchronization). On Arch Linux, I have noticed tearing on Gnome 3 and Mate desktop environments.

[A post on Gaming on Linux](https://www.gamingonlinux.com/articles/i-have-finally-found-a-way-to-sort-out-screen-tearing-on-nvidia-with-linux.7213) explains the problem well and offers a solution for Nvidia users. However, I found the [most useful solution in the comments section](https://www.gamingonlinux.com/articles/i-have-finally-found-a-way-to-sort-out-screen-tearing-on-nvidia-with-linux.7213/page=7#r61138). The way I understand the solution is an additional compositor can be applied which forces VSYNC, even if the current desktop environment does not. A script was shared which turns on and off the additional compositor pipeline.

My script for enabling VSYNC is:
<script src="https://gist.github.com/dmp1ce/82debdbdba40776f64c4e33ab759843b.js"></script>

Then, if I want to undo this additional compositing layer, I run:
<script src="https://gist.github.com/dmp1ce/c178bf8d21379d761f5809972327c0c2.js"></script>

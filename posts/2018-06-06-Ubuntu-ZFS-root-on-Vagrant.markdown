---
title: Ubuntu ZFS root on Vagrant
date: 2018-06-06 19:15:00
---

When building my local server which runs a variety of services using [Docker](https://en.wikipedia.org/wiki/Docker_%28software%29) I wanted to make sure I has a dependable file system, so of course I chose [ZFS](https://en.wikipedia.org/wiki/ZFS).
For my first server I used [ext4 filesystem](https://en.wikipedia.org/wiki/Ext4) as the root partition and only used ZFS for storing the application data which the services depended.
[This is the recommended approach by many.](https://serverfault.com/a/798692/156354)
However, I want added benefits of having the entire root partition using ZFS.
The benefits include:

* Drive redundancy on the system level files such as `/boot` and `var`
* [Snapshots](https://en.wikipedia.org/wiki/ZFS#Snapshots) of the entire system
* [Docker using ZFS driver](https://docs.docker.com/storage/storagedriver/zfs-driver/)
* Needing one less drive used only for root
 
However, I wanted to test my ideas before buying the hardware needed to run the server.
To work through the challenges I might find by running a root ZFS system I decided build myself a [VM](https://en.wikipedia.org/wiki/Virtual_machine).
I really want to use [Ubuntu 18.04](https://en.wikipedia.org/wiki/Ubuntu_version_history#Ubuntu_18.04_LTS_(Bionic_Beaver)) because has [community support for ZFS](https://github.com/zfsonlinux/zfs/wiki/Ubuntu-18.04-Root-on-ZFS), the problem is that [Vagrant](https://en.wikipedia.org/wiki/Vagrant_%28software%29) has no Ubuntu 18.04 ZFS root boxes available for me to test with.
Here is how I went about creating a Vagrant box with root ZFS for Ubuntu 18.04.

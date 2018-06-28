---
title: Ubuntu ZFS root on Vagrant
date: 2018-06-28 14:28:03
---

When building my local server, which runs a variety of services using [Docker](https://en.wikipedia.org/wiki/Docker_%28software%29), I wanted to make sure I had a dependable file system.
Of course I chose [ZFS](https://en.wikipedia.org/wiki/ZFS).
For my first server I used [ext4 filesystem](https://en.wikipedia.org/wiki/Ext4) as the root partition and only used ZFS for storing the application data which the services depended.
[This is the recommended approach by many.](https://serverfault.com/a/798692/156354)
However, I want the additional benefits of having the entire root partition using ZFS.
The benefits include:

* Drive redundancy on the system level files such as `/boot` and `/var`
* [Snapshots](https://en.wikipedia.org/wiki/ZFS#Snapshots) of the entire system
* [Docker using ZFS storage driver](https://docs.docker.com/storage/storagedriver/zfs-driver/)
* Needing one less drive used only for root
 
To test my ideas before buying the hardware needed to run the server I needed to [virtualize the environment](https://en.wikipedia.org/wiki/Virtual_machine).
I really wanted to use [Ubuntu 18.04](https://en.wikipedia.org/wiki/Ubuntu_version_history#Ubuntu_18.04_LTS_(Bionic_Beaver)) because it has [community support for ZFS](https://github.com/zfsonlinux/zfs/wiki/Ubuntu-18.04-Root-on-ZFS).
The problem is that [Vagrant](https://en.wikipedia.org/wiki/Vagrant_%28software%29) has no Ubuntu 18.04 ZFS root boxes available for me to test with.

At first I researched [how to create a Vagrant box from an Ubuntu install ISO](https://blog.codeship.com/packer-vagrant-tutorial/) but I quickly became stuck when I realized [Debain](https://www.ubuntu.com/community/debian) has not shipped the [partman-zfs](https://packages.debian.org/sid/partman-zfs) package which is needed to partition a disk with ZFS on install.
Theoretically, it should still be possible to script the root ZFS install within [Packer](https://www.packer.io/intro/getting-started/vagrant.html) using shell commands, but that sounded too error prone and tedious for my liking.
I also was unsure how to configure the [preseed.cfg](https://wiki.debian.org/DebianInstaller/Preseed) to make the installer do what I wanted.
Maybe it would have been a better use of my time to work on whatever issues are holding back partman-zfs as that is what should really be doing all the lower level partitioning work.

Eventually, I decided to take a [currently working Ubuntu 18.04 Vagrant box](https://app.vagrantup.com/ubuntu/boxes/bionic64) and migrate the filesystem root to a ZFS root partition.
The steps I used were:

1. Initialize a Vagrantfile with Ubuntu 18.04 base
2. Creates an additional disk for the VM
3. Partition the new disk for ZFS
4. Create the ZFS root pool
5. Copy the root system to the ZFS root pool 
6. Make the ZFS root pool bootable
7. Reboot

Feel free to view my [source to build the box](https://github.com/dmp1ce/ubuntu-root-ZFS) or [start using the box right away](https://app.vagrantup.com/dmp1ce/boxes/ubuntu1804zfs).

I am happy with the results as I can test all the features of ZFS and how they effect the root Ubuntu system.
In particular, I'm curious how docker will perform with ZFS as the storage driver.

In the future I would like to mirror sda and sdc disks.
Maybe someone could help me create a Vagrant box from the Ubuntu install ISO as I would prefer a clean root ZFS install over the solution I came up with.

Have fun testing ZFS based Ubuntu setups!

I used the following guides heavily in creating this post.

- [Move Existing Linux Install to ZFS root](https://blog.heckel.xyz/2016/12/31/move-existing-linux-install-zfs-root/)
- [Ubuntu 18.04 Root on ZFS](https://github.com/zfsonlinux/zfs/wiki/Ubuntu-18.04-Root-on-ZFS)

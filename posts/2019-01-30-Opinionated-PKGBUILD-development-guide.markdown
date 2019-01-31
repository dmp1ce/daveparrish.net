---
title: Opinionated development guide for Arch User Repository package maintainers
---

# Introduction

I learned most of what I know about developing and maintaining an Arch Linux package by reading [documentation on the Arch Linux Wiki](https://wiki.archlinux.org/index.php/Creating_packages) and by studying [several example packages posted on the Arch User Repository](https://aur.archlinux.org/packages/). Later I discovered tools to make maintaining packages easier to develop, less prone to errors and more inline with community standards.

# Terms

- [Arch Linux](https://www.archlinux.org/) — Rolling Linux distro based on the Pacman package manager. This post only applies to Arch Linux derived operating systems.
- PKGBUILD — The source file used to build the Arch Linux package. It is a shell script. I often use PKGBUILD and Arch Linux Package synonymously.
- [AUR](https://aur.archlinux.org/) — The Arch User Repository. Where most non-offical user created packages are centrally stored.

# Tools

- [clean-chroot-manager](https://aur.archlinux.org/packages/clean-chroot-manager/) — Command line tool for building PKGBUILD scripts in a clean environment. Also `ccm64` or `ccm32` on the command line.
- [VirtualBox](https://wiki.archlinux.org/index.php/VirtualBox) — Convenient virtual machine software for testing packages.
- [Spacemacs](https://wiki.archlinux.org/index.php/Spacemacs) — The editor I use for editing the PKGBUILD source. Any editor should work but Spacemacs checks syntax with [shellcheck](https://www.archlinux.org/packages/community/x86_64/shellcheck/) and provides syntax highlighting.
- [git](https://wiki.archlinux.org/index.php/Git) — Source control tool used to publish to the AUR.

# Process

Usually I am creating PKGBUILDs for software projects which I use but are not available as a package in Arch Linux. What I first need to do is figure out how the project is built by reading the project's documentation and then I'll craft my own PKGBUILD or copy another PKGBUILD which is close to what I need. If you don't have any PKGBUILD examples, I suggest looking through the [AUR packages](https://aur.archlinux.org/packages/).

Please make sure the package you want to create isn't available already on the AUR. See the [naming](#naming) section.

## Editing

I use the Spacemacs editor to craft the `PKGBUILD` file to my needs. To get Spacemacs to work for PKGBUILDs, you'll need to:

  - [Install Spacemacs](https://wiki.archlinux.org/index.php/Spacemacs#Install_Emacs)
  - Install [shellcheck](https://www.archlinux.org/packages/community/x86_64/shellcheck/) (`pacman -S shellcheck`)
  - Enable the [Shell Scripts layer](http://develop.spacemacs.org/layers/+lang/shell-scripts/README.html) in Spacemacs
  - Turn on `shell-script-mode` in Spacemacs while editing the `PKGBUILD`.
  
You'll then be able to start modifying the shell commands without needing to worry about common mistakes, which the syntax highlighter and shellcheck will show you in Spacemacs.

## Building

When building the PKGBUILD, you should be careful not to assume other Arch Linux users will have the same packages installed as you do. In order to test in a "clean" environment, I use [clean-chroot-manager](https://aur.archlinux.org/packages/clean-chroot-manager/). clean-chroot-manager will create a clean chroot environment and build the PKGBUILD in that environment.

### tmpfs

I wanted to build packages completely in memory in order to avoid unnessary disk usage and speed up builds. In order to do this `ccm64` (clean-chroot-manager) creates a `~/.conf/clean-chroot-manager.conf` file which can be configured to build in a custom directory. I set that directory to be `/mnt/chroot64` and I made that directory a [tmpfs partition](https://wiki.archlinux.org/index.php/Tmpfs). In order to create the tmpfs partition I have the following entry in my `/etc/fstab` file.

```
# AUR development chroot
tmpfs	/mnt/chroot64	tmpfs	mode=1777,strictatime,nodev,size=10G 0 0
```

This entry will create a 10-gigabyte tmpfs partition on reboot. Make sure your system has enough memory before doing this! This partition can also be manually mounted with `mount /mnt/chroot64`.

### chroot

Once `ccm64` is configured how you want, you can setup a chroot in your PKGBUILD directory by running `ccm64 c`. You'll need to be the root user in order for the command to work. I always use `sudo ccm64`.

Use `ccm64 s` to build the PKGBUILD. A successful build will leave a `.tar.xz` file as well as several log files. If there are other AUR dependencies, then they can be added by building in another directory and adding with `ccm64 a`.

Take note of the [namcap](https://wiki.archlinux.org/index.php/Namcap) errors, as namcap is good at finding errors in the PKGBUILD that aren't always obvious, such as dependencies which are missing or not needed.

## Testing

For many packages it may be perfectly fine to test on your host OS. However, often I find it helpful to install on a VirtualBox VM to make sure it installs and runs as expected in a newly installed desktop environment. I used an [Antergos ISO](https://antergos.com/try-it/) to quickly get an Arch Linux distro installed. I then created a snapshot of the "clean" environment so I can reset back to a "clean" state every time I want to test a new package.

I did have some trouble sharing my `.tar.xz` files with the VirtualBox VM using guest additions so I ended up using sshfs to send the file to the guest. In order to do this, you'll need to run [sshd on the guest](https://wiki.archlinux.org/index.php/OpenSSH#Daemon_management) and then [connect sshfs to the guest from the host](https://wiki.archlinux.org/index.php/SSHFS#Mounting). I created a simple script mount sshfs for me:

```
#!/bin/bash
sshfs -p 3022 dave@localhost:/home/dave/aur_drop /home/david/Workspace/aur/guest_drop

```

Some things I check for in the VM are:

- Is the desktop icon showing up correctly?
- Is the desktop icon launching the application?
- Is the application running as expected?
- Is the application missing any dependencies at runtime which are causing it to fail?

## Publishing

Once you are sure your PKGBUILD is ready to share with others, you'll want to publish it to the AUR. Here are the steps to publish:

- Find an available name in the AUR.
- Generate a `.SRCINFO` file.
- Initialize a git repository with the correct remote.
- Commit code changes.
- Push the code to the AUR.

### Naming

No two packages on the AUR can have the same name. Most of the time this isn't a problem because often if someone wants to create a package based on the latest git commit or latest binary release, they can create a [new package with a suffix](https://wiki.archlinux.org/index.php/Arch_User_Repository#What_is_the_difference_between_foo_and_foo-git_packages?) such as `-git` or `-bin`. The name chosen needs to also be included in the `PKGBUILD` as the `pkgname` variable value.

### Git

Once an unused name is chosen, initialize a git repository (if you haven't done so already) and create an upstream remote like so:

```
git init
git remote add origin ssh://aur@aur.archlinux.org/$(EXAMPLE).git
```

Replace `$(EXAMPLE)` with the name of your package.

Commit your `PKGBUILD` and `.SRCINFO` files (along with any other source files you need for your package) with git. I use [maggit](https://www.emacswiki.org/emacs/Magit) in Spacemacs as a front-end to git but this isn't necessary.

Then push your code to the AUR using git. For example `git push origin master`. You will need to both have created an AUR account and [uploaded your SSH key](https://wiki.archlinux.org/index.php/Arch_User_Repository#Authentication) for the push to work.

# Conclusion

At this point you should have a published package on the AUR, which users can install manually with `makepkg` or use one of the several [AUR helpers](https://wiki.archlinux.org/index.php/AUR_helpers) to install. The AUR allows for flagging a package when out-of-date and comments when something isn't working right. So, expect feedback about your package if it is used and left unmaintained. Don't worry, you can disown or delete an unmaintained package, but try to keep it up-to-date based on feedback and recent releases.

Have fun and feel free to ask me for help if you get stuck.

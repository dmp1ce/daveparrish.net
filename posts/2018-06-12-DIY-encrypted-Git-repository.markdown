---
title: DIY encrypted Git repository
date: 2018-06-12 11:50:11
---

If you are like me and you don't want to depend on any services if you don't have to, you might have wanted to run your own Git repository.
You might have also wanted to encrypt your repository.
In my case, I'm using [plain text accounting](http://plaintextaccounting.org/) and managing the changes to the ledger with [Git](https://git-scm.com/).
Here are the components I used to create my own encrypted Git repository.

The easy way to get encrypted Git repositories to use the excellent [Keybase](https://keybase.io/blog/encrypted-git-for-everyone) services.
In my case, I already had a private Git server setup using [Gitolite](http://gitolite.com/gitolite/).
I used [Docker](https://www.docker.com/what-docker), [docker-compose](https://docs.docker.com/compose/) and [some home grown bash scripting](https://github.com/dmp1ce/decompose-gitolite) to setup the Gitolite service on [Vultr](https://www.vultr.com/) infrastructure.
Since I already had the Git service, it was just a matter of encrypted the data before it goes to the server.
For encrypting push and decrypting pull commands I am using [git-remote-gcrypt](https://github.com/spwhitton/git-remote-gcrypt).
I had looked at [git-crypt](https://www.agwa.name/projects/git-crypt/) but it seems that git-crypt is better for encrypting a few files while git-remote-gcrypt is better for encrypting the entire repository.
The only encryption method available for git-remote-gcrypt is [GPG](https://gnupg.org/), which is OK with me, but it would have been nice to have the option for symmetric encryption with a passphrase file.

To install, all I needed to do was `apt install git-remote-gcrypt` on [Debian/Ubuntu](https://www.ubuntu.com/community/debian) but I could also easily install on a [Mac](https://en.wikipedia.org/wiki/MacOS) using the `install.sh` script.
The install script is only copying a [Git hook](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks).

Once installed, you'll need need to specify the GPG keys to use for encrypting the data.
First create the remote.

```
git remote add cryptremote gcrypt::rsync://example.com:repo
```

Then set the participants keys.

```
git config remote.cryptremote.gcrypt-participants "KEY1 KEY2"
```

The only other thing I did was to set git-remote-gcrypt to publish keys because it was too annoying for me to type in the passphrase for every private key on my computer.

```
git config remote.cryptremote.gcrypt-publish-participants true
```

At this point, your push commands to the `cryptremote` repository will be encrypted using the participants keys and fetches will be decrypted using your private key.

Make sure the users of the Git repository have rewrite permissions.
In Gitolite, `RW+` gives users the permission to rewrite history.
Rewrite permission is important for git-remote-gcrypt because each push is a complete rewrite of history.
Be careful to pull before pushing or else you will override the latest pushed changes!
The other participant would need to push their changes again.

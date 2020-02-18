---
title: Run BorgBackup automatically on Linux
---

# Introduction

Most people will have some files they want to keep secret. In order to keep the files secret, it is recommended to keep them in a encrypted partition. One popular method for encrypting a files is [eCryptfs](https://wiki.archlinux.org/index.php/ECryptfs).

To prevent important files from being lost, a backup could be taken of the `~/.Private` folder, which is where all of the encrypted files exist. This is sufficient for recovering the files, as long as you can setup the eCryptfs partitions correctly again. However, I want to backup specific files from the unencrypted `~/Private` partition just in case I want to quickly roll back a specific file.

The problem is, how can I start the backup only when the `~/Private` folder is mounted and unencrypted files are available? I will describe my solution to the problem which uses [BorgBackup](https://www.borgbackup.org/), [borgmatic](https://torsion.org/borgmatic/), [systemd](https://www.freedesktop.org/software/systemd/man/systemd.service.html) and some [bash scripting](http://mywiki.wooledge.org/BashGuide).

# BorgBackup

The first step is to get a BorgBackup repository working.

## Read the docs

If just getting started, I recommend reading through the [BorgBackup documentation](https://borgbackup.readthedocs.io/en/stable/quickstart.html). For our purposes, you will need to know how to initialize a BorgBackup repository so backups can be stored somewhere.

## SSH repository

I use an SSH repository to a server. Because the access to the server is managed by an SSH key file, a password prompt can be avoided to make it easier to do an automated backup.

My BorgBackup init command looked something like this: `borg init -e repokey-blake2 david@myserver:backup_dir`.

## Use a strong passphrase

If you are backing up unencrypted files, you'll want to make sure to use a strong passphrase so that no one with access to the backup repository can view the files. The passphrase will be used every time BorgBackup is ran using the environment variable `BORG_PASSPHRASE`.

## Remember the passphrase

The passphrase is needed if you ever need to recover the files. Put the passphrase in a location separate from the files being backed up or the location of the encrypted backup files. I recommend a password manager that is ideally backed up a different way, such as Bitwarden or a replicated Keepass database.

I also backed up the borg init command just in case the encryption algorithm and server directory I was using was lost.

## Borg helper scripts

I never remember the exact arguments to use with BorgBackup to initiate backups and list my backups with, so I created a simple script. 


``` bash
#!/bin/bash
# borg.bash

# Helper script to run borg commands
export BORG_REPO="david@myserver:backup_dir"
export BORG_PASSPHRASE; BORG_PASSPHRASE='my-secure-passphrase'

borg "$@"

```

I will sometimes use this script to list files backed up using the following commands:

```
$ ./borg.bash list
myserver-2020-01-30T09:13:05.446876       Thu, 2020-01-30 09:13:06 [4...1]
myserver-2020-02-04T06:48:15.991816       Tue, 2020-02-04 06:48:16 [2...0]
myserver-2020-02-12T09:47:39.618958       Wed, 2020-02-12 09:47:40 [0...3]

$ ./borg.bash list ::myserver-2020-01-30T09:13:05.446876
-rw-r--r-- david  david      3115 Wed, 2020-01-29 10:54:59 home/david/Private/file1
drwxrwxrwx david  david         0 Wed, 2018-12-26 18:59:58 home/david/Private/file2
-rw-r--r-- david  david        67 Sat, 2017-04-29 09:47:41 home/david/Private/note.txt
```

I rely on borgmatic to do the actual backup command, since a backup involves listing many folders and files.

*WARNING: Storing the passphrase in a script is a security risk! Consider keeping scripts with passphrases in an encrypted volume.*

# Borgmatic

Setup a borgmatic configuration to easily run borgmatic. I personally never run the `borg` command directly to do backups. I first create a borgmatic configuration and always us the `borgmatic` command to launch `borg` for me.

## More docs

Like BorgBackup, I recommend referring to the [documentation for borgmatic](https://torsion.org/borgmatic/). Borgmatic has a lot of options for how the configuration file can be set. First, a configuration file needs to be generated with `generate-borgmatic-config`.

Some important sections of the borgmatic configuration file are `source_directories`, `repositories`, `encryption_passphrase`, and `retention`. My `borgmatic-config.yaml` looks like this (without a lot of the comments):


``` yaml
# Where to look for files to backup, and where to store those backups. See
# https://borgbackup.readthedocs.io/en/stable/quickstart.html and
# https://borgbackup.readthedocs.io/en/stable/usage.html#borg-create for details.
location:
    # List of source directories to backup (required). Globs and tildes are expanded.
    source_directories:
        - ~/.gnupg
        - ~/.config/systemd/user
        - ~/Private/borg
        - ~/Private/other-important-files

    # Paths to local or remote repositories (required). Tildes are expanded. Multiple
    # repositories are backed up to in sequence. See ssh_command for SSH options like
    # identity file or port.
    repositories:
        - david@myserver:backup_dir

# Repository storage options. See
# https://borgbackup.readthedocs.io/en/stable/usage.html#borg-create and
# https://borgbackup.readthedocs.io/en/stable/usage/general.html#environment-variables for
# details.
storage:
    encryption_passphrase: "my-secure-passphrase"

# Retention policy for how many backups to keep in each category. See
# https://borgbackup.readthedocs.org/en/stable/usage.html#borg-prune for details.
# At least one of the "keep" options is required for pruning to work. See
# https://torsion.org/borgmatic/docs/how-to/deal-with-very-large-backups/
# if you'd like to skip pruning entirely.
retention:
    # Keep all archives within this time interval.
    keep_within: 3H
    # keep_secondly: 60
    # keep_minutely: 60
    keep_hourly: 24
    keep_daily: 7
    keep_weekly: 4
    keep_monthly: 6
    keep_yearly: 5

hooks:
    before_backup:
        - echo "Starting a backup."
    after_backup:
        - echo "Created a backup."
    on_error:
        - echo "Error while creating a backup or running a backup hook."
```

## Usage

After setting up the configuration file, using borgmatic to run borg is as easy as specifying the configuration file with the `-c` switch. For example: `borgmatic -c borgmatic-config.yaml list`.

### Helper script

Again, I try to make things a little bit easier for myself by creating a small helper script for borgmatic. I don't even need to remember to include the configuration flag. Here is my helper script where I can run borgmatic with `./borgmatic.bash list`.

```
#!/bin/bash

# Helper script to run borgmatic commands
borgmatic -v 1 -c borgmatic-config.yaml "$@"
```

## Time for a backup

At this point, since all of the source files and repositories have been specified in the `borgmatic-config.yaml`, a backup can be taken. `./borgmatic.bash` should be all that is needed to take a backup.

My directory containing my helper scripts and borgmatic configuration file now look like this:

```
$ tree
.
├── borg.bash
├── borgmatic.bash
└── borgmatic-config.yaml

0 directories, 3 files
```

If you are not interesting in an automatic backup then you can stop here. I recommend an automatic backup however, so you will have a backup ready if you need one. It is easy to forget to do regular backups manually.

# Systemd timer

I decided to automate the borgmatic backup with Systemd. A user systemd timer and service can be created in the `~/.config/systemd/user` directory to run the backup daily.

My `~/.config/systemd/user` directory looks like this after creating three necessary files:

```
/home/david/.config/systemd/user/
├── borgmatic.service
├── borgmatic.timer
└── timers.target.wants
    ├── borgmatic.timer -> /home/david/.config/systemd/user/borgmatic.timer
    └── vdirsyncer.timer -> /usr/lib/systemd/user/vdirsyncer.timer
```

`borgmatic.service` describes how to run the backup.

```
[Unit]
Description=borgmatic backup
Wants=network-online.target
After=network-online.target
ConditionACPower=true

[Service]
Type=oneshot

# Lower CPU and I/O priority.
Nice=19
CPUSchedulingPolicy=batch
IOSchedulingClass=best-effort
IOSchedulingPriority=7
IOWeight=100

Restart=no
LogRateLimitIntervalSec=0

# Delay start to prevent backups running during boot.
ExecStartPre=sleep 1m
# Test for private files. Should cause failure config file doesn't exist
ExecStartPre=/usr/bin/test -f /home/david/Private/borg/borgmatic-config.yaml

ExecStart=systemd-inhibit --who="david" --why="Prevent interrupting scheduled backup" /usr/bin/borgmatic --syslog-verbosity 1 -c /home/david/Private/borg/borgmatic-config.yaml

# Restart on failure. Keep trying to create backup.
RestartSec=10m
Restart=on-failure
```

`borgmatic.timer` is a scheduler for when to run the backup.

```
[Unit]
Description=Run borgmatic backup

[Timer]
OnCalendar=daily
Persistent=true

[Install]
WantedBy=timers.target
```

`timers.target.wants` is a directory created by systemd.

Of course you'll want to change instances of `david` to your username.

## Test service

First, it is important to see if the service is setup correctly. Scan for new user unit files with `systemctl --user daemon-reload`.

Then run the service file with `systemctl --user start borgmatic.service`.

`journalctl --user -n 200 -f -u borgmatic.service` should show the text output of the backup.

## Enable timer

Since the service file should now be working, enable the timer so the service is schedule regularly. Run `systemctl enable --now borgmatic.timer` to enable the timer.

# Sanity check

How to check if everything is working.

## List backups

The easiest thing to do is list backups and then list files backed up for a specific backup. Here is now:

```
./borgmatic.bash list
david@bison.parrish:backup_bar: Listing archives
myserver-2020-01-19T11:58:43.835270       Sun, 2020-01-19 11:58:44 [7...6]
myserver-2020-01-19T12:01:09.110944       Sun, 2020-01-19 12:01:10 [1...f]
myserver-2020-01-27T07:43:41.464375       Mon, 2020-01-27 07:43:42 [8...9]
myserver-2020-01-30T09:13:05.446876       Thu, 2020-01-30 09:13:06 [4...1]
myserver-2020-02-04T06:48:15.991816       Tue, 2020-02-04 06:48:16 [2...0]
myserver-2020-02-12T09:47:39.618958       Wed, 2020-02-12 09:47:40 [0...3]
myserver-2020-02-12T10:47:58.633934       Wed, 2020-02-12 10:47:59 [3...e]
myserver-2020-02-14T11:47:16.646835       Fri, 2020-02-14 11:47:17 [6...8]
myserver-2020-02-15T07:17:20.876995       Sat, 2020-02-15 07:17:21 [0...8]
myserver-2020-02-17T09:26:10.644048       Mon, 2020-02-17 09:26:11 [f...8]
myserver-2020-02-18T08:12:29.699794       Tue, 2020-02-18 08:12:30 [b...3]
myserver-2020-02-18T10:05:44.636999       Tue, 2020-02-18 10:05:45 [d...8]
myserver-2020-02-18T10:22:29.626931       Tue, 2020-02-18 10:22:30 [5...f]

summary:
/home/david/Private/borg/borgmatic-config.yaml: Successfully ran configuration file

$ ./borg.bash list ::myserver-2020-01-30T09:13:05.446876
-rw-r--r-- david  david      3115 Wed, 2020-01-29 10:54:59 home/david/Private/file1
drwxrwxrwx david  david         0 Wed, 2018-12-26 18:59:58 home/david/Private/file2
-rw-r--r-- david  david        67 Sat, 2017-04-29 09:47:41 home/david/Private/note.txt
```

If you see the backups running every day and your files are in the backup, then you should be all good!

## Show output

View output from systemctl and jounalctl for more information on the running backups.

`systemctl --user status borgmatic.service` to see the latest status of the backup.

`journalctl --user -n 100 -u borgmatic.service` to get line-by-line output from the last backup. Add a `-f` flag to "follow" the output in as the backup is taking place.

# Conclusion

Hopefully this post helps you follow along with a backup configuration which works for me. Please follow the references listed below for more information on setting up a backup for yourself.

## References

- [BorgBackup](https://www.borgbackup.org/)
- [borgmatic](https://torsion.org/borgmatic/)
- [systemd reference](https://www.freedesktop.org/software/systemd/man/systemd.service.html)
- [Bash scripting guide](http://mywiki.wooledge.org/BashGuide)
- [How to delay systemd service until a file exists](https://unix.stackexchange.com/a/388485/55139)

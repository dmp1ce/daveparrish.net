---
title: "Managing ZFS Snapshots: The Problem with Docker ZFS Datasets"
---

---

On several occasions, I wanted to automatically create ZFS snapshots but I was faced with ignoring many datasets (created by Docker) which I don't care about. Here is how to create automatic ZFS snapshots but ignore Docker snapshots.

- [ZFS](#zfs)
- [Docker on ZFS](#docker-on-zfs)
- [Auto snapshots with Docker datasets](#auto-snapshots-with-docker-datasets)
- [Migrating Docker ZFS datasets](#migrating-docker-zfs-datasets)
- [Syncoid Replication]()
- [Conclusion](#conclusion)


# ZFS

ZFS is a useful file system for me because of it's redundancy and near instant snapshots. See [ZFS' long feature list](https://en.wikipedia.org/wiki/ZFS#Features). ZFS is a great file system, but it isn't standard for many Linux distributions because of [licensing issues](https://openzfs.github.io/openzfs-docs/Project%20and%20Community/FAQ.html#licensing). 

ZFS is readily available on Ubuntu and Arch Linux as explained in their online documentation. The [Ubuntu ZFS documentation](https://wiki.ubuntu.com/ZFS) can be used to install ZFS on any up-to-date Ubuntu setup and [the Ubuntu install process can be used to install ZFS as the root file system](https://ubuntu.com/blog/zfs-focus-on-ubuntu-20-04-lts-whats-new). [Arch Linux ZFS documentation is very helpful in learning how to use ZFS](https://wiki.archlinux.org/index.php/ZFS) and [the ALEZ installer can be used to setup a root ZFS system](https://github.com/danboid/ALEZ).


# Docker on ZFS

Docker uses a compatible layered file system to manage it's images. The file system used can be modified in the Docker settings. By default, on root ZFS system Docker will use ZFS as the file system for images. Also, by default, the datasets are created in the root of the pool which docker was installed. This causes Docker to create many datasets which look something like this:

```
$ zfs list -d 1 zroot | head
NAME                                                                          USED  AVAIL     REFER  MOUNTPOINT
zroot                                                                        42.4G   132G       96K  none
zroot/0004106facc034e1d2d75d4372f4b7f28e1aba770e715b48d0ed1dd9221f70c9        212K   132G      532M  legacy
zroot/006a51b4a6b323b10e9885cc8ef9023a725307e61f334e5dd373076d80497a52       44.6M   132G      388M  legacy
zroot/00d07f72b0c5e3fed2f69eeebbe6d82cdc9c188c046244ab3163dbdac592ae2b       6.89M   132G     6.88M  legacy
zroot/011c18866423f0a535294033c020f4e548a8e07432262f80e8e488fa02fe250a       11.4M   132G      503M  legacy
zroot/013f7639f37b7a1263f3e65bbfd4890f1103c3c9e27a8f42e69083f6b1cba545        140K   132G     25.4M  legacy
zroot/01e79194da4df11f6fe8bcbd1a94885041d5d9269b0c744fe39c4d76101f2d56        262M   132G      309M  legacy
zroot/021fef9b5d63f0ff4b5621d1498a4e666ae38dd31660265e21f1a2bb0ac414f7        152K   132G      200M  legacy
zroot/022c3a6e9a40f3f62e3d13189970603b8b2ba6bb013817abd5db67c335e97e4c        116K   132G      162M  legacy
```

# Auto snapshots with Docker datasets

zfs-auto-snapshot is a useful package, available on both [Arch Linux](https://aur.archlinux.org/packages/zfs-auto-snapshot/) and [Ubuntu](http://manpages.ubuntu.com/manpages/focal/man8/zfs-auto-snapshot.8.html), for making snapshots automatically.  This is great for accidental deletes or if wanting to get an earlier version of a file. Snapshots will take up more space over time, because deleted data will need to be kept around, but automatic snapshots can be turned off with the `com.sun:auto-snapshot=false` property set.

The problem comes in when automatic snapshots are combined with the many Docker datasets created. By default, if Docker has been used and zfs-auto-snapshot is running then *several* snapshots will be create for each and every Docker dataset. With so many snapshots, ZFS runs a little slower when listing snapshots for example. Also, there is no reason for the snapshots to exist because Docker datasets never change!

So, how can the auto snapshots be turned of for Docker datasets? The easiest thing to do would be to set `com.sun:auto-snapshot=false` on the parent dataset in ZFS. This will work, but usually, the parent dataset is a dataset which is wanted to be automatically snapshot because it has other data besides the Docker datasets. What needs to be done is to move the Docker datasets into their own dataset parent, where auto snapshots can be safely turned off without effecting snapshots for other data.

## zfs-auto-snapshot aside

It is also helpful to know [zfs-auto-snapshot systemd service files can be overriden](https://unix.stackexchange.com/questions/398540/how-to-override-systemd-unit-file-settings) to [exclude snapshots by default](http://manpages.ubuntu.com/manpages/focal/man8/zfs-auto-snapshot.8.html) using `--default-exclude`. I couldn't figure out a way to use this to exclude the Docker datasets and include the parent dataset though. If `--default-exclude` is used, any dataset with the property `com.sun:auto-snapshot=true` will automatically be snapshot and none will be snapshot by default.

# Migrating Docker ZFS datasets

To move the Docker ZFS datasets, I followed the following steps:

- [Create a new parent dataset](#create-a-new-parent-dataset)
- [Set new dataset location for Docker](#set-new-dataset-location-for-docker)
- [Turn off Docker](#turn-off-docker)
- [Use a script to move over all Docker datasets](#use-a-script-to-move-over-all-docker-datasets)
- [Turn on Docker](#turn-on-docker)

## Create a new parent dataset

Create a new dataset where the Docker datasets can live. Something like command `sudo zfs create rpool/docker` should do it. Also, turn off automatic snapshots for that dataset with a command like `sudo zfs set com.sun:auto-snapshot=false rpool/docker`.

## Turn off Docker

Turn off Docker service before changing Docker settings. The command `sudo systemctl stop docker` will turn of Docker service and all running Docker services.

## Set new dataset location for Docker

While Docker is off, the `/etc/docker/daemon.json` can be create/edited to contain the setting `"storage-opts": ["zfs.fsname=zroot/docker"]`.


For example: `$ cat /etc/docker/dameon.json`
```
{
    "storage-opts": [
        "zfs.fsname=rpool/docker"
    ]
}

```

## Use a script to move over all Docker datasets

Unfortunately, I do not know of built in way to move many child datasets. I had to use a bash script to move all the child datasets which in my case was only Docker datasets. For your case you might need to make a different script to target only the Docker datasets. Using a command like `./move_zfs_child_datasets.bash zpool/ROOT/default zpool/docker` should move all the datasets.

Here is the script I used to move the datasets:
``` bash
#!/bin/bash

# Move all ZFS child datasets to a new parent dataset

if [ ! $# -eq 2 ]; then
    echo "Please supply a source and destination ZFS dataset as the first and second parameter"
    echo "Example: ./move_zfs_child_datasets.bash zpool/ROOT/default zpool/docker"
    exit;
fi

currentDatasetPath=$1
newDatasetPath=$2

currentDatasets=$(zfs list -H -t filesystem -d 1 -o name "$currentDatasetPath" | tail +2)

for dataset in $currentDatasets ; do
    newDataset="$newDatasetPath/${dataset##*/}"
    echo "Move $dataset to $newDataset"
    sudo zfs rename "$dataset" "$newDataset"
done
```

Or the Gist version:
<script src="https://gist.github.com/dmp1ce/5f51f798eee3806654f171e2b0272590.js"></script>

## Turn on Docker

After moving all the datasets, it should be safe to turn Docker back on. All Docker services should resume as normal. Start Docker with the command `sudo systemctl start docker`.

# Syncoid replication

Another reason why I didn't want all the Docker datasets and snapshots is because syncoid commands took a long time. Even when nothing changed, I found it took 10 minutes just to traverse all of the snapshots! With Docker datasets on their own parent dataset, it is straightforward to exclude Docker datasets during replication.

I use a command like `syncoid --exclude="zroot\/docker" --exclude="zroot\/docker\/[\w]+(-init)?" --exclude="zroot/swap" -r zroot myserver:rpool/backups/mylaptop/zroot` to replicate almost all of the ZFS data on another computer.

## Why Syncoid

I wanted to take advantage of ZFS for quick backups. I have some big datasets and I felt like Syncoid which takes advantage of `zfs send` made the most sense.

I would have liked to figure out how to encrypt the destination of the replication, but other than that, the replication process works very well, after the many snapshots from Docker datasets are excluded.

## Installing Syncoid

On both Ubuntu and Arch Linux I needed some extra packages for Syncoid to work optimally. [mbuffer](https://aur.archlinux.org/packages/mbuffer/) and [lzop](https://www.archlinux.org/packages/extra/x86_64/lzop/) also needed to be installed along with [sanoid](https://aur.archlinux.org/packages/sanoid/) from the AUR for Arch Linux. For Ubuntu [sanoid](https://packages.ubuntu.com/focal/sanoid), [lzop](https://www.archlinux.org/packages/extra/x86_64/lzop/) and [mbuffer](https://packages.ubuntu.com/focal/mbuffer) should also be installed for Syncoid.

I don't think sanoid needs to be installed if you are not running the syncoid command on that system, although mbuffer and lzop might still be required on a remote ZFS server.

# Conclusion

After going through all this effort to avoid syncing unnecessary datasets and snapshots, I'm still not sure it is worth it. Maybe my time would have been better spent setting up a Borg Backup configuration and not worry about these ZFS issues. On the other hand, my replications of datasets is now very fast, easy to maintain and kept at a minimum size.

Hopefully this will be useful to more than just myself. I know I learned more about how ZFS handles datasets and snapshots. Please let me know if you know a better way to backup/replicate ZFS datasets while ignoring unnecessary datasets like Docker creates.

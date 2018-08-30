---
title: Vagrant for Snap development
date: 2018-08-30 11:39:03
---

The only way I have found to develop Snap packages on Arch Linux is to use a Ubuntu 16.04 virtual machine to do the development. Getting the vagrant machine is straight forward. You'll first need to add the image to the `Vagrantfile` like so:

``` ruby
# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://vagrantcloud.com/search.
  config.vm.box = "ubuntu/xenial64"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  config.vm.provider "virtualbox" do |vb|
    # Customize the amount of memory on the VM:
    vb.memory = "4096"
  end
end
```

I've also upped the memory to 4 GB because I was getting some out of memory errors on a Snap build.

Now you should be able to `vagrant up` and `vagrant ssh` to enter the vagrant VM and work on the snap.

The only other caveat is that you'll not be able to build the Snap in `/vagrant` which is where the VM shares files with the host. You'll need to copy your `snapcraft.yml` file into another directory. I create a `~/projects/mySnapProject` directory to hold my Snap projects. I would then copy the `snapcraft.yml` and other project files into the `~/projects/mySnapProject` directory.

Knowing all of this you should be able to follow the [snapcraft setup to start building snaps](https://docs.snapcraft.io/build-snaps/get-started-snapcraft). Although, the `snapcraft cleanbuild` did not work for me with the build I was created. I resorted to building with only the `snapcraft` command.

Once the `.snap` file is created it can be copied into `/vagrant` so that it can be installed on the host machine, tested and published.

Here are a couple of issues I had getting started which I posted on the Snapcraft Forum.

- [Unable to test a container build on Arch Linux](https://forum.snapcraft.io/t/unable-to-test-a-container-build-on-arch-linux/6404?u=dmp1ce)
- [Cleanbuild fails differently locally than on snapcraft.io](https://forum.snapcraft.io/t/cleanbuild-fails-differently-locally-than-on-snapcraft-io/7048?u=dmp1ce)

---
title: Gotchas When Creating a Snap Package for Haskell (Stack) Program
---

# Introduction

Building snap packages and publishing them on the [Snapcraft Store](https://snapcraft.io/store) is [well documented](https://snapcraft.io/docs/snapcraft-overview) by Canonical. For the most part, creating a snap package for a [Haskell](https://www.haskell.org/) program (built with [Stack](https://docs.haskellstack.org/en/stable/README/)) is the same as building any other Snap package. However, I did need to know a few things to successfully build my Snap package.

# GHC Memory Requirements

[GHC](https://www.haskell.org/ghc/) is a memory hog. Many libraries can take over 4 GB of memory to compile. By default, snapcraft will only allocate 2 GB of memory when building an application with [multipass](https://multipass.run/). Multipass gets used by snapcraft when [core18](https://snapcraft.io/docs/snapcraft-top-level-metadata) is specified in the `snapcraft.yaml` as a base.

To change the memory allocated by multipass, use an [snapcraft environment variable](https://forum.snapcraft.io/t/environment-variables-that-snapcraft-consumes/9416), when running snapcraft. The snapcraft command will look like, `SNAPCRAFT_BUILD_ENVIRONMENT_MEMORY=4G snapcraft`, to allocate 4 GB. If snapcraft already ran with lower memory then you'll need to rebuild the multipass VM by running `snapcraft clean` first.

# Using Stack tool in Snapcraft

Many Haskell applications build using the stack tool. There is not a snapcraft plugin available for the stack tool so a build override is needed. The following snippet will download and install stack. Next the snippet will build the program with stack.

``` yaml
override-build: |
  if [ ! -x "$(command -v stack)" ]; then
    curl -sSL https://get.haskellstack.org/ | sh
  fi
  stack build --copy-bins
```

Use `plugin: nil` for the part building the Haskell program.

Also, make sure `curl` and any other build dependencies you need are in the `build-packages` section.

# Automatic Versioning

Snapcraft can use the same version specified in the `package.yaml` like so:

``` yaml
override-pull: |
  snapcraftctl pull
  snapcraftctl set-version "$(grep version package.yaml | awk '{ print $NF }')"
```

Use `adopt-info` in the `snapcraft.yaml` to [select the part to use the version](https://snapcraft.io/docs/using-external-metadata) info from.

# Example

For a working example, see my [s17-turbo-switch snap configuration](https://github.com/dmp1ce/s17-turbo-switch/blob/master/snap/snapcraft.yaml).

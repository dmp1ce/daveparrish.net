---
title: Connecting to L2TP/IPSec VPN with Ubuntu
---

---

*TL;DR*

These steps [probably](https://github.com/nm-l2tp/network-manager-l2tp/blob/master/README.md#example-workaround-for-3des-sha1-and-modp1024-broken-algorithms) work for most off the shelf VPNs using L2TP/IPSec.

1. Install the package network-manager-l2tp-gnome with `sudo apt install network-manager-l2tp-gnome`.
2. Enable insecure algorithms if needed. Use ike-scan to find algorithms used by VPN. Set Phase1 and Phase2 Algorithms. In my case I used `3des-sha1-modp1024` and `3des-sha1` for Phase1 and Phase2.
3. Check *only* the authentication method you need. In my case I *only* checked "MSCHAPv2".
4. Enter credentials.
5. Still not working? Check error logs with `sudo journalctl -f` and search error messages. Good Luck!

**EDIT:** Another route to take is to use [a Docker image](https://github.com/ubergarm/l2tp-ipsec-vpn-client) to take care of connecting to the VPN.

---

# Introduction

I'm writing this article out of frustration because my Chromebook connected to the L2TP/IPSec VPN out of the box without any installation or any changing of settings besides user credentials. I wanted to connect with Ubuntu because it is easier to work from Ubuntu than Chrome OS. Eventually I was able to figure out how to configuration Ubuntu 19.04 to connect to the L2TP/IPSec after an hour or two of reading errors logs and searching.

# Installation

By default Ubuntu not installed with the VPN software necessary to connect to L2TP/IPSec VPN. The software package you probably will want to install is [network-manager-l2tp](https://github.com/nm-l2tp/network-manager-l2tp) which will install [strongSwan](https://www.strongswan.org/) and the GUI software (network-manager-l2tp-gnome) to manage the configuration. Run `sudo apt install network-manager-l2tp-gnome` on the command line to install both network-manager-l2tp and the Gnome GUI to manage the configuration.

# Using Insecure Algorithms

Here is where I was stuck for awhile because by default network-manager-l2tp will not use the most common algorithms used by many VPN consumer devices. The [reason network-manager-l2tp doesn't use the algorithms is because they are considered insecure](https://github.com/nm-l2tp/network-manager-l2tp/blob/master/README.md#example-workaround-for-3des-sha1-and-modp1024-broken-algorithms). To force network-manager-l2tp to use insecure algorithms the "Phase1 Algorithms" and "Phase2 Algorithms" need to be set. In my case I put `3des-sha1-modp1024` for "Phase1 Algorithms" and `3des-sha1` for "Phase2 Algorithms". These settings are found in "IPSec Settings..." when creating a VPN under the Network settings in Gnome.

In order to figure out what algorithms your VPN is expecting use the ike-scan tool which I installed using `sudo apt install ike-scan`. Then use `sudo ike-scan vpn_host_ip`. [More information on ike-scan and insecure algorithms commonly used can be found on the linked Ask Ubuntu answer.](https://askubuntu.com/a/920497/25776)

# Pick Authentication Method

It would seem that network-manager-l2tp could not determine which authentication method to use in my case. I needed to uncheck all of the authentication methods except for "MSCHAPv2" in order for the client to connect. I don't understand why and tried this fix thanks to [an answer on Stack Overflow](https://stackoverflow.com/a/39687810/350221).

# Enter Credentials

This is the only step I expected. It is straight forward. Enter the host IP, the username, the password and pre-shared key in the VPN settings. "Pre-shared key" text field is in the "IPSec Settings...". You'll also have to set if you want the password to be stored on Ubuntu 19.04 on the right side of the "Password" text field.

# Still not Connecting?

I am sorry if your VPN still doesn't connect. I have only documented what worked for me and tried to leave a trail of the resources which helped me. I don't understand all the setting for L2TP/IPSec and I don't understand why things just don't work by default.

Please take a look at the logs by running `sudo journalctl -f` and search for the error message which jump out to you. By trying to connect from the Gnome GUI should produce the errors, if there are any. Good luck!

## EDIT: Docker

If all these steps seem too frustrating and you are comfortable with the command line, give a docker image a try. I tried [ubergarm/l2tp-ipsec-vpn-client](https://github.com/ubergarm/l2tp-ipsec-vpn-client) which was enough to get connected to the VPN by starting the container with `--privileged` access on `--host` network. Here is the Docker Compose configuration I used:

``` yaml
# docker-compose.yaml

version: "3"

services:
  vpn:
    image: ubergarm/l2tp-ipsec-vpn-client
    privileged: true
    network_mode: host
    environment:
      VPN_SERVER_IPV4: 'xxx.xxx.xxx.xxx'
      VPN_PSK: 'shareKey'
      VPN_USERNAME: 'UserName'
      VPN_PASSWORD: 'UserPassword'
    volumes:
      - "/lib/modules:/lib/modules:ro"
```

After starting with `docker-compsoe up -d` You'll then need to route your network traffic through the VPN with something like: `sudo ip route add 1.2.3.4 via 10.10.10.1 dev ppp0`. Please see the [documentation on the GitHub project page](https://github.com/ubergarm/l2tp-ipsec-vpn-client#route).

I also found this method useful because a service can be added to the Docker Compose configuration without ever enabling the VPN on the host network. This is done by using the `vpn` service network like so:

``` yaml
  myservice:
    image: apline
    network_mode: service:vpn
    command: sh -c "while true; do date; sleep 60; done"
```

You just need to expose the ports to the service on the `vpn` service because `vpn` and `myservice` effectively share the same IP.

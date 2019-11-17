---
title: How to Create a PKGBUILD for a AppImage binary
---

---

On Arch Linux PKGBUILD scripts can be used to help manage installing and updating AppImage software releases.

## AppImage PKGBUILD Template

Using the following template it should be easy to create your own PKGBUILD script from an AppImage posted on public site like Github.

To use the PKGBUILD run `makepkg` to build a `my-project-appimage-0.0.1-1-x86_64.pkg.tar.xz`. Then the package can be installed with `pacman -U my-project-appimage-0.0.1-1-x86_64.pkg.tar.xz`. The AppImage can then be started by running `my-project-appimage` on the command line to start the appimage.

When there is a new release for the AppImage, the version, AppImage source location and sha256 hash can be updated, rebuilt and reinstalled.

---

**NOTE:** See [new post](/posts/2019-11-16-Better-AppImage-PKGBUILD-template.html) on improving upon this template for better GUI integration.


``` bash
# Maintainer: My Name <myemail@domain.me>

pkgname=my-project-appimage
pkgver=0.0.1
pkgrel=1
pkgdesc="Description of my project"
arch=('x86_64')
url="https://github.com/user/repo/"
license=('custom:Unlicense')
depends=('zlib')
options=(!strip)
source_x86_64=("${pkgname}-${pkgver}.AppImage::https://github.com/MyGithubUSERNAME/MyProjectRepo/releases/download/${pkgver}/myproject.AppImage"
               "https://raw.githubusercontent.com/MyGithubUSERNAME/MyProjectRepo/${pkgver}/LICENSE"
              )
noextract=("${pkgname}-${pkgver}.AppImage")
sha256sums_x86_64=('000000000000000000000000000000000000000000000000000000000000000'
                   '000000000000000000000000000000000000000000000000000000000000000')

package() {
    install -Dm755 "${srcdir}/${pkgname}-${pkgver}.AppImage" "${pkgdir}/opt/${pkgname}/${pkgname}.AppImage"
    install -Dm644 "${srcdir}/LICENSE" "${pkgdir}/opt/${pkgname}/LICENSE"

    # Symlink executable
    mkdir -p "${pkgdir}/usr/bin"
    ln -s "/opt/${pkgname}/${pkgname}.AppImage" "${pkgdir}/usr/bin/${pkgname}"

    # Symlink license
    mkdir -p "${pkgdir}/usr/share/licenses/$pkgname"
    ln -s "/opt/$pkgname/LICENSE" "$pkgdir/usr/share/licenses/$pkgname"
}
```

## Why?

AppImages are already easy to install and run as noted in [this StackOverflow answer](https://superuser.com/a/1307178/109464). You could just copy the appimage to `/usr/local/bin` and make it executable with a `chmod +x /usr/local/bin/my-project.AppImage` so you could run it from the command line. When updating, just replace the AppImage file with the new version. Many AppImage applications notify the user when an update is available.

This does require each AppImage is updated independently and the user will only know when the application notifies them of an update.

## A framework for managing many updates

Using an [AUR helper](https://wiki.archlinux.org/index.php/AUR_helpers) all the AppImages can be updated automatically, just like other packages on Arch Linux. For this reason, I will usually go ahead and upload my PKGBUILD to the [AUR](https://aur.archlinux.org) if one doesn't already exist. Others can then take advantage of the script I have already created. See my [guide to maintaining PKGBUILDs on the AUR](/posts/2019-01-30-Opinionated-PKGBUILD-development-guide.html).

## Desktop Launcher

My template doesn't install a desktop launcher because when running the AppImage for the first time, it will prompt the user for installing the desktop launcher. On the first run, the user will need to launch the application from the command line. From then on, the AppImage application will be available where the other desktop applications are, if the user selected to install the desktop launcher.

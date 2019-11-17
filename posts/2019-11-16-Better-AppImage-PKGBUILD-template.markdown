---
title: A better PKGBUILD template for AppImage packages
---

---

Presented here is a template for creating an Arch Linux desktop package from an AppImage. This improves on my last post [How to Create a PKGBUILD for a AppImage binary](/posts/2019-11-07-HowTo-PKGBUILD-for-AppImage.html) which had the problem of not installing a `.desktop` file so the application can be started easily from GUI.

## The Template

``` bash
# Maintainer: My name <myemail@domain.me>

_pkgname=my-project

pkgname="${_pkgname}"-appimage
pkgver=0.0.1
pkgrel=1
pkgdesc="Description of my project"
arch=('x86_64')
url="https://github.com/user/repo/"
license=('custom:Unlicense')
depends=('zlib' 'hicolor-icon-theme')
options=(!strip)
_appimage="${pkgname}-${pkgver}.AppImage"
source_x86_64=("${_appimage}::https://github.com/user/repo/releases/download/${pkgver}/${_pkgname}.${pkgver}.AppImage"
               "https://raw.githubusercontent.com/user/repo/${pkgver}/LICENSE"
              )
noextract=("${_appimage}")
sha256sums_x86_64=('0000000000000000000000000000000000000000000000000000000000000000'
                   '0000000000000000000000000000000000000000000000000000000000000000')

prepare() {
    chmod +x "${_appimage}"
    ./"${_appimage}" --appimage-extract
}

build() {
    # Adjust .desktop so it will work outside of AppImage container
    sed -i -E "s|Exec=AppRun|Exec=env DESKTOPINTEGRATION=false /usr/bin/${_pkgname}|"\
        "squashfs-root/${_pkgname}.desktop"
    # Fix permissions; .AppImage permissions are 700 for all directories
    chmod -R a-x+rX squashfs-root/usr
}

package() {
    # AppImage
    install -Dm755 "${srcdir}/${_appimage}" "${pkgdir}/opt/${pkgname}/${pkgname}.AppImage"
    install -Dm644 "${srcdir}/LICENSE" "${pkgdir}/opt/${pkgname}/LICENSE"

    # Desktop file
    install -Dm644 "${srcdir}/squashfs-root/${_pkgname}.desktop"\
            "${pkgdir}/usr/share/applications/${_pkgname}.desktop"

    # Icon images
    install -dm755 "${pkgdir}/usr/share/"
    cp -a "${srcdir}/squashfs-root/usr/share/icons" "${pkgdir}/usr/share/"

    # Symlink executable
    install -dm755 "${pkgdir}/usr/bin"
    ln -s "/opt/${pkgname}/${pkgname}.AppImage" "${pkgdir}/usr/bin/${_pkgname}"

    # Symlink license
    install -dm755 "${pkgdir}/usr/share/licenses/${pkgname}/"
    ln -s "/opt/$pkgname/LICENSE" "$pkgdir/usr/share/licenses/$pkgname"
}
```

## Explanation

After looking at a [few other AppImage PKGBUILDs](https://aur.archlinux.org/packages/?O=0&K=appimage&SB=v&SO=d) I realized some authors already had a solution for `.desktop` files. Their solution is:

1. Extract AppImage
2. Modify the `.desktop` so it works outside of the AppImage binary
3. Package the `.desktop` file and icon images

These steps work well, but I was still faced with a nagging popup which prompted to add a `.desktop` file again! Fortunately, there seems to be a way to suppress the popup with the environment variable `DESKTOPINTEGRATION`, as I found by poking around in an AppImage I was trying to package. Keep in mind, not all AppImages have this popup problem, so the `DESKTOPINTEGRATION` variable might be unnecessary.

## Credit

Thank you to [Igor Moura](https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=freecad-appimage) and [Frederik “Freso” S. Olesen](https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=wootility-appimage) for their AppImage examples.

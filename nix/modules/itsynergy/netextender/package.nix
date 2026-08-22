{ lib
, stdenv
, fetchurl
, dpkg
, autoPatchelfHook
, makeWrapper
, wrapGAppsHook3
, gtk3
, webkitgtk_4_1
, iproute2
, iptables
, nftables
, procps
, util-linux
, nettools
, which
, bash

, coreutils
, gnugrep
, gnused
, gawk
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "netextender";
  version = "10.3.5-36";

  src = fetchurl {
    url = "https://software.sonicwall.com/NetExtender/NetExtender-linux-amd64-${finalAttrs.version}.deb";
    hash = "sha256-xoUcrBiIAX5NIyGlaSq2xcL1TTcUzUo42xSMX0u4Hi0=";
  };

  nativeBuildInputs = [ dpkg autoPatchelfHook makeWrapper wrapGAppsHook3 ];

  buildInputs = [
    gtk3
    webkitgtk_4_1
    stdenv.cc.cc.lib
  ];

  # The wrapping is done by hand in postFixup so the CLI and daemon binaries get
  # the same PATH treatment as the GUI.
  dontWrapGApps = true;

  unpackCmd = "dpkg-deb -x $curSrc source";
  sourceRoot = "source/usr/local/netextender";

  installPhase = ''
    runHook preInstall

    mkdir -p $out/opt/netextender
    cp -r . $out/opt/netextender/

    # Only the webkit2gtk-4.1 build of the GUI is usable: webkitgtk_4_0 no longer
    # exists in nixpkgs, so the 4.0 binary could never be patchelfed.
    rm $out/opt/netextender/NetExtender_webkit2_40
    mv $out/opt/netextender/NetExtender_webkit2_41 $out/opt/netextender/NetExtender

    mkdir -p $out/bin
    ln -s $out/opt/netextender/NetExtender $out/bin/NetExtender
    ln -s $out/opt/netextender/nxcli $out/bin/nxcli
    ln -s $out/opt/netextender/nxcli $out/bin/netExtender
    ln -s $out/opt/netextender/NEService $out/bin/NEService
    ln -s $out/opt/netextender/wg $out/bin/wg
    ln -s $out/opt/netextender/wg-quick $out/bin/wg-quick

    # share/pixmaps rather than share/icons/hicolor: the icon is 144x144, and
    # hicolor's index.theme lists no such size, so a themed install would never
    # be found by name. pixmaps is the spec's unthemed fallback, matched by name
    # at whatever size the file happens to be.
    install -Dm444 $out/opt/netextender/nx-icon.png \
      $out/share/pixmaps/com.sonicwall.NetExtender.png

    mkdir -p $out/share/applications
    substitute $out/opt/netextender/com.sonicwall.NetExtender.desktop \
      $out/share/applications/com.sonicwall.NetExtender.desktop \
      --replace-fail /usr/local/netextender/NetExtender $out/bin/NetExtender \
      --replace-fail /usr/local/netextender/nx-icon.png com.sonicwall.NetExtender

    runHook postInstall
  '';

  # These binaries shell out to network tooling by name, and wg-quick needs a
  # full POSIX toolbox. The daemon additionally execs its own wg/wg-quick via
  # the hardcoded /usr/local/netextender path — see the NixOS module.
  #
  # Deliberately absent: any provider of resolvconf(8). Which one is correct
  # depends on how the host manages DNS (systemd-resolved's shim vs openresolv),
  # and this wrapper prefixes PATH, so anything listed here would shadow the
  # host's choice. The NixOS module picks the right one for the daemon.
  postFixup =
    let
      runtimePath = lib.makeBinPath [
        iproute2
        iptables
        nftables
        procps
        # NEService configures DNS by bind-mounting resolv.conf inside a private
        # mount namespace, i.e. it execs unshare(1) and mount(8).
        util-linux
        # route/hostname, referenced alongside the iptables calls.
        nettools
        which
        bash
        coreutils
        gnugrep
        gnused
        gawk
      ];
    in
    ''
      for prog in NEService nxcli NetExtender; do
        wrapProgram $out/opt/netextender/$prog \
          --prefix PATH : "${runtimePath}:$out/opt/netextender" \
          "''${gappsWrapperArgs[@]}"
      done
    '';

  meta = {
    description = "SonicWall NetExtender SSL VPN client";
    homepage = "https://www.sonicwall.com";
    license = lib.licenses.unfree;
    platforms = [ "x86_64-linux" ];
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    mainProgram = "NetExtender";
  };
})

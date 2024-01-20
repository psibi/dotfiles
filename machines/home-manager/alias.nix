{ pkgs }: {
  pi = "ping 8.8.8.8";
  clipboard = "wl-copy";
  ls = "lsd";
  o = "xdg-open";
  bc = "${pkgs.kalker}/bin/kalker";
  cat = "bat";
}

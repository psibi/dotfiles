{ pkgs }: {
  pi = "ping 8.8.8.8";
  clipboard = "xclip -sel clip";
  ls = "lsd";
  o = "xdg-open";
  bc = "${pkgs.kalker}/bin/kalker";
  cat = "bat";
}

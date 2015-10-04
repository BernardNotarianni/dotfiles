# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModprobeConfig = ''
    options snd_hda_intel enable=0,1
  '';

  fileSystems."/tmp" = {
    device = "tmpfs";
    fsType = "tmpfs";
    options = "size=2048M";
  };

  # fileSystems."/windows" = {
  #  device = "/dev/disk/by-label/Windows8_OS";
  #  fsType = "ntfs-3g";
  #  options = "exec,permissions";
  # };

  networking = {
    hostName = "T450s"; # Define your hostname.
    wireless.enable = true;
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "fr";
    # defaultLocale = "fr_FR.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
     wget nox zsh git inotifyTools
     vim emacs
     rxvt_unicode
     i3 i3status dmenu
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;


  services.mysql = {
    enable = true;
    package = pkgs.mysql;
  };


  # Enable the X11 windowing system.
  services.xserver.videoDrivers = [ "intel-2015-07-22" ];
  services.xserver.enable = true;
  services.xserver.layout = "fr";
  services.xserver.xkbOptions = "eurosign:e";

  services.xserver.displayManager.slim = {
    enable = true;
    autoLogin = true;
    defaultUser = "bernard";
  };

  services.xserver.windowManager = {
     i3.enable = true;
     default = "i3";
  };

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  services.xserver.desktopManager.xterm.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      inconsolata
      ubuntu_font_family
      source-code-pro
    ];
  };

  users.extraUsers.bernard = 
   { isNormalUser = true;
     home = "/home/bernard";
     description = "Bernard Notarianni";
     group = "users";
     password = "bernard";
     createHome = true;
   };

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # serveur nginx
  services.nginx.enable = true;
  services.nginx.config = pkgs.lib.readFile /home/bernard/baton-de-parole/server/nginx/nginx.conf.bernard;
  networking.firewall.allowedTCPPorts = [ 8080 8090 ];

}

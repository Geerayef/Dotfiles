{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Europe/Vienna";

  networking.hostName = "NixiusThrive";
  # networking.networkmanager.enable = true;

  nix = {
    extraOptions = ''experimental-features = nix-command flakes'';
    settings.auto-optimise-store = true;
    gc.options = "--delete-older-than 30d";
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-128n.psf.gz";
    packages = with pkgs; [ terminus_font ];
    keyMap = "uk";
  #   useXkbConfig = true; # use xkbOptions in tty.
  };

  services.xserver = {
    enable = true;
    layout = "gb";
    xkbOptions = "ctrl:nocaps";
    displayManager = {
      defaultSession = "none+bspwm";
      lightdm = {
	enable = true;
	greeters.slick.enable = true;
      };
    };
    windowManager.bspwm = {
      enable = true;
      configFile = "/home/tibor/.config/bspwm/bspwmrc";
      sxhkd.configFile = "/home/tibor/.config/sxhkd/sxhkdrc";
    };
  };

  # Sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.defaultUserShell = pkgs.bash;
  programs = {
      zsh = {
          enable = true;
      };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tibor = {
    shell = pkgs.zsh;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  fonts.fonts = with pkgs; [
    ( nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" ]; })
  ];

  environment.systemPackages = with pkgs; [
    brave
    neovim
    alacritty
    starship
    zsh

    # Desktop
    bspwm
    polybar
    feh
    rofi
    sxhkd

    # Tools
    fzf
    tmux
    ripgrep
    killall
    zip
    unzip
    ffmpeg
    wget
    curl
    
    # Dev
    gh
    git
    # make
    gnumake
    cmake
    gcc
    lld

    # Lang
    nodejs_20
  ];

  # ~  Services
  services.picom.enable = true;

  services.openssh.enable = true;

  networking.firewall.enable = false;

  system.copySystemConfiguration = true;

  system.stateVersion = "23.05"; # Did you read the comment?

}


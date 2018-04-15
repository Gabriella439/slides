{ machine = { pkgs, ... }: {
    networking.firewall.allowedTCPPorts = [ 8080 ];

    nixpkgs.config = import ./config.nix;

    systemd.services.simple-ci = {
      wantedBy = [ "multi-user.target" ];

      script = ''
        ${pkgs.haskellPackages.simple-ci}/bin/simple-ci
      '';
    };
  };
}

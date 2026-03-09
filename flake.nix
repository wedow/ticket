{
  description = "Git-backed issue tracker for AI agents";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        runtimeDeps = with pkgs; [
          coreutils
          findutils
          gawk
          gnused
          gnugrep
          ripgrep
          jq
        ];
      in
      {
        packages.default = pkgs.stdenvNoCC.mkDerivation {
          pname = "ticket";
          version = self.shortRev or self.dirtyShortRev or "dev";
          src = ./.;

          nativeBuildInputs = [ pkgs.makeWrapper ];

          dontBuild = true;

          installPhase = ''
            install -Dm755 ticket $out/libexec/tk
            install -Dm644 LICENSE $out/share/licenses/ticket/LICENSE

            makeWrapper $out/libexec/tk $out/bin/tk \
              --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}

            # Install plugins
            for plugin in plugins/ticket-*; do
              [ -L "$plugin" ] && continue
              install -Dm755 "$plugin" "$out/bin/$(basename "$plugin")"
            done
            install -Dm755 plugins/ticket-ls "$out/bin/ticket-list"
          '';
        };
      }
    ) // {
      overlays.default = final: prev: {
        ticket = self.packages.${final.system}.default;
      };
    };
}

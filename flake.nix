{
  inputs = {
    omnixiah.url = "tarball+https://git.tatikoma.dev/omnixiah/omnixiah/archive/master.tar.gz";
  };

  outputs =
    { omnixiah, ... }:
    let
      inherit (omnixiah.inputs) flake-utils;
      inherit (flake-utils.lib) eachSystem allSystems;
    in
    eachSystem allSystems (
      arch:
      let
        pkgs = omnixiah.pkgs.${arch};
      in
      {
        formatter = pkgs.nixfmt;

        devShells.default = pkgs.mkShell {
          name = "git-appraise";
          packages = with pkgs; [
            coreutils
            findutils
            git
            git-appraise
            just
            jq
            ((emacsPackagesFor emacs).emacsWithPackages (
              epkgs: with epkgs; [
                buttercup
                undercover
                closql
                magit
                transient
                a
                s
                ghub
                uuidgen
                deferred
                markdown-mode
                forge
                emojify
              ]
            ))
          ];
          shellHook = ''

          '';
        };
      }
    );
}

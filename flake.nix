{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    rUtils.url = "git+https://scm.openanalytics.eu/git/oa-r-utils-nix.git";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
      rUtils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import ./overlay.nix { inherit rUtils; })
          ];
        };
        
        editbl = rUtils.lib.buildRPackage {
          inherit pkgs;
          src = self + "/editbl";
        };
        extrasNotInDescription = [
          pkgs.rPackages.devtools
          pkgs.rPackages.testthat
          pkgs.rPackages.editbl
          pkgs.rPackages.qpdf
        ];
      in
      {
        devShells = {
          default = rUtils.lib.mkRShell {
            inherit pkgs;
            shell_name = "default";
            
            # Using your DESCRIPTION file packages will automatically be installed e.g.:
            descriptionPath = editbl/DESCRIPTION;

            # If you want to exclude the Suggests packages:
            excludeSuggests = false;

            packages = extrasNotInDescription;
            
            # Here you can add commands that will run everytime you enter the
            # development shell
            shellHook = ''
            '';
          };
          test = rUtils.lib.mkRShell {
            inherit pkgs;
            shell_name = "test";
            packages = extrasNotInDescription ++ [
              editbl
            ];
            shellHook = ''
            '';
          };
          
        };
        packages.default = editbl;
      }
    );
}
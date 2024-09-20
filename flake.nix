{
  description = "A Nix-flake-based OCaml development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:

    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [ ocaml ocamlformat emacs.pkgs.tuareg ] ++
          (with pkgs.ocamlPackages; [ dune_3 odoc opam ocaml-lsp menhir utop ocp-indent merlin ]);

        shellHook = ''
          ${pkgs.ocaml}/bin/ocaml --version
        '';
      };
    });
}

# vim:fileencoding=utf-8:foldmethod=marker
#: Tip: If you are using (n)vim, you can press zM to fold all the config blocks quickly (za to fold under cursor)
#: Tip: search keywords to start quickly
{
  #: Inputs {{{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    blackbox.url = "github:cubewhy/blackbox-flakes";

    #: Do not forget to modify the `overlays` variable below after you added new overlays

    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  #: inputs end }}}

  outputs = {
    self,
    blackbox,
    rust-overlay,
    nixpkgs,
    ...
  }: let
    #: Overlays {{{
    overlays = [
      (import rust-overlay)
    ];
    #: overlays end }}}
  in {
    devShells =
      blackbox.lib.eachSystem {
        inherit nixpkgs overlays;
      } (pkgs: {
        default = blackbox.lib.mkShell {
          inherit pkgs;

          #: Config {{{
          config = {
            #: Languages {{{

            #: Rust {{{
            blackbox.languages.rust = {
              enable = true;
              toolchainFile = ./rust-toolchain.toml;
            };
            #: rust end }}}

            blackbox.languages.javascript = {
              enable = true;
              manager = "pnpm";
            };

            #: languages end }}}

            #: Libraries {{{
            blackbox.libraries = {
              #: OpenSSL {{{
              openssl.enable = false;
              #: openssl end }}}

              #: shared libraries {{{
              #: blackbox-flake will config LD_LIBRARY_PATH and LIBRARY_PATH for the following packages
              shared = with pkgs; [
                # alsa-lib
                # pipewire
                # libpulseaudio
              ];
              #: }}}
            };

            #: libraries end }}}

            #: Tools {{{
            blackbox.tools = {
              #: Pre-commit {{{
              pre-commit = {
                enable = false;
                #: Force run `pre-commit install` when enter shell
                #: This is not recommended, please don't enable it.
                runOnStart = false;
              };
              #: pre-commit end }}}
            };
            #: tools end }}}
          };
          #: config end }}}

          #: Custom options {{{

          #: mkShell builtin options are available
          # shellHook = ''
          # '';

          #: custom options end }}}
        };
      });
  };
}

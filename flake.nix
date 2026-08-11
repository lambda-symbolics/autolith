{
  description = "Autolith - a live, self-modifying Common Lisp agent";

  # Autolith pins an exact SBCL (see sbcl.version) and its Quicklisp package
  # set is generated against a matching nixpkgs. Pin that nixpkgs here so the
  # build is reproducible; bump it in lockstep whenever sbcl.version changes.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/d482ef84049d9b7276b83a06e4e4d76983830097";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      # Nix builds run on Linux x86-64 (the packaged release target) and on
      # macOS arm64. nix/package.nix asserts the same platform set.
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { pkgs, ... }:
        let
          autolith = import ./nix/package.nix {
            inherit pkgs;
            src = inputs.self;
          };
          upgradeSource = pkgs.runCommand "autolith-upgrade-regression-source" {} ''
            cp -R ${inputs.self}/. "$out"
            chmod -R u+w "$out"
            printf '%s\n' ';; Nix package upgrade regression source.' >> \
              "$out/src/core/time.lisp"
          '';
          upgradeAutolith = import ./nix/package.nix {
            inherit pkgs;
            src = upgradeSource;
          };
          imageIdentity = builtins.baseNameOf (toString autolith.imageIdentity);
          upgradeImageIdentity =
            builtins.baseNameOf (toString upgradeAutolith.imageIdentity);
        in
        {
          packages = {
            default = autolith;
            autolith = autolith;
          };

          devShells.default = pkgs.mkShell {
            packages = [
              autolith.runtime
              pkgs.coreutils
              pkgs.git
              pkgs.gnugrep
              pkgs.perl
            ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.bubblewrap ];

            AUTOLITH_NIX_DEVELOPMENT = "1";
            AUTOLITH_SBCL = "${autolith.runtime}/bin/sbcl";
            AUTOLITH_SBCL_SOURCE_ROOT = "${autolith.sbclSource}";
            AUTOLITH_PROJECT_SETUP =
              "${inputs.self}/script/nix-project-setup.lisp";
            COLORLISP_NATIVE_LIBRARY =
              "${autolith.colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
            AUTOLITH_FFF_LIBRARY =
              "${autolith.fffLibrary}/lib/libfff_c${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}";
            CL_EXEC_SANDBOX_BWRAP = pkgs.lib.optionalString
              pkgs.stdenv.isLinux "${pkgs.bubblewrap}/bin/bwrap";
            CL_EXEC_SANDBOX_HELPER = pkgs.lib.optionalString
              pkgs.stdenv.isLinux
              "${autolith.sandboxHelper}/libexec/cl-exec-sandbox-helper";
          };

          apps.default = {
            type = "app";
            program = "${autolith}/bin/autolith";
            meta.description = "Run Autolith";
          };

          checks = {
            image-validation = autolith.imageValidation;

            startup = pkgs.runCommand "autolith-startup-check" {
              nativeBuildInputs = [
                autolith
                upgradeAutolith
                pkgs.coreutils
                pkgs.gnugrep
              ];
            } ''
              export HOME="$TMPDIR/home"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/data"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_STATE_HOME/autolith"
              printf '%s\n' preserved > "$XDG_STATE_HOME/autolith/private-state"

              runtime_root="$XDG_DATA_HOME/autolith/runtimes/${pkgs.sbcl.version}"
              mkdir -p "$runtime_root/source"
              printf '%s\n' 'unmanaged source tree' > \
                "$runtime_root/source/generate-version.sh"
              chmod 000 "$runtime_root/source/generate-version.sh"

              # A stale pre-identity Nix layer and another package identity must
              # never be selected or removed by the current package.
              mkdir -p "$XDG_DATA_HOME/autolith/nix/active"
              printf '%s\n' stale > \
                "$XDG_DATA_HOME/autolith/nix/active/autolith-active.core"
              old_identity="$XDG_DATA_HOME/autolith/nix/images/old-package"
              mkdir -p "$old_identity/active" "$old_identity/recovery"
              printf '%s\n' old > "$old_identity/identity"
              for artifact in \
                active/autolith-active.core active/manifest.sexp \
                recovery/autolith-recovery.core recovery/manifest.sexp; do
                printf '%s\n' old > "$old_identity/$artifact"
              done

              image_directory="$XDG_DATA_HOME/autolith/nix/images/${imageIdentity}"
              mkdir -p "$image_directory/active" "$image_directory/recovery"
              printf '%s\n' "${autolith.imageIdentity}" > \
                "$image_directory/identity"
              printf '%s\n' truncated > \
                "$image_directory/active/autolith-active.core"
              printf '%s\n' '(:ACTIVE-IMAGE :VERSION 1)' > \
                "$image_directory/active/manifest.sexp"
              printf '%s\n' truncated > \
                "$image_directory/recovery/autolith-recovery.core"
              printf '%s\n' '(:RECOVERY-IMAGE :VERSION 2)' > \
                "$image_directory/recovery/manifest.sexp"

              first_output="$TMPDIR/first-output"
              concurrent_output="$TMPDIR/concurrent-output"
              second_output="$TMPDIR/second-output"
              autolith --version > "$first_output" &
              first_pid=$!
              autolith --version > "$concurrent_output" &
              concurrent_pid=$!
              wait "$first_pid"
              wait "$concurrent_pid"
              test "$(tail -n 1 "$first_output")" = \
                "autolith ${autolith.autolithSystem.version}"
              test "$(tail -n 1 "$concurrent_output")" = \
                "autolith ${autolith.autolithSystem.version}"
              test "$(cat "$first_output" "$concurrent_output" | \
                grep -c 'Installed preloaded active image')" = 1
              test "$(cat "$first_output" "$concurrent_output" | \
                grep -c 'Installed pristine recovery image')" = 1

              test "$(cat "$image_directory/identity")" = "${autolith.imageIdentity}"
              for artifact in \
                active/autolith-active.core active/manifest.sexp \
                recovery/autolith-recovery.core recovery/manifest.sexp; do
                test -f "$image_directory/$artifact"
                test ! -L "$image_directory/$artifact"
              done
              test -w "$image_directory/active/autolith-active.core"
              test -w "$image_directory/active/manifest.sexp"
              test -d "$old_identity"
              test -f "$XDG_DATA_HOME/autolith/nix/active/autolith-active.core"
              test "$(cat "$XDG_STATE_HOME/autolith/private-state")" = preserved
              test -d "$runtime_root/source"
              test ! -L "$runtime_root/source"
              test -e "$runtime_root/source/generate-version.sh"

              test "$(cat "${autolith.imageValidation}/image-validation")" = validated
              test -z "$(find "${autolith.imageValidation}" -type f \
                \( -name '*.core' -o -name 'manifest.sexp' \) -print -quit)"

              active_hash=$(sha256sum \
                "$image_directory/active/autolith-active.core" | cut -d ' ' -f 1)
              recovery_hash=$(sha256sum \
                "$image_directory/recovery/autolith-recovery.core" | cut -d ' ' -f 1)
              autolith --version > "$second_output"
              test "$(cat "$second_output")" = \
                "autolith ${autolith.autolithSystem.version}"
              test "$active_hash" = "$(sha256sum \
                "$image_directory/active/autolith-active.core" | cut -d ' ' -f 1)"
              test "$recovery_hash" = "$(sha256sum \
                "$image_directory/recovery/autolith-recovery.core" | cut -d ' ' -f 1)"
              ! grep -F 'Installed preloaded active image' "$second_output"
              ! grep -F 'Installed pristine recovery image' "$second_output"

              # A second package identity sharing the same XDG roots must build
              # and select its own complete pair without touching package A or
              # the user's private state.
              upgrade_output="$TMPDIR/upgrade-output"
              "${upgradeAutolith}/bin/autolith" --version > "$upgrade_output"
              test "$(tail -n 1 "$upgrade_output")" = \
                "autolith ${upgradeAutolith.autolithSystem.version}"
              upgrade_directory="$XDG_DATA_HOME/autolith/nix/images/${upgradeImageIdentity}"
              test "$upgrade_directory" != "$image_directory"
              test "$(cat "$upgrade_directory/identity")" = \
                "${upgradeAutolith.imageIdentity}"
              for artifact in \
                active/autolith-active.core active/manifest.sexp \
                recovery/autolith-recovery.core recovery/manifest.sexp; do
                test -f "$upgrade_directory/$artifact"
                test ! -L "$upgrade_directory/$artifact"
              done
              test "$active_hash" = "$(sha256sum \
                "$image_directory/active/autolith-active.core" | cut -d ' ' -f 1)"
              test "$recovery_hash" = "$(sha256sum \
                "$image_directory/recovery/autolith-recovery.core" | cut -d ' ' -f 1)"
              test "$(cat "$XDG_STATE_HOME/autolith/private-state")" = preserved
              ! grep -F 'fast startup image is missing or stale' "$upgrade_output"
              ! grep -F 'Autolith bootstrap needs Quicklisp' "$upgrade_output"

              export COLORLISP_NATIVE_LIBRARY="${autolith.colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}"
              "${autolith.runtime}/bin/sbcl" \
                --noinform \
                --no-sysinit \
                --no-userinit \
                --non-interactive \
                --eval '(require :asdf)' \
                --eval '(asdf:load-system :colorlisp)' \
                --eval '(unless (find :number (colorlisp:highlight-spans "fn main() { 42 }" :language :rust) :key (function colorlisp:span-category)) (error "Packaged ColorLisp failed to classify a Rust number."))'
              touch "$out"
            '';
          };
        };
    };
}

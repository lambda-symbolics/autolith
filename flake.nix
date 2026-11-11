{
  description = "Autolith - a live, self-modifying Common Lisp agent";

  # Autolith pins an exact SBCL (see sbcl.version) and its Quicklisp package
  # set is generated against a matching nixpkgs. Pin that nixpkgs here so the
  # build is reproducible; bump it in lockstep whenever sbcl.version changes.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/d482ef84049d9b7276b83a06e4e4d76983830097";
    crane.url = "github:ipetkov/crane";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://symbolics.cachix.org"
    ];
    extra-trusted-public-keys = [
      "symbolics.cachix.org-1:Wdgpxequ4fzP4FAckIf4MqMG9OpNxWz6CttNwiGf4AI="
    ];
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      perSystem = { pkgs, ... }:
        let
          autolith = import ./nix/package.nix {
            inherit pkgs;
            inherit (inputs) crane;
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
            inherit (inputs) crane;
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
              umask 022
              export HOME="$TMPDIR/home"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/data"
              export XDG_CACHE_HOME="$TMPDIR/cache"
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

              legacy_cache="$XDG_DATA_HOME/autolith/nix/asdf-cache/old-package"
              mkdir -p "$legacy_cache"
              printf '%s\n' legacy > "$legacy_cache/fixture.fasl"
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

              chmod 750 "$XDG_DATA_HOME/autolith" "$XDG_STATE_HOME/autolith"
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
                "autolith version ${autolith.autolithSystem.version}"
              test "$(tail -n 1 "$concurrent_output")" = \
                "autolith version ${autolith.autolithSystem.version}"
              test "$(cat "$first_output" "$concurrent_output" | \
                grep -c 'Installed preloaded active image')" = 1
              test "$(cat "$first_output" "$concurrent_output" | \
                grep -c 'Installed pristine recovery image')" = 1

              cache_directory="$XDG_CACHE_HOME/autolith/nix/asdf-cache/${imageIdentity}"
              test -d "$cache_directory"
              test -n "$(find "$cache_directory" -name '*.fasl' -print -quit)"
              test "$(stat -c %a "$XDG_CACHE_HOME/autolith")" = 700
              test "$(cat "$legacy_cache/fixture.fasl")" = legacy
              test ! -e "$XDG_DATA_HOME/autolith/nix/asdf-cache/${imageIdentity}"
              chmod 750 "$XDG_CACHE_HOME/autolith"
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
              test "$(stat -c %a "$XDG_DATA_HOME/autolith")" = 750
              test "$(stat -c %a "$XDG_STATE_HOME/autolith")" = 750
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
                "autolith version ${autolith.autolithSystem.version}"
              test "$active_hash" = "$(sha256sum \
                "$image_directory/active/autolith-active.core" | cut -d ' ' -f 1)"
              test "$recovery_hash" = "$(sha256sum \
                "$image_directory/recovery/autolith-recovery.core" | cut -d ' ' -f 1)"
              ! grep -F 'Installed preloaded active image' "$second_output"
              ! grep -F 'Installed pristine recovery image' "$second_output"
              test "$(stat -c %a "$XDG_CACHE_HOME/autolith")" = 750

              HOME="$TMPDIR/unset-cache-home" env -u XDG_CACHE_HOME \
                autolith --version > "$TMPDIR/unset-cache-output"
              test -d "$TMPDIR/unset-cache-home/.cache/autolith/nix/asdf-cache/${imageIdentity}"
              test "$(stat -c %a "$TMPDIR/unset-cache-home/.cache/autolith")" = 700

              # Relative XDG bases are invalid and must use the HOME fallback.
              fallback_output="$TMPDIR/fallback-output"
              XDG_DATA_HOME=relative/data XDG_STATE_HOME=relative/state \
                XDG_CACHE_HOME=relative/cache autolith --version > "$fallback_output"
              test "$(tail -n 1 "$fallback_output")" = \
                "autolith version ${autolith.autolithSystem.version}"
              fallback_image="$HOME/.local/share/autolith/nix/images/${imageIdentity}"
              test -f "$fallback_image/active/autolith-active.core"
              test ! -e "$TMPDIR/relative/data/autolith/nix"
              test "$(stat -c %a "$HOME/.local/share/autolith")" = 700
              test "$(stat -c %a "$HOME/.local/state/autolith")" = 700
              test -d "$HOME/.cache/autolith/nix/asdf-cache/${imageIdentity}"
              test "$(stat -c %a "$HOME/.cache/autolith")" = 700
              test ! -e "$TMPDIR/relative/cache/autolith/nix"

              # A second package identity sharing the same XDG roots must build
              # and select its own complete pair without touching package A or
              # the user's private state.
              upgrade_output="$TMPDIR/upgrade-output"
              "${upgradeAutolith}/bin/autolith" --version > "$upgrade_output"
              test "$(tail -n 1 "$upgrade_output")" = \
                "autolith version ${upgradeAutolith.autolithSystem.version}"
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
                --load "${autolith.runtime.asdfFasl}/asdf.${autolith.runtime.faslExt}" \
                --eval '(asdf:load-system :colorlisp)' \
                --eval '(unless (find :number (colorlisp:highlight-spans "fn main() { 42 }" :language :rust) :key (function colorlisp:span-category)) (error "Packaged ColorLisp failed to classify a Rust number."))'
              touch "$out"
            '';

            acp-startup = pkgs.runCommand "autolith-acp-startup-check" {} ''
              export HOME="$TMPDIR/home"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/data"
              export XDG_STATE_HOME="$TMPDIR/state"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              mkdir -p "$HOME"
              "${autolith.testRuntime}/bin/sbcl" \
                --noinform --no-sysinit --no-userinit --non-interactive \
                --load "${autolith.testRuntime.asdfFasl}/asdf.${autolith.testRuntime.faslExt}" \
                --eval '(asdf:load-system :autolith/tests)' \
                --eval '(dolist (name (list "ASDF_OUTPUT_TRANSLATIONS" "CL_SOURCE_REGISTRY")) (autolith::platform-unsetenv name))' \
                --eval '(let ((result (autolith::acp-launcher-tests--initialize "${autolith}/bin/autolith" :arguments (quote ("acp")) :directory (uiop:ensure-directory-pathname (uiop:getenv "TMPDIR"))))) (assert (equal "autolith" (agentcomms:json-get (agentcomms:json-get result "agentInfo") "name"))) (assert (agentcomms:acp-capability-enabled-p (agentcomms:json-get result "agentCapabilities") "loadSession")))'
              touch "$out"
            '';
          };
        };
    };
}

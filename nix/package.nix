{ pkgs, src }:

let
  lib = pkgs.lib;
  expectedSbclVersion = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl.version");
  expectedSbclSourceHash = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl-source.sha256");

  clColorist = pkgs.sbcl.buildASDFSystem {
    pname = "cl-colorist";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "cl-colorist";
      rev = "91041f50af55fa82f7f099b7be222055624b20af";
      hash = "sha256-a6ITI24TPXsy6AkRbuZlu/0NC6w2QwDBS4NJIQ4hotc=";
    };
  };

  clinedi = pkgs.sbcl.buildASDFSystem {
    pname = "clinedi";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clinedi";
      rev = "5c1ccedba423cbe424214593345962b70fc0512c";
      hash = "sha256-HV9+8WpCEhZ7qd0I+0doITTdveX4bcfs9azCMKbIXBA=";
    };
    lispLibs = [ clColorist ];
  };

  mcparen = pkgs.sbcl.buildASDFSystem {
    pname = "mcparen";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "mcparen";
      rev = "a0981df8ca0910fc0676e1b34b5507fbd54ac901";
      hash = "sha256-2Tc7m/5isu0wxo0rPJ5iL+HJhPDQbwGSL93UGlFaxA4=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      dexador
      serapeum
      yason
    ];
  };

  colorlispSource = pkgs.fetchFromGitHub {
    owner = "luciusmagn";
    repo = "colorlisp";
    rev = "6e1ee575bf57628fa864acd6f0a61209af9990b1";
    hash = "sha256-4c/yexgk8hBsBk7pvTNKS79vGLKIeK6+vUcWvcqb5No=";
  };

  colorlispNativeLibrary = pkgs.stdenv.mkDerivation {
    pname = "colorlisp-tree-sitter";
    version = "0.2.0";
    src = colorlispSource;
    dontConfigure = true;
    buildPhase = ''
      runHook preBuild
      cc -shared -fPIC -O2 -std=gnu11 -fvisibility=hidden \
        -I vendor/tree-sitter/include \
        -I vendor/tree-sitter/src \
        $(find vendor/grammars -mindepth 1 -maxdepth 1 -type d -printf '-I %p ') \
        -o libcolorlisp-tree-sitter.so \
        native/colorlisp-tree-sitter.c \
        vendor/tree-sitter/src/lib.c \
        $(find vendor/grammars -type f -name parser.c -print | sort) \
        $(find vendor/grammars -type f -name scanner.c -print | sort)
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 libcolorlisp-tree-sitter.so \
        "$out/lib/libcolorlisp-tree-sitter.so"
      runHook postInstall
    '';
  };

  colorlisp = pkgs.sbcl.buildASDFSystem {
    pname = "colorlisp";
    version = "0.2.0";
    src = colorlispSource;
    lispLibs = with pkgs.sbclPackages; [
      babel
      cffi
      cl-ppcre
    ];
  };

  clifff = pkgs.sbcl.buildASDFSystem {
    pname = "clifff";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clifff";
      rev = "29c19b6bdb1d19e1ceb3ed5279eed5b81b0872d8";
      hash = "sha256-oDAr7M8uqS7P4YGp3Azw9V83cy7ik1MQ3q1shW/3tZs=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      cffi
    ];
  };

  sexpStore = pkgs.sbcl.buildASDFSystem {
    pname = "sexp-store";
    version = "0.2.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "sexp-store";
      rev = "a03ddb709eb43efdd2f1a98dd87aa4e7f444940c";
      hash = "sha256-ftX6Ohcy748mzgWC9qe1/09aczXjyvAdPC9O5zEaGtg=";
    };
  };

  sbclWorkers = pkgs.sbcl.buildASDFSystem {
    pname = "sbcl-workers";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "sbcl-workers";
      rev = "fff2bc4bbeb8eec93a963c5ad1f7af85bbf7a6a3";
      hash = "sha256-lfL8HsQI7ZOMo5nqEghYZyVVEtCmSy8UPzyzBRS7Wd8=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      sexpStore
    ];
  };

  clExecSandboxSource = pkgs.fetchFromGitHub {
    owner = "luciusmagn";
    repo = "cl-exec-sandbox";
    rev = "8c47a1dadb64eba1629742ef6b43789ed7d73b36";
    hash = "sha256-zcqvLvsjRDf7LKJj3YAHRMvKkXB0Tb37GSCFyyuG5TU=";
  };

  clExecSandbox = pkgs.sbcl.buildASDFSystem {
    pname = "cl-exec-sandbox";
    version = "0.1.0";
    src = clExecSandboxSource;
  };

  sandboxHelper = pkgs.stdenv.mkDerivation {
    pname = "cl-exec-sandbox-helper";
    version = "0.1.0";
    src = clExecSandboxSource;
    nativeBuildInputs = [ pkgs.bash ];
    dontConfigure = true;
    buildPhase = ''
      runHook preBuild
      bash scripts/build-helper
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 build/cl-exec-sandbox-helper \
        "$out/libexec/cl-exec-sandbox-helper"
      runHook postInstall
    '';
  };

  fffLibrary = pkgs.rustPlatform.buildRustPackage {
    pname = "fff-c";
    version = "0.9.6";
    src = pkgs.fetchFromGitHub {
      owner = "dmtrKovalenko";
      repo = "fff";
      rev = "44a5b259570730a4236ecbf06673d43ef7b2263e";
      hash = "sha256-TfXlPzdGHvDrXWD2S24UgwkUAMGHR8w5FeWhW4h1tWs=";
    };
    cargoHash = "sha256-QxEp8Cw45SywJRoCPZayC6MnK/wSN2Bk6PIZ/8NqEk4=";
    cargoBuildFlags = [ "-p" "fff-c" ];
    cargoTestFlags = [ "-p" "fff-c" ];
    nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
    buildInputs = [ pkgs.zlib ];
    installPhase = ''
      runHook preInstall
      install -Dm755 \
        "$(find target -type f -name libfff_c.so -print -quit)" \
        "$out/lib/libfff_c.so"
      runHook postInstall
    '';
  };

  autolithSystem = pkgs.sbcl.buildASDFSystem {
    pname = "autolith";
    version = "0.16.3";
    inherit src;
    systems = [ "autolith" "autolith/tests" ];
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      cl-base64
      cffi
      closer-mop
      colorlisp
      dexador
      ironclad
      opticl
      quri
      serapeum
      yason
      clColorist
      clinedi
      clExecSandbox
      clifff
      mcparen
      sbclWorkers
      sexpStore
    ];
    nativeBuildInputs = [ pkgs.git ];

    postInstall = ''
      # Upstream launchers load .qlot/setup.lisp. Map that tiny interface to
      # the Nix-provided ASDF registry so startup and image builds stay offline.
      mkdir -p "$out/.qlot"
      cat > "$out/.qlot/setup.lisp" <<'LISP'
      (require :asdf)
      (let* ((source-root (uiop:getenv "AUTOLITH_NIX_SOURCE_ROOT"))
             (cache-root  (uiop:getenv "AUTOLITH_ASDF_CACHE")))
        (when (and source-root cache-root)
          (let* ((source
                   (uiop:ensure-directory-pathname source-root))
                 (configuration
                   (asdf/output-translations:parse-output-translations-string
                    (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")))
                 (entry
                   (find-if
                    (lambda (candidate)
                      (and (consp candidate)
                           (stringp (first candidate))
                           (uiop:pathname-equal
                            source
                            (uiop:ensure-directory-pathname
                             (first candidate)))))
                    (rest configuration))))
            (unless entry
              (error "No Nix ASDF mapping exists for ~A" source-root))
            (setf (second entry) (format nil "~A//" cache-root))
            (asdf:initialize-output-translations configuration))))
      (defpackage #:ql
        (:use #:cl)
        (:export #:quickload))
      (in-package #:ql)
      (defun quickload (system &key silent &allow-other-keys)
        (declare (ignore silent))
        (asdf:load-system system))
      LISP

      rm -f "$out/.gitignore"
      cp ${src}/.gitignore "$out/.gitignore"
      chmod u+w "$out/.gitignore"
      printf '\n/nix-support/\n' >> "$out/.gitignore"

      # Autolith records source provenance with Git. Flake source archives do
      # not contain .git, so create a deterministic, read-only repository.
      git init --quiet --initial-branch=master "$out"
      git -C "$out" config user.name "Autolith Nix build"
      git -C "$out" config user.email "nix-build@localhost"
      git -C "$out" config gc.auto 0
      git -C "$out" config maintenance.auto false
      git -C "$out" add --all
      GIT_AUTHOR_DATE='2000-01-01T00:00:00Z' \
        GIT_COMMITTER_DATE='2000-01-01T00:00:00Z' \
        git -C "$out" commit --quiet --message "Autolith source"

      # A stat-less index does not need refreshing when Git reads it from the
      # immutable Nix store at runtime.
      rm "$out/.git/index"
      git -C "$out" read-tree HEAD

      # Pack synchronously before Nix scans the output. Background maintenance
      # can otherwise remove loose objects during the fixup phase.
      git -C "$out" gc --quiet --prune=now
    '';
  };

  runtime = pkgs.sbcl.withPackages (_: [ autolithSystem ]);

  sbclSource = pkgs.runCommand "autolith-sbcl-${expectedSbclVersion}-source" {
    nativeBuildInputs = [ pkgs.bzip2 pkgs.coreutils pkgs.gnutar ];
  } ''
    actual_hash=$(sha256sum ${pkgs.sbcl.src} | cut -d ' ' -f 1)
    if [ "$actual_hash" != "${expectedSbclSourceHash}" ]; then
      echo "SBCL source hash mismatch: expected ${expectedSbclSourceHash}, got $actual_hash" >&2
      exit 1
    fi

    mkdir -p "$out"
    tar -xjf ${pkgs.sbcl.src} --strip-components=1 -C "$out"
    test -f "$out/version.lisp-expr"
    test -f "$out/src/code/list.lisp"
  '';

in
assert pkgs.stdenv.hostPlatform.isx86_64;
assert pkgs.sbcl.version == expectedSbclVersion;
pkgs.writeShellApplication {
  name = "autolith";
  runtimeInputs = [
    pkgs.bash
    pkgs.bubblewrap
    pkgs.coreutils
    pkgs.git
    pkgs.gnugrep
    runtime
  ];
  text = ''
    home="''${HOME:-/home/user}"
    data_home="''${XDG_DATA_HOME:-$home/.local/share}"
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"
    export COLORLISP_NATIVE_LIBRARY="${colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter.so"
    export AUTOLITH_FFF_LIBRARY="${fffLibrary}/lib/libfff_c.so"
    export CL_EXEC_SANDBOX_BWRAP="${pkgs.bubblewrap}/bin/bwrap"
    export CL_EXEC_SANDBOX_HELPER="${sandboxHelper}/libexec/cl-exec-sandbox-helper"

    # The packaged source repository is root-owned in /nix/store. Permit Git
    # provenance reads without weakening safe.directory globally.
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    runtime_root="$data_home/autolith/runtimes/${expectedSbclVersion}"
    asdf_cache="$data_home/autolith/asdf-cache/${builtins.baseNameOf (toString autolithSystem)}"
    mkdir -p "$runtime_root"
    mkdir -p "$asdf_cache"
    export AUTOLITH_ASDF_CACHE="$asdf_cache"
    export AUTOLITH_NIX_SOURCE_ROOT="${autolithSystem}/"
    export AUTOLITH_INSTALLATION_KIND=nix

    if [ "$(readlink "$runtime_root/source" 2>/dev/null || true)" != "${sbclSource}" ]; then
      rm -rf "$runtime_root/source"
      ln -s "${sbclSource}" "$runtime_root/source"
    fi
    printf '%s %s\n' \
      "${expectedSbclVersion}" \
      "${expectedSbclSourceHash}" > "$runtime_root/source.identity"
    # The Nix runtime needs companion libraries and environment from this
    # wrapper. Its bare persisted SBCL path would poison source-based tooling.
    recorded_runtime=
    if [ -r "$runtime_root/command" ]; then
      IFS= read -r recorded_runtime < "$runtime_root/command" || recorded_runtime=
    fi
    case "$recorded_runtime" in
      /nix/store/*-sbcl-with-packages*/bin/sbcl)
        rm -f "$runtime_root/command"
        ;;
    esac

    recovery_core="$data_home/autolith/recovery/autolith-recovery.core"
    active_core="$data_home/autolith/active/autolith-active.core"

    # Build the fast recovery + active images whenever the packaged Autolith
    # changes (its store path is the upgrade signal) or an image is missing.
    # Upstream's own bootstrap fetches SBCL and deps over the network, so we
    # drive the offline build scripts directly with the Nix-provided runtime.
    # We gate on the store path rather than re-deriving upstream's image-
    # manifest validity check: a core that is corrupt without a version change
    # is caught by the launcher we exec below, which re-validates and falls
    # back to a (slower) source load.
    build_marker="$data_home/autolith/images.built-for"
    if [ ! -f "$recovery_core" ] || [ ! -f "$active_core" ] ||
       [ "$(cat "$build_marker" 2>/dev/null || true)" != "${autolithSystem}" ]; then
      echo "Building Autolith images (first run or package upgrade)..." >&2
      "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-recovery.lisp"
      "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-active.lisp"
      printf '%s\n' "${autolithSystem}" > "$build_marker"
    fi

    exec ${pkgs.bash}/bin/bash "${autolithSystem}/bin/autolith" "$@"
  '';

  meta = {
    description = "A live, self-modifying Common Lisp agent";
    homepage = "https://github.com/luciusmagn/autolith";
    license = lib.licenses.mit;
    mainProgram = "autolith";
    platforms = [ "x86_64-linux" ];
  };

  passthru = {
    inherit autolithSystem clColorist clExecSandbox clifff clinedi colorlisp
      colorlispNativeLibrary fffLibrary runtime sandboxHelper sbclSource
      mcparen sbclWorkers sexpStore;
  };
}

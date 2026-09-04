{ pkgs, src }:

let
  lib = pkgs.lib;
  inherit (pkgs.stdenv.hostPlatform.extensions) sharedLibrary;
  colorlispSharedLibraryFlag = if pkgs.stdenv.isDarwin
    then "-dynamiclib"
    else "-shared";
  expectedSbclVersion = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl.version");
  expectedSbclSourceHash = lib.removeSuffix "\n" (builtins.readFile "${src}/sbcl-source.sha256");
  fffSourceCommit = lib.removeSuffix "\n" (builtins.readFile "${src}/native/fff/commit");

  # Quicklisp's NYAML archive includes dangling symlinks in its unused test data.
  nyaml = pkgs.sbclPackages.nyaml.overrideAttrs (old: {
    postInstall = (old.postInstall or "") + ''
      rm -rf "$out/test/yaml-test-suite-data"
    '';
  });

  clColorist = pkgs.sbcl.buildASDFSystem {
    pname = "cl-colorist";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "cl-colorist";
      rev = "a4b65e63f40248c091d8ccf6023ad6fef5de7f0d";
      hash = "sha256-UhQnhWYyX+VYhYbiLCMfw3vutdNyWn/CJj2xQPKYcAM=";
    };
  };

  clLlmProviderApi = pkgs.sbcl.buildASDFSystem {
    pname = "cl-llm-provider-api";
    version = "0.2.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "cl-llm-provider-api";
      rev = "a25e9b141628e682933fc9d45fc7cfb00a6dcdd2";
      hash = "sha256-1bhh/lniQRksPGQp0X9lACgzdHOLo23Ux/J7qRMhwew=";
    };
    lispLibs = with pkgs.sbclPackages; [
      babel
      bordeaux-threads
      ironclad
    ];
  };

  clRfc8628 = pkgs.sbcl.buildASDFSystem {
    pname = "cl-rfc8628";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "cl-rfc8628";
      rev = "37aaeef092c1212d0886bdd68af6eebecd77a98f";
      hash = "sha256-VBYhkBkCUXHduMTQQpy15GBAKjRalHp5zT1AV7BAMG8=";
    };
    lispLibs = with pkgs.sbclPackages; [
      cl-base64
      dexador
      quri
      yason
    ];
  };

  clinkerTranscript = pkgs.sbcl.buildASDFSystem {
    pname = "clinker-transcript";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "clinker-transcript";
      rev = "73aebf6c498bc6c9b42d2a5fdbe961163feaa933";
      hash = "sha256-qGMjMofpUw6jIWROTgMKEwfwIjR2joleCPxUMu9EW7I=";
    };
    lispLibs = with pkgs.sbclPackages; [
      yason
    ];
  };

  imageDaemon = pkgs.sbcl.buildASDFSystem {
    pname = "image-daemon";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "image-daemon";
      rev = "fb530e996ab83f91b79d316b93c98517b7e25bfd";
      hash = "sha256-B231xdSn7nuB1zEWricG7lPEx3HeYnrgWDSOuw94YpA=";
    };
    lispLibs = [
      idsmall
      pkgs.sbclPackages.ironclad
    ];
  };

  lsCompat = pkgs.sbcl.buildASDFSystem {
    pname = "ls-compat";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "ls-compat";
      rev = "84ca8b1f3be29817f5aff93ac2b341347fd287fb";
      hash = "sha256-D5aCdrpX8nzAcAZ6LLv+GbjYfZUoQVE4Ei77V/SGUb8=";
    };
    systems = [ "ls-compat" "ls-compat/posix" ];
    lispLibs = with pkgs.sbclPackages; [
      babel
      serapeum
    ];
  };

  lsFlock = pkgs.sbcl.buildASDFSystem {
    pname = "ls-flock";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "ls-flock";
      rev = "ff7a6abbc53f069c13d9649232ade062409c12da";
      hash = "sha256-vzTiH1dlDTVWpfcTphdMRU4vziDsa1YuWYHdoiuU6P0=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
    ];
  };

  clSkills = pkgs.sbcl.buildASDFSystem {
    pname = "cl-skills";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "cl-skills";
      rev = "aafcf34e186bf85c8d8e70ab7e86f7259bcbf412";
      hash = "sha256-wpNxMJeYefW+BhZMx5Sr9Qae3AKeopLWM9aYK4lQBV0=";
    };
    lispLibs = [
      pkgs.sbclPackages.ironclad
      nyaml
      pkgs.sbclPackages.serapeum
      sexpConfig
    ];
  };

  clinedi = pkgs.sbcl.buildASDFSystem {
    pname = "clinedi";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clinedi";
      rev = "de133d8467ed211369923285078cbfa456e1d92b";
      hash = "sha256-2R6YLxOLx20VKstHdkPWKlLZPlqEjqipH1Nh6i3UfDU=";
    };
    lispLibs = [ clColorist ];
  };

  mcparen = pkgs.sbcl.buildASDFSystem {
    pname = "mcparen";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "mcparen";
      rev = "58ba29a8dd8d06417452b7978748c3d453bc3287";
      hash = "sha256-VMZH7DahgtSv4cRdhrGbaCqAkzOKcmnXSVwjmsbL2Hg=";
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
    rev = "05a3313d52e2a2c643219a750dd5045df56c1cd7";
    hash = "sha256-P8zoiBaNyZpR9QDJOi3wF/D3BWy3GaPOlz/LPBd4Tyc=";
  };

  colorlispNativeLibrary = pkgs.stdenv.mkDerivation {
    pname = "colorlisp-tree-sitter";
    version = "0.2.0";
    src = colorlispSource;
    nativeBuildInputs = [ pkgs.findutils ];
    dontConfigure = true;
    buildPhase = ''
      runHook preBuild
      cc ${colorlispSharedLibraryFlag} -fPIC -O2 -std=gnu11 -fvisibility=hidden \
        -I vendor/tree-sitter/include \
        -I vendor/tree-sitter/src \
        $(find vendor/grammars -mindepth 1 -maxdepth 1 -type d -printf '-I %p ') \
        -o libcolorlisp-tree-sitter${sharedLibrary} \
        native/colorlisp-tree-sitter.c \
        vendor/tree-sitter/src/lib.c \
        $(find vendor/grammars -type f -name parser.c -print | sort) \
        $(find vendor/grammars -type f -name scanner.c -print | sort)
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      install -Dm755 libcolorlisp-tree-sitter${sharedLibrary} \
        "$out/lib/libcolorlisp-tree-sitter${sharedLibrary}"
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

  colordiff = pkgs.sbcl.buildASDFSystem {
    pname = "colordiff";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "colordiff";
      rev = "e224fd4148c399998ba893e71b7da0cc8a2c658a";
      hash = "sha256-GdjAlug81TLNG6AcwZw/dlLQbep2UtEDAGQY9/pTqKo=";
    };
    lispLibs = [
      clColorist
      colorlisp
    ];
  };

  clTermdown = pkgs.sbcl.buildASDFSystem {
    pname = "cl-termdown";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "cl-termdown";
      rev = "340579ac634074791e9c2cc3c35323fec3b7cf66";
      hash = "sha256-etsARI3MrICmoJNCriaaptUrPVd31eLefPE8Vtfjtz0=";
    };
    lispLibs = with pkgs.sbclPackages; [
      clinedi
      colordiff
      colorlisp
      serapeum
    ];
  };

  parenchek = pkgs.sbcl.buildASDFSystem {
    pname = "parenchek";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "parenchek";
      rev = "a7dc0a7e2c6250056c294ab39d5d2872dba71592";
      hash = "sha256-3qBzFYCreWV12EZ3WnzQ3wZM7449O2fR2km1TEeOvtY=";
    };
    lispLibs = with pkgs.sbclPackages; [
      serapeum
    ];
  };

  orgTemplater = pkgs.sbcl.buildASDFSystem {
    pname = "org-templater";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "org-templater";
      rev = "918d456528474e880014fe8377e4810e4a55e6a9";
      hash = "sha256-M9kMLo9bBnxIMUVCVFdXqLpFi2/mJSx+iBcOCBw7feQ=";
    };
  };

  structlisp = pkgs.sbcl.buildASDFSystem {
    pname = "structlisp";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "structlisp";
      rev = "66bbfbe947b69f3105f71f5e4293637decc91746";
      hash = "sha256-ax+mwi9opGFkycQjNNDJyLPUI11aOdTtljb4WDthUI4=";
    };
  };

  clifff = pkgs.sbcl.buildASDFSystem {
    pname = "clifff";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "clifff";
      rev = "8bf3ebb0985108593cad105d31639cedbe9e5373";
      hash = "sha256-PAp4odBnJG/NcGZRjuQCQBpm75QZgNbq/cQcMHccgtw=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      cffi
    ];
  };

  sexpStore = pkgs.sbcl.buildASDFSystem {
    pname = "sexp-store";
    version = "0.3.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "sexp-store";
      rev = "815ef23b48b9bd9ef974ada28b9e8c1b5cf11b1f";
      hash = "sha256-ulkpH9fxM+9nobkrBRQwTal6U0gqou6iiR+N38jcf/I=";
    };
    lispLibs = [ lsCompat ];
  };

  sbclWorkers = pkgs.sbcl.buildASDFSystem {
    pname = "sbcl-workers";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lambda-symbolics";
      repo = "sbcl-workers";
      rev = "85bbab4a4c97a7f345ddfa6f15fa187211c96ac7";
      hash = "sha256-9v8eXI+o7FMpILgp78kOhFe6rJYMdo9pk/aJekrG74I=";
    };
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      sexpStore
    ];
  };

  idsmall = pkgs.sbcl.buildASDFSystem {
    pname = "idsmall";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "idsmall";
      rev = "3f4b8e067a1e41b5388f7c5af58585e8c9ab51b9";
      hash = "sha256-KnK/eWB1DQrI1LdO63rBNbeDhWESI6b5BZq1s9Xlp2A=";
    };
    lispLibs = with pkgs.sbclPackages; [ bordeaux-threads ];
  };

  sexpConfig = pkgs.sbcl.buildASDFSystem {
    pname = "sexp-config";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "sexp-config";
      rev = "408fa906e2d16aa515b39c943d3affcab3811ffc";
      hash = "sha256-r4bCQHHVOC8peLofhzh0m8Y2vuu/uaaT/oOwAB8TaXQ=";
    };
  };

  sbclGenerations = pkgs.sbcl.buildASDFSystem {
    pname = "sbcl-generations";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "sbcl-generations";
      rev = "e65e27e9ac969c5a83c3f9c2318ae4237856d53f";
      hash = "sha256-cPWQDgAxpwR23lMCSc/bhPzCrQqOTfcpwplcObb3RLs=";
    };
    lispLibs = with pkgs.sbclPackages; [ bordeaux-threads ];
  };

  clJobpond = pkgs.sbcl.buildASDFSystem {
    pname = "cl-jobpond";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "luciusmagn";
      repo = "cl-jobpond";
      rev = "ee2e2eb0bd5080c52db55036f853c00671962178";
      hash = "sha256-2brHuyCb2dxzDqtLZa7upi9EZuEQKOximh/RwYjjb6U=";
    };
    lispLibs = with pkgs.sbclPackages; [ bordeaux-threads ];
  };

  clExecSandboxSource = pkgs.fetchFromGitHub {
    owner = "lambda-symbolics";
    repo = "cl-exec-sandbox";
    rev = "9af411ec2779efc06a7c56996babdd931d77e371";
    hash = "sha256-olVSuyKBJaoX9NYYKdTfn+bdVN+49GyI1D13XSMXnEs=";
  };

  clExecSandbox = pkgs.sbcl.buildASDFSystem {
    pname = "cl-exec-sandbox";
    version = "0.1.0";
    src = clExecSandboxSource;
  };

  # The native helper wraps Linux bubblewrap, seccomp, and network namespaces.
  # macOS uses cl-exec-sandbox's built-in Seatbelt backend without a helper.
  sandboxHelper = if pkgs.stdenv.isLinux then pkgs.stdenv.mkDerivation {
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
  } else null;

  fffLibrary = pkgs.rustPlatform.buildRustPackage {
    pname = "fff-c";
    version = "0.10.3";
    src = pkgs.fetchFromGitHub {
      owner = "dmtrKovalenko";
      repo = "fff";
      rev = fffSourceCommit;
      hash = "sha256-pE4DsaCvlvgTKJtyV8uGhAQvbJpxpgXlIkoVh8I15qw=";
    };
    cargoHash = "sha256-iRQa3K5/E520hbq6yO+RRG8pjJBTamj/nm13XCHNOZs=";
    cargoBuildFlags = [ "-p" "fff-c" ];
    cargoTestFlags = [ "-p" "fff-c" ];
    nativeBuildInputs = [ pkgs.cmake pkgs.pkg-config ];
    buildInputs = [ pkgs.zlib ];
    installPhase = ''
      runHook preInstall
      install -Dm755 \
        "$(find target -type f -name 'libfff_c${sharedLibrary}' -print -quit)" \
        "$out/lib/libfff_c${sharedLibrary}"
      runHook postInstall
    '';
  };

  fetchGist = pkgs.sbcl.buildASDFSystem {
    pname = "fetch-gist";
    version = "0.1.0";
    src = pkgs.fetchFromGitHub {
      owner = "lenny99";
      repo = "fetch-gist";
      rev = "6b8188343a1ae20630c0242d7028a19ffa1d283a";
      hash = "sha256-SmyoXDWwpQeewI8/p68udJzfF7YPhD9LqA90PSo8DLo=";
    };
    lispLibs = with pkgs.sbclPackages; [
      alexandria
      babel
      dexador
      iterate
      plump
      quri
      serapeum
    ];
  };

  autolithSystem = pkgs.sbcl.buildASDFSystem {
    pname = "autolith";
    version = "0.46.1";
    inherit src;
    systems = [ "autolith" "autolith/tests" ];
    lispLibs = with pkgs.sbclPackages; [
      bordeaux-threads
      cl-base64
      cffi
      clingon
      closer-mop
      colorlisp
      colordiff
      clTermdown
      dexador
      fetchGist
      ironclad
      opticl
      parenchek
      orgTemplater
      quri
      serapeum
      yason
      clColorist
      clinedi
      clExecSandbox
      clifff
      clinkerTranscript
      clJobpond
      clLlmProviderApi
      clRfc8628
      clSkills
      idsmall
      imageDaemon
      lsFlock
      mcparen
      sbclGenerations
      sbclWorkers
      sexpConfig
      sexpStore
      structlisp
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

  imageIdentity = pkgs.writeText "autolith-image-identity" ''
    ${autolithSystem}
  '';

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

  # Sandboxing uses Bubblewrap and the private helper on Linux; other
  # platforms fall back to the portable unsandboxed path in cl-exec-sandbox.
  sandboxEnvironment = lib.optionalString pkgs.stdenv.isLinux ''
    export CL_EXEC_SANDBOX_BWRAP="${pkgs.bubblewrap}/bin/bwrap"
    export CL_EXEC_SANDBOX_HELPER="${sandboxHelper}/libexec/cl-exec-sandbox-helper"
  '';

  # SBCL saved cores are intentionally transient here. Their bytes are not
  # reproducible, so the derivation publishes only a deterministic proof that
  # the complete Nix closure can build and probe both required images.
  imageValidation = pkgs.runCommand "autolith-image-validation-${expectedSbclVersion}" {
    nativeBuildInputs = [ pkgs.git ];
  } ''
    export HOME="$TMPDIR/home"
    export XDG_CONFIG_HOME="$TMPDIR/config"
    export XDG_DATA_HOME="$TMPDIR/data"
    export XDG_STATE_HOME="$TMPDIR/state"
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"
    export AUTOLITH_ASDF_CACHE="$TMPDIR/asdf-cache"
    export AUTOLITH_NIX_SOURCE_ROOT="${autolithSystem}/"
    export AUTOLITH_INSTALLATION_KIND=nix
    export COLORLISP_NATIVE_LIBRARY="${colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${sharedLibrary}"
    export AUTOLITH_FFF_LIBRARY="${fffLibrary}/lib/libfff_c${sharedLibrary}"
    ${sandboxEnvironment}
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    image_root="$TMPDIR/images"
    mkdir -p "$HOME" "$AUTOLITH_ASDF_CACHE" \
      "$image_root/active" "$image_root/recovery"
    "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-recovery.lisp" \
      "$image_root/recovery/autolith-recovery.core"
    "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-active.lisp" \
      "$image_root/active/autolith-active.core"
    test -f "$image_root/recovery/autolith-recovery.core"
    test -f "$image_root/recovery/manifest.sexp"
    test -f "$image_root/active/autolith-active.core"
    test -f "$image_root/active/manifest.sexp"

    mkdir -p "$out"
    printf '%s\n' validated > "$out/image-validation"
  '';

  imageLockRunner = pkgs.writeText "autolith-image-lock.pl" ''
    use strict;
    use warnings;
    use Fcntl qw(LOCK_EX);

    my $lock_path = shift @ARGV;
    open my $lock, '>>', $lock_path or die "$lock_path: $!\n";
    flock($lock, LOCK_EX) or die "$lock_path: $!\n";
    my $status = system @ARGV;
    die "Could not start image materializer: $!\n" if $status == -1;
    exit(128 + ($status & 127)) if $status & 127;
    exit($status >> 8);
  '';

  imageMaterializer = pkgs.writeShellScript "autolith-materialize-nix-images" ''
    set -eu

    image_root=$1
    final=$2
    expected_identity='${imageIdentity}'
    stage=
    work=
    previous=

    image_set_valid()
    {
      directory=$1
      [ -d "$directory" ] &&
        [ ! -L "$directory" ] &&
        [ -f "$directory/identity" ] &&
        [ ! -L "$directory/identity" ] &&
        [ "$(cat "$directory/identity")" = "$expected_identity" ] &&
        [ -d "$directory/active" ] &&
        [ ! -L "$directory/active" ] &&
        [ -d "$directory/recovery" ] &&
        [ ! -L "$directory/recovery" ] &&
        [ -f "$directory/active/autolith-active.core" ] &&
        [ ! -L "$directory/active/autolith-active.core" ] &&
        [ -f "$directory/active/manifest.sexp" ] &&
        [ ! -L "$directory/active/manifest.sexp" ] &&
        [ -f "$directory/recovery/autolith-recovery.core" ] &&
        [ ! -L "$directory/recovery/autolith-recovery.core" ] &&
        [ -f "$directory/recovery/manifest.sexp" ] &&
        [ ! -L "$directory/recovery/manifest.sexp" ] &&
        [ -r "$directory/identity" ] &&
        [ -r "$directory/active/autolith-active.core" ] &&
        [ -r "$directory/active/manifest.sexp" ] &&
        [ -r "$directory/recovery/autolith-recovery.core" ] &&
        [ -r "$directory/recovery/manifest.sexp" ]
    }

    image_set_usable()
    {
      directory=$1
      image_set_valid "$directory" &&
        ${pkgs.gnugrep}/bin/grep -Eq \
          '^\(:ACTIVE-IMAGE :VERSION 1([[:space:]]|$)' \
          "$directory/active/manifest.sexp" &&
        ${pkgs.gnugrep}/bin/grep -Eq \
          '^\(:RECOVERY-IMAGE :VERSION 2([[:space:]]|$)' \
          "$directory/recovery/manifest.sexp" &&
        "$AUTOLITH_SBCL" --noinform \
          --core "$directory/recovery/autolith-recovery.core" \
          --end-runtime-options "${autolithSystem}/" --probe \
          >/dev/null 2>&1 &&
        "$AUTOLITH_SBCL" --noinform \
          --core "$directory/active/autolith-active.core" \
          --end-runtime-options "${autolithSystem}/" \
          --autolith-internal-active-image-probe >/dev/null 2>&1
    }

    cleanup()
    {
      if [ -n "$stage" ] && [ -d "$stage" ]; then
        rm -rf "$stage"
      fi
      if [ -n "$work" ] && [ -d "$work" ]; then
        rm -rf "$work"
      fi
      if [ -n "$previous" ] && \
         { [ -e "$previous" ] || [ -L "$previous" ]; }; then
        rm -rf "$previous"
      fi
    }
    trap cleanup EXIT
    trap 'exit 1' HUP INT TERM

    if image_set_usable "$final"; then
      exit 0
    fi

    stage=$(mktemp -d "$image_root/.stage.XXXXXXXX")
    work=$(mktemp -d "$image_root/.work.XXXXXXXX")
    mkdir -p "$stage/active" "$stage/recovery" \
      "$work/home" "$work/config" "$work/data" "$work/state"
    export HOME="$work/home"
    export XDG_CONFIG_HOME="$work/config"
    export XDG_DATA_HOME="$work/data"
    export XDG_STATE_HOME="$work/state"

    "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-recovery.lisp" \
      "$stage/recovery/autolith-recovery.core"
    "$AUTOLITH_SBCL" --script "${autolithSystem}/script/build-active.lisp" \
      "$stage/active/autolith-active.core"
    printf '%s\n' "$expected_identity" > "$stage/identity"
    image_set_valid "$stage"
    chmod u+w "$stage/active/autolith-active.core" \
      "$stage/active/manifest.sexp"

    if [ -e "$final" ] || [ -L "$final" ]; then
      previous=$(mktemp -d "$image_root/.invalid.XXXXXXXX")
      rmdir "$previous"
      mv "$final" "$previous"
    fi
    mv "$stage" "$final"
    stage=
    if [ -n "$previous" ]; then
      rm -rf "$previous"
      previous=
    fi
  '';

in
assert with pkgs.stdenv.hostPlatform;
  (isLinux && (isx86_64 || isAarch64)) || (isDarwin && isAarch64);
assert pkgs.sbcl.version == expectedSbclVersion;
pkgs.writeShellApplication {
  name = "autolith";
  runtimeInputs = [
    pkgs.bash
    pkgs.coreutils
    pkgs.git
    pkgs.gnugrep
    pkgs.perl
    runtime
  ] ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.bubblewrap ];
    text = ''
      xdg_base_directory()
      {
        case ''${1:-} in
          /*) printf '%s\n' "$1" ;;
          *) printf '%s\n' "$2" ;;
        esac
      }

      home="''${HOME:-/home/user}"
      data_home=$(xdg_base_directory "''${XDG_DATA_HOME:-}" "$home/.local/share")
    export AUTOLITH_SBCL="${runtime}/bin/sbcl"
    export AUTOLITH_SBCL_SOURCE_ROOT="${sbclSource}"
    export COLORLISP_NATIVE_LIBRARY="${colorlispNativeLibrary}/lib/libcolorlisp-tree-sitter${sharedLibrary}"
    export AUTOLITH_FFF_LIBRARY="${fffLibrary}/lib/libfff_c${sharedLibrary}"
    ${sandboxEnvironment}

    # The packaged source repository is root-owned in /nix/store. Permit Git
    # provenance reads without weakening safe.directory globally.
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0="${autolithSystem}"
    export GIT_OPTIONAL_LOCKS=0

    # Keep Nix-managed image and ASDF state separate from source installs while
    # retaining the user's conversations and private mutation history.
    nix_root="$data_home/autolith/nix"
    identity_name="${builtins.baseNameOf (toString imageIdentity)}"
    image_root="$nix_root/images"
    image_directory="$image_root/$identity_name"
    asdf_cache="$nix_root/asdf-cache/$identity_name"
    mkdir -p "$image_root" "$asdf_cache"
    export AUTOLITH_ASDF_CACHE="$asdf_cache"
    export AUTOLITH_NIX_SOURCE_ROOT="${autolithSystem}/"
    export AUTOLITH_INSTALLATION_KIND=nix

    # Nix validates image construction at package-build time, but SBCL cores
    # are machine-local mutable state rather than reproducible store outputs.
    # Serialize first-use construction and publish each package identity as one
    # complete directory so upgrades never expose or reuse a partial image pair.
    test -f "${imageValidation}/image-validation"
    ${pkgs.perl}/bin/perl "${imageLockRunner}" \
      "$image_root/.materialize.lock" \
      "${imageMaterializer}" "$image_root" "$image_directory"

    export AUTOLITH_ACTIVE_CORE="$image_directory/active/autolith-active.core"
    export AUTOLITH_RECOVERY_CORE="$image_directory/recovery/autolith-recovery.core"

    exec ${pkgs.bash}/bin/bash "${autolithSystem}/bin/autolith" "$@"
  '';

  meta = {
    description = "A live, self-modifying Common Lisp agent";
    homepage = "https://github.com/luciusmagn/autolith";
    license = lib.licenses.mit;
    mainProgram = "autolith";
    platforms = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
  };

  passthru = {
    inherit autolithSystem clColorist clExecSandbox clifff clinedi clJobpond
      colorlisp colorlispNativeLibrary fffLibrary idsmall imageIdentity
      imageValidation runtime sandboxHelper sbclGenerations sbclSource mcparen
      sbclWorkers sexpConfig sexpStore;
  };
}

#!/usr/bin/env bash

# Shared preflight for source, Nix, and packaged launchers. Application command
# validation belongs to Clingon; this boundary only selects a launch or update.

autolith_update_usage()
{
  printf 'Usage: autolith update [--help]\n       autolith --update [--help]\n'
}

autolith_update_help()
{
  autolith_update_usage
  printf '\nInstall the latest packaged release and exit without starting a session.\nSource checkouts: update the checkout, then run ./script/bootstrap.\nNix installations: update through your flake or Nix profile.\n'
}

autolith_uninstall_usage()
{
  printf 'Usage: autolith uninstall [--yes] [--help]\n'
}

autolith_uninstall_help()
{
  autolith_uninstall_usage
  printf '\nRemove the Autolith installation, managed runtimes, built images, and caches\nwithout starting a session. User data stays: conversations, memories, agendas,\nimage commits, settings, credentials, and configuration. --yes skips the\nconfirmation prompt.\n'
}

autolith_launcher_parse()
{
  local installation_kind=$1
  shift
  local argument command_seen=false take_value=false options_ended=false
  local -a original_arguments=("$@")

  autolith_installation_kind=$installation_kind
  recovery_requested=false
  from_source_requested=false
  update_requested=false
  uninstall_requested=false
  uninstall_confirmed=false
  data_requested=false
  acp_requested=false
  remaining_arguments=()
  for argument in "$@"; do
    if [[ $take_value == true ]]; then
      remaining_arguments+=("$argument")
      take_value=false
      continue
    fi
    if [[ $options_ended == true ]]; then
      remaining_arguments+=("$argument")
      continue
    fi
    case $argument in
      --)
        options_ended=true
        remaining_arguments+=("$argument")
        ;;
      --recovery) recovery_requested=true ;;
      --from-source) from_source_requested=true ;;
      --pristine)
        from_source_requested=true
        remaining_arguments+=("$argument")
        ;;
      --permissions|--image|-i|--localgroup-handoff|--site-config-root|--id|--input|--output|\
      --events|--generation|--status|--capsule|--original-argument|--workspace)
        # Forward a value verbatim, even when it resembles a launcher flag.
        # These are arities only, not a second application option parser.
        take_value=true
        remaining_arguments+=("$argument")
        ;;
      --update)
        if [[ $command_seen == false ]]; then
          update_requested=true
        fi
        remaining_arguments+=("$argument")
        ;;
      -*) remaining_arguments+=("$argument") ;;
      *)
        if [[ $command_seen == false && $argument == update ]]; then
          update_requested=true
        fi
        if [[ $command_seen == false && $argument == uninstall ]]; then
          uninstall_requested=true
        fi
        if [[ $command_seen == false && $argument == data ]]; then
          data_requested=true
        fi
        if [[ $command_seen == false && $argument == acp ]]; then
          acp_requested=true
        fi
        command_seen=true
        remaining_arguments+=("$argument")
        ;;
    esac
  done

  if [[ $uninstall_requested == true ]]; then
    # An uninstall is a standalone operation with one optional confirmation
    # flag. The launcher performs it before any Lisp starts, so nothing here
    # depends on a runnable image.
    set -- "${original_arguments[@]}"
    if [[ $# -gt 1 && ${!#} == -- ]]; then
      set -- "${original_arguments[@]:0:$#-1}"
    fi
    if [[ ${1:-} != uninstall ]]; then
      autolith_uninstall_usage >&2
      exit 64
    fi
    shift
    for argument in "$@"; do
      case $argument in
        --yes) uninstall_confirmed=true ;;
        --help|-h)
          autolith_uninstall_help
          exit 0
          ;;
        *)
          autolith_uninstall_usage >&2
          exit 64
          ;;
      esac
    done
  fi

  if [[ $update_requested == true ]]; then
    # An update is a standalone operation, never a session option. A final --
    # ends option parsing; anything after it would be an unexpected operand.
    set -- "${original_arguments[@]}"
    if [[ $# -gt 1 && ${!#} == -- ]]; then
      set -- "${original_arguments[@]:0:$#-1}"
    fi
    if [[ (${1:-} != update && ${1:-} != --update) || $# -gt 2 ||
          ($# -eq 2 && ${2:-} != --help && ${2:-} != -h) ]]; then
      autolith_update_usage >&2
      exit 64
    fi
    if [[ $# -eq 2 ]]; then
      autolith_update_help
      exit 0
    fi
    case $installation_kind in
      release) ;;
      nix)
        printf 'Update this Nix installation through your flake or Nix profile.\n' >&2
        exit 64
        ;;
      source)
        printf 'Update the source checkout, then run ./script/bootstrap.\n' >&2
        exit 64
        ;;
    esac
  fi
}

autolith_xdg_directory()
{
  case ${1:-} in
    /*) printf '%s\n' "$1" ;;
    *) printf '%s\n' "$2" ;;
  esac
}

autolith_resolve_path()
{
  local path=$1
  local directory
  local target

  case "$path" in
    /*) ;;
    *) path=$PWD/$path ;;
  esac
  while [[ -L "$path" ]]; do
    directory=$(CDPATH= cd -P "$(dirname "$path")" && pwd) || return 1
    target=$(readlink "$path") || return 1
    case "$target" in
      /*) path=$target ;;
      *) path=$directory/$target ;;
    esac
  done
  directory=$(CDPATH= cd -P "$(dirname "$path")" && pwd) || return 1
  printf '%s/%s\n' "$directory" "$(basename "$path")"
}

autolith_uninstall_remove()
{
  # Remove one file, link, or tree. Managed runtimes and release trees hold
  # read-only files, so directories are made writable first.
  local path=$1
  if [[ -L $path || ! -d $path ]]; then
    rm -f -- "$path"
  else
    chmod -R u+w -- "$path" 2>/dev/null || true
    rm -rf -- "$path"
  fi
}

autolith_uninstall()
{
  # Remove every installed Autolith artifact and keep user data. KIND is the
  # launcher's installation kind; INSTALL_ROOT is the packaged release root
  # being removed, when the launcher runs from one; SOURCE_ROOT names the
  # checkout or Nix store path only for the closing advice.
  local kind=$1
  local install_root=${2:-}
  local source_root=${3:-}
  local home=${HOME:-}
  local data_root state_root cache_root config_root bin_directory
  local entry link target candidate status=0
  local -a targets=()
  local -a links=()

  if [[ -z $home ]]; then
    printf 'Autolith uninstall failed: HOME is not set.\n' >&2
    return 1
  fi
  data_root=$(autolith_xdg_directory "${XDG_DATA_HOME:-}" "$home/.local/share")/autolith
  state_root=$(autolith_xdg_directory "${XDG_STATE_HOME:-}" "$home/.local/state")/autolith
  cache_root=$(autolith_xdg_directory "${XDG_CACHE_HOME:-}" "$home/.cache")/autolith
  config_root=$(autolith_xdg_directory "${XDG_CONFIG_HOME:-}" "$home/.config")/autolith
  bin_directory=${AUTOLITH_BIN_DIR:-$home/.local/bin}

  # Built images, managed runtimes, retained generation cores, worker images,
  # Nix image and cache trees, release markers, and the packaged installation.
  for entry in active recovery generations lisp-images runtimes nix native \
               asdf-cache recovery-worktrees release-images images.built-for \
               installation; do
    candidate=$data_root/$entry
    if [[ -e $candidate || -L $candidate ]]; then
      targets+=("$candidate")
    fi
  done
  if [[ -n $install_root && $install_root != "$data_root/installation" &&
        ( -e $install_root || -L $install_root ) ]]; then
    targets+=("$install_root")
  fi
  # Crash capsules, launcher pointers, session sockets and leases, the
  # selected-generation pointer, and provider caches. Credentials, settings,
  # the mutation journal, and the mutation history stay.
  for entry in crashes crash-pointers recovery-session-pointers \
               restart-pointers localgroup conversation-leases \
               current-generation.sexp update-state.sexp provider-models.sexp; do
    candidate=$state_root/$entry
    if [[ -e $candidate || -L $candidate ]]; then
      targets+=("$candidate")
    fi
  done
  if [[ -e $cache_root || -L $cache_root ]]; then
    targets+=("$cache_root")
  fi
  # Command links that point into a removed installation.
  for link in "$bin_directory/autolith" "$(command -v autolith 2>/dev/null || true)"; do
    [[ -n $link && -L $link ]] || continue
    target=$(autolith_resolve_path "$link" 2>/dev/null) || continue
    case $target in
      "$data_root/installation/"*) links+=("$link") ;;
      *)
        if [[ -n $install_root && $target == "$install_root/"* ]]; then
          links+=("$link")
        fi
        ;;
    esac
  done

  if [[ ${#targets[@]} -eq 0 && ${#links[@]} -eq 0 ]]; then
    printf 'Nothing to remove: no Autolith installation, runtime, image, or cache was found.\n' >&2
  else
    printf 'Autolith uninstall removes:\n' >&2
    for entry in ${targets[@]+"${targets[@]}"} ${links[@]+"${links[@]}"}; do
      printf '  %s\n' "$entry" >&2
    done
    printf 'User data stays in %s, %s, and %s.\n' \
      "$data_root" "$state_root" "$config_root" >&2
    if [[ $uninstall_confirmed != true ]]; then
      if [[ ! -t 0 ]]; then
        printf 'Confirm with --yes when no terminal is available.\n' >&2
        return 64
      fi
      printf 'Continue? [y/N] ' >&2
      IFS= read -r entry || entry=
      case $entry in
        y|Y|yes|YES|Yes) ;;
        *)
          printf 'Autolith is not removed.\n' >&2
          return 1
          ;;
      esac
    fi
    for entry in ${links[@]+"${links[@]}"} ${targets[@]+"${targets[@]}"}; do
      if autolith_uninstall_remove "$entry"; then
        printf 'Removed %s\n' "$entry" >&2
      else
        printf 'Could not remove %s\n' "$entry" >&2
        status=1
      fi
    done
  fi
  case $kind in
    release)
      if [[ -z $install_root ]]; then
        printf 'This release runs outside an installation root. Delete its extracted directory yourself.\n' >&2
      fi
      ;;
    nix)
      printf 'Remove the package from your flake or Nix profile to finish.\n' >&2
      ;;
    source)
      printf 'The source checkout%s stays. Delete it yourself to finish.\n' \
        "${source_root:+ at $source_root}" >&2
      ;;
  esac
  return "$status"
}

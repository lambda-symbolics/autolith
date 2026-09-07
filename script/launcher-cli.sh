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

autolith_launcher_parse()
{
  local installation_kind=$1
  shift
  local argument command_seen=false take_value=false options_ended=false
  local -a original_arguments=("$@")

  recovery_requested=false
  from_source_requested=false
  update_requested=false
  data_requested=false
  run_job_requested=false
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
      --permissions|--image|-i|--localgroup-handoff|--id|--input|--output|\
      --generation|--status|--capsule|--original-argument|--workspace)
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
        if [[ $command_seen == false && $argument == data ]]; then
          data_requested=true
        fi
        if [[ $command_seen == false && $argument == run-job ]]; then
          run_job_requested=true
        fi
        command_seen=true
        remaining_arguments+=("$argument")
        ;;
    esac
  done

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

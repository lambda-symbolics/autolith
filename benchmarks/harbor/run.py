"""Generate and run the Autolith Harbor comparison job."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import shutil
import subprocess
import sys
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from harbor.models.job.config import JobConfig

from benchmarks.harbor.agents import (
    CODEX_VERSION,
    OPENROUTER_BASE_URL,
    PI_VERSION,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
ARTIFACT_DIRECTORY = REPOSITORY_ROOT / "var" / "harbor" / "artifacts"
DEFAULT_JOBS_DIRECTORY = REPOSITORY_ROOT / "var" / "harbor" / "jobs"
DEFAULT_MODEL = "openrouter/openai/gpt-5.6-luna"
DEFAULT_DATASET = "terminal-bench"
DEFAULT_DATASET_VERSION = "2.0"
_COMMON_REASONING_EFFORTS = frozenset({"minimal", "low", "medium", "high", "xhigh"})


def _positive_integer(value: str) -> int:
    parsed = int(value)
    if parsed < 1:
        raise argparse.ArgumentTypeError("value must be at least 1")
    return parsed


def _default_job_name() -> str:
    timestamp = datetime.now(UTC).strftime("%Y-%m-%d__%H-%M-%S")
    return f"autolith-codex-pi__{timestamp}"


def _latest_artifact(pattern: str) -> Path | None:
    candidates = [path for path in ARTIFACT_DIRECTORY.glob(pattern) if path.is_file()]
    if not candidates:
        return None
    return max(candidates, key=lambda path: path.stat().st_mtime)


def _resolve_artifact(
    explicit: Path | None,
    environment_name: str,
    pattern: str,
    placeholder: str,
    *,
    required: bool,
) -> str:
    candidate = explicit
    if candidate is None and os.environ.get(environment_name):
        candidate = Path(os.environ[environment_name]).expanduser()
    if candidate is None:
        candidate = _latest_artifact(pattern)
    if candidate is None:
        if required:
            raise ValueError(
                f"Set {environment_name} or build an artifact under {ARTIFACT_DIRECTORY}"
            )
        return placeholder
    candidate = candidate.resolve()
    if required and not candidate.is_file():
        raise ValueError(f"Artifact does not exist: {candidate}")
    return str(candidate)


def _validate_model(model_name: str) -> None:
    if not model_name.startswith("openrouter/"):
        raise ValueError("HARBOR_MODEL must begin with openrouter/")
    routed_name = model_name.removeprefix("openrouter/")
    if "/" not in routed_name:
        raise ValueError("HARBOR_MODEL must include the provider namespace")


def build_job_config(
    *,
    archive: str,
    relay_library: str,
    relay_cli: str,
    dataset: str = DEFAULT_DATASET,
    dataset_version: str = DEFAULT_DATASET_VERSION,
    model: str = DEFAULT_MODEL,
    n_tasks: int = 10,
    n_attempts: int = 1,
    n_concurrent_trials: int = 1,
    setup_timeout_seconds: int = 1800,
    jobs_directory: Path = DEFAULT_JOBS_DIRECTORY,
    job_name: str | None = None,
    install_only: bool = False,
    reasoning_effort: str | None = None,
) -> dict[str, Any]:
    """Build and validate the three-agent Harbor JobConfig."""
    _validate_model(model)
    if reasoning_effort and reasoning_effort not in _COMMON_REASONING_EFFORTS:
        choices = ", ".join(sorted(_COMMON_REASONING_EFFORTS))
        raise ValueError(f"HARBOR_REASONING_EFFORT must be one of: {choices}")

    key_template = (
        "${OPENROUTER_API_KEY:-}" if install_only else "${OPENROUTER_API_KEY}"
    )
    autolith_env = {
        "AUTOLITH_BENCHMARK_ARCHIVE": archive,
        "AUTOLITH_BENCHMARK_RELAY_LIBRARY": relay_library,
        "OPENROUTER_API_KEY": key_template,
    }
    codex_kwargs: dict[str, str] = {"version": CODEX_VERSION}
    pi_kwargs: dict[str, str] = {"version": PI_VERSION}
    if reasoning_effort:
        autolith_env["AUTOLITH_REASONING_EFFORT"] = reasoning_effort
        codex_kwargs["reasoning_effort"] = reasoning_effort
        pi_kwargs["thinking"] = reasoning_effort

    config: dict[str, Any] = {
        "job_name": job_name or _default_job_name(),
        "jobs_dir": str(jobs_directory.resolve()),
        "n_attempts": n_attempts,
        "install_only": install_only,
        "n_concurrent_trials": n_concurrent_trials,
        "environment": {
            "type": "docker",
            "delete": True,
        },
        "agents": [
            {
                "name": "autolith",
                "import_path": "benchmarks.harbor.agents:Autolith",
                "model_name": model,
                "override_setup_timeout_sec": setup_timeout_seconds,
                "include_logs": [
                    "nemo-relay/**",
                    "autolith.txt",
                    "autolith-result.sexp",
                    "autolith-state/**",
                ],
                "env": autolith_env,
            },
            {
                "name": "relay-codex",
                "import_path": "benchmarks.harbor.agents:RelayCodex",
                "model_name": model,
                "override_setup_timeout_sec": setup_timeout_seconds,
                "include_logs": [
                    "nemo-relay/**",
                    "codex.txt",
                    "sessions/**",
                ],
                "kwargs": codex_kwargs,
                "env": {
                    "OPENAI_API_KEY": key_template,
                    "OPENAI_BASE_URL": OPENROUTER_BASE_URL,
                    "NEMO_RELAY_BENCHMARK_CLI": relay_cli,
                },
            },
            {
                "name": "relay-pi",
                "import_path": "benchmarks.harbor.agents:RelayPi",
                "model_name": model,
                "override_setup_timeout_sec": setup_timeout_seconds,
                "include_logs": [
                    "nemo-relay/**",
                    "pi.txt",
                    "pi/**",
                ],
                "kwargs": pi_kwargs,
                "env": {
                    "OPENROUTER_API_KEY": key_template,
                    "NEMO_RELAY_BENCHMARK_CLI": relay_cli,
                },
            },
        ],
        "datasets": [
            {
                "name": dataset,
                "version": dataset_version,
                "n_tasks": n_tasks,
            }
        ],
    }
    JobConfig.model_validate(config)
    return config


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--print-config", action="store_true")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="write the config and print the Harbor command without running it",
    )
    parser.add_argument(
        "--install-only",
        action="store_true",
        help="run Harbor agent installation without model calls or verification",
    )
    parser.add_argument("--archive", type=Path)
    parser.add_argument("--relay-library", type=Path)
    parser.add_argument("--relay-cli", type=Path)
    parser.add_argument(
        "--dataset",
        default=os.environ.get("HARBOR_DATASET", DEFAULT_DATASET),
    )
    parser.add_argument(
        "--dataset-version",
        default=os.environ.get("HARBOR_DATASET_VERSION", DEFAULT_DATASET_VERSION),
    )
    parser.add_argument(
        "--model",
        default=os.environ.get("HARBOR_MODEL", DEFAULT_MODEL),
    )
    parser.add_argument(
        "--n-tasks",
        type=_positive_integer,
        default=os.environ.get("HARBOR_N_TASKS", "10"),
    )
    parser.add_argument(
        "--n-attempts",
        type=_positive_integer,
        default=os.environ.get("HARBOR_N_ATTEMPTS", "1"),
    )
    parser.add_argument(
        "--n-concurrent-trials",
        type=_positive_integer,
        default=os.environ.get("HARBOR_N_CONCURRENT_TRIALS", "1"),
    )
    parser.add_argument(
        "--setup-timeout-seconds",
        type=_positive_integer,
        default=os.environ.get("HARBOR_SETUP_TIMEOUT_SECONDS", "1800"),
    )
    parser.add_argument(
        "--reasoning-effort",
        default=os.environ.get("HARBOR_REASONING_EFFORT"),
    )
    parser.add_argument(
        "--jobs-directory",
        type=Path,
        default=Path(os.environ.get("HARBOR_JOBS_DIR", DEFAULT_JOBS_DIRECTORY)),
    )
    parser.add_argument("--job-name", default=os.environ.get("HARBOR_JOB_NAME"))
    parser.add_argument("--config-output", type=Path)
    return parser


def _harbor_command(config_path: Path) -> list[str]:
    executable = shutil.which("harbor")
    if executable is None:
        raise RuntimeError(
            "harbor is not installed in PATH; run this through the benchmark uv project"
        )
    return [executable, "run", "--config", str(config_path)]


def main(argv: list[str] | None = None) -> int:
    parser = _parser()
    args = parser.parse_args(argv)
    config_only = args.print_config or args.dry_run

    try:
        archive = _resolve_artifact(
            args.archive,
            "AUTOLITH_BENCHMARK_ARCHIVE",
            "autolith-*.tar.gz",
            "<AUTOLITH_BENCHMARK_ARCHIVE>",
            required=not config_only,
        )
        relay_library = _resolve_artifact(
            args.relay_library,
            "AUTOLITH_BENCHMARK_RELAY_LIBRARY",
            "libnemo_relay_ffi.so",
            "<AUTOLITH_BENCHMARK_RELAY_LIBRARY>",
            required=not config_only,
        )
        relay_cli = _resolve_artifact(
            args.relay_cli,
            "NEMO_RELAY_BENCHMARK_CLI",
            "nemo-relay",
            "<NEMO_RELAY_BENCHMARK_CLI>",
            required=not config_only,
        )
        if (
            not config_only
            and not args.install_only
            and not os.environ.get("OPENROUTER_API_KEY")
        ):
            raise ValueError("OPENROUTER_API_KEY is required")
        config = build_job_config(
            archive=archive,
            relay_library=relay_library,
            relay_cli=relay_cli,
            dataset=args.dataset,
            dataset_version=args.dataset_version,
            model=args.model,
            n_tasks=args.n_tasks,
            n_attempts=args.n_attempts,
            n_concurrent_trials=args.n_concurrent_trials,
            setup_timeout_seconds=args.setup_timeout_seconds,
            jobs_directory=args.jobs_directory,
            job_name=args.job_name,
            install_only=args.install_only,
            reasoning_effort=args.reasoning_effort,
        )
    except ValueError as error:
        parser.error(str(error))

    serialized = json.dumps(config, indent=2) + "\n"
    if args.print_config:
        sys.stdout.write(serialized)
        return 0

    config_path = args.config_output
    if config_path is None:
        config_path = (
            REPOSITORY_ROOT
            / "var"
            / "harbor"
            / "configs"
            / f"{config['job_name']}.json"
        )
    config_path = config_path.expanduser().resolve()
    config_path.parent.mkdir(parents=True, exist_ok=True)
    config_path.write_text(serialized)

    try:
        command = _harbor_command(config_path)
    except RuntimeError as error:
        parser.error(str(error))
    if args.dry_run:
        print(" ".join(shlex.quote(argument) for argument in command))
        print(f"Config: {config_path}")
        return 0

    child_env = os.environ.copy()
    existing_pythonpath = child_env.get("PYTHONPATH")
    child_env["PYTHONPATH"] = (
        str(REPOSITORY_ROOT)
        if not existing_pythonpath
        else f"{REPOSITORY_ROOT}{os.pathsep}{existing_pythonpath}"
    )
    print(f"Config: {config_path}")
    return subprocess.run(command, env=child_env, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())

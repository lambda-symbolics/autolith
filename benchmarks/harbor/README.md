# Harbor agent comparison

This project runs the same Harbor tasks and OpenRouter model through Autolith,
Codex, and Pi. Each agent emits NeMo Relay ATOF events and ATIF trajectories
under its Harbor trial logs.

## Pinned inputs

| Component | Revision or version |
| --- | --- |
| [Harbor](https://github.com/harbor-framework/harbor) | `c29f416af4b02ef593d6874f88b59d38bf164646` |
| [NeMo Relay](https://github.com/NVIDIA/NeMo-Relay) CLI source | `ba60230bb3042136b580ad24d36c1f472c46d112` |
| NeMo Relay CLI | `0.9.0` |
| Autolith NeMo Relay FFI source | `5a2381e4234024309f20262a091d1d6ab088b655` |
| Codex CLI | `0.149.0` |
| Pi coding agent | `0.84.2` |

Harbor and Relay behavior was inspected at the commits above. Autolith embeds
the older pinned Relay FFI commit. Codex and Pi use the separate Relay 0.9.0
CLI because Pi support is not present in Autolith's FFI revision.
The Relay CLI is built from the inspected source commit instead of depending on
a separately published binary.

## Prepare the Python environment

Run commands from the Autolith repository root:

```sh
uv sync --project benchmarks/harbor --locked
```

Run the focused adapter checks with:

```sh
uv run --project benchmarks/harbor \
  python -m pytest benchmarks/harbor/tests
```

## Build the benchmark artifacts

Commit and push the branch before building. The builder clones the exact remote
branch HEAD inside an Ubuntu 20.04 container, packages Autolith, builds the
pinned NeMo Relay FFI, builds a static Relay 0.9.0 CLI, and writes all three
artifacts under `var/harbor/artifacts`.

```sh
./benchmarks/harbor/build-autolith
```

The benchmark artifacts target `linux/amd64`, matching the default Harbor task
container platform.

The runner discovers the newest archive, `libnemo_relay_ffi.so`, and
`nemo-relay` in the artifact directory. Explicit paths take precedence:

```sh
export AUTOLITH_BENCHMARK_ARCHIVE=$PWD/var/harbor/artifacts/autolith-VERSION-x86_64-linux.tar.gz
export AUTOLITH_BENCHMARK_RELAY_LIBRARY=$PWD/var/harbor/artifacts/libnemo_relay_ffi.so
export NEMO_RELAY_BENCHMARK_CLI=$PWD/var/harbor/artifacts/nemo-relay
```

## Inspect the generated job

Config-only modes do not require artifacts or credentials:

```sh
uv run --project benchmarks/harbor \
  python -m benchmarks.harbor.run --print-config

uv run --project benchmarks/harbor \
  python -m benchmarks.harbor.run --dry-run
```

`--dry-run` writes the generated JSON under `var/harbor/configs` and prints the
Harbor command without executing it.

## Smoke-test agent installation

This installs all three agents and Relay integrations without model calls:

```sh
uv run --project benchmarks/harbor \
  python -m benchmarks.harbor.run --install-only --n-tasks 1
```

All three built artifacts are required. `OPENROUTER_API_KEY` is not required
for this mode.

## Run the comparison

```sh
export OPENROUTER_API_KEY=...
uv run --project benchmarks/harbor \
  python -m benchmarks.harbor.run
```

Defaults:

- Dataset: `terminal-bench@2.0`
- Model: `openrouter/openai/gpt-5.6-luna`
- Tasks: `10`
- Attempts per agent: `1`
- Concurrent trials: `1`
- Jobs directory: `var/harbor/jobs`

The model override must retain the OpenRouter prefix, for example:

```sh
HARBOR_MODEL=openrouter/openai/gpt-5.6-luna \
HARBOR_N_TASKS=25 \
HARBOR_N_CONCURRENT_TRIALS=3 \
uv run --project benchmarks/harbor \
  python -m benchmarks.harbor.run
```

Optional `HARBOR_REASONING_EFFORT` values shared by all three adapters are
`minimal`, `low`, `medium`, `high`, and `xhigh`.

## Model routing

The Harbor model name is transformed only where each tool requires it:

- Autolith receives `AUTOLITH_MODEL=openai/gpt-5.6-luna` and
  `OPENROUTER_API_KEY`.
- Codex receives `--model openai/gpt-5.6-luna`, `OPENAI_API_KEY`, and
  `OPENAI_BASE_URL=https://openrouter.ai/api/v1`.
- Pi receives `--provider openrouter --model openai/gpt-5.6-luna` and
  `OPENROUTER_API_KEY`.

The Codex subclass preserves the provider namespace that Harbor's built-in
adapter otherwise removes. The Pi adapter does not set `OPENROUTER_BASE_URL` or
`PI_CODING_AGENT_DIR`, so Relay's installed Pi extension remains discoverable.

## Trial logs

Each trial includes:

- `nemo-relay/events.jsonl` with ATOF events
- `nemo-relay/trajectory-*.json` with ATIF trajectories
- Native Codex or Pi output and session logs
- Autolith output, run-job result, and state logs

Harbor synchronizes these files into the corresponding job trial directory.

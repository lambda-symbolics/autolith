"""Harbor agents for comparing Autolith, Codex, and Pi with NeMo Relay."""

from __future__ import annotations

import json
import shlex
from collections.abc import Mapping
from pathlib import Path, PurePosixPath
from typing import Any, override

import toml

from harbor.agents.capabilities import AgentCapabilities
from harbor.agents.installed.base import BaseInstalledAgent
from harbor.agents.installed.codex import Codex
from harbor.agents.installed.pi import Pi
from harbor.agents.model_connection import ModelConnectionSpec
from harbor.environments.base import BaseEnvironment
from harbor.models.agent.context import AgentContext


HARBOR_SOURCE_COMMIT = "c29f416af4b02ef593d6874f88b59d38bf164646"
RELAY_SOURCE_COMMIT = "ba60230bb3042136b580ad24d36c1f472c46d112"
AUTOLITH_RELAY_SOURCE_COMMIT = "5a2381e4234024309f20262a091d1d6ab088b655"
RELAY_VERSION = "0.9.0"
CODEX_VERSION = "0.149.0"
PI_VERSION = "0.84.2"
OPENROUTER_BASE_URL = "https://openrouter.ai/api/v1"

_AUTOLITH_ARCHIVE_ENV = "AUTOLITH_BENCHMARK_ARCHIVE"
_AUTOLITH_RELAY_LIBRARY_ENV = "AUTOLITH_BENCHMARK_RELAY_LIBRARY"
_RELAY_CLI_ENV = "NEMO_RELAY_BENCHMARK_CLI"
_AUTOLITH_ROOT = PurePosixPath("/opt/autolith-benchmark")
_AUTOLITH_RELAY_LIBRARY = _AUTOLITH_ROOT / "lib" / "libnemo_relay_ffi.so"
_AUTOLITH_JOB_INPUT = PurePosixPath("/tmp/autolith-benchmark-job.sexp")
_AUTOLITH_JOB_OUTPUT = PurePosixPath("/logs/agent/autolith-result.sexp")
_AUTOLITH_OUTPUT = PurePosixPath("/logs/agent/autolith.txt")
_RELAY_CONFIG_DIRECTORY = PurePosixPath("/tmp/harbor-nemo-relay")
_RELAY_CONFIG_PATH = _RELAY_CONFIG_DIRECTORY / "config.toml"
_RELAY_PLUGIN_CONFIG_PATH = _RELAY_CONFIG_DIRECTORY / "plugins.toml"
_RELAY_LOG_DIRECTORY = PurePosixPath("/logs/agent/nemo-relay")
_RELAY_BINARY = "/usr/local/bin/nemo-relay"
_CODEX_RUN_MARKER = "codex exec "
_PI_RUN_MARKER = "pi --print "


def relay_base_config() -> str:
    """Return the shared NeMo Relay CLI configuration."""
    return toml.dumps(
        {
            "upstream": {"openai_base_url": OPENROUTER_BASE_URL},
            "agents": {
                "codex": {"command": "codex"},
                "pi": {"command": "pi"},
            },
        }
    )


def relay_plugin_config(agent_name: str, model_name: str) -> str:
    """Return the observability PluginConfig used by all benchmark agents."""
    return toml.dumps(
        {
            "version": 1,
            "components": [
                {
                    "kind": "observability",
                    "enabled": True,
                    "config": {
                        "version": 4,
                        "enable_full_payloads": True,
                        "atof": {
                            "enabled": True,
                            "sinks": [
                                {
                                    "type": "file",
                                    "output_directory": _RELAY_LOG_DIRECTORY.as_posix(),
                                    "filename": "events.jsonl",
                                    "mode": "append",
                                }
                            ],
                        },
                        "atif": {
                            "enabled": True,
                            "agent_name": agent_name,
                            "model_name": model_name,
                            "output_directory": _RELAY_LOG_DIRECTORY.as_posix(),
                            "filename_template": "trajectory-{session_id}.json",
                        },
                    },
                }
            ],
        }
    )


def lisp_string(value: str) -> str:
    """Serialize VALUE as a portable Common Lisp string literal."""
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def autolith_job_request(prompt: str, job_id: str) -> str:
    """Return a data-only Autolith run-job request for PROMPT."""
    return (
        "(:autolith-job\n"
        " :version 1\n"
        f" :id {lisp_string(job_id)}\n"
        ' :role "task"\n'
        f" :prompt {lisp_string(prompt)}\n"
        " :input nil\n"
        " :output-contract\n"
        " (:type :object\n"
        '  :properties (("summary" (:type :string)))\n'
        '  :required ("summary")\n'
        "  :additional-properties nil)\n"
        " :timeout-seconds 86400)\n"
    )


def _openrouter_model_name(model_name: str | None) -> str:
    """Remove Harbor's OpenRouter provider prefix from MODEL_NAME."""
    if not model_name or not model_name.startswith("openrouter/"):
        raise ValueError("Model name must use the openrouter/provider/model format")
    routed_model = model_name.removeprefix("openrouter/")
    if "/" not in routed_model:
        raise ValueError("OpenRouter model name must include its provider namespace")
    return routed_model


def _relay_session_metadata(agent: BaseInstalledAgent) -> str:
    """Return stable Harbor trial identity as compact JSON."""
    metadata: dict[str, str] = {}
    if agent.context_id is not None:
        metadata["context_id"] = str(agent.context_id)
    if agent.session_id is not None:
        metadata["session_id"] = agent.session_id
    return json.dumps(metadata, separators=(",", ":"), sort_keys=True)


def _relay_launch_prefix(agent_name: str, metadata_json: str) -> str:
    """Return the Relay wrapper prefix up to the launched agent arguments."""
    return (
        f"{_RELAY_BINARY} run "
        f"--agent {shlex.quote(agent_name)} "
        f"--config {shlex.quote(_RELAY_CONFIG_PATH.as_posix())} "
        f"--session-metadata {shlex.quote(metadata_json)} -- "
    )


async def _prepare_relay_config(
    agent: BaseInstalledAgent,
    environment: BaseEnvironment,
    *,
    agent_name: str,
    model_name: str,
    include_base_config: bool,
) -> None:
    """Upload one trial's Relay configuration and create its log directory."""
    await agent.exec_as_agent(
        environment,
        command=(
            f"mkdir -p {shlex.quote(_RELAY_CONFIG_DIRECTORY.as_posix())} "
            f"{shlex.quote(_RELAY_LOG_DIRECTORY.as_posix())}"
        ),
    )
    if include_base_config:
        await agent._upload_config_text(
            environment,
            content=relay_base_config(),
            remote_path=_RELAY_CONFIG_PATH.as_posix(),
            filename="config.toml",
        )
    await agent._upload_config_text(
        environment,
        content=relay_plugin_config(agent_name, model_name),
        remote_path=_RELAY_PLUGIN_CONFIG_PATH.as_posix(),
        filename="plugins.toml",
    )


def _local_artifact_path(value: str | None, name: str) -> Path:
    """Return one required host artifact path."""
    if not value:
        raise ValueError(f"{name} is required")
    path = Path(value).expanduser()
    if not path.is_file():
        raise ValueError(f"{name} does not name a file: {path}")
    return path


async def _install_relay_cli(
    agent: BaseInstalledAgent,
    environment: BaseEnvironment,
    *,
    install_pi_extension: bool,
) -> None:
    """Install the benchmark-built Relay CLI and optionally its Pi extension."""
    local_path = _local_artifact_path(agent._get_env(_RELAY_CLI_ENV), _RELAY_CLI_ENV)

    remote_path = "/tmp/nemo-relay-benchmark"
    await environment.upload_file(local_path, remote_path)
    await agent.exec_as_root(
        environment,
        command=(
            "mkdir -p /usr/local/bin && "
            f"cp {shlex.quote(remote_path)} {_RELAY_BINARY} && "
            f"chmod 755 {_RELAY_BINARY} && "
            f"rm -f {shlex.quote(remote_path)}"
        ),
    )
    extension_command = (
        f" && {_RELAY_BINARY} install pi" if install_pi_extension else ""
    )
    await agent.exec_as_agent(
        environment,
        cwd="/tmp",
        command=(
            f'test "$({_RELAY_BINARY} --version)" = '
            f"{shlex.quote(f'nemo-relay {RELAY_VERSION}')}"
            f"{extension_command}"
        ),
    )


class Autolith(BaseInstalledAgent):
    """Run a packaged Autolith build through its headless run-job boundary."""

    capabilities = AgentCapabilities()
    MODEL_CONNECTION = ModelConnectionSpec(passthrough=True)

    @staticmethod
    @override
    def name() -> str:
        return "autolith"

    @classmethod
    @override
    def preflight(
        cls,
        kwargs: dict[str, Any] | None = None,
        env: Mapping[str, str] | None = None,
    ) -> None:
        """Validate the two host artifacts before Harbor queues trials."""
        super().preflight(kwargs=kwargs, env=env)
        resolved_env = env or {}
        for name in (_AUTOLITH_ARCHIVE_ENV, _AUTOLITH_RELAY_LIBRARY_ENV):
            _local_artifact_path(resolved_env.get(name), name)

    def _artifact_path(self, name: str) -> Path:
        return _local_artifact_path(self._get_env(name), name)

    @override
    async def install(self, environment: BaseEnvironment) -> None:
        await self.ensure_system_dependencies(environment, ("tar", "coreutils"))
        archive = self._artifact_path(_AUTOLITH_ARCHIVE_ENV)
        relay_library = self._artifact_path(_AUTOLITH_RELAY_LIBRARY_ENV)
        remote_archive = "/tmp/autolith-benchmark.tar.gz"
        remote_library = "/tmp/libnemo_relay_ffi.so"

        await self.exec_as_root(
            environment,
            command=(
                f"rm -rf {shlex.quote(_AUTOLITH_ROOT.as_posix())} && "
                f"mkdir -p {shlex.quote((_AUTOLITH_ROOT / 'lib').as_posix())}"
            ),
        )
        await environment.upload_file(archive, remote_archive)
        await environment.upload_file(relay_library, remote_library)
        await self.exec_as_root(
            environment,
            command=(
                f"tar -xzf {shlex.quote(remote_archive)} --strip-components=1 "
                f"-C {shlex.quote(_AUTOLITH_ROOT.as_posix())} && "
                f"cp {shlex.quote(remote_library)} "
                f"{shlex.quote(_AUTOLITH_RELAY_LIBRARY.as_posix())} && "
                f"chmod 644 {shlex.quote(_AUTOLITH_RELAY_LIBRARY.as_posix())} && "
                f"ln -sfn {shlex.quote((_AUTOLITH_ROOT / 'bin' / 'autolith').as_posix())} "
                "/usr/local/bin/autolith && "
                f"test -x {shlex.quote((_AUTOLITH_ROOT / 'bin' / 'autolith').as_posix())} && "
                f"rm -f {shlex.quote(remote_archive)} {shlex.quote(remote_library)}"
            ),
        )

    @override
    async def run(
        self,
        instruction: str,
        environment: BaseEnvironment,
        context: AgentContext,
    ) -> None:
        model_name = _openrouter_model_name(self.model_name)
        await _prepare_relay_config(
            self,
            environment,
            agent_name=self.name(),
            model_name=self.model_name or model_name,
            include_base_config=False,
        )

        job_id = self.session_id or str(self.context_id or "harbor-autolith")
        await self._upload_config_text(
            environment,
            content=autolith_job_request(instruction, job_id),
            remote_path=_AUTOLITH_JOB_INPUT.as_posix(),
            filename="job.sexp",
        )

        xdg_root = PurePosixPath("/tmp/autolith-xdg")
        env = dict(self.model_connection.env)
        env.update(
            {
                "AUTOLITH_MODEL": model_name,
                "AUTOLITH_RELAY": "on",
                "AUTOLITH_RELAY_CONFIG": _RELAY_PLUGIN_CONFIG_PATH.as_posix(),
                "AUTOLITH_RELAY_LIBRARY": _AUTOLITH_RELAY_LIBRARY.as_posix(),
                "XDG_CONFIG_HOME": (xdg_root / "config").as_posix(),
                "XDG_DATA_HOME": (xdg_root / "data").as_posix(),
                "XDG_STATE_HOME": "/logs/agent/autolith-state",
                "XDG_CACHE_HOME": (xdg_root / "cache").as_posix(),
            }
        )
        reasoning_effort = self._get_env("AUTOLITH_REASONING_EFFORT")
        if reasoning_effort:
            env["AUTOLITH_REASONING_EFFORT"] = reasoning_effort

        await self.exec_as_agent(
            environment,
            command=(
                f"mkdir -p {shlex.quote((xdg_root / 'config').as_posix())} "
                f"{shlex.quote((xdg_root / 'data').as_posix())} "
                f"{shlex.quote((xdg_root / 'cache').as_posix())} "
                "/logs/agent/autolith-state && "
                "autolith --permissions full run-job "
                f"--input {shlex.quote(_AUTOLITH_JOB_INPUT.as_posix())} "
                f"--output {shlex.quote(_AUTOLITH_JOB_OUTPUT.as_posix())} "
                "2>&1 </dev/null | "
                f"stdbuf -oL tee {shlex.quote(_AUTOLITH_OUTPUT.as_posix())}"
            ),
            env=env,
        )


class RelayCodex(Codex):
    """Harbor's Codex agent wrapped by NeMo Relay."""

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        kwargs.setdefault("version", CODEX_VERSION)
        super().__init__(*args, **kwargs)
        self._relay_active = False
        self._relay_metadata_json = "{}"

    @classmethod
    @override
    def preflight(
        cls,
        kwargs: dict[str, Any] | None = None,
        env: Mapping[str, str] | None = None,
    ) -> None:
        """Validate the Relay CLI artifact before Harbor queues trials."""
        super().preflight(kwargs=kwargs, env=env)
        _local_artifact_path((env or {}).get(_RELAY_CLI_ENV), _RELAY_CLI_ENV)

    @override
    async def install(self, environment: BaseEnvironment) -> None:
        await super().install(environment)
        await _install_relay_cli(
            self,
            environment,
            install_pi_extension=False,
        )

    def _wrap_relay_command(self, command: str) -> str:
        if not self._relay_active or _CODEX_RUN_MARKER not in command:
            return command
        wrapped = command.replace(
            _CODEX_RUN_MARKER,
            _relay_launch_prefix("codex", self._relay_metadata_json) + "exec ",
            1,
        )
        leaf_model = (self.model_name or "").split("/")[-1]
        routed_model = _openrouter_model_name(self.model_name)
        return wrapped.replace(
            f"--model {leaf_model} ",
            f"--model {routed_model} ",
            1,
        )

    @override
    async def exec_as_agent(
        self,
        environment: BaseEnvironment,
        command: str,
        env: dict[str, str] | None = None,
        cwd: str | None = None,
        timeout_sec: int | None = None,
    ) -> Any:
        return await super().exec_as_agent(
            environment,
            command=self._wrap_relay_command(command),
            env=env,
            cwd=cwd,
            timeout_sec=timeout_sec,
        )

    @override
    async def run(
        self,
        instruction: str,
        environment: BaseEnvironment,
        context: AgentContext,
    ) -> None:
        await _prepare_relay_config(
            self,
            environment,
            agent_name="codex",
            model_name=self.model_name or "unknown",
            include_base_config=True,
        )
        self._relay_metadata_json = _relay_session_metadata(self)
        self._relay_active = True
        try:
            await super().run(instruction, environment, context)
        finally:
            self._relay_active = False


class RelayPi(Pi):
    """Harbor's Pi agent wrapped by NeMo Relay."""

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        kwargs.setdefault("version", PI_VERSION)
        super().__init__(*args, **kwargs)
        self._relay_active = False
        self._relay_metadata_json = "{}"

    @classmethod
    @override
    def preflight(
        cls,
        kwargs: dict[str, Any] | None = None,
        env: Mapping[str, str] | None = None,
    ) -> None:
        """Validate the Relay CLI artifact before Harbor queues trials."""
        super().preflight(kwargs=kwargs, env=env)
        _local_artifact_path((env or {}).get(_RELAY_CLI_ENV), _RELAY_CLI_ENV)

    @override
    async def install(self, environment: BaseEnvironment) -> None:
        await super().install(environment)
        await _install_relay_cli(
            self,
            environment,
            install_pi_extension=True,
        )

    def _wrap_relay_command(self, command: str) -> str:
        if not self._relay_active or _PI_RUN_MARKER not in command:
            return command
        return command.replace(
            _PI_RUN_MARKER,
            _relay_launch_prefix("pi", self._relay_metadata_json) + "--print ",
            1,
        )

    @override
    async def exec_as_agent(
        self,
        environment: BaseEnvironment,
        command: str,
        env: dict[str, str] | None = None,
        cwd: str | None = None,
        timeout_sec: int | None = None,
    ) -> Any:
        return await super().exec_as_agent(
            environment,
            command=self._wrap_relay_command(command),
            env=env,
            cwd=cwd,
            timeout_sec=timeout_sec,
        )

    @override
    async def run(
        self,
        instruction: str,
        environment: BaseEnvironment,
        context: AgentContext,
    ) -> None:
        await _prepare_relay_config(
            self,
            environment,
            agent_name="pi",
            model_name=self.model_name or "unknown",
            include_base_config=True,
        )
        self._relay_metadata_json = _relay_session_metadata(self)
        self._relay_active = True
        try:
            await super().run(instruction, environment, context)
        finally:
            self._relay_active = False

from __future__ import annotations

import asyncio
import shutil
import subprocess
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from uuid import UUID

import pytest
from harbor.agents.factory import AgentFactory
from harbor.models.trial.config import AgentConfig

from benchmarks.harbor.agents import (
    OPENROUTER_BASE_URL,
    RelayCodex,
    RelayPi,
    _install_relay_cli,
    autolith_job_request,
    lisp_string,
    relay_base_config,
    relay_plugin_config,
)
from benchmarks.harbor.run import build_job_config


MODEL = "openrouter/openai/gpt-5.6-luna"


@dataclass
class FakeExecResult:
    return_code: int = 0
    stdout: str = ""
    stderr: str = ""


class FakeEnvironment:
    default_user = "agent"

    def __init__(self) -> None:
        self.commands: list[dict[str, Any]] = []
        self.uploads: list[tuple[Path, str]] = []

    async def exec(
        self,
        *,
        command: str,
        user: str | None = None,
        env: dict[str, str] | None = None,
        cwd: str | None = None,
        timeout_sec: int | None = None,
    ) -> FakeExecResult:
        self.commands.append(
            {
                "command": command,
                "user": user,
                "env": env,
                "cwd": cwd,
                "timeout_sec": timeout_sec,
            }
        )
        return FakeExecResult()

    async def upload_file(self, local_path: Path, remote_path: str) -> None:
        self.uploads.append((local_path, remote_path))


@pytest.fixture
def codex(tmp_path: Path) -> RelayCodex:
    agent = RelayCodex(
        logs_dir=tmp_path,
        model_name=MODEL,
        extra_env={
            "OPENAI_API_KEY": "test-key",
            "OPENAI_BASE_URL": OPENROUTER_BASE_URL,
        },
    )
    agent.context_id = UUID("d639f898-67f7-4f99-8dc8-048349880cb9")
    agent.session_id = "trial__agent"
    agent._relay_metadata_json = (
        '{"context_id":"d639f898-67f7-4f99-8dc8-048349880cb9",'
        '"session_id":"trial__agent"}'
    )
    return agent


@pytest.fixture
def pi(tmp_path: Path) -> RelayPi:
    agent = RelayPi(
        logs_dir=tmp_path,
        model_name=MODEL,
        extra_env={"OPENROUTER_API_KEY": "test-key"},
    )
    agent.context_id = UUID("d639f898-67f7-4f99-8dc8-048349880cb9")
    agent.session_id = "trial__agent"
    agent._relay_metadata_json = (
        '{"context_id":"d639f898-67f7-4f99-8dc8-048349880cb9",'
        '"session_id":"trial__agent"}'
    )
    return agent


def test_relay_plugin_config_has_file_observability() -> None:
    config = tomllib.loads(relay_plugin_config("autolith", MODEL))
    assert config["version"] == 1
    component = config["components"][0]
    assert component["kind"] == "observability"
    assert component["enabled"] is True
    observability = component["config"]
    assert observability["version"] == 4
    assert observability["enable_full_payloads"] is True
    assert observability["atof"] == {
        "enabled": True,
        "sinks": [
            {
                "type": "file",
                "output_directory": "/logs/agent/nemo-relay",
                "filename": "events.jsonl",
                "mode": "append",
            }
        ],
    }
    assert observability["atif"] == {
        "enabled": True,
        "agent_name": "autolith",
        "model_name": MODEL,
        "output_directory": "/logs/agent/nemo-relay",
        "filename_template": "trajectory-{session_id}.json",
    }


def test_relay_base_config_routes_openai_through_openrouter() -> None:
    config = tomllib.loads(relay_base_config())
    assert config["upstream"]["openai_base_url"] == OPENROUTER_BASE_URL
    assert config["agents"] == {
        "codex": {"command": "codex"},
        "pi": {"command": "pi"},
    }


def test_codex_run_command_is_wrapped_once_and_restores_model_slug(
    codex: RelayCodex,
) -> None:
    environment = FakeEnvironment()
    codex._relay_active = True
    original = (
        ". ~/.nvm/nvm.sh; codex exec --dangerously-bypass-approvals-and-sandbox "
        "--model gpt-5.6-luna --json -- 'solve it'"
    )

    asyncio.run(codex.exec_as_agent(environment, command=original))
    wrapped = environment.commands[-1]["command"]
    assert wrapped.count("nemo-relay run") == 1
    assert "/usr/local/bin/nemo-relay run" in wrapped
    assert "--agent codex" in wrapped
    assert "-- exec --dangerously-bypass-approvals-and-sandbox" in wrapped
    assert "--model openai/gpt-5.6-luna" in wrapped
    assert "--model gpt-5.6-luna" not in wrapped

    asyncio.run(codex.exec_as_agent(environment, command=wrapped))
    assert environment.commands[-1]["command"].count("nemo-relay run") == 1


def test_codex_setup_command_is_not_wrapped(codex: RelayCodex) -> None:
    environment = FakeEnvironment()
    codex._relay_active = True
    asyncio.run(codex.exec_as_agent(environment, command="codex --version"))
    command = environment.commands[-1]["command"]
    assert "codex --version" in command
    assert "nemo-relay" not in command


def test_pi_run_command_is_wrapped_once(pi: RelayPi) -> None:
    environment = FakeEnvironment()
    pi._relay_active = True
    original = (
        ". ~/.nvm/nvm.sh; pi --print --mode json --provider openrouter "
        "--model openai/gpt-5.6-luna 'solve it'"
    )

    asyncio.run(pi.exec_as_agent(environment, command=original))
    wrapped = environment.commands[-1]["command"]
    assert wrapped.count("nemo-relay run") == 1
    assert "/usr/local/bin/nemo-relay run" in wrapped
    assert "--agent pi" in wrapped
    assert "-- --print --mode json" in wrapped
    assert "--provider openrouter" in wrapped
    assert "--model openai/gpt-5.6-luna" in wrapped

    asyncio.run(pi.exec_as_agent(environment, command=wrapped))
    assert environment.commands[-1]["command"].count("nemo-relay run") == 1


def test_pi_setup_command_is_not_wrapped(pi: RelayPi) -> None:
    environment = FakeEnvironment()
    pi._relay_active = True
    asyncio.run(pi.exec_as_agent(environment, command="pi --version"))
    command = environment.commands[-1]["command"]
    assert "pi --version" in command
    assert "nemo-relay" not in command


def test_lisp_string_uses_common_lisp_escaping() -> None:
    value = 'quote " backslash \\ and literal\nnewline'
    serialized = lisp_string(value)
    assert serialized == '"quote \\" backslash \\\\ and literal\nnewline"'
    assert "\\n" not in serialized
    assert "\n" in serialized


def test_relay_cli_install_uses_built_artifact(tmp_path: Path) -> None:
    relay_cli = tmp_path / "nemo-relay"
    relay_cli.touch()
    agent = RelayPi(
        logs_dir=tmp_path,
        model_name=MODEL,
        extra_env={
            "OPENROUTER_API_KEY": "test-key",
            "NEMO_RELAY_BENCHMARK_CLI": str(relay_cli),
        },
    )
    environment = FakeEnvironment()

    asyncio.run(_install_relay_cli(agent, environment, install_pi_extension=True))

    assert environment.uploads == [(relay_cli, "/tmp/nemo-relay-benchmark")]
    commands = [entry["command"] for entry in environment.commands]
    assert any(
        "cp /tmp/nemo-relay-benchmark /usr/local/bin/nemo-relay" in command
        for command in commands
    )
    assert any(
        "nemo-relay --version)\" = 'nemo-relay 0.9.0'" in command
        for command in commands
    )
    assert any(
        "/usr/local/bin/nemo-relay install pi" in command for command in commands
    )


def test_autolith_job_request_is_data_only_and_readable(tmp_path: Path) -> None:
    prompt = 'quote " backslash \\ newline\n#.(error "reader evaluation ran")'
    request = autolith_job_request(prompt, "harbor-job")
    request_path = tmp_path / "job.sexp"
    request_path.write_text(request)

    assert request.startswith("(:autolith-job\n")
    assert ":output-contract" in request
    assert "#.(error" in request

    sbcl = shutil.which("sbcl")
    if sbcl is None:
        pytest.skip("SBCL is required for the Common Lisp reader check")
    expression = (
        "(let ((*read-eval* nil)) "
        f"(with-open-file (stream {lisp_string(str(request_path))}) "
        "(let ((form (read stream nil :eof))) "
        "(assert (eq (first form) :autolith-job)) "
        "(assert (= (getf (rest form) :version) 1)) "
        f"(assert (equal (getf (rest form) :prompt) {lisp_string(prompt)})) "
        '(assert (equal (getf (rest form) :id) "harbor-job")) '
        "(assert (eq (read stream nil :eof) :eof)))))"
    )
    subprocess.run(
        [sbcl, "--noinform", "--non-interactive", "--eval", expression],
        check=True,
        capture_output=True,
        text=True,
    )


def test_job_config_uses_one_model_and_agent_specific_credentials(
    tmp_path: Path,
) -> None:
    config = build_job_config(
        archive=str(tmp_path / "autolith.tar.gz"),
        relay_library=str(tmp_path / "libnemo_relay_ffi.so"),
        relay_cli=str(tmp_path / "nemo-relay"),
        model=MODEL,
        job_name="test-job",
        jobs_directory=tmp_path / "jobs",
    )
    agents = {agent["name"]: agent for agent in config["agents"]}

    assert {agent["model_name"] for agent in config["agents"]} == {MODEL}
    assert agents["autolith"]["env"]["OPENROUTER_API_KEY"] == ("${OPENROUTER_API_KEY}")
    assert agents["relay-codex"]["env"] == {
        "OPENAI_API_KEY": "${OPENROUTER_API_KEY}",
        "OPENAI_BASE_URL": OPENROUTER_BASE_URL,
        "NEMO_RELAY_BENCHMARK_CLI": str(tmp_path / "nemo-relay"),
    }
    assert agents["relay-pi"]["env"] == {
        "OPENROUTER_API_KEY": "${OPENROUTER_API_KEY}",
        "NEMO_RELAY_BENCHMARK_CLI": str(tmp_path / "nemo-relay"),
    }
    assert "OPENROUTER_BASE_URL" not in agents["relay-pi"]["env"]


def test_harbor_factory_imports_and_preflights_custom_agents(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    archive = tmp_path / "autolith.tar.gz"
    relay_library = tmp_path / "libnemo_relay_ffi.so"
    relay_cli = tmp_path / "nemo-relay"
    archive.touch()
    relay_library.touch()
    relay_cli.touch()
    monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")
    config = build_job_config(
        archive=str(archive),
        relay_library=str(relay_library),
        relay_cli=str(relay_cli),
        model=MODEL,
        job_name="factory-test",
        jobs_directory=tmp_path / "jobs",
    )

    for agent_config in config["agents"]:
        AgentFactory.run_preflight(AgentConfig.model_validate(agent_config))

# Proposed parent-agent architecture

```mermaid
flowchart TB
    User["User / terminal"]
    Providers["Model providers"]
    Workspace["Approved workspace"]

    subgraph Parent["Trusted parent"]
        UI["Terminal and approvals"]
        Session["Session and request control"]
        Models["Model calls and credentials"]
        Storage["Private storage"]
        Supervisor["Access policy, launch and recovery"]

        UI <--> Session
        Session <--> Models
        Session <--> Storage
        Session --> Supervisor
    end

    subgraph Agent["Restricted agent"]
        Runtime["Reasoning and context"]
        Tools["Tools and Lisp workers"]
        Mutation["Live self-modification"]

        Runtime <--> Tools
        Runtime <--> Mutation
    end

    User <--> UI
    Session <-->|"Private Protobuf channel"| Runtime
    Models <--> Providers
    Supervisor -->|"Start and restrict"| Agent
    Tools <--> Workspace
```

The parent owns credentials, private storage, approvals, and policy enforcement.
The agent owns reasoning, tools, workers, and live changes.

This is the target architecture. Runtime integration is incomplete; legacy launch
is still the default. See [the protocol specification](parent-agent-protocol.org)
for the boundary contract and [the architecture guide](architecture.org) for the
current implementation.

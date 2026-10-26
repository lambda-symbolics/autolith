# Proposed parent-agent architecture

```mermaid
flowchart TB
    User["User / terminal"]

    subgraph Parent["Trusted parent process"]
        UI["Terminal I/O and approvals"]
        Owner["Session owner queue<br/>Turn and request lifecycle"]
        Writer["Bounded protocol writer"]
        Models["Model service<br/>Profiles, retries, cancellation, deadlines"]
        Auth["Credentials and accounts"]
        Transport["Provider HTTP and streaming"]
        Stores["Private storage<br/>Sessions, replay journal, records"]
        Policy["Access policy and grants"]
        Launcher["Process supervisor<br/>Launch, stop, restart, recovery"]

        UI <--> Owner
        Owner --> Writer
        Owner <--> Models
        Models --> Auth
        Models <--> Transport
        Owner <--> Stores
        Models <--> Stores
        Owner <--> Policy
        Policy --> Launcher
    end

    subgraph Agent["Restricted agent process"]
        Reader["Sole protocol reader"]
        Client["Parent-service client<br/>Correlated requests and events"]
        Runtime["Agent reasoning and context"]
        Tools["Tools and workspace operations"]
        Mutation["Live self-modification<br/>Journal, checks, replay"]
        Workers["Disposable Lisp workers"]

        Reader --> Client
        Client <--> Runtime
        Runtime <--> Tools
        Runtime <--> Mutation
        Tools <--> Workers
    end

    Providers["External model providers"]
    Workspace["Approved workspace"]
    Recovery["Pristine recovery image"]

    User <--> UI
    Writer -->|"Private framed Protobuf"| Reader
    Client -->|"Requests through sole parent reader"| Owner
    Transport <--> Providers
    Launcher -->|"Apply OS policy and start"| Agent
    Launcher --> Recovery
    Tools <--> Workspace
```

The parent owns credentials, private storage, approvals, and policy enforcement.
The agent owns reasoning, tools, workers, and live changes.

This is the target architecture. Runtime integration is incomplete; legacy launch
is still the default. See [the protocol specification](parent-agent-protocol.org)
for the boundary contract and [the architecture guide](architecture.org) for the
current implementation.

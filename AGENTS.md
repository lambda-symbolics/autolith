# Repository Guidelines

## Purpose and Sources of Truth

Autolith is a small, live, self-modifying Common Lisp agent. This file contains
its enduring architectural and repository policy. `docs/guide.org` documents
user-visible behavior, `docs/architecture.org` maps runtime and source
boundaries, and tracked source plus behavioral tests are executable truth. Do
not create another omnibus product specification.

Core principles:

- Treat the live Lisp image as the primary runtime while keeping source
  sufficient for a clean rebuild.
- Prefer Common Lisp and focused CLOS protocols over generated scripts,
  parallel type dispatch, duplicated data structures, or overlapping public
  interfaces.
- Keep self-modification explicit, auditable, reconstructible, and confined to
  a small operation set.
- Preserve durable conversations, user state, working generations, and a
  pristine recovery path.
- Use process boundaries for reliability and accidental-damage containment,
  never as a hostile-code security claim.
- Keep platform-specific behavior behind narrow adapters.
- Treat migrations and compatibility readers as temporary release machinery.
  Give them an explicit removal boundary instead of accumulating them forever.

Do not leave TODOs, FIXMEs, stubs, placeholders, or knowingly partial
implementations. If a requirement is genuinely too broad or conflicts with
another requirement, stop and ask.

Supported source-development targets are Linux x86-64, macOS arm64, and
Windows x86-64 on SBCL with a terminal interface, one primary agent, and no
claim of hostile-code sandboxing. Nix builds support Linux x86-64 and macOS
musl, macOS x86-64 and arm64, FreeBSD x86-64, NetBSD x86-64, OpenBSD x86-64,
    and Windows x86-64.

Host differences live behind the platform protocol in `src/core/platform.lisp`
with one adapter per host family; `#+win32` appears only in those adapters, in
`autolith.asd` feature expressions, and in the standalone scripts that run
before the system loads. Windows supports supervised detached sessions and
exact-heap restart checkpoints, while withholding fork-based image saves and
filesystem sockets. Windows commands use the native AppContainer
helper with explicit workspace, private temporary, and read-only tool scopes. Checks
that depend on POSIX facilities go through the test fixture protocol in
`tests/test-support.lisp` and are recorded as skipped where the host lacks
them. Sources and tests alike change the environment through
`platform-setenv` and `platform-unsetenv`, resolve links through
`platform-truename`, and remove trees through `platform-delete-directory-tree`,
never through `sb-posix`, `truename`, or `uiop:delete-directory-tree`
directly: Windows keeps two environments, leaves links unresolved in
`truename`, and refuses to delete read-only files. Names carrying pathname
metacharacters or backslashes belong only behind the `:wildcard-file-names`
fixture.

## Upstream References

When a change depends on established behavior in another agent, inspect a
current upstream checkout outside this Git worktree. Keep reference checkouts
read-only, record the repository URL and exact inspected commit in the review
notes, and refresh the checkout before making claims about current behavior.
References are research inputs, not Autolith dependencies; do not edit them or
copy their architecture wholesale.

## Architectural Guardrails

- Keep the codebase small and prefer Common Lisp, ASDF, and UIOP for
  filesystem, process, networking, and build work.
- Do not generate ad hoc Python or shell files unless a non-Lisp dependency
  genuinely requires them.
- Keep the stable launcher, mutable active agent, disposable Lisp worker, and
  pristine recovery image as distinct components.
- All `lisp.*` operations run in a separate disposable SBCL worker. They do
  not share heap state with the active agent.
- `self.*` operations act on the active image. Normal `self.*` tools must not
  modify the stable launcher or pristine recovery artifacts.
- Treat process separation as an accidental-damage and reliability boundary,
  never as a security sandbox.
- Tool paths outside the workspace and source roots are a permission question,
  never a hard refusal. Resolve every tool path through `workspace-tool-path`,
  which asks command authorization for `TOOL -- PATH`, so full access never
  refuses. Only the command sandbox, which cannot run elsewhere, and callers
  that already authorized a path use `workspace-tool-confined-path`.
- Keep provider authentication and transport behind a replaceable interface.
  Do not launch or bundle the Codex CLI to implement subscription access.
- Keep credentials out of saved cores, conversation files, and Git.
- Gray streams subclass and specialize the portable `trivial-gray-streams`
  classes and generic functions, never an implementation's own Gray package
  such as `sb-gray`, so stream code moves into portable libraries unchanged.
- The qlfile is the only place a runtime git dependency is pinned.
  `./script/bootstrap` writes `qlfile.lock`, and `nix/package.nix` reads that
  lock for every git source, so never copy a ref or hash into Nix. A new git
  dependency needs only its build metadata (systems and `lispLibs`) in
  `nix/package.nix`, and Nix evaluation fails until that entry exists.
- Development-only tools are dev-dependencies, like Cargo's: `checks/qlfile`
  lists them, `checks/autolith-check-tools.asd` names them for qlot, and
  `./script/check` installs them on first use into Autolith's cache, outside
  the source tree, and makes them visible only to its own processes. Never add
  them to the root qlfile, the Nix package, bootstrap, or release archives.
  cheque, the shared Lambda Symbolics source checker, is deliberately
  unpinned: `./script/check` refreshes it to its newest commit at most hourly,
  while the lock keeps pinning every other tool.
- `VERSION` is the single release-version source. ASDF, runtime reporting, and
  Nix derive their versions from it. For a release, update that file and add
  the release notes.
- Source is authoritative for clean rebuilds. Saved cores preserve exact
  working live states, but never replace tracked source.
- Dumped images are the normal way to start Autolith. Fresh Autolith
  processes, including pristine workers and test children, boot a saved core
  whose recorded provenance matches the source; loading FASLs is only the
  fallback when no matching core exists. Do not add launch paths or test
  fixtures that load the whole system in every child.
- Operate on Lisp forms for durable source edits. Do not use blind regular
  expression replacement of source code.
- Keep platform-specific behavior behind narrow adapters.

For a durable live mutation, preserve the specified order:

1. Journal the intended mutation.
2. Compile and install it in the active image.
3. Run relevant checks.
4. Publish the complete reconstructible state as an immutable private image
   commit, clean-process probe its replay script, and retain it in private Git
   history.
5. Atomically select the private commit and mark the journal entry durable.

Autolith's durable live mutations never patch its own tracked repository;
selected private replay scripts load after the tracked system. The ordinary
workspace tools may develop the tracked repository, including the launcher
and recovery sources, exactly like any other project.

Conversation files and mutation journals are append-only sequences of
top-level readable forms. Bind `*read-eval*` to `nil` when reading persisted
conversation data, keep that data portable, and tolerate an incomplete final
form after a crash. Publish checkpoints and manifests atomically. Recovery
must remain possible without loading a damaged active core.

## Package Policy

Use one project package, `#:autolith`. Do not create scoped, subsystem, feature,
file-local, or test packages unless the user explicitly changes this policy.
Split the implementation into focused files while keeping those files in the
single project package. Runtime component boundaries are not package
boundaries.

- Define the package once and use `(in-package #:autolith)` in project source.
- `:use` only `#:cl`.
- Import individual third-party symbols with `:import-from`; do not wholesale
  `:use` third-party packages.
- Do not introduce packages merely to express internal architecture. Express
  those boundaries with files, functions, CLOS protocols, and clear naming.

## Code Organization

- Keep boot files limited to startup, shutdown, configuration, and component
  wiring. Put substantive behavior in focused implementation files.
- Split code by coherent responsibility. Do not create `misc`, `helpers`, or
  a growing multi-purpose `util` dumping ground.
- Keep utility files single-purpose.
- Preserve public entry points when splitting code, and move one coherent
  concern at a time.
- Group definitions by functionality, not alphabetically.
- Within a file, prefer this order where applicable: types and classes,
  generic functions, methods, public functions, private functions, and
  conditions.

Prefer simple, established, readable solutions. Keep business logic above
low-level mechanics. Prefer small, documented functions even when a helper is
used only once. Use CLOS when it provides a useful semantic protocol instead
of repeating type or state dispatch in `cond` trees.

## Common Lisp Style

### Naming

- Use kebab-case symbols without abbreviations.
- Do not use `defconstant` or `define-constant`.
- Use `defparameter` for reloadable policy and defaults.
- Use `defvar` only for state or identity intended to survive reload.
- Special variables use surrounding asterisks, for example `*active-image*`.
- Functions use clear, entity-prefixed names where an entity exists, for
  example `generation-find` and `conversation-append-record`.
- Internal functions use a double hyphen after the entity or subsystem name,
  for example `generation--validate-manifest`.
- Predicates use a `-p` suffix.
- Conversion functions use `->`, for example `record->message`.
- Classes are singular lowercase names. Accessors are prefixed with the
  entity name.
- Functions and methods with four or more parameters use keyword arguments.
  Do not introduce new positional lambda lists with four or more parameters.
- Prefer `first` and `rest` over `car` and `cdr` in application code.
- Quote keywords used as data. Whenever a keyword value directly follows a
  keyword-argument name, in a call, an evaluated plist, or a `defclass`
  `:initform`, quote the value, for example `:status ':durable`. Never quote
  keywords in unevaluated syntax positions: `defclass` `:initarg` names,
  `case` clause keys, `member` type specifiers, quoted configuration data,
  and macro metadata the macro quotes itself.

### Types and Documentation

- Declare function types with Serapeum's imported `->` notation:

  ```lisp
  (-> generation-find (string) (option generation))
  ```

  Declare each function once, directly before its definition. Generic
  functions take no `->` declaration; their documentation and their methods'
  specializers describe them.

- Keep reusable custom types in one coherent location.
- Do not define weak aliases that merely expand to `list` or another generic
  type. Validate structured types with `(satisfies predicate)` or a stronger
  type expression.
- Give functions and macros documentation strings. Give classes, slots,
  generic functions, and conditions documentation in their supported
  documentation locations.
- Use this comment hierarchy:

  ```lisp
  ;;;; -- Major Section --
  ;;; Minor section
  ;; Regular comment
  ; Inline comment, rarely
  ```

### Formatting

Use two-space indentation and vertical alignment where related forms expose a
repeated structure. In particular, align class slot options, `let` bindings,
and keyword arguments in multi-line calls.

```lisp
(let* ((manifest-path (generation-manifest-path generation))
       (core-path     (generation-core-path generation))
       (commit        (generation-commit generation)))
  (generation-validate :manifest-path manifest-path
                       :core-path     core-path
                       :commit        commit))
```

- Put one blank line between definitions and two blank lines between major
  sections.
- Leave no trailing whitespace.
- Put a conditional clause body on the line after its test, even for `nil`.
- In `labels`, leave a blank line between local function definitions.
- A literal percent sign in a `format` control string is `%`, not `%%`.
  Common Lisp `format` directives begin with `~`.

### Command notation

Document Autolith commands in canonical Common Lisp form, such as `(status)` or
`(settings :transcript-style :repl)`. Mention slash syntax only when explaining
its syntactic sugar or its parser.

### Conditions, Restarts, and Returns

- Define domain-specific conditions with structured data, documentation, and
  helpful report functions. Do not use raw error strings where callers need
  to distinguish or recover from failures.
- Use `handler-case` for expected failures and establish useful restarts where
  practical.
- Keep the condition and restart model explicit at component boundaries,
  especially in mutation, provider, checkpoint, and recovery code.
- Guard LLM-assisted condition handling against recursion. A serious failure
  while that path is already handling a condition is fatal.
- Use `(block nil ... (return ...))` for clear early returns.
- Boolean functions return exactly `t` or `nil`.
- Use explicit `values` forms for intentional multiple-value returns and
  document those values.

### Macros and Dependencies

- Prefer functions to macros when functions suffice.
- Name context and resource macros with a `with-` prefix.
- Prevent variable capture with gensyms or deliberate block names, and
  document expansion and evaluation behavior.
- Anaphoric forms may be used when they materially improve readability. Do
  not add an anaphora dependency without asking if the project does not
  already use one.

## Quality and Testing

- Implement complete behavior, including the difficult failure paths. Do not
  substitute a cheap approximation for a specified invariant.
- Add tests for behavior, state transitions, persistence, recovery,
  serialization, and external boundaries. Avoid tests whose main value is
  pinning formatting, incidental source shape, or a private implementation
  detail.
- Prefer table-driven cases for families of literal inputs and expected
  outputs.
- Give each contract one primary test owner at the strongest executable boundary;
  additional layers need a distinct transport, lifecycle, storage, or platform risk.
- Exercise operation lookup, completion, and invocation instead of copying
  command/tool name inventories. Retain aggregate test runners only when scripts
  or documented entry points still call them.
- Test expected failures as well as successful paths.
- Run focused checks (selected suites or cases, a library's own tests) while
  developing and before each commit. Run the complete repository check once
  when a batch of related commits is finished, including after documentation
  and configuration changes, and before pushing.
- Do not invent a test command. When the project gains test and lint entry
  points, document the exact commands here and keep them current.
- Never commit with known failing relevant checks. If an environmental failure
  prevents a check, report the exact failure rather than claiming success.

Run the complete repository check from the repository root with:

```sh
./script/check
```

The check runs FiveAM cases in independent SBCL processes, defaulting to the
available logical CPU count (one worker if detection fails), and includes the
recovery probes. It saves a source-matched active image for nested Autolith
processes and a separate image containing the test system. Each worker boots the
test image; test configurations use `AUTOLITH_TEST_ACTIVE_CORE` for nested
processes. `--jobs` overrides concurrency; at most one worker per selected case
is started. Select suites or individual cases for focused work:

```sh
./script/check --list
./script/check --suite workspace-resource --suite fixtures
./script/check --test test-workspace-file-resources --jobs 1
./script/check --jobs 8 --timeout 900
```

Selectors are exact, case-insensitive names; repeated selectors form a union.
Unknown names fail. Selected runs and `--list` skip the source checks and
recovery probes. `--timeout`
is a per-worker deadline in seconds (default 600). Every run reports case timings
and the slowest cases. Worker failures, invalid results, and timeouts fail the
command; worker process groups are terminated and reaped on exit.

A complete run also checks the source and its compiler warnings. While the
cases run, a separate process compiles every system supported on the host from
scratch into a private FASL directory and records each warning SBCL would print.
Every warning nobody silenced on purpose, style warnings included, fails the
check; silence one only where its reason is visible in the source. The rules
of [cheque](https://github.com/lambda-symbolics/cheque), configured by
`cheque.sexp`, enforce the mechanical parts of this file: layout and
alignment, section headers, documentation strings, `->` declarations, keyword
parameters, list accessors, `format` percent signs, constants, the project
package, TODO and FIXME comments, and em dashes. They also require every Lisp
file to be a fixed point of parinfer-rust's paren mode, so editors running
parinfer never reindent committed source. cheque builds its private parinfer
library on first use from the commit its `native/parinfer/commit` pins;
bootstrap and release builds never do. The checks also run the rules of the
Mallet linter that agree with this file, listed with the reasons for those left
out in cheque's `lambda-symbolics` preset. Fix rule behavior in cheque itself,
not in this repository.
Code that breaks a Mallet rule on purpose says why in place, as in
`; mallet:suppress no-ignore-errors -- best-effort cleanup`. Every finding is
reported at the end of the run.

```sh
./script/check --source
./script/check --fix
```

`--source` runs only the source rules and the warning compile. `--fix` repairs
what the rules can repair, then reports the rest. Layout repairs that would
change anything but whitespace outside string literals are refused. Review a
repair like any other change.

Set `AUTOLITH_TEST_STACK_AFTER` to a positive integer number of seconds when
diagnosing a stalled worker. Inspect its captured log for `RUN` entries and
live Lisp thread backtraces.

Register zero-argument test functions in `tests/test-suites.lisp` using
`define-test-suite`. Use FiveAM assertions or `test-assert`, which records a
FiveAM check in registered tests. `with-test-configuration` binds a fresh
configuration and optional temporary root, cleaning it up on every exit.
`with-test-environment` installs `(NAME VALUE)` bindings and restores previous
values; `nil` means unset. Use `test-call-with-function-replacements` for scoped
function replacement. Global function and environment mutation requires process
isolation, not parallel test threads.

Test runs own their configuration directories and remove them on exit. The CLI
also removes each worker's directories after a crash or timeout, once its
process group has stopped.

For interactive serial execution after loading `autolith/tests`:

```lisp
(autolith:run-tests :suites '("workspace-resource"))
(autolith:run-tests :tests '("test-workspace-file-resources"))
```

Materialize the locked project dependencies with:

```sh
./script/bootstrap
```

On macOS arm64, install a host SBCL, Quicklisp under `~/quicklisp`, Rust
with Cargo, the Xcode command-line tools, CMake, `pkg-config`, and OpenSSL before
running bootstrap. The SBCL must satisfy the tracked minimum version. Custom
build suffixes such as `2.6.8.termux` are accepted by the shared Lisp runtime
check. Bootstrap records the complete runtime identity and installs matching,
hash-verified source when listed in `sbcl-source-releases.sha256`; otherwise it
continues without managed implementation sources. It never replaces the
Homebrew host SBCL.

On Windows x86-64, install Git for Windows, Rust with the MSVC toolchain and
Cargo, `clang` on `PATH`, and Quicklisp under `%USERPROFILE%\quicklisp`, then
run `script\bootstrap.ps1` from PowerShell. An SBCL 2.6.6 or newer on `PATH` is
used when present; otherwise the bootstrap installs the official Windows binary
pinned in `sbcl-windows-releases.sha256` below the data root. `script\check.ps1`
runs the checks and `bin\autolith.cmd` starts a session.

Run the standalone Windows OpenSSL discovery fixtures without bootstrapping Lisp:

```powershell
powershell -NoProfile -File script/check-windows-openssl.ps1
pwsh -NoProfile -File script/check-windows-openssl.ps1
```

Run the Qlot temporary-checkout cleanup fixtures in either PowerShell:

```powershell
powershell -NoProfile -File script/check-windows-qlot-cleanup.ps1
pwsh -NoProfile -File script/check-windows-qlot-cleanup.ps1
```

After bootstrap, run the Windows ColorLisp packaging fixtures with:

```powershell
powershell -NoProfile -File script/check-windows-colorlisp.ps1
pwsh -NoProfile -File script/check-windows-colorlisp.ps1
```

Rebuild only the installed pristine recovery image with:

```sh
./script/build-recovery
```

For a fast delimiter check before loading edited Lisp, use the built-in
`lisp.paren-check` operation on each relevant source tree:

```lisp
(lisp.paren-check :path "src")
(lisp.paren-check :path "recovery")
(lisp.paren-check :path "tests")
```

## Commit Policy

- Use primitive-style, imperative commit messages with a title line only,
  shorter than 72 characters.
- Do not add `Co-Authored-By` or other attribution trailers.
- Keep commits tiny, granular, single-purpose, and independently reviewable.
- Prefer one commit per regression fix, feature slice, or coherent structural
  change. Never put more than one issue in a commit.
- For broad work, make a sequence of small vertical commits instead of one
  final catch-all commit.
- Do not bundle unrelated cleanup, refactoring, or formatting with a behavior
  change.
- Rebase instead of merging and avoid merge commits.
- Run focused checks for each change before committing it.
- Commit each completed change on its own; push once the complete repository
  check passes on the finished batch.
- Do not rewrite, discard, or include unrelated user changes. Inspect the
  worktree and index before committing, and stage only the intended change.

Never use em dashes in source comments, documentation, commit messages, or
user-facing text.

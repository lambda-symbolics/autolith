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

Supported source-development targets are Linux x86-64 and macOS arm64 on SBCL
with a terminal interface, one primary agent, and no claim of hostile-code
sandboxing. Nix builds and packaged binary releases support Linux x86-64 and
macOS arm64.

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
- Keep provider authentication and transport behind a replaceable interface.
  Do not launch or bundle the Codex CLI to implement subscription access.
- Keep credentials out of saved cores, conversation files, and Git.
- Source is authoritative for clean rebuilds. Saved cores preserve exact
  working live states, but never replace tracked source.
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

Autolith's runtime never patches its own tracked repository; selected private
replay scripts load after the tracked system, and repository changes remain
with the user and their development tools.

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
- Test expected failures as well as successful paths.
- Run focused checks while developing, then every repository-wide check that
  exists after every change, including documentation and configuration
  changes, and before committing.
- Do not invent a test command. When the project gains test and lint entry
  points, document the exact commands here and keep them current.
- Never commit with known failing relevant checks. If an environmental failure
  prevents a check, report the exact failure rather than claiming success.

Run the complete repository check from the repository root with:

```sh
./script/check
```

Materialize the locked project dependencies with:

```sh
./script/bootstrap
```

On macOS arm64, install a host release SBCL, Quicklisp under `~/quicklisp`, Rust
with Cargo, the Xcode command-line tools, CMake, `pkg-config`, and OpenSSL before
running bootstrap. The SBCL must satisfy the tracked minimum version and have a
source archive identity in `sbcl-source-releases.sha256`. Bootstrap records that
runtime and installs its hash-verified matching source under the Autolith data
root; it never replaces the Homebrew host SBCL.

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
- Run the relevant checks before each commit.
- Commit each completed change after its checks pass, and push after each
  commit.
- Do not rewrite, discard, or include unrelated user changes. Inspect the
  worktree and index before committing, and stage only the intended change.

Never use em dashes in source comments, documentation, commit messages, or
user-facing text.

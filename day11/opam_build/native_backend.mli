(** Native (host-side) build backend.

    Runs [opam-build] directly on the host with a temporary
    [OPAMROOT] instead of inside a runc container. No sudo required.
    Deps are hardlinked into the temp prefix via
    {!Day11_layer.Stack.merge_no_sudo}; captured files are
    identified by {!Day11_layer.Snapshot} diff.

    Use when:
    - The deps you need are already native-built (user-owned), OR
    - The host has a working opam root that can seed a temp switch.

    Container-specific parameters ([mounts], [prep_upper]) are
    accepted for interface compatibility but ignored. The strategy's
    [cmd] is still honoured — it runs as a subprocess. [strategy.cleanup]
    is applied to the temp prefix before the diff is computed so
    build/source directories don't leak into the captured layer.

    {1 Limitations}

    This backend is a work in progress. The exact opam-build
    invocation env (OPAMROOT, HOME, OPAMSWITCH) matches what the
    container backend sets up, but bootstrapping an OPAMROOT from
    scratch is left to the caller — the backend only merges dep
    layers into the temp prefix and runs the build. Callers that
    want to build the first compiler layer must seed the temp
    prefix themselves via a custom prep step (not yet exposed). *)

include Backend.S

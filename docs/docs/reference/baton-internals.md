# Baton system internals

This page documents how the baton memory management system works internally. You do not need to understand this to write Unnameable programs — see [Memory management](memory-management.md) for the user-facing guide.

> **Stability notice:** The baton system is under active development. The design described here is correct but some cases have known bugs including double frees and leaks. This document reflects current design intent, not a guarantee of behavior in all cases.

## Overview

The baton system is a last-use deterministic memory manager. It tracks heap variables through three coordinated phases — semantics, auditor, and IR generation — and inserts frees at the correct points without any runtime overhead.

The system is built around a single concept: the **baton**. Only one node in the program can hold the baton for a given variable at any time. Whoever holds the baton at the end of analysis is responsible for freeing the memory.

## The responsibility table

When a `heap` variable is declared the compiler registers it in a **responsibility table** separate from the AST. Each entry is assigned a **lifetime ID** — a unique key that identifies the variable throughout the system.

```
heap i32 x    →    N0 born, registered in responsibility table
heap ptr i32 p    →    P0 born, registered in responsibility table
```

Integer variables get IDs prefixed `N`. Pointer variables get IDs prefixed `P`.

## The baton

A baton is a data structure attached to each lifetime ID. It carries:

- The lifetime ID
- Whether it is currently responsible for freeing (`isResponsible`)
- A list of dependents — other IDs this baton is responsible for freeing
- A pointer count — how many pointers currently hold this baton
- Alive status

## The three phases

### Phase 1 — Semantics

The semantics phase walks the AST and moves batons as it encounters identifiers. Every time a heap variable is used the baton transfers to that node. By the end of semantics the baton sits on the last encountered use of each variable.

Only two node types can hold a baton — a **declaration node** and an **identifier node**. No other node type is a valid baton holder.

Because semantics walks the AST linearly it is **myopic** — it sees uses in order but cannot reason about branches, loops, or complex ownership patterns. The positions it produces are a starting point, not a final answer.

### Phase 2 — The auditor

The auditor corrects what semantics got wrong. It runs two sub-passes:

**Classification pass** — every heap variable is classified as either a **native** or a **foreigner** relative to each block it appears in. A native was born in that block. A foreigner was born outside it.

**Audit pass** — the auditor walks the AST again, simulates frees, detects robberies, resolves ownership transfers, and decides what gets bunkered.

### Phase 3 — IR generation

When the IR generator encounters a node holding a baton it emits the deallocation call. The actual free is a call to `unn_dealloc` — the runtime's deallocation function.

## Atomicity of identifiers

The baton system always resolves down to the atomic identifier level regardless of how complex the containing expression is. A composite node like `trace x, y, z` does not hold batons — the system drills into it and attaches batons to `x`, `y`, and `z` individually.

This means no matter how deeply nested an identifier is, the baton will find it.

## Inhibitors

The IR generator is greedy — when it sees a baton it wants to emit a free immediately. This causes problems mid-operation. For example in `y = x + 1` the system must not free `x` or `y` before the assignment completes.

Inhibitors suppress the free signal during composite node processing. A composite node raises an inhibitor before processing its children and lowers it when done. The IR generator checks for inhibitors before emitting any free.

## Robbery

When a pointer is bound to a heap variable a **robbery** occurs. The pointer (robber) takes the baton from the variable (victim). The victim's `isResponsible` is set to false — it can no longer free itself. The robber's baton records the victim as a dependent.

```
heap i32 x        →    N0 born, isResponsible = true
heap ptr i32 p -> addr x    →    P0 robs N0
                               N0 isResponsible = false
                               P0 dependents = [N0]
```

When the robber dies it frees its dependents first, then itself.

## Baton rearm

If the robber dies but the victim is still alive and outliving the robber, the auditor **rearms** the victim — restoring its `isResponsible` flag so it can free itself independently:

```
[REARM] Rearming N0
[REARM] Success N0 is now a free agent
```

## Candidate transfer

When a robber dies the auditor checks a **candidate registry** — a list of IDs that attempted to rob the same baton. If a living candidate exists it becomes the new responsible party. 

If the candidate was itself robbed, ownership climbs the chain to the candidate's master — not the candidate directly. The system respects master-slave dynamics.

## Assignment heist

An assignment like `y = x + 1` triggers a special robbery called an **assignment heist**. The left-hand side (`y`) attempts to rob the right-hand side value baton (`x`). If approved, `y` takes responsibility for `x`'s memory.

If the heist creates a circular dependency the auditor detects the cycle and bunkers the involved IDs to scope end.

## Bunkering

Bunkering is the safe fallback. A baton is bunkered when:

- The analysis detects a cycle that cannot be safely resolved
- A stack variable interacts with a heap variable (**heist bunkering**) — the compiler cannot reason about the stack variable's usage so it holds the heap variable to scope end
- A foreigner would otherwise be freed inside a branch or loop

Bunkered batons are collected in a basket per block and freed during the block's cleanup phase in IR generation.

## Native and foreigner cleanup

At the end of each block the IR generator runs two cleanup passes:

**Native cleanup** — processes any bunkered natives registered for this block and emits their frees.

**Foreigner cleanup** — processes any foreigners that were used in this block and emits their frees at the merge point.

Variables freed earlier at their last use do not appear in either cleanup pass — their basket entry is already empty.

## persist

The `persist` modifier bypasses last-use analysis entirely. A persisted baton is forcibly bunkered to scope end regardless of where its last use is. The IR generator holds it until the block's native cleanup phase.
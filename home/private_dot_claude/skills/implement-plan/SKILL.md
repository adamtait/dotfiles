---
name: implement-plan
description: Use immediately after a plan or spec has been approved and the user says to implement, build, or execute it — especially right after exiting plan mode in Claude Code. Drives the full loop: implement the plan while keeping a running implementation-notes file of every off-spec decision, change, tradeoff, and surprise; then open a PR, run code review, and apply the resulting fixes. Trigger whenever the user says "implement the plan", "go ahead and build it", "execute the plan", "make it so", or hands you an approved plan to carry out — even if they don't explicitly mention notes, a PR, or review, because those are exactly the steps they most often forget to ask for but always want.
---

# Implement a Plan

You have (or are about to have) an approved plan. Your job is to carry it out end to end without losing the things the user cares about along the way: an honest record of what you decided, a reviewable PR, and the fixes that review turns up.

The loop is:

**Implement (+ running notes) → open a PR → run review → apply the fixes → hand off.**

Don't skip the notes and don't batch them at the end. The whole point is that the notes are written *while* the decisions are fresh, not reconstructed from memory afterward.

## Before you start

1. **Find the plan.** It may be in the conversation (e.g. the plan you just exited plan mode with), in a file the user pointed at, or in an issue/PR description. If you can't locate a concrete plan, stop and ask — don't invent one.
2. **Confirm scope in one line.** State what you're about to build and, briefly, any assumption you're making about ambiguous parts. Then proceed — don't wait for a reply unless something is genuinely blocking or expensive to get wrong.
3. **Get on a branch.** If you're on `main` (or the repo's default branch), create a feature branch first. Never build directly on the default branch.
4. **Start the notes file.** Create `implementation-notes.md` at the repo root (see below) and put a one-line header on it before you write any code, so there's somewhere to append to from the first decision onward.

## Phase 1 — Implement while keeping running notes

Work through the plan in small, coherent steps. Match the surrounding code's style and conventions; read before you write. Run the tests/build/typecheck as you go so you catch breakage early rather than discovering it all at the PR stage.

### The running notes file

Keep `implementation-notes.md` open in your head the entire time. **The moment you make a call that isn't spelled out in the plan, append an entry — before you move on to the next thing.** If you find yourself at the end of implementation with an empty notes file, something went wrong: either you followed a spec that left nothing to decide (rare), or you forgot to write as you went (much more likely).

Markdown is the default because it diffs cleanly and lives naturally in the repo. If the user specifically asked for HTML, write `implementation-notes.html` instead with the same content.

By default, **keep the notes file out of the feature commits** — it's a heads-up document for the user and reviewers, not part of the shipped change. Surface its highlights in the PR description instead. If the user wants it committed, commit it.

**What belongs in the notes (signal, not a play-by-play):**

- **Decisions not in the spec** — anything the plan left open that you had to resolve. What you chose, and why.
- **Changes from the plan** — where reality diverged from the plan and you deviated. What the plan said, what you did, why.
- **Tradeoffs** — where you picked one option over another with real downsides. Name the alternative you rejected.
- **Surprises / discovered constraints** — things you learned mid-implementation that the user would want to know (a library limitation, an existing bug you worked around, a dependency you had to add).
- **Deferred / TODO** — anything you consciously left out of scope, with a note on why and what it'd take to finish.
- **Anything that would make the user go "wait, why did it do that?"** — get ahead of that question.

Do **not** log routine spec-following, obvious mechanical steps, or a narration of every file you touched. If it wouldn't surprise or inform the user, leave it out. The notes are valuable in proportion to how much they save the user from having to reverse-engineer your reasoning.

**Entry format** — keep it lightweight and scannable:

```markdown
# Implementation notes — <plan / feature name>

## <short title of the decision>
**What:** what you did.
**Why:** the reasoning, and what the plan said (if it differed).
**Alternatives / tradeoff:** what you considered instead and why you passed on it. (Omit if there was no real fork.)
**Impact:** anything downstream this affects — follow-up needed, perf, a TODO. (Omit if none.)
```

## Phase 2 — Open a PR

Once implementation is complete and the tests/build pass:

1. Commit the work in focused commits with clear messages describing the *why*. Follow the user's git conventions (see their global/project instructions for commit trailers and branch rules).
2. Push the branch and open a PR with `gh pr create`.
3. Write a PR description that summarizes what changed and why, and folds in the **highlights from the implementation notes** — the deviations and tradeoffs a reviewer should know about. Link the plan/issue if there is one.

Creating a PR implies pushing, which the user has asked for here — so pushing this branch is authorized. Do not force-push or touch other branches.

## Phase 3 — Run review

Run code review on the change. Use the review command the user has available:

- `/review` reviews the GitHub PR you just opened — the natural fit once the PR exists.
- `/code-review` reviews the current working diff — use it if there's no PR yet or you're reviewing locally before pushing.

Let the review run and collect its findings. If the environment has neither command, fall back to a thorough self-review across correctness, readability, security, and performance, or spawn a code-reviewer subagent.

## Phase 4 — Apply the suggested fixes

Don't apply review findings blindly — triage them first. Reviews surface real bugs, but they also raise things that are out of scope, wrong, or a matter of taste.

- **Apply** the findings that are correct and in scope. Fix them, and re-run the tests/build.
- **Decline** findings you disagree with — but record *why* in the implementation notes, so the user can see what you chose not to do and overrule you if they want.
- **Log every fix and decline in the notes file** as you go, same as Phase 1. Review-driven changes are exactly the kind of thing the user will want a record of.

Commit the fixes (separate, clearly-messaged commits are nice — they show the reviewer what changed in response) and push to update the PR.

## Hand off

When the loop is done, give the user a short wrap-up:

- The PR link.
- Where the implementation notes live, and a one-line sense of what's in them (e.g. "3 off-spec decisions, 1 tradeoff, 2 review fixes applied, 1 declined").
- Anything still open — deferred TODOs, findings you declined and why, or a decision you'd like the user to confirm.

The measure of success is that the user can skim the notes and the PR and understand not just *what* you built, but every place where you had to think for them — with nothing surprising buried in the diff.

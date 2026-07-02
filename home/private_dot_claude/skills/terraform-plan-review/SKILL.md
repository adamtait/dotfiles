---
name: terraform-plan-review
description: >-
  Reviews a Terraform plan for danger before it is applied — data loss,
  production-breaking changes, errors, omissions, inconsistencies, and likely
  future problems — and returns a severity-ranked report with a clear
  apply/don't-apply verdict. Use this whenever the user wants a Terraform (or
  OpenTofu) plan checked, vetted, audited, or sanity-checked, or asks to
  "review the terraform plan", "is this plan safe to apply", "will this destroy
  anything", "what's getting replaced", "check this tfplan", "look over my infra
  changes", or pastes `terraform plan` / `terraform show` output, a `.tfplan`
  file, or a plan JSON. Trigger even when they don't say the word "review" — any
  request to evaluate, verify, or get a second opinion on infrastructure changes
  before applying them should use this skill. Especially important before
  applying changes to production, stateful resources (databases, buckets,
  secrets), or anything with potential for downtime or irreversible deletion.
---

# Terraform Plan Review

Apply is the dangerous moment. A `terraform plan` looks innocuous — a wall of
green `+`, yellow `~`, red `-` — but buried in it can be a single `-/+` that
destroys and recreates a database, a removed IAM binding that 403s a running
service, or a silent no-op where a change was *expected*. The reviewer's job is
to surface the few lines that matter, explain *why* they're dangerous, and give
the human a defensible go / no-go decision.

The reader is usually about to run `terraform apply` against real
infrastructure. Treat the review as the last line of defense.

## What "good" looks like

A trustworthy review is **skeptical, specific, and ranked**:

- **Skeptical** — assume the diff hides something. The destroy/replace actions
  and the *absence* of expected changes are where damage lives, not the additions.
- **Specific** — name the exact resource, the exact attribute forcing the
  change, and the concrete consequence ("the `(default)` Firestore database is
  destroyed and recreated, losing all documents"). Vague warnings get ignored.
- **Ranked** — lead with what can hurt production. A reader skimming the first
  three lines must learn the worst thing in the plan.

The opposite — a flat restatement of the diff, or a pile of low-severity nits
that buries the one Critical finding — is worse than no review, because it
looks thorough while missing the thing that takes down prod.

## Workflow

### 1. Get the plan into a reviewable form

You need the *change set* (what will be added / changed / destroyed), and
ideally in machine-readable JSON so nothing is missed. Auto-detect what's
available, in this order:

1. **A plan the user already gave you** — pasted `terraform plan` text, a
   `terraform show` dump, a saved `*.tfplan` binary, or a plan JSON file. Prefer
   these; they reflect the exact state the user saw.
2. **A binary plan file** (`*.tfplan`, `plan.out`) on disk — render both views:
   ```bash
   terraform show <planfile>            # human-readable, shows replace reasons inline
   terraform show -json <planfile> | jq '.' > /tmp/plan.json   # complete + parseable
   ```
3. **No plan yet, but a Terraform working dir is present** — generate one,
   *only if* the dir is initialized and credentials/backend access exist:
   ```bash
   terraform plan -out=/tmp/review.tfplan && terraform show -json /tmp/review.tfplan > /tmp/plan.json
   ```
   `plan` is read-only (it never mutates infra), so it's safe to run. But it
   commonly fails for lack of cloud credentials or backend access — if it does,
   **don't fake it**: fall back to step 4 and say so.
4. **Source-only fallback** — if no plan can be produced, review the `.tf`
   source directly. This catches misconfigurations and omissions but *cannot*
   see replacements or destroys (those depend on current state). State this
   limitation prominently — a source-only review is not a substitute for a plan
   review, and the reader must know that.

**Always state your review basis** (json plan / text plan / source-only) and its
limitations near the top of the report. A reader who thinks they got a full plan
review when they got a source skim is being misled.

When you have JSON, the change set lives in `.resource_changes[]`. The single
most useful classification:

```bash
# Bucket every resource by action. ["delete","create"] = REPLACE (the danger zone).
jq -r '.resource_changes[] | "\(.change.actions | join(",")) \(.address)"' /tmp/plan.json | sort | uniq -c
```

`.change.actions` values: `["create"]`, `["update"]`, `["delete"]`,
`["delete","create"]` or `["create","delete"]` (= **replace**), `["no-op"]`,
`["read"]`. The `.change.replace_paths` array tells you *which attribute* forced
a replacement — quote it, because "why is this being replaced" is the reader's
first question.

### 2. Read the source and intent, not just the diff

The diff shows *what* changes; the `.tf` source and surrounding code show what
*should* change. Read the modules touched by the plan to understand:

- **Lifecycle protections** — `prevent_destroy`, `deletion_protection`,
  `delete_protection_state`, `force_destroy`. A plan that destroys a resource
  guarded by `prevent_destroy` will *fail apply* — that's an error finding, not
  just a risk.
- **Intent** — `lifecycle { precondition { ... } }`, comments, variable
  defaults. These reveal what the author was protecting against.
- **Omissions** — the highest-value, hardest findings. A change is missing when
  the code/config implies it should be there: a new env var or secret referenced
  by the application but not wired into the service, a new service account with
  no IAM grant for the bucket it must write to (a classic silent 403 at
  runtime), a new resource with no corresponding monitoring/alert. The plan
  won't flag these — you have to notice the *gap*. When application code is
  available and relevant, cross-check it.

### 3. Analyze across every dimension

Walk the change set and the source looking for each category below. For
provider-specific rules — which GCP/AWS/Azure resources are stateful, which
attributes force replacement, which changes cause downtime — read
`references/provider-pitfalls.md`. Don't try to recall every force-replacement
attribute from memory; that file is the catalog.

| Category | What you're hunting for |
|----------|-------------------------|
| **Data loss** | Destroy or **replace** of any *stateful* resource — databases, buckets, disks, secret versions, stateful sets. Replacing storage = irreversible deletion. Check whether `force_destroy`/`deletion_protection` would even *allow* it (if not → it's also an apply error). |
| **Breaking production** | Changes that interrupt running services — service/container replacement (downtime), removed IAM bindings a live workload depends on, firewall/egress/network changes that sever connectivity, DNS record changes, secret rotation a consumer can't pick up, scaling changes (min instances → 0). |
| **Errors / misconfig** | Things that will *fail the apply* or be wrong if applied — destroy blocked by `prevent_destroy`, invalid references, precondition violations, name-length/format limits, wrong region/project, type mismatches, cycle. |
| **Omissions** | Expected changes that are *absent* — see step 2. The plan being smaller than expected is a finding, not a relief. |
| **Inconsistencies** | Contradictions — a resource referencing another that's being destroyed, mismatched names across modules, env/region drift, two resources claiming the same name. |
| **Future problems** | Won't break today, will bite later — deprecated arguments/providers, `lifecycle_rule` deletion ages that quietly remove data, approaching quotas, hardcoded values that won't scale, churn that recreates a revision every apply, missing `prevent_destroy` on something irreplaceable. |
| **Cost** (note if obvious) | New always-on/expensive resources, large provisioned capacity, removed cost controls. Not the focus, but flag the glaring ones. |

For each finding, assign a **severity**:

- 🔴 **Critical** — irreversible data loss, or a guaranteed production outage on
  apply. The reader must not apply without addressing this.
- 🟠 **High** — likely breakage, hard to reverse, the apply will error, or a
  security regression. Needs a deliberate decision.
- 🟡 **Medium** — works but risky, degraded, or will clearly bite later.
- 🔵 **Low** — minor, stylistic, or informational.

When unsure between two levels, say what would push it either way rather than
silently picking one — e.g. "High if the bucket has live objects, Low if it's
empty; check before applying."

### 4. Write the report

Use the structure below. The ordering is deliberate: **verdict and the worst
findings first**, because many readers act on the first screen alone.

## Report structure

ALWAYS open with the verdict and summary, then a scannable table, then details.

```markdown
# Terraform Plan Review — <scope / module / PR>

**Verdict:** 🔴 Do not apply | 🟠 Apply with caution | 🟢 Safe to apply
**Plan summary:** N to add · M to change · K to destroy  (R replacements)
**Review basis:** JSON plan | text plan | source-only — <one line on coverage/limits>

> One- or two-sentence bottom line: the single most important thing the reader
> needs to know before deciding.

## Findings

| # | Sev | Category | Resource | Issue (one line) |
|---|-----|----------|----------|------------------|
| 1 | 🔴 | Data loss | google_firestore_database.default | Replaced via location_id change — all documents lost |
| 2 | 🟠 | Breaking prod | google_cloud_run_v2_service.dispatcher | In-place but new image fails health check rollback |
| … |

### 🔴 1 · [Data loss] Firestore database destroyed and recreated
- **Resource:** `module.firestore.google_firestore_database.default`
- **Action:** replace (delete → create)
- **What forces it:** `location_id` changed `us-central1` → `nam5`
- **Consequence:** the `(default)` database and every document are permanently
  deleted; Firestore has no undo. The configured `delete_protection_state`
  would also block this, so the apply will likely error before any deletion.
- **Recommendation:** revert `location_id`, or migrate data deliberately
  (export → new DB → import) rather than letting Terraform recreate it.

(Repeat per finding, ordered by severity. Keep each tight: resource, action,
cause, consequence, recommendation.)

## Destroy / replace inventory
Every resource being destroyed or replaced, listed explicitly — these are the
highest-risk actions and the reader should see the full set at a glance.

- `module.firestore.google_firestore_database.default` — REPLACE (data loss)
- `module.x.google_…` — DESTROY
- …
(If none: "No destroys or replacements — all changes are additions or in-place
updates." That sentence is itself reassuring and worth stating.)

## Looks reasonable
A few words on the changes you checked and judged safe, so the reader trusts the
review covered the plan rather than cherry-picking. Don't pad this.

## Before you apply
Concrete verification steps tied to the findings — e.g. "confirm the artifacts
bucket is empty: `gcloud storage ls gs://…`", "snapshot Firestore first",
"re-run plan after reverting location_id and confirm 0 to destroy".
```

## Calibration

- **A clean plan is a valid result.** If the plan only adds resources and
  in-place-updates safe attributes, say so plainly with a 🟢 verdict and a short
  destroy/replace inventory showing "none". Don't invent Critical findings to
  look useful — a false alarm on a safe plan trains the reader to ignore you.
- **Don't drown the signal.** If there's one Critical and ten Lows, the Critical
  must dominate. Consider collapsing trivial Lows into a single line.
- **Quantify when you can.** "Replaces the bucket" is weaker than "replaces the
  bucket, deleting ~all objects under gs://… (force_destroy=false, so apply will
  actually *fail* unless that's flipped first)".
- **Distinguish "will fail" from "will succeed and cause harm."** Both are worth
  flagging, but they're different problems: an apply that errors is annoying; an
  apply that succeeds and silently drops a table is a catastrophe. Make clear
  which one each finding is.

## Reference files

- `references/provider-pitfalls.md` — per-provider catalog: which resources are
  stateful, which attribute changes force replacement, which changes cause
  downtime. GCP-heavy (with AWS/Azure sections). Read it during step 3 rather
  than recalling force-replacement rules from memory — they're easy to get wrong.

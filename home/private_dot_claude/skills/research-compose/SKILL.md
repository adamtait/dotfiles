---
name: research-compose
description: Researches, fact-checks, and drafts a section of a technical document — either from a prompt describing the section, or by scanning a draft for placeholder markers (e.g. `[... insert introduction ...]`, TK, TODO) that flag where work is needed. Every load-bearing claim is tied to a primary source and adversarially checked before it is written. Produces a sourced scaffold in the author's voice for the author to finish, never a drop-in final draft. Use when asked to research and draft a section, fill in a stubbed section, fact-check and write a passage, or find and complete the "insert X here" gaps in a document.
---

# Research & Compose Section Skill

This skill drafts a section of a technical document backed by real, checked sources. It
does research the author can trust and hands back a scaffold — not finished prose.

It is designed to be usable by writers who, like Nathan Lambert (Ai2 / *Interconnects*),
**do not ship AI-generated prose** (the standards it's tuned to are recorded in the
companion `docs/notes/nathan-lambert-writing-process.md`, if present).
The value here is the research and fact-checking — the slow, verifiable part — plus a
voice-matched skeleton that makes the author's own rewrite faster. The output is explicitly
a draft to be rewritten, with every claim sourced so the author can trust or challenge it.

## When to Use

- When asked to research and draft a section, or to fact-check and write a passage.
- When asked to fill in stubbed/labeled sections of an existing draft.
- When asked to find the "insert X here" gaps in a document and propose content for them.

Not for: full-document authoring (use `blog-post-author`), editing finished prose (use
`read-through-pass`), or open-ended research reports (use the `deep-research` skill).

## Prime Directive: research is the product, prose is scaffold

The author owns the voice and the final words. This skill owns the sources and the facts.

- **Never present output as publish-ready.** Label it a draft; end with a "verify before
  publishing" note.
- **Every load-bearing claim carries a citation** — inline, linking a primary or
  authoritative source. A claim you cannot source is flagged as unverified, not smoothed in.
- **Match the author's voice, don't impose one.** Read the surrounding document for tone,
  terminology, sentence rhythm, and stance; draft toward that. When the surrounding voice
  is unknown, keep prose plain and minimal so the author's rewrite has room.
- Apply anti-slop standards (shared with the companion `read-through-pass` skill): no
  filler, no hedging, no formulaic "it's not just X, it's Y," no lulling lofty sentences.

## Ask when in doubt

When the section brief is ambiguous, ask the author instead of guessing or producing a
vague, formulaic draft. If your go-to response would be generic — the marker's intent is
unclear, a key term or product name is undefined, the audience or angle is uncertain, or
sources conflict on a load-bearing claim — stop and ask before you research or draft. A
short clarifying question produces a better section than a confident guess. (Do not,
however, ask instead of doing the research you could do yourself — ask about intent, not
about facts you can look up.)

Always ask through the interactive, clickable question interface (the `AskUserQuestion`
tool) with concrete options the author can click — never a numbered list of questions in
prose.

## Two input modes

### Mode A — Prompt
The caller passes a prompt describing the section to write (topic, angle, audience, length,
where it sits in the document). Treat that prompt as the section brief and run the workflow
below. If a target document is available, read it for voice and context first.

### Mode B — Scan an existing document for labeled work
Scan the target document for markers that flag a section needing work. Detect, in priority
order:

1. **Bracketed instructions** — `[... insert introduction ...]`, `[... list examples and
   discuss trade-offs ...]`, `[... conclusion ...]`. This is the primary convention in this
   repo; the text inside the brackets **is the brief** for that section.
2. **Inline stubs** — `TK` (journalism "to come"), `TODO`, `FIXME`, `WRITEME`, `[HELP]`,
   `[DRAFT]`, and HTML comments like `<!-- write: ... -->`.

For each marker found, use the surrounding headings and prose as context, treat the marker
text as the brief, and run the workflow. List all detected markers first and confirm scope
before doing expensive research if there are many.

## Workflow (per section)

Run these in order. Do not skip research to save time — unsourced drafting is the failure
mode this skill exists to prevent.

### 1. Scope the section
- State the single job of this section: what claim, argument, or explanation must it deliver?
- Pull context from the document: what has already been said (don't repeat it), what comes
  next (set it up), the target audience, and the surrounding voice/terminology.
- List the specific questions the research must answer.

### 2. Research (fan out)
- Run parallel searches across distinct angles; prefer **primary and authoritative sources**
  (official docs, papers, source code, standards, first-party posts) over aggregators and
  SEO content.
- Fetch the actual sources — do not rely on search-result snippets for any claim you'll write.
- For deep or contested topics, delegate to the `deep-research` skill and use its cited
  report as input.
- Capture each source as a URL plus the exact supporting quote or data point.

### 3. Fact-check (adversarial)
For every claim the section will make:
- Find at least one primary source that supports it; quote the load-bearing line.
- **Try to refute it.** Assume it's wrong and look for a source that contradicts or narrows
  it. Prefer the more careful claim.
- Classify each claim: **verified** (primary source), **plausible** (secondary/uncertain —
  say what would confirm it), or **unverified** (no source — do not state as fact; flag it
  or drop it).
- Numbers, dates, benchmarks, version-specific behavior, and "X is the first/only/fastest"
  claims get extra scrutiny — these are where confident writing goes wrong.

### 4. Compose the scaffold
- Draft the section in the author's voice, short and concrete, opinionated where the
  document's stance supports it (flag forced neutrality rather than defaulting to it).
- Attach an inline source to each load-bearing claim.
- Mark any **plausible** claim with a visible confidence note; **omit** unverified claims
  from the prose and list them separately as open questions.
- Include code/diagrams only if complete and correct — no placeholders inside the draft.

### 5. Hand back
Do not silently overwrite the marker. Propose the draft and let the author place it.

## Output Format

### 1. Section brief
The one-job statement and the questions research had to answer. In Mode B, quote the marker
and its location.

### 2. Sources
Numbered list: each source as a link + the exact quote/data point it supports, tagged
`[primary]` or `[secondary]`.

### 3. Fact-check ledger
Each claim the draft makes → its classification (verified / plausible / unverified) → the
supporting source. This is the part the author is meant to trust or challenge.

### 4. Draft (scaffold)
The proposed section, in the author's voice, with inline citations. Prefaced with a plain
"Draft — rewrite in your voice before publishing" line.

### 5. Open questions & unverified claims
What could not be sourced, what is contested, and what the author must decide or confirm.

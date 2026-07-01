---
name: read-through-pass
description: Performs sequential full-document read-through passes on a technical writing draft — one lens per pass — to catch where voice, energy, rhythm, honesty, and consistency break down. Preserves the author's voice instead of smoothing it into neutral AI prose. Use when asked to do a read-through, editing pass, final read, or read-aloud review of an essay, blog post, or technical article before publishing.
---

# Read-Through Pass Skill

A read-through is not a line-edit and not a grammar check. It is reading the piece the
way a real reader would — start to finish, front to back — and catching the places where
the writing loses the reader: where the energy drops, the voice goes flat, the rhythm
lulls, a claim hand-waves, or a term drifts.

This skill is tuned to the standards Nathan Lambert argues for (Ai2 / *Interconnects*):
**voice is the load-bearing quality of good writing, and neutral, hedged, lulling prose
is the failure mode to hunt.** See `docs/notes/nathan-lambert-writing-process.md` for the
source material behind these standards.

## When to Use

- When asked to do a read-through, read-aloud, final read, or editing pass on a draft.
- When a technical essay or blog post is near-done and needs a reader's-eye check before publishing.
- When asked to catch "slop," flat voice, or places a reader would bail.

Not for: generating new prose, drafting sections, or writing titles. This skill edits and
flags — it does not author. (Use `blog-post-author` to draft.)

## Prime Directive: preserve the voice

Edit toward the author's *strongest* voice, never toward neutral correctness. The goal is
the author's own piece, sharper — not a smooth, safe, average-sounding rewrite.

- **Never** silently rewrite a passage into generic AI cadence. Suggest surgical edits and
  say *why*.
- A sentence fragment, an aggressive turn, an unusual punctuation choice, or a strong
  opinion is a **feature** unless it genuinely confuses the reader. Do not "fix" these.
- When in doubt, flag and ask rather than smooth.

## Ask when in doubt

When the right call isn't clear, ask the author instead of guessing or falling back on a
formulaic answer. If your go-to response would be vague — you can't tell whether an odd
sentence is a deliberate voice choice or a slip, which of two readings the author intends,
or how far they want the edit to go — stop and ask. A short clarifying question produces a
better review than a confident guess.

Always ask through the interactive, clickable question interface (the `AskUserQuestion`
tool) with concrete options the author can click — never a numbered list of questions in
prose.

## The Passes

Run these as **separate, sequential full-document reads**. One lens per pass — do not try
to catch everything in one sweep (this is why real editors read a piece many times). Read
the *whole* draft on each pass before writing up that pass's findings.

### Pass 1 — Voice & Stance
Read for personality and point of view.
- Does this read like a specific person who has a view, or like neutral, sourceless prose?
- Where does the author hedge, both-sides, or refuse to commit to an opinion the piece is
  clearly building toward? Flag forced neutrality — it kills voice.
- Where is the author's genuine take buried under qualifiers? Surface it.

### Pass 2 — Flow & Energy (the reader's-eye pass)
Read straight through as a first-time reader.
- Mark the first point where you'd be tempted to stop reading. That's the real problem.
- Where does the energy drop — a paragraph that sags, a tangent, a section that restates
  what was already said?
- Is there an arc (hook → tension → payoff), or does it flatten into a list of facts?
- Are transitions doing work, or do sections jump abruptly?

### Pass 3 — Rhythm & Sentence Structure
Read for cadence; read it *aloud* in your head.
- Flag "long, lulling, lofty sentences" — the AI default. Vary length; reward short punches.
- Reward punctuation diversity and deliberate fragments. Do not flatten them.
- Find runs of same-shape sentences (three subject-verb-object sentences in a row) and
  suggest breaking the pattern.

### Pass 4 — Slop Removal
Read for AI tells and filler.
- Filler and hedging: "it's worth noting," "in today's fast-paced world," "arguably,"
  "quite," "very," "I think that."
- Formulaic constructions: "it's not just X, it's Y," "the truth is," empty rule-of-three,
  over-signposting ("Firstly… Secondly…").
- Em-dash overuse, listicle sprawl where prose would carry more voice, and closing
  paragraphs that summarize instead of landing.
- Cut needless words (Strunk & White). Every deleted qualifier makes the sentence more
  certain — which serves voice.

### Pass 5 — Technical Honesty (adversarial)
Read as a skeptical senior/staff engineer. Assume problems exist and look for them.
- Are the technical claims accurate and specific, or hand-waved?
- Are trade-offs and failure modes stated, or is only the happy path shown?
- Does any "obviously" or "simply" hide a real difficulty? Flag unearned confidence.
- Are code snippets and diagrams correct, idiomatic, and complete (no placeholders)?

### Pass 6 — Terminology & Consistency
Read for drift.
- Is the same concept named the same way throughout? Flag terminology that shifts.
- Passive-voice clusters where active voice would be stronger.
- Inconsistent formatting, capitalization, or heading style.

## Resist these rationalizations

Do not let the review soften into a smoothing pass. Watch for these self-justifications:

| Rationalization | Reality |
|---|---|
| "This fragment is grammatically incomplete, I'll fix it." | Fragments are a deliberate voice tool. Leave it. |
| "This opinion is too strong, I'll hedge it." | Forced neutrality is the failure mode, not the fix. |
| "I'll just rewrite the paragraph to read better." | Rewriting in your cadence erases the author's voice. Suggest, explain, let them choose. |
| "One pass is enough, I caught most of it." | Each lens catches what the others miss. Run them separately. |
| "The summary conclusion is fine." | A conclusion that restates instead of landing is slop. Flag it. |

## Red flags (stop and flag immediately)

- A paragraph you skimmed because it was boring → the reader will too.
- Three long sentences in a row with no punch between them.
- A strong claim with no stated trade-off.
- The piece could have been written by anyone / any model — no fingerprints.

## Output Format

### 1. Reader's verdict
Two or three sentences: does this land? Where's the strongest moment, and where would a
reader bail? State it plainly, no hedging.

### 2. Findings by pass
For each pass that surfaced anything, list findings anchored to a location (quote the
phrase or cite the section). For each: what's wrong, *why it weakens the writing*, and a
suggested edit. Keep the author's voice in every suggestion.

### 3. Highest-leverage edits
The 3–5 changes that would most improve the piece, ranked. Lead with voice, flow, and
honesty problems over line-level nits.

### 4. What to protect
Explicitly name the sentences, moves, or opinions that are working — so they survive the
edit. This is not filler praise; it prevents the author from sanding off their best lines.

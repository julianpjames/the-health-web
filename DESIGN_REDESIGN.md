# The Health Web — Hybrid Editorial Redesign

## Design objective
Make a research-grounded public-health exploration tool feel like a considered
editorial publication, with an immersive interactive map at its center.
Avoid a generic dashboard or decorative "AI website" aesthetic.

## Visual system
- **Paper surfaces:** warm off-white (#F3F0E9, #FAF8F3), dark green ink (#182B2A)
- **Accent:** forest green (#254F47) and reserved warm amber (#DCAE76)
- **Typography:** editorial Georgia headings paired with Manrope functional text
- **Layout:** generous whitespace, clear hierarchy, quiet backgrounds, restrained borders
- **Network:** preserve the dark visualization canvas for contrast and focus
- **Motion:** subtle transitions only; respect reduced-motion preferences

## User journey
1. **Arrive:** understand the big idea in one readable headline and paragraph.
2. **Start:** choose food insecurity, type 2 diabetes, or asthma.
3. **Explore:** the selected concept focuses the existing network, and the page scrolls to it.
4. **Interpret:** select nodes/relationships, inspect explanation and cited evidence.
5. **Connect to place:** enter a ZIP code where supported to see carefully labeled indicators.

## Implemented in this first pass
- Editorial hero and three guided starting-point buttons
- Responsive card layout
- Cleaner navigation, section intro, search surface, and spacing
- Warm paper/forest palette around the existing graph
- Scoped CSS in `www/redesign.css` to ease iteration
- Reduced-motion handling for new hover/scroll behaviors

## Remaining design work, in priority order
1. **QA / interaction tests:** verify hero choices, node focus, side panel, and browser scroll;
   check keyboard, mobile, tablet, and desktop behavior.
2. **Information panel:** shorten above-the-fold copy, introduce progressive disclosure,
   establish clearly labeled "What this is / Why it matters / Sources / Local context" sections.
3. **Network onboarding:** improve initial map framing, simplify key and controls,
   ensure keyboard alternatives to pointer-only interactions.
4. **Evidence presentation:** make distinctions between causality, association,
   population estimates and individual health more legible.
5. **Mobile experience:** evaluate touch/scroll interactions and reposition the panel
   for small screens rather than merely shrinking the desktop canvas.
6. **Quality/performance:** measure graph render and animation CPU use, fix regressions.
7. **Cross-page system:** apply the editorial layout consistently to About,
   How It Works, Evidence and Feedback.

## Acceptance checklist
- [ ] The app starts successfully on the target Shiny runtime
- [ ] Three hero buttons select intended nodes
- [ ] Existing search and reset still work
- [ ] Clicking a node or edge still opens the correct panel
- [ ] The map and panel do not obscure each other on mobile
- [ ] Contrasts and focus indicators are accessible
- [ ] Reduced-motion users do not receive automatic smooth scrolling
- [ ] Data, source citations and scientific explanations are unchanged
- [ ] No changes land on main without review

This is an initial design implementation, not a fully validated production redesign.

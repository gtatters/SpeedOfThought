# The Speed of a Thought — Reaction Time Lab

An interactive Shiny app for measuring and analysing human reaction time,
developed for **SCIE 1P01: Introduction to Scientific Methods** at Brock University.

Students test their own reaction time under three conditions directly in the
browser, conduct a physical ruler-drop experiment, and analyse the resulting
distributions — individually and as a class.

---

## Live App

> 🔗 **[https://gtatters.github.io/SpeedOfThought]**

Runs in any modern web browser. No installation required.

---

## What the App Does

### Three in-browser reaction time conditions

| Tab | Condition | What happens |
|-----|-----------|--------------|
| **Simple RT** | One signal, one response | Screen flashes green — tap anywhere as fast as possible |
| **Spatial RT** | Locate and respond | A green circle appears at a random position — tap the circle |
| **Choice RT** | Decide then respond | A green circle and orange square appear — tap only the circle |

The number of trials per condition is adjustable (5–30) via a slider. All game
logic runs in JavaScript — there is no server round-trip during gameplay, so
the app is responsive even on a slow connection.

### Ruler Drop companion experiment

The **Ruler Drop** tab explains the classic physical method for measuring
reaction time without electronics:

- Partner A drops a ruler without warning; Partner B catches it
- Distance fallen (cm) is converted to milliseconds using **d = ½gt²**
- The tab covers the physics of free fall, the neuroscience of the
  visual-motor pathway, and the concept of **systematic error** arising
  from the visual perceptual threshold

Students enter their cm measurements and the app converts them to ms
automatically.

### Your Results

Once any condition is complete, the **Your Results** tab shows:

- A bar chart comparing mean ± SD across all conditions
- Side-by-side histograms for each condition with an **ex-Gaussian
  distribution** fitted by method of moments and overlaid as a dashed curve
- A plain-language explanation of the ex-Gaussian parameters (μ, σ, τ) and
  what τ means psychologically
- A summary statistics table including τ for each condition
- A **summary code** encoding each student's mean, SD, and n — used for
  class data pooling

### Class Data

The **Class Data** tab offers three ways to pool results across an entire class
(since each student's app runs independently with no shared state):

| Sub-tab | Method |
|---------|--------|
| **Enter class means** | Instructor types one mean per student per condition |
| **Use a summary code** | Each student reads their code; instructor pastes all codes in |
| **Simulated class demo** | Realistic simulated data to preview the class-level analysis before the lab |

Class results are displayed as boxplots with jittered individual points.

### References

A dedicated **References** tab cites the foundational literature:

- **Donders (1868/1969)** — the subtraction method and mental chronometry
- **Hick (1952)** — choice RT increases logarithmically with number of
  alternatives (Hick's Law)
- **Hyman (1953)** — stimulus information as a determinant of RT
- **Hohle (1965)** — ex-Gaussian components of reaction time
- **Ratcliff (1979)** — group RT distributions and distribution statistics
- **Kandel et al. (2021)** — neural pathway of the visual-motor response

---

## Scientific Background

### Why do the three conditions differ?

Simple RT < Spatial RT < Choice RT. This progression replicates **Donders'
subtraction method** (1868): each additional cognitive step — locating the
target, then deciding between targets — adds measurable time. Students
observe **Hick's Law** in their own data without being told to expect it.

### Why is the distribution skewed?

Reaction time distributions are not normal. They have a hard left boundary
(neural conduction sets a physical floor on how fast you can respond) and a
long right tail from occasional slow or distracted trials. The
**ex-Gaussian distribution** — a Normal plus an Exponential component —
describes this shape well.

The three parameters carry psychological meaning:

| Parameter | Meaning |
|-----------|---------|
| **μ (mu)** | Mean of the Normal component — your typical fast responses |
| **σ (sigma)** | SD of the Normal component — consistency of fast responses |
| **τ (tau)** | Mean of the Exponential component — attentional lapses and decision difficulty |

τ is expected to be largest in the Choice RT condition, where the decision
step pulls more trials into the slow tail.

### The ruler drop and systematic error

The ruler is in free fall from the instant it is released, so the kinematic
formula is exact. However, the visual system requires the ruler to move a
small distance before motion is consciously detected (the **perceptual
threshold**). This means the ruler has already fallen a few millimetres before
the catcher perceives it moving, causing a small systematic underestimate of
true RT. Because this bias is consistent across all trials, comparisons
between conditions remain valid — but it introduces a useful discussion of
systematic vs. random error.

---

## Design Notes

- **Colour palette:** RColorBrewer Dark2 (consistent with companion biostats
  apps developed for BIOL 3P96)
- **iPad first:** layout and touch targets designed for tablet use during
  lectures; the RT game area is 360px tall with large tap zones
- **No dependencies beyond base R:** all plots use base R graphics; no
  `ggplot2`, `tidyverse`, or other packages required
- **JS architecture:** game logic runs entirely client-side during play;
  results are sent to the R server only once per completed run via
  `Shiny.setInputValue()`, making the app responsive on any connection speed

---

## Intended Use

This app is designed for **first-year science students** with no prior
statistics or neuroscience background. Explanatory text throughout the app
avoids mathematical notation where possible and connects every concept to the
student's own data.

Suggested lab flow (approximately 45–60 minutes):

1. Students complete all three in-app RT conditions (~15 min)
2. Students conduct the ruler-drop experiment with a partner (~10 min)
3. Students explore their **Your Results** tab and note their summary code (~10 min)
4. Class data is pooled via one of the three Class Data methods (~5 min)
5. Class-level patterns are discussed: Hick's Law, individual differences,
   the ex-Gaussian shape (~15 min)

---

## Author

**Glenn Tattersall, PhD**  
Department of Biological Sciences  
Brock University  

Developed for SCIE 1P01 — Introduction to Scientific Methods  
Faculty of Mathematics and Science, Brock University

---

## References

Donders, F. C. (1868/1969). On the speed of mental processes. *Acta
Psychologica, 30*, 412–431.

Hick, W. E. (1952). On the rate of gain of information. *Quarterly Journal
of Experimental Psychology, 4*, 11–26.

Hohle, R. H. (1965). Inferred components of reaction times as functions of
foreperiod duration. *Journal of Experimental Psychology, 69*, 382–386.

Hyman, R. (1953). Stimulus information as a determinant of reaction time.
*Journal of Experimental Psychology, 45*, 188–196.

Kandel, E. R., Koester, J. D., Mack, S. H., & Siegelbaum, S. A. (Eds.).
(2021). *Principles of Neural Science* (6th ed.). McGraw-Hill.

Proctor, R. W., & Schneider, D. W. (2018). Hick's law for choice reaction
time: A review. *Quarterly Journal of Experimental Psychology, 71*,
1281–1299.

Ratcliff, R. (1979). Group reaction time distributions and an analysis of
distribution statistics. *Psychological Bulletin, 86*, 446–461.
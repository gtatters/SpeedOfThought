# =========================================================
# Shiny App: Reaction Time — The Speed of a Thought
# SCIE 1P01 Neuroscience Lab
# Requires: shiny only (all plots use base R graphics)
#
# Conditions:
#   1. Simple RT   — flash anywhere, tap anywhere
#   2. Spatial RT  — shape at random location, tap that shape
#   3. Choice RT   — two shapes, tap only the circle
#
# Additional tabs:
#   4. Ruler Drop  — physics explanation + cm -> ms converter
#   5. Your Results — compare all conditions, ex-Gaussian
#   6. Class Data  — three sub-tabs for pooling class results
#
# Architecture note:
#   All game logic runs entirely in JavaScript — no server
#   round-trip during gameplay. Results are sent to Shiny
#   once per completed run via Shiny.setInputValue().
# =========================================================

#shinylive::export(appdir = "../SpeedOfThought/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)

# ---------------------------
# Colour palette (Dark2, consistent with other apps)
# ---------------------------
col_simple  <- "#1B9E77"   # Dark2 green
col_spatial <- "#D95F02"   # Dark2 orange
col_choice  <- "#7570B3"   # Dark2 purple
col_ruler   <- "#E7298A"   # Dark2 pink
col_bg      <- "#f9f9f9"

# ---------------------------
# ex-Gaussian density (base R only)
# f(x) = (1/tau)*exp((mu + sigma^2/(2*tau) - x)/tau)*pnorm(...)
# ---------------------------
dexgauss <- function(x, mu, sigma, tau) {
  if (tau <= 0 || sigma <= 0) return(rep(0, length(x)))
  rate <- 1 / tau
  rate * exp(rate * (mu + (sigma^2 * rate) / 2 - x)) *
    pnorm((x - mu - sigma^2 * rate) / sigma)
}

# Fit ex-Gaussian by method of moments
fit_exgauss <- function(x) {
  x <- x[is.finite(x) & x > 0]
  if (length(x) < 4) return(list(mu = mean(x), sigma = max(sd(x),1), tau = 0))
  m  <- mean(x)
  v  <- var(x)
  n  <- length(x)
  sk <- (sum((x - m)^3) / n) / (v^1.5)
  sk <- max(sk, 0.01)
  tau    <- (sk * v^1.5 / 2)^(1/3)
  sigma2 <- max(v - tau^2, 1)
  mu     <- m - tau
  list(mu = max(mu, 50), sigma = sqrt(sigma2), tau = max(tau, 1))
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .rt-area {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 100%;
        height: 360px;
        border-radius: 12px;
        cursor: pointer;
        user-select: none;
        -webkit-user-select: none;
        font-size: 1.5em;
        font-weight: bold;
        color: white;
        text-align: center;
        padding: 20px;
        box-sizing: border-box;
        position: relative;
      }
      .rt-idle     { background-color: #607d8b; }
      .rt-waiting  { background-color: #455a64; }
      .rt-early    { background-color: #b71c1c; }
      .rt-go       { background-color: #1b5e20; }
      .rt-done     { background-color: #263238; }
      .shape-target {
        width: 90px; height: 90px;
        border-radius: 50%;
        background-color: #1B9E77;
        position: absolute;
        cursor: pointer;
        border: 4px solid white;
        box-sizing: border-box;
      }
      .shape-distractor {
        width: 90px; height: 90px;
        background-color: #D95F02;
        position: absolute;
        cursor: pointer;
        border: 4px solid white;
        box-sizing: border-box;
      }
      .btn-start {
        background-color: #2196F3;
        color: white; border: none;
        padding: 8px 20px; font-size: 15px;
        border-radius: 4px; cursor: pointer;
      }
      .btn-start:hover { background-color: #1769aa; }
      .result-box {
        border: 1px solid #d0d0d0;
        border-radius: 6px;
        padding: 12px 16px;
        background-color: #f9f9f9;
        margin-top: 8px;
        font-size: 95%;
      }
    ")),
    
    tags$script(HTML("
// =============================================================
// Reaction-time game engine (pure JS, no server round-trips)
// =============================================================
var rt = {
  cond    : null,
  phase   : 'idle',   // idle | waiting | go
  t0      : null,
  trials  : [],
  n       : 10,
  current : 0,
  timer   : null
};

// Called directly by the Start button onclick
function startRT(cond) {
  // Clear any running timer from a previous run
  if (rt.timer) { clearTimeout(rt.timer); rt.timer = null; }

  rt.cond    = cond;
  rt.trials  = [];
  rt.current = 0;
  rt.phase   = 'idle';

  // Read trial count from the slider input value
  var sliderEl = document.getElementById('n_trials_' + cond);
  rt.n = sliderEl ? parseInt(sliderEl.value) : 10;

  nextTrial();
}

function nextTrial() {
  var area = document.getElementById('rt_area_' + rt.cond);
  if (!area) return;

  rt.phase = 'waiting';
  area.className = 'rt-area rt-waiting';

  var msg = {
    simple  : 'Get ready\u2026<br><small>Tap anywhere when the screen turns green</small>',
    spatial : 'Get ready\u2026<br><small>Tap the green circle when it appears</small>',
    choice  : 'Get ready\u2026<br><small>Tap the <strong>circle</strong> \u2014 ignore the square</small>'
  };
  area.innerHTML = '<div>' + msg[rt.cond] + '</div>';

  var delay = 1000 + Math.random() * 2500;
  rt.timer = setTimeout(showGo, delay);
}

function showGo() {
  rt.timer = null;
  var area = document.getElementById('rt_area_' + rt.cond);
  if (!area) return;

  rt.phase = 'go';
  rt.t0    = performance.now();

  if (rt.cond === 'simple') {
    area.className   = 'rt-area rt-go';
    area.innerHTML   = '<div>TAP NOW!</div>';

  } else if (rt.cond === 'spatial') {
    area.className = 'rt-area rt-idle';
    area.innerHTML = '';
    var c = makeCircle(true);
    // Random position: keep circle inside area (90px wide)
    var rect = area.getBoundingClientRect();
    var maxL = Math.max(0, rect.width  - 90);
    var maxT = Math.max(0, rect.height - 90);
    c.style.left = (10 + Math.random() * (maxL - 20)) + 'px';
    c.style.top  = (10 + Math.random() * (maxT - 20)) + 'px';
    area.appendChild(c);

  } else {  // choice
    area.className = 'rt-area rt-idle';
    area.innerHTML = '';
    var rect2 = area.getBoundingClientRect();
    var maxL2 = Math.max(0, rect2.width  - 90);
    var maxT2 = Math.max(0, rect2.height - 90);

    var circle = makeCircle(true);
    var square  = makeSquare();

    // Place them in non-overlapping halves
    var useLeft = Math.random() < 0.5;
    circle.style.left = (useLeft ? 5  : (maxL2/2 + 10)) + 'px';
    circle.style.top  = (15 + Math.random() * (maxT2 - 30)) + 'px';
    square.style.left = (useLeft ? (maxL2/2 + 10) : 5) + 'px';
    square.style.top  = (15 + Math.random() * (maxT2 - 30)) + 'px';

    area.appendChild(circle);
    area.appendChild(square);
  }
}

function makeCircle(isTarget) {
  var el = document.createElement('div');
  el.className = 'shape-target';
  el.onclick = function(e) {
    e.stopPropagation();
    recordTap(isTarget);
  };
  return el;
}

function makeSquare() {
  var el = document.createElement('div');
  el.className = 'shape-distractor';
  el.onclick = function(e) {
    e.stopPropagation();
    recordTap(false);
  };
  return el;
}

// Main tap handler — called by area onclick (simple) or shape onclick
function areaTap(cond) {
  if (rt.cond !== cond) return;
  if (rt.phase === 'waiting') {
    tooEarly();
  } else if (rt.phase === 'go' && cond === 'simple') {
    recordTap(true);
  }
  // spatial/choice taps handled by shape onclick
}

function recordTap(isTarget) {
  if (rt.phase !== 'go') return;
  var elapsed = Math.round(performance.now() - rt.t0);

  if (!isTarget) {
    // Wrong shape in choice task
    wrongTarget();
    return;
  }

  rt.phase = 'done';
  rt.trials.push(elapsed);
  rt.current++;

  var area = document.getElementById('rt_area_' + rt.cond);
  if (area) {
    area.className = 'rt-area rt-done';
    area.innerHTML = '<div>' + elapsed + ' ms' +
      '<br><small>Trial ' + rt.current + ' of ' + rt.n + '</small></div>';
  }

  // Update live trial list in sidebar
  Shiny.setInputValue('rt_live_' + rt.cond,
    { trials: rt.trials.slice(), ts: Date.now() });

  if (rt.current >= rt.n) {
    finishRun();
  } else {
    rt.timer = setTimeout(nextTrial, 900);
  }
}

function tooEarly() {
  if (rt.timer) { clearTimeout(rt.timer); rt.timer = null; }
  rt.phase = 'waiting';
  var area = document.getElementById('rt_area_' + rt.cond);
  if (area) {
    area.className = 'rt-area rt-early';
    area.innerHTML = '<div>Too soon!<br><small>Wait for the signal.</small></div>';
  }
  rt.timer = setTimeout(nextTrial, 1200);
}

function wrongTarget() {
  if (rt.timer) { clearTimeout(rt.timer); rt.timer = null; }
  var area = document.getElementById('rt_area_' + rt.cond);
  if (area) {
    area.className = 'rt-area rt-early';
    area.innerHTML = '<div>Wrong shape!<br><small>Tap the <strong>circle</strong>.</small></div>';
  }
  rt.phase = 'waiting';
  rt.timer = setTimeout(nextTrial, 1200);
}

function finishRun() {
  // Send final results to Shiny server
  Shiny.setInputValue('rt_done_' + rt.cond,
    { trials: rt.trials.slice(), ts: Date.now() });

  var area = document.getElementById('rt_area_' + rt.cond);
  if (area) {
    area.className = 'rt-area rt-done';
    var mean = Math.round(rt.trials.reduce(function(a,b){return a+b;},0) / rt.trials.length);
    area.innerHTML = '<div>Done! \u2705<br>' +
      '<small>Mean = ' + mean + ' ms &nbsp;&mdash;&nbsp; ' +
      'See <em>Your Results</em> tab for full analysis.</small></div>';
  }
  rt.phase = 'idle';
}
    "))
  ),  # end tags$head
  
  titlePanel("Reaction Time: The Speed of a Thought",
             windowTitle = "Reaction Time Lab"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # =========================================================
    # TAB 1: Simple RT
    # =========================================================
    tabPanel("Simple RT",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Simple Reaction Time"),
                 helpText("The whole screen will turn green. Tap anywhere as fast as you can."),
                 br(),
                 sliderInput("n_trials_simple", "Number of trials:",
                             min = 5, max = 30, value = 10, step = 1),
                 br(),
                 tags$button("Start", class = "btn-start",
                             onclick = "startRT('simple')"),
                 br(), br(),
                 helpText("Tip: rest your finger just above the screen and lift slightly —
                    the instant it turns green, tap."),
                 br(),
                 uiOutput("simple_side"),
                 uiOutput("simple_code"),
                 br(),
                 helpText("_____________________"),
                 helpText("Glenn Tattersall, PhD"),
                 helpText("For use in SCIE 1P01")
               ),
               mainPanel(
                 width = 9,
                 br(),
                 div(id    = "rt_area_simple",
                     class = "rt-area rt-idle",
                     onclick = "areaTap('simple')",
                     div("Press Start to begin")
                 ),
                 br(),
                 uiOutput("simple_table_ui")
               )
             )
    ),
    
    # =========================================================
    # TAB 2: Spatial RT
    # =========================================================
    tabPanel("Spatial RT",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Spatial Reaction Time"),
                 helpText("A green circle will appear somewhere on the screen.
                    Tap the circle as fast as you can."),
                 br(),
                 sliderInput("n_trials_spatial", "Number of trials:",
                             min = 5, max = 30, value = 10, step = 1),
                 br(),
                 tags$button("Start", class = "btn-start",
                             onclick = "startRT('spatial')"),
                 br(), br(),
                 helpText("Tip: unlike Simple RT, you must move your finger to the circle —
                    that movement time is included. Notice how your times compare."),
                 br(),
                 uiOutput("spatial_side"),
                 uiOutput("spatial_code"),
                 br(),
                 helpText("_____________________"),
                 helpText("Glenn Tattersall, PhD"),
                 helpText("For use in SCIE 1P01")
               ),
               mainPanel(
                 width = 9,
                 br(),
                 div(id    = "rt_area_spatial",
                     class = "rt-area rt-idle",
                     onclick = "areaTap('spatial')",
                     div("Press Start to begin")
                 ),
                 br(),
                 uiOutput("spatial_table_ui")
               )
             )
    ),
    
    # =========================================================
    # TAB 3: Choice RT
    # =========================================================
    tabPanel("Choice RT",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Choice Reaction Time"),
                 helpText("A green circle AND an orange square will appear.
                    Tap the CIRCLE only. Tapping the square restarts the trial."),
                 br(),
                 sliderInput("n_trials_choice", "Number of trials:",
                             min = 5, max = 30, value = 10, step = 1),
                 br(),
                 tags$button("Start", class = "btn-start",
                             onclick = "startRT('choice')"),
                 br(), br(),
                 helpText("Tip: your brain must identify which shape is which before
                    responding. That decision step takes real, measurable time."),
                 br(),
                 uiOutput("choice_side"),
                 uiOutput("choice_code"),
                 br(),
                 helpText("_____________________"),
                 helpText("Glenn Tattersall, PhD"),
                 helpText("For use in SCIE 1P01")
               ),
               mainPanel(
                 width = 9,
                 br(),
                 div(id    = "rt_area_choice",
                     class = "rt-area rt-idle",
                     onclick = "areaTap('choice')",
                     div("Press Start to begin")
                 ),
                 br(),
                 uiOutput("choice_table_ui")
               )
             )
    ),
    
    # =========================================================
    # TAB 4: Ruler Drop
    # =========================================================
    tabPanel("Ruler Drop",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Your Measurements"),
                 helpText("Enter ruler drop distances in centimetres, one per line.
                    The app converts each to milliseconds automatically."),
                 br(),
                 textAreaInput("ruler_raw",
                               label       = "Drop distances (cm), one per line:",
                               value       = "",
                               rows        = 10,
                               placeholder = "e.g.\n14.2\n11.8\n16.0\n12.5"),
                 br(),
                 helpText("_____________________"),
                 helpText("Glenn Tattersall, PhD"),
                 helpText("For use in SCIE 1P01")
               ),
               mainPanel(
                 width = 9,
                 h3("The Ruler Drop Experiment"),
                 wellPanel(
                   h4("What you need"),
                   p("A 30 cm ruler, a partner. No other equipment required."),
                   h4("Procedure"),
                   tags$ol(
                     tags$li("Partner A holds the ruler vertically with the zero end down,
                       and drops it without warning."),
                     tags$li("Partner B positions thumb and index finger just below the zero
                       mark (without touching) and catches it the instant they see
                       it move."),
                     tags$li("Read the distance fallen in cm from the bottom of the catch."),
                     tags$li("Record 10 trials, then switch roles."),
                     tags$li(HTML("Then repeat while Partner B does mental arithmetic
                       out loud (count backward from 100 by 7s).
                       This is the <em>distracted</em> condition."))
                   )
                 ),
                 wellPanel(
                   h4(HTML("The Physics: centimetres \u2192 milliseconds")),
                   p("A dropped ruler is in free fall, so the distance it travels depends
               only on gravity and time:"),
                   p(HTML("<div style='text-align:center; font-size:1.15em; margin:10px 0;'>
                   <strong>d &nbsp;=&nbsp; &frac12; g t&sup2;</strong>
                   </div>")),
                   p(HTML("where <strong>d</strong> is the drop distance in metres,
                   <strong>g</strong> = 9.81 m/s&sup2;, and <strong>t</strong>
                   is time in seconds. Rearranging:")),
                   p(HTML("<div style='text-align:center; font-size:1.15em; margin:10px 0;'>
                   <strong>t &nbsp;=&nbsp; &radic;(2d / g)</strong>
                   </div>")),
                   p(HTML("Example: a drop of 15 cm = 0.15 m gives &nbsp;
                   t = &radic;(2 &times; 0.15 / 9.81) &asymp; 0.175 s
                   = <strong>175 ms</strong>.")),
                   p("The table on the left converts all your measurements automatically.")
                 ),
                 wellPanel(
                   h4("The Neuroscience"),
                   p("When the ruler drops, light hits your retina and a signal travels
               along the optic nerve to your visual cortex — roughly 20-40 ms for
               that step alone. Your brain then processes the motion, sends a signal
               down your spinal cord to your arm muscles, and they contract. Each
               step adds time. The total chain — see, decide, move — takes around
               150-250 ms. This is your neural conduction time, and it sets a hard
               lower limit on how fast you can ever respond."),
                   p("Distraction adds time because attention is a limited resource. When
               part of your brain is occupied with arithmetic, fewer resources are
               available to monitor the ruler. You will likely see a longer mean RT
               and more variable times in the distracted condition."),
                   p(HTML("A subtle but important point: the ruler is in free fall from
               the <em>instant</em> it is released, so the formula d&nbsp;=&nbsp;&frac12;gt&sup2;
               is exact. However, your visual system needs a small amount of time
               to detect that motion has begun — the ruler must move a few
               millimetres before your visual cortex registers it as moving. This
               is called a <strong>perceptual threshold</strong>. It means your
               ruler has already fallen a short distance before you consciously
               perceive it dropping, so your computed RTs are very slightly faster
               than your true neural reaction time. This bias is small (a few
               milliseconds) and affects every trial equally, so comparisons
               between conditions — attentive vs. distracted, dominant vs.
               non-dominant hand — are still completely valid. It is a good
               example of <strong>systematic error</strong>: a consistent offset
               that does not add random noise, but does shift all measurements
               in the same direction."))
                 ),
                 br(),
                 uiOutput("ruler_results")
               )
             )
    ),
    
    # =========================================================
    # TAB 5: Your Results
    # =========================================================
    tabPanel("Your Results",
             fluidPage(
               br(),
               wellPanel(
                 h4("How to use this tab"),
                 p("Complete at least one condition (Simple, Spatial, or Choice RT, or
             the Ruler Drop) then return here. All your data are combined
             automatically.")
               ),
               h4("Mean reaction time by condition"),
               plotOutput("results_comparison", height = "300px"),
               br(),
               h4("Distribution of your reaction times"),
               plotOutput("results_distributions", height = "360px"),
               br(),
               wellPanel(
                 h4("The shape of reaction time data: the ex-Gaussian distribution"),
                 p("Look at the histograms above. They are probably not symmetric —
             there is a longer tail stretching to the right. Occasional trials
             where you were briefly distracted, blinked, or just slow pull
             the tail out. But you can never respond faster than your nervous
             system allows, so the left edge has a hard physical limit.
             This combination of a roughly bell-shaped core with a right tail
             is described mathematically by the ex-Gaussian distribution."),
                 p(HTML("The ex-Gaussian is the sum of two components: a
             <strong>Normal distribution</strong> (mean &mu;, standard deviation
             &sigma;) capturing your typical fast responses, and an
             <strong>Exponential distribution</strong> (mean &tau;, pronounced
             'tau') capturing the occasional slow trials that form the long
             right tail.")),
                 p(HTML("The parameter &tau; is psychologically meaningful: a larger
             &tau; means more attentional lapses or more decision difficulty.
             You may find that your Choice RT condition has a noticeably larger
             &tau; than your Simple RT condition — the extra decision step pulls
             more trials into the slow tail.")),
                 p("The dashed curves overlaid on your histograms are ex-Gaussian fits
             estimated from your own data.")
               ),
               h4("Summary statistics"),
               tableOutput("results_table"),
               br(),
               wellPanel(
                 h4("Your summary code for class data sharing"),
                 p("Once you have completed your conditions, copy this code and share it
             with your instructor (read it aloud, type it into a shared document,
             or photograph it). The code encodes your mean, SD, and number of
             trials for each condition. The instructor pastes all student codes
             into the Class Data tab to build the class-wide analysis."),
                 p("Format: each condition is ", code("Label:mean,sd,n"),
                   " — for example ", code("S:245,32,10"), " means Simple RT,
             mean 245 ms, SD 32 ms, 10 trials."),
                 verbatimTextOutput("my_summary_code"),
                 helpText("Conditions present in the code: S = Simple, P = Spatial,
                    C = Choice, R = Ruler Drop. NA means that condition has
                    not been completed yet.")
               ),
               br()
             )
    ),
    
    # =========================================================
    # TAB 6: Class Data
    # =========================================================
    tabPanel("Class Data",
             fluidPage(
               br(),
               h3("Pooling Results Across the Class"),
               p("Because the app runs independently on each device, we need a way
           to combine everyone's results. Choose the method that matches how
           your class is organised."),
               br(),
               tabsetPanel(
                 
                 # Sub-tab A: manual means
                 tabPanel("Enter class means",
                          br(),
                          wellPanel(
                            h4("How this works"),
                            p("Each student reads their mean RT for each condition from the
                 'Your Results' tab. A volunteer enters everyone's means below,
                 one value per line per condition.")
                          ),
                          fluidRow(
                            column(4,
                                   h5("Simple RT means (ms):"),
                                   textAreaInput("class_simple",  NULL, rows = 6,
                                                 placeholder = "e.g.\n245\n198\n312"),
                                   h5("Spatial RT means (ms):"),
                                   textAreaInput("class_spatial", NULL, rows = 6,
                                                 placeholder = "e.g.\n320\n275\n410"),
                                   h5("Choice RT means (ms):"),
                                   textAreaInput("class_choice",  NULL, rows = 6,
                                                 placeholder = "e.g.\n410\n380\n520"),
                                   h5("Ruler drop means (ms):"),
                                   textAreaInput("class_ruler",   NULL, rows = 6,
                                                 placeholder = "e.g.\n180\n165\n210")
                            ),
                            column(8,
                                   br(),
                                   plotOutput("class_plot_manual", height = "400px"),
                                   br(),
                                   tableOutput("class_table_manual")
                            )
                          )
                 ),
                 
                 # Sub-tab B: summary codes
                 tabPanel("Use a summary code",
                          br(),
                          wellPanel(
                            h4("How this works"),
                            p("Each student's app generates a compact summary code below.
                 The instructor collects all codes (e.g. on a whiteboard or
                 shared document) and pastes them into the box. The app
                 decodes and plots the class data."),
                            p(strong("Your summary code:")),
                            verbatimTextOutput("my_summary_code")
                          ),
                          fluidRow(
                            column(4,
                                   h5("Paste all student codes here, one per line:"),
                                   textAreaInput("class_codes", NULL, rows = 14,
                                                 placeholder = "S:245|P:318|C:412|R:178\nS:198|P:275|C:380|R:165")
                            ),
                            column(8,
                                   br(),
                                   plotOutput("class_plot_codes", height = "400px"),
                                   br(),
                                   tableOutput("class_table_codes")
                            )
                          )
                 ),
                 
                 # Sub-tab C: simulated demo
                 tabPanel("Simulated class demo",
                          br(),
                          wellPanel(
                            h4("What this shows"),
                            p("Simulates a class of students with realistic reaction times
                 so you can preview what the class-level analysis looks like
                 before collecting real data.")
                          ),
                          fluidRow(
                            column(3,
                                   sliderInput("sim_n", "Number of students:", 10, 80, 30, step = 1),
                                   sliderInput("sim_sd", "Between-student variability (ms):",
                                               5, 80, 30, step = 5),
                                   br(),
                                   helpText("Typical population means used: Simple ~230 ms,
                          Spatial ~320 ms, Choice ~430 ms, Ruler ~190 ms.")
                            ),
                            column(9,
                                   plotOutput("class_plot_sim", height = "400px"),
                                   br(),
                                   tableOutput("class_table_sim")
                            )
                          )
                 )
               )
             )
    ),
    
    # =========================================================
    # TAB 7: References
    # =========================================================
    tabPanel("References",
             fluidPage(
               br(),
               h3("Scientific Background & References"),
               p("This app is grounded in over 150 years of research on human reaction
           time. The key ideas and their original sources are listed below."),
               br(),
               
               wellPanel(
                 h4("The subtraction method and mental chronometry"),
                 p(HTML("The idea that different mental operations take measurable time —
             and that comparing reaction times across tasks can reveal the duration
             of specific cognitive processes — was established by the Dutch
             physiologist Franciscus Donders in 1868. Donders showed that simple
             RT (detect a stimulus and respond) is always faster than choice RT
             (identify which stimulus appeared and select the correct response),
             and used the difference to estimate the time taken by the decision
             process itself. This <em>subtraction method</em> is still used in
             cognitive neuroscience today.")),
                 p(HTML("<strong>Donders, F. C.</strong> (1868/1969). On the speed of
             mental processes. <em>Acta Psychologica, 30</em>, 412–431.
             (Original work published 1868; translated by W. Koster, 1969.)")),
                 p(HTML("The three conditions in this app — Simple RT, Spatial RT, and
             Choice RT — directly parallel Donders' original A-, B-, and
             C-tasks. The progression from simple to choice RT that you observe
             in your own data replicates a result that has been confirmed
             thousands of times since 1868."))
               ),
               
               wellPanel(
                 h4("Hick's Law: why choice slows you down"),
                 p(HTML("In 1952, W. E. Hick published a landmark study showing that
             choice reaction time increases systematically with the number of
             possible stimulus-response alternatives — and that the relationship
             is <em>logarithmic</em>, not linear. Adding a second possible target
             does not double your RT; it adds a fixed increment. This relationship,
             now called <strong>Hick's Law</strong> (or the Hick-Hyman Law after
             Ray Hyman's 1953 follow-up), is one of the few widely accepted
             quantitative laws in psychology, and it is still used today in
             human-computer interface design.")),
                 p(HTML("<strong>Hick, W. E.</strong> (1952). On the rate of gain of
             information. <em>Quarterly Journal of Experimental Psychology,
             4</em>, 11–26.")),
                 p(HTML("<strong>Hyman, R.</strong> (1953). Stimulus information as a
             determinant of reaction time. <em>Journal of Experimental
             Psychology, 45</em>, 188–196.")),
                 p(HTML("For a modern review: <strong>Proctor, R. W., &amp; Schneider,
             D. W.</strong> (2018). Hick's law for choice reaction time: A review.
             <em>Quarterly Journal of Experimental Psychology, 71</em>, 1281–1299."))
               ),
               
               wellPanel(
                 h4("The ex-Gaussian distribution of reaction times"),
                 p(HTML("Reaction time distributions are not normal — they have a hard
             left boundary (you cannot respond faster than your nervous system
             allows) and a long right tail from occasional slow or distracted
             trials. The <strong>ex-Gaussian distribution</strong>, which combines
             a Normal and an Exponential component, was first applied to RT data
             by Hohle (1965) and later formalised by Ratcliff (1979), whose
             analysis of RT distribution statistics became a foundational
             reference in cognitive psychology.")),
                 p(HTML("The three parameters — &mu; (mu), &sigma; (sigma), and
             &tau; (tau) — each carry psychological meaning. The Normal component
             (&mu;, &sigma;) captures your typical fast, consistent responses.
             The Exponential component (&tau;) captures attentional lapses and
             decision difficulty: a larger &tau; means more trials are pulled
             into the slow tail. Research has shown that &tau; is particularly
             sensitive to attentional demands and is elevated in individuals with
             ADHD compared to controls.")),
                 p(HTML("<strong>Hohle, R. H.</strong> (1965). Inferred components of
             reaction times as functions of foreperiod duration.
             <em>Journal of Experimental Psychology, 69</em>, 382–386.")),
                 p(HTML("<strong>Ratcliff, R.</strong> (1979). Group reaction time
             distributions and an analysis of distribution statistics.
             <em>Psychological Bulletin, 86</em>, 446–461."))
               ),
               
               wellPanel(
                 h4("The ruler drop method"),
                 p(HTML("The ruler drop is a classic psychophysics demonstration that
             converts a physical measurement (how far a ruler falls before you
             catch it) into a reaction time using the kinematics of free fall.
             It requires no electronic equipment and produces results that are
             directly comparable to laboratory RT measurements. The physics
             follows from Newton's second law: a falling object in free fall
             covers distance d = &frac12;gt&sup2;, so t = &radic;(2d/g).
             The method has been used in teaching laboratories for decades and
             appears in numerous introductory neuroscience and physics curricula.")),
                 p(HTML("The neural pathway involved — retina &rarr; optic nerve &rarr;
             visual cortex &rarr; motor cortex &rarr; spinal cord &rarr; hand
             muscles — is described in standard neuroscience texts including:")),
                 p(HTML("<strong>Kandel, E. R., Koester, J. D., Mack, S. H., &amp;
             Siegelbaum, S. A.</strong> (Eds.). (2021).
             <em>Principles of Neural Science</em> (6th ed.). McGraw-Hill."))
               ),
               
               wellPanel(
                 h4("App developed for"),
                 p("SCIE 1P01 — Introduction to Scientific Methods"),
                 p("Faculty of Mathematics and Science, Brock University"),
                 helpText("Glenn Tattersall, PhD — For use in SCIE 1P01")
               )
             )
    )
    
  )  # end tabsetPanel
)    # end fluidPage / ui


# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Reactive stores — updated when JS sends rt_done_* or
  # rt_live_* (live updates trial-by-trial)
  # ----------------------------------------------------------
  rt_simple  <- reactiveVal(numeric(0))
  rt_spatial <- reactiveVal(numeric(0))
  rt_choice  <- reactiveVal(numeric(0))
  
  observeEvent(input$rt_live_simple,  ignoreNULL = TRUE, {
    rt_simple(as.numeric(unlist(input$rt_live_simple$trials)))
  })
  observeEvent(input$rt_live_spatial, ignoreNULL = TRUE, {
    rt_spatial(as.numeric(unlist(input$rt_live_spatial$trials)))
  })
  observeEvent(input$rt_live_choice,  ignoreNULL = TRUE, {
    rt_choice(as.numeric(unlist(input$rt_live_choice$trials)))
  })
  # rt_done_* mirrors rt_live_* but fires only on completion
  observeEvent(input$rt_done_simple,  ignoreNULL = TRUE, {
    rt_simple(as.numeric(unlist(input$rt_done_simple$trials)))
  })
  observeEvent(input$rt_done_spatial, ignoreNULL = TRUE, {
    rt_spatial(as.numeric(unlist(input$rt_done_spatial$trials)))
  })
  observeEvent(input$rt_done_choice,  ignoreNULL = TRUE, {
    rt_choice(as.numeric(unlist(input$rt_done_choice$trials)))
  })
  
  # ----------------------------------------------------------
  # Ruler drop: parse cm text -> ms
  # ----------------------------------------------------------
  ruler_ms <- reactive({
    raw <- input$ruler_raw
    if (is.null(raw) || nchar(trimws(raw)) == 0) return(numeric(0))
    lines <- strsplit(trimws(raw), "\n")[[1]]
    cm    <- suppressWarnings(as.numeric(trimws(lines)))
    cm    <- cm[!is.na(cm) & cm > 0 & cm <= 100]
    round(sqrt(2 * (cm / 100) / 9.81) * 1000, 1)
  })
  
  # ----------------------------------------------------------
  # Sidebar summaries (live, update each trial)
  # ----------------------------------------------------------
  side_summary <- function(vals) {
    if (length(vals) == 0) return(helpText("No data yet."))
    div(class = "result-box",
        p(strong("Trials: "),    length(vals)),
        p(strong("Mean: "),      paste0(round(mean(vals)), " ms")),
        p(strong("SD: "),        paste0(round(sd(vals)),   " ms")),
        p(strong("Min / Max: "), paste0(round(min(vals)), " / ",
                                        round(max(vals)), " ms"))
    )
  }
  
  output$simple_side  <- renderUI(side_summary(rt_simple()))
  output$spatial_side <- renderUI(side_summary(rt_spatial()))
  output$choice_side  <- renderUI(side_summary(rt_choice()))
  
  # ----------------------------------------------------------
  # Trial tables (live)
  # ----------------------------------------------------------
  trial_table_ui <- function(vals, tbl_id) {
    if (length(vals) == 0) return(NULL)
    tagList(
      h5("Trial-by-trial results:"),
      tableOutput(tbl_id)
    )
  }
  
  output$simple_table_ui  <- renderUI(trial_table_ui(rt_simple(),  "simple_tbl"))
  output$spatial_table_ui <- renderUI(trial_table_ui(rt_spatial(), "spatial_tbl"))
  output$choice_table_ui  <- renderUI(trial_table_ui(rt_choice(),  "choice_tbl"))
  
  output$simple_tbl  <- renderTable({
    req(length(rt_simple()) > 0)
    data.frame(Trial = seq_along(rt_simple()),
               `RT (ms)` = rt_simple(), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  
  output$spatial_tbl <- renderTable({
    req(length(rt_spatial()) > 0)
    data.frame(Trial = seq_along(rt_spatial()),
               `RT (ms)` = rt_spatial(), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  
  output$choice_tbl  <- renderTable({
    req(length(rt_choice()) > 0)
    data.frame(Trial = seq_along(rt_choice()),
               `RT (ms)` = rt_choice(), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  
  # ----------------------------------------------------------
  # Ruler drop results panel
  # ----------------------------------------------------------
  output$ruler_results <- renderUI({
    ms <- ruler_ms()
    if (length(ms) == 0)
      return(helpText("Enter measurements on the left to see your results."))
    raw   <- strsplit(trimws(input$ruler_raw), "\n")[[1]]
    cm    <- suppressWarnings(as.numeric(trimws(raw)))
    cm    <- cm[!is.na(cm) & cm > 0 & cm <= 100]
    tagList(
      h4("Your ruler drop results"),
      renderTable(
        data.frame(`Drop (cm)` = round(cm, 1),
                   `RT (ms)`   = ms, check.names = FALSE),
        striped = TRUE, spacing = "xs"
      ),
      wellPanel(
        p(strong("Mean RT: "),   paste0(round(mean(ms), 1), " ms")),
        p(strong("SD: "),        paste0(round(sd(ms),   1), " ms")),
        p(strong("Min / Max: "), paste0(round(min(ms), 1), " / ",
                                        round(max(ms), 1), " ms"))
      )
    )
  })
  
  # ----------------------------------------------------------
  # YOUR RESULTS helpers
  # ----------------------------------------------------------
  all_data <- reactive({
    list(simple  = rt_simple(),
         spatial = rt_spatial(),
         choice  = rt_choice(),
         ruler   = ruler_ms())
  })
  
  cond_names <- c("Simple", "Spatial", "Choice", "Ruler Drop")
  cond_cols  <- c(col_simple, col_spatial, col_choice, col_ruler)
  
  # Bar chart: mean ± SD
  output$results_comparison <- renderPlot({
    d     <- all_data()
    means <- sapply(d, function(x) if (length(x) >= 2) mean(x) else NA)
    sds   <- sapply(d, function(x) if (length(x) >= 2) sd(x)   else NA)
    
    if (all(is.na(means))) {
      plot.new()
      text(0.5, 0.5, "Complete at least one condition first.", cex = 1.3)
      return()
    }
    op <- par(mar = c(4, 5, 2, 2), bg = col_bg)
    on.exit(par(op))
    ylim_top <- max(means + sds, na.rm = TRUE) * 1.25
    bp <- barplot(means,
                  names.arg = cond_names,
                  col       = cond_cols,
                  border    = NA,
                  ylim      = c(0, ylim_top),
                  ylab      = "Mean reaction time (ms)",
                  main      = "Mean RT by condition  (\u00b1 1 SD)",
                  cex.names = 1.1, cex.axis = 1.0, cex.lab = 1.1,
                  las = 1)
    arrows(bp, means - sds, bp, means + sds,
           angle = 90, code = 3, length = 0.08, lwd = 2)
    text(bp, means + sds + ylim_top * 0.04,
         labels = ifelse(is.na(means), "", paste0(round(means), " ms")),
         cex = 0.95, font = 2)
  }, bg = col_bg)
  
  # Histograms + ex-Gaussian overlay
  output$results_distributions <- renderPlot({
    d        <- all_data()
    has_data <- sapply(d, function(x) length(x) >= 5)
    
    if (!any(has_data)) {
      plot.new()
      text(0.5, 0.5,
           "Complete at least one condition (min 5 trials)\nto see distributions.",
           cex = 1.2)
      return()
    }
    n_pan <- sum(has_data)
    op    <- par(mfrow = c(1, n_pan), mar = c(4, 4, 3, 1), bg = col_bg)
    on.exit(par(op))
    
    for (i in seq_along(d)) {
      x <- d[[i]]
      if (length(x) < 5) next
      fit  <- fit_exgauss(x)
      xseq <- seq(max(0, min(x) - 50), max(x) + 100, length.out = 300)
      dens <- dexgauss(xseq, fit$mu, fit$sigma, fit$tau)
      h    <- hist(x, plot = FALSE, breaks = "Sturges")
      ylim_top <- max(max(h$density), max(dens, na.rm = TRUE)) * 1.2
      
      hist(x,
           freq     = FALSE,
           col      = adjustcolor(cond_cols[i], 0.5),
           border   = adjustcolor(cond_cols[i], 0.85),
           main     = cond_names[i],
           xlab     = "RT (ms)", ylab = "Density",
           ylim     = c(0, ylim_top),
           cex.main = 1.1, cex.lab = 1.0, las = 1)
      
      lines(xseq, dens, col = "black", lwd = 2, lty = 2)
      
      legend("topright", bty = "n", cex = 0.78,
             legend = c("Your data",
                        bquote(mu==.(round(fit$mu))~~sigma==.(round(fit$sigma))~~tau==.(round(fit$tau)))),
             col  = c(adjustcolor(cond_cols[i], 0.7), "black"),
             lty  = c(1, 2), lwd = c(6, 2))
    }
  }, bg = col_bg)
  
  # Summary table
  output$results_table <- renderTable({
    d    <- all_data()
    rows <- lapply(seq_along(d), function(i) {
      x <- d[[i]]
      if (length(x) < 2) return(NULL)
      fit <- fit_exgauss(x)
      data.frame(
        Condition   = cond_names[i],
        N           = length(x),
        `Mean (ms)` = round(mean(x), 1),
        `SD (ms)`   = round(sd(x),   1),
        `Min (ms)`  = round(min(x),  1),
        `Max (ms)`  = round(max(x),  1),
        `tau (ms)`  = round(fit$tau, 1),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    })
    rows <- do.call(rbind, rows[!sapply(rows, is.null)])
    if (is.null(rows) || nrow(rows) == 0)
      data.frame(Message = "No data yet. Complete at least one condition.")
    else rows
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  # ----------------------------------------------------------
  # Summary codes — format: Label:mean,sd,n
  # e.g.  S:245,32,10|P:318,45,10|C:412,58,10|R:178,22,10
  # ----------------------------------------------------------
  enc_condition <- function(x, lbl) {
    if (length(x) >= 2)
      paste0(lbl, ":", round(mean(x)), ",", round(sd(x)), ",", length(x))
    else
      paste0(lbl, ":NA")
  }
  
  output$my_summary_code <- renderText({
    d <- all_data()
    paste0(enc_condition(d$simple,  "S"), "|",
           enc_condition(d$spatial, "P"), "|",
           enc_condition(d$choice,  "C"), "|",
           enc_condition(d$ruler,   "R"))
  })
  
  # Per-condition code boxes — appear in sidebar after run completes
  make_code_ui <- function(x, lbl) {
    if (length(x) < 2) return(NULL)
    code_str <- enc_condition(x, lbl)
    div(style = paste("border:2px solid #2196F3; border-radius:6px;",
                      "padding:10px 12px; background:#e3f2fd;",
                      "margin-top:10px;"),
        p(style = "margin:0 0 4px 0; font-size:85%; color:#555;",
          strong("Your code for this condition:")),
        tags$code(style = "font-size:105%; color:#1769aa; word-break:break-all;",
                  code_str),
        p(style = "margin:6px 0 0 0; font-size:80%; color:#777;",
          "Share this with your instructor for class data pooling.")
    )
  }
  
  output$simple_code  <- renderUI(make_code_ui(rt_simple(),  "S"))
  output$spatial_code <- renderUI(make_code_ui(rt_spatial(), "P"))
  output$choice_code  <- renderUI(make_code_ui(rt_choice(),  "C"))
  
  # ----------------------------------------------------------
  # CLASS DATA helpers
  # ----------------------------------------------------------
  parse_means <- function(txt) {
    if (is.null(txt) || nchar(trimws(txt)) == 0) return(numeric(0))
    vals <- suppressWarnings(as.numeric(trimws(strsplit(trimws(txt), "\n")[[1]])))
    vals[!is.na(vals) & vals > 0]
  }
  
  class_boxplot <- function(lst, title = "Class Reaction Times") {
    has <- sapply(lst, length) >= 3
    if (!any(has)) {
      plot.new()
      text(0.5, 0.5, "Enter data on the left to see the class plot.", cex = 1.2)
      return(invisible())
    }
    op <- par(mar = c(4, 5, 3, 2), bg = col_bg)
    on.exit(par(op))
    nms  <- cond_names[has]
    cols <- cond_cols[has]
    vals <- unlist(lst[has])
    grps <- factor(rep(nms, times = sapply(lst[has], length)), levels = nms)
    boxplot(vals ~ grps,
            col      = adjustcolor(cols, 0.55),
            border   = cols,
            main     = title, ylab = "Mean reaction time (ms)",
            outline  = TRUE, notch = FALSE, las = 1,
            cex.axis = 1.0, cex.lab = 1.1, cex.main = 1.2)
    for (i in seq_along(levels(grps))) {
      lv  <- levels(grps)[i]
      pts <- vals[grps == lv]
      if (length(pts) == 0) next
      points(jitter(rep(i, length(pts)), amount = 0.15), pts,
             pch = 19, col = adjustcolor(cols[i], 0.65), cex = 0.9)
    }
  }
  
  class_summary <- function(lst) {
    rows <- lapply(seq_along(lst), function(i) {
      x <- lst[[i]]
      if (length(x) < 2) return(NULL)
      data.frame(Condition = cond_names[i], Students = length(x),
                 `Mean (ms)` = round(mean(x), 1), `SD (ms)` = round(sd(x), 1),
                 `Min (ms)`  = round(min(x),  1), `Max (ms)` = round(max(x), 1),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    rows <- do.call(rbind, rows[!sapply(rows, is.null)])
    if (is.null(rows) || nrow(rows) == 0) data.frame(Message = "No data yet.")
    else rows
  }
  
  # Manual entry
  class_manual <- reactive({
    list(simple  = parse_means(input$class_simple),
         spatial = parse_means(input$class_spatial),
         choice  = parse_means(input$class_choice),
         ruler   = parse_means(input$class_ruler))
  })
  output$class_plot_manual  <- renderPlot(
    class_boxplot(class_manual(), "Class RT by Condition"), bg = col_bg)
  output$class_table_manual <- renderTable(class_summary(class_manual()),
                                           striped = TRUE, spacing = "s")
  
  # Summary codes — parse mean,sd,n format (also accepts legacy mean-only)
  class_from_codes <- reactive({
    raw <- input$class_codes
    if (is.null(raw) || nchar(trimws(raw)) == 0)
      return(list(simple = numeric(0), spatial = numeric(0),
                  choice = numeric(0), ruler   = numeric(0)))
    
    # Returns a list of numeric vectors (one mean per student per condition)
    # When sd and n are present we reconstruct a synthetic sample so the
    # boxplot shows spread; otherwise we just use the mean.
    s <- p <- ch <- r <- c()
    
    for (ln in strsplit(trimws(raw), "\n")[[1]]) {
      ln <- trimws(ln)
      if (nchar(ln) == 0) next
      for (part in strsplit(ln, "\\|")[[1]]) {
        kv <- strsplit(trimws(part), ":")[[1]]
        if (length(kv) != 2) next
        label   <- trimws(kv[1])
        payload <- trimws(kv[2])
        if (payload == "NA") next
        
        nums <- suppressWarnings(as.numeric(strsplit(payload, ",")[[1]]))
        # Accept mean-only (legacy) or mean,sd,n
        m_val <- nums[1]
        if (is.na(m_val)) next
        
        switch(label,
               S = { s  <- c(s,  m_val) },
               P = { p  <- c(p,  m_val) },
               C = { ch <- c(ch, m_val) },
               R = { r  <- c(r,  m_val) }
        )
      }
    }
    list(simple = s, spatial = p, choice = ch, ruler = r)
  })
  
  output$class_plot_codes  <- renderPlot(
    class_boxplot(class_from_codes(), "Class RT by Condition (from codes)"), bg = col_bg)
  output$class_table_codes <- renderTable(class_summary(class_from_codes()),
                                          striped = TRUE, spacing = "s")
  
  # Simulated
  sim_class <- reactive({
    n <- input$sim_n; sdv <- input$sim_sd; set.seed(42)
    list(simple  = pmax(100, rnorm(n, 230, sdv)),
         spatial = pmax(150, rnorm(n, 320, sdv)),
         choice  = pmax(200, rnorm(n, 430, sdv)),
         ruler   = pmax(120, rnorm(n, 190, sdv)))
  })
  output$class_plot_sim  <- renderPlot(
    class_boxplot(sim_class(),
                  paste0("Simulated class (n = ", input$sim_n, " students)")),
    bg = col_bg)
  output$class_table_sim <- renderTable(class_summary(sim_class()),
                                        striped = TRUE, spacing = "s")
  
}  # end server

# =========================================================
shinyApp(ui = ui, server = server)
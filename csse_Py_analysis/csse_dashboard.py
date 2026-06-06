# =============================================================================
# CSSE 11+ Examination – Interactive Multi-Page Dashboard (Python / Plotly Dash)
# =============================================================================
# Prerequisites:
#   pip install dash plotly pandas scipy openpyxl dash-bootstrap-components
#
# Usage:
#   1.  Run csse_data_prep.py first to generate csse_cleaned.csv
#   2.  python csse_dashboard.py
#   3.  Open browser at http://127.0.0.1:8050
#
# Pages
# -----
#   Page 1 – Overview        : Year-by-year trend of mean/median scores
#   Page 2 – Age Effect      : Older / Middle / Younger age group analysis
#   Page 3 – Gender          : Male vs Female performance per subject & year
#   Page 4 – Thresholds      : % of candidates scoring ≥40 and ≥50
#   Page 5 – Percentile Tool : Look up what percentile any score falls in
# =============================================================================

import os
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

import dash
from dash import dcc, html, Input, Output, State, dash_table
import dash_bootstrap_components as dbc

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
DATA_DIR = os.environ.get("CSSE_DATA_DIR", PROJECT_ROOT)
CSV_PATH = os.path.join(DATA_DIR, "csse_cleaned.csv")

COLOURS = {
    "Male"   : "#2196F3",
    "Female" : "#E91E63",
    "Older"  : "#4CAF50",
    "Middle" : "#FF9800",
    "Younger": "#9C27B0",
    "English": "#00BCD4",
    "Maths"  : "#FF5722",
    "bg"     : "#0F1117",
    "card"   : "#1A1D23",
    "text"   : "#E8EAF6",
    "muted"  : "#9E9E9E",
    "accent" : "#7C4DFF",
}

FONT        = "Inter, Segoe UI, Arial, sans-serif"
MONTH_ORDER = ["Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr",
               "May","Jun","Jul","Aug"]
AGE_ORDER   = ["Older (Sep-Dec)", "Middle (Jan-Apr)", "Younger (May-Aug)"]


# ---------------------------------------------------------------------------
# LOAD DATA
# ---------------------------------------------------------------------------

df = pd.read_csv(CSV_PATH)
df["Birth_Month"] = pd.Categorical(df["Birth_Month"],
                                    categories=MONTH_ORDER, ordered=True)
df["Age_Group"]   = pd.Categorical(df["Age_Group"],
                                    categories=AGE_ORDER, ordered=True)
YEARS = sorted(df["Entry_Year"].unique())


# ---------------------------------------------------------------------------
# SHARED UTILITIES
# ---------------------------------------------------------------------------

def base_layout(title: str, xtitle: str = "", ytitle: str = "") -> dict:
    """Return a standard dark-theme Plotly layout dictionary."""
    return dict(
        title         = dict(text=title,
                             font=dict(size=15, color=COLOURS["text"]), x=0.01),
        paper_bgcolor = COLOURS["bg"],
        plot_bgcolor  = COLOURS["card"],
        font          = dict(family=FONT, color=COLOURS["text"], size=12),
        xaxis         = dict(title=xtitle, gridcolor="#2A2D35",
                             zeroline=False, tickfont=dict(size=11)),
        yaxis         = dict(title=ytitle, gridcolor="#2A2D35",
                             zeroline=False, tickfont=dict(size=11)),
        legend        = dict(bgcolor="rgba(0,0,0,0)", borderwidth=0),
        margin        = dict(l=58, r=20, t=52, b=55),
        hovermode     = "x unified",
    )


def card(children, **kwargs):
    return dbc.Card(
        dbc.CardBody(children),
        style={"backgroundColor": COLOURS["card"],
               "border": "1px solid #2A2D35", "borderRadius": "12px"},
        className="mb-3 shadow-sm",
        **kwargs,
    )


def kpi_card(label: str, value: str, colour: str = None):
    colour = colour or COLOURS["accent"]
    return dbc.Col(
        dbc.Card(dbc.CardBody([
            html.P(label, className="mb-1",
                   style={"color": COLOURS["muted"], "fontSize": "12px"}),
            html.H4(value, style={"color": colour, "fontWeight": "700"}),
        ]),
        style={"backgroundColor": COLOURS["card"],
               "border": f"1px solid {colour}30",
               "borderRadius": "10px"}),
        width="auto", className="mb-2",
    )


# ---------------------------------------------------------------------------
# PRE-COMPUTE SUMMARY TABLES  (avoids repeated groupby inside callbacks)
# ---------------------------------------------------------------------------

yr_stats = (
    df.groupby("Entry_Year")
    .agg(
        N          = ("Gender",      "count"),
        Eng_Mean   = ("English",     "mean"),
        Eng_Median = ("English",     "median"),
        Eng_SD     = ("English",     "std"),
        Mat_Mean   = ("Maths",       "mean"),
        Mat_Median = ("Maths",       "median"),
        Mat_SD     = ("Maths",       "std"),
    )
    .round(2)
    .reset_index()
)

gen_yr = (
    df.groupby(["Entry_Year", "Gender_Label"])
    .agg(
        N          = ("English",     "count"),
        Eng_Mean   = ("English",     "mean"),
        Eng_Median = ("English",     "median"),
        Mat_Mean   = ("Maths",       "mean"),
        Mat_Median = ("Maths",       "median"),
        Eng40_Pct  = ("Eng_Above40", "mean"),
        Eng50_Pct  = ("Eng_Above50", "mean"),
        Mat40_Pct  = ("Mat_Above40", "mean"),
        Mat50_Pct  = ("Mat_Above50", "mean"),
    )
    .round(3)
    .reset_index()
)
for c in ["Eng40_Pct","Eng50_Pct","Mat40_Pct","Mat50_Pct"]:
    gen_yr[c] = (gen_yr[c] * 100).round(1)

age_yr = (
    df.groupby(["Entry_Year", "Age_Group"], observed=True)
    .agg(Eng_Mean=("English","mean"), Mat_Mean=("Maths","mean"),
         N=("English","count"))
    .round(2)
    .reset_index()
)

thr = (
    df.groupby("Entry_Year")
    .agg(Eng40=("Eng_Above40","mean"), Eng50=("Eng_Above50","mean"),
         Mat40=("Mat_Above40","mean"), Mat50=("Mat_Above50","mean"))
    .reset_index()
)
for c in ["Eng40","Eng50","Mat40","Mat50"]:
    thr[c] = (thr[c] * 100).round(1)


# ============================================================================
# PAGE BUILDERS
# ============================================================================

def page_overview():
    # Combined trend figure
    fig_both = go.Figure()
    for subj, col in [("English", COLOURS["English"]),
                       ("Maths",   COLOURS["Maths"])]:
        m_col = subj[:3] + "_Mean"
        med_col = subj[:3] + "_Median"
        fig_both.add_trace(go.Scatter(
            x=yr_stats["Entry_Year"], y=yr_stats[m_col],
            mode="lines+markers", name=f"{subj} Mean",
            line=dict(color=col, width=3), marker=dict(size=9)))
        fig_both.add_trace(go.Scatter(
            x=yr_stats["Entry_Year"], y=yr_stats[med_col],
            mode="lines+markers", name=f"{subj} Median",
            line=dict(color=col, width=2, dash="dot"),
            marker=dict(size=7, symbol="diamond")))
    fig_both.update_layout(base_layout(
        "English & Maths – Mean & Median Trend (2021–2026)",
        xtitle="Entry Year", ytitle="Raw Score (0–60)"))

    def subj_trend(subj):
        col = COLOURS[subj]
        m_col   = subj[:3] + "_Mean"
        med_col = subj[:3] + "_Median"
        sd_col  = subj[:3] + "_SD"
        se = yr_stats[sd_col] / np.sqrt(yr_stats["N"])
        fig = go.Figure()
        x_rev = yr_stats["Entry_Year"][::-1]
        fig.add_trace(go.Scatter(
            x=pd.concat([yr_stats["Entry_Year"], x_rev]),
            y=pd.concat([yr_stats[m_col]+se, (yr_stats[m_col]-se)[::-1]]),
            fill="toself",
            fillcolor=f"rgba({int(col[1:3],16)},{int(col[3:5],16)},{int(col[5:7],16)},0.15)",
            line=dict(color="rgba(0,0,0,0)"),
            showlegend=False, hoverinfo="skip"))
        fig.add_trace(go.Scatter(
            x=yr_stats["Entry_Year"], y=yr_stats[m_col],
            mode="lines+markers", name="Mean",
            line=dict(color=col, width=3), marker=dict(size=9)))
        fig.add_trace(go.Scatter(
            x=yr_stats["Entry_Year"], y=yr_stats[med_col],
            mode="lines+markers", name="Median",
            line=dict(color=col, width=2, dash="dash"),
            marker=dict(size=8, symbol="diamond")))
        layout = base_layout(f"{subj} – Mean & Median by Year",
                             xtitle="Entry Year", ytitle="Raw Score (0–60)")
        layout["xaxis"]["tickmode"] = "linear"
        fig.update_layout(layout)
        return fig

    tbl = yr_stats.copy()
    tbl.columns = ["Year","N","Eng Mean","Eng Median","Eng SD",
                   "Maths Mean","Maths Median","Maths SD"]

    ey = yr_stats["Entry_Year"]
    r0 = yr_stats[yr_stats["Entry_Year"] == ey.min()].iloc[0]
    r1 = yr_stats[yr_stats["Entry_Year"] == ey.max()].iloc[0]
    d_eng = round(r1["Eng_Mean"] - r0["Eng_Mean"], 2)
    d_mat = round(r1["Mat_Mean"] - r0["Mat_Mean"], 2)

    kpis = dbc.Row([
        kpi_card("Latest Cohort Size",
                 f"{int(yr_stats[yr_stats['Entry_Year']==ey.max()]['N'].values[0]):,}"),
        kpi_card("Δ English Mean (2021→latest)",
                 f"{'+'if d_eng>=0 else ''}{d_eng}", COLOURS["English"]),
        kpi_card("Δ Maths Mean (2021→latest)",
                 f"{'+'if d_mat>=0 else ''}{d_mat}", COLOURS["Maths"]),
        kpi_card("Years of Data", f"{len(YEARS)}"),
    ], className="g-2 mb-2")

    return html.Div([
        html.H5("📈  Score Trends Overview",
                style={"color": COLOURS["text"], "marginBottom": "16px"}),
        kpis,
        card(dcc.Graph(figure=fig_both, config={"displayModeBar": False},
                       style={"height": "360px"})),
        dbc.Row([
            dbc.Col(card(dcc.Graph(figure=subj_trend("English"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
            dbc.Col(card(dcc.Graph(figure=subj_trend("Maths"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
        ]),
        card(dash_table.DataTable(
            data=tbl.to_dict("records"),
            columns=[{"name": c, "id": c} for c in tbl.columns],
            style_header={"backgroundColor": COLOURS["bg"],
                          "color": COLOURS["accent"],
                          "fontWeight": "600", "border": "none"},
            style_cell={"backgroundColor": COLOURS["card"],
                        "color": COLOURS["text"],
                        "border": "1px solid #2A2D35",
                        "textAlign": "center", "padding": "8px"},
            style_data_conditional=[{"if": {"row_index": "odd"},
                                      "backgroundColor": "#1F2229"}],
        )),
    ])


def page_age():
    age_clr = {
        "Older (Sep-Dec)" : COLOURS["Older"],
        "Middle (Jan-Apr)": COLOURS["Middle"],
        "Younger (May-Aug)": COLOURS["Younger"],
    }

    def age_line(subj):
        col = "Eng_Mean" if subj == "English" else "Mat_Mean"
        fig = go.Figure()
        for grp in AGE_ORDER:
            sub = age_yr[age_yr["Age_Group"] == grp]
            fig.add_trace(go.Scatter(
                x=sub["Entry_Year"], y=sub[col],
                mode="lines+markers", name=grp,
                line=dict(color=age_clr[grp], width=2.5),
                marker=dict(size=8)))
        fig.update_layout(base_layout(
            f"{subj} Mean by Age Group & Year",
            xtitle="Entry Year", ytitle=f"Mean {subj} Score"))
        return fig

    month_avg = (
        df.groupby("Birth_Month", observed=True)
        .agg(English=("English","mean"), Maths=("Maths","mean"))
        .reset_index().round(2)
    )
    bar_colours = [
        COLOURS["Older"]   if m in ["Sep","Oct","Nov","Dec"] else
        COLOURS["Middle"]  if m in ["Jan","Feb","Mar","Apr"] else
        COLOURS["Younger"]
        for m in month_avg["Birth_Month"].astype(str)
    ]
    fig_month = make_subplots(rows=1, cols=2,
                               subplot_titles=["English", "Maths"])
    for ci, (subj, ycol) in enumerate([("English","English"),("Maths","Maths")],1):
        fig_month.add_trace(go.Bar(
            x=month_avg["Birth_Month"].astype(str),
            y=month_avg[ycol],
            marker_color=bar_colours, showlegend=False,
            hovertemplate="%{x}: %{y:.2f}<extra></extra>"),
            row=1, col=ci)
    fig_month.update_layout(
        paper_bgcolor=COLOURS["bg"], plot_bgcolor=COLOURS["card"],
        font=dict(family=FONT, color=COLOURS["text"]),
        title=dict(text="Average Score by Birth Month (all years pooled)",
                   font=dict(size=15, color=COLOURS["text"]), x=0.01),
        margin=dict(l=50, r=20, t=60, b=50))
    fig_month.update_xaxes(gridcolor="#2A2D35")
    fig_month.update_yaxes(gridcolor="#2A2D35")

    fig_box = make_subplots(rows=1, cols=2, subplot_titles=["English","Maths"])
    for ci, subj in enumerate(["English","Maths"],1):
        for grp in AGE_ORDER:
            sub = df[df["Age_Group"] == grp][subj].dropna()
            fig_box.add_trace(go.Box(
                y=sub, name=grp.split(" ")[0],
                marker_color=age_clr[grp], boxmean="sd",
                showlegend=(ci==1)), row=1, col=ci)
    fig_box.update_layout(
        paper_bgcolor=COLOURS["bg"], plot_bgcolor=COLOURS["card"],
        font=dict(family=FONT, color=COLOURS["text"]),
        title=dict(text="Score Distribution by Age Group (all years pooled)",
                   font=dict(size=15, color=COLOURS["text"]), x=0.01),
        boxmode="group", margin=dict(l=50, r=20, t=60, b=50),
        legend=dict(bgcolor="rgba(0,0,0,0)"))
    fig_box.update_xaxes(gridcolor="#2A2D35")
    fig_box.update_yaxes(gridcolor="#2A2D35")

    return html.Div([
        html.H5("🎂  Age Effect on Performance",
                style={"color": COLOURS["text"], "marginBottom": "16px"}),
        dbc.Row([
            dbc.Col(card(dcc.Graph(figure=age_line("English"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
            dbc.Col(card(dcc.Graph(figure=age_line("Maths"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
        ]),
        card(dcc.Graph(figure=fig_month, config={"displayModeBar": False},
                       style={"height": "320px"})),
        card(dcc.Graph(figure=fig_box,   config={"displayModeBar": False},
                       style={"height": "360px"})),
    ])


def page_gender():
    def gen_line(subj):
        col = "Eng_Mean" if subj == "English" else "Mat_Mean"
        fig = go.Figure()
        for g in ["Male","Female"]:
            sub = gen_yr[gen_yr["Gender_Label"] == g]
            fig.add_trace(go.Scatter(
                x=sub["Entry_Year"], y=sub[col],
                mode="lines+markers", name=g,
                line=dict(color=COLOURS[g], width=2.5),
                marker=dict(size=9)))
        fig.update_layout(base_layout(
            f"{subj} Mean – Male vs Female",
            xtitle="Entry Year", ytitle=f"Mean {subj} Score"))
        return fig

    gap_wide = (
        gen_yr.pivot_table(index="Entry_Year", columns="Gender_Label",
                            values=["Eng_Mean","Mat_Mean"])
        .reset_index()
    )
    gap_wide.columns = ["Year","EngF","EngM","MatF","MatM"]
    gap_wide["EngGap"] = (gap_wide["EngF"] - gap_wide["EngM"]).round(2)
    gap_wide["MatGap"] = (gap_wide["MatF"] - gap_wide["MatM"]).round(2)

    fig_gap = go.Figure()
    fig_gap.add_trace(go.Bar(
        x=gap_wide["Year"], y=gap_wide["EngGap"],
        name="English (F − M)",
        marker_color=[COLOURS["English"] if v >= 0 else "#FF5252"
                      for v in gap_wide["EngGap"]]))
    fig_gap.add_trace(go.Bar(
        x=gap_wide["Year"], y=gap_wide["MatGap"],
        name="Maths (F − M)",
        marker_color=[COLOURS["Maths"] if v >= 0 else "#FF5252"
                      for v in gap_wide["MatGap"]]))
    fig_gap.add_hline(y=0, line_dash="dash",
                      line_color=COLOURS["muted"], line_width=1)
    gap_layout = base_layout(
        "Gender Gap (Female Mean − Male Mean) by Year",
        xtitle="Entry Year", ytitle="Score gap (positive = females higher)")
    gap_layout["barmode"] = "group"
    fig_gap.update_layout(gap_layout)

    return html.Div([
        html.H5("⚤  Gender Comparison",
                style={"color": COLOURS["text"], "marginBottom": "16px"}),
        dbc.Row([
            dbc.Col(card(dcc.Graph(figure=gen_line("English"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
            dbc.Col(card(dcc.Graph(figure=gen_line("Maths"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
        ]),
        card(dcc.Graph(figure=fig_gap, config={"displayModeBar": False},
                       style={"height": "320px"})),
        card([
            html.Label("Select year for score distribution:",
                       style={"color": COLOURS["muted"], "fontSize": "13px"}),
            dcc.Dropdown(
                id="gen-violin-year",
                options=[{"label": str(y), "value": y} for y in YEARS],
                value=max(YEARS), clearable=False,
                style={"backgroundColor": COLOURS["bg"],
                       "color": "#000", "width": "160px",
                       "marginBottom": "10px"}),
            dcc.Graph(id="gen-violin", config={"displayModeBar": False},
                      style={"height": "360px"}),
        ]),
    ])


def page_thresholds():
    def thr_line(subj):
        cols   = ["Eng40","Eng50"] if subj == "English" else ["Mat40","Mat50"]
        clrs   = ([COLOURS["English"], COLOURS["accent"]]
                  if subj == "English" else [COLOURS["Maths"], "#FF9800"])
        labels = [f"≥40 ({subj})", f"≥50 ({subj})"]
        fig    = go.Figure()
        for col, clr, lab in zip(cols, clrs, labels):
            fig.add_trace(go.Scatter(
                x=thr["Entry_Year"], y=thr[col],
                mode="lines+markers+text", name=lab,
                line=dict(color=clr, width=2.5), marker=dict(size=9),
                text=[f"{v}%" for v in thr[col]],
                textposition="top center",
                textfont=dict(size=10, color=clr)))
        layout = base_layout(f"{subj} – % Scoring ≥40 and ≥50",
                             xtitle="Entry Year", ytitle="% of Candidates")
        layout["yaxis"]["range"] = [0, 100]
        fig.update_layout(layout)
        return fig

    def gen_thr_bar(subj):
        col = "Eng40_Pct" if subj == "English" else "Mat40_Pct"
        fig = go.Figure()
        for g in ["Male","Female"]:
            sub = gen_yr[gen_yr["Gender_Label"] == g]
            fig.add_trace(go.Bar(
                x=sub["Entry_Year"], y=sub[col],
                name=g, marker_color=COLOURS[g]))
        layout = base_layout(f"{subj}: % ≥ 40 by Gender",
                             xtitle="Entry Year",
                             ytitle=f"% Candidates scoring ≥ 40")
        layout["barmode"] = "group"
        fig.update_layout(layout)
        return fig

    return html.Div([
        html.H5("🎯  Threshold Analysis",
                style={"color": COLOURS["text"], "marginBottom": "16px"}),
        dbc.Row([
            dbc.Col(card(dcc.Graph(figure=thr_line("English"),
                                   config={"displayModeBar": False},
                                   style={"height": "320px"})), width=6),
            dbc.Col(card(dcc.Graph(figure=thr_line("Maths"),
                                   config={"displayModeBar": False},
                                   style={"height": "320px"})), width=6),
        ]),
        dbc.Row([
            dbc.Col(card(dcc.Graph(figure=gen_thr_bar("English"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
            dbc.Col(card(dcc.Graph(figure=gen_thr_bar("Maths"),
                                   config={"displayModeBar": False},
                                   style={"height": "300px"})), width=6),
        ]),
    ])


def page_percentile():
    return html.Div([
        html.H5("🔍  Percentile Score Lookup",
                style={"color": COLOURS["text"], "marginBottom": "16px"}),
        card(dbc.Row([
            dbc.Col([
                html.Label("Subject", style={"color": COLOURS["muted"]}),
                dcc.Dropdown(
                    id="pct-subject",
                    options=[{"label":"English","value":"English"},
                             {"label":"Maths",  "value":"Maths"}],
                    value="English", clearable=False,
                    style={"backgroundColor": COLOURS["bg"],
                           "color": "#000"}),
            ], width=3),
            dbc.Col([
                html.Label("Score (0–60)", style={"color": COLOURS["muted"]}),
                dcc.Input(
                    id="pct-score", type="number",
                    min=0, max=60, step=1, value=40,
                    style={"backgroundColor": COLOURS["bg"],
                           "color": COLOURS["text"],
                           "border": "1px solid #2A2D35",
                           "borderRadius": "6px",
                           "padding": "8px", "width": "100%"}),
            ], width=3),
            dbc.Col([
                html.Br(),
                dbc.Button("Look Up", id="pct-btn",
                           style={"backgroundColor": COLOURS["accent"],
                                  "border": "none"}),
            ], width=2, className="d-flex align-items-center"),
        ], className="g-3")),
        card(dcc.Graph(id="pct-chart", config={"displayModeBar": False},
                       style={"height": "380px"})),
        card(html.Div(id="pct-table")),
    ])


# ============================================================================
# APP LAYOUT
# ============================================================================

app = dash.Dash(
    __name__,
    external_stylesheets=[dbc.themes.DARKLY],
    suppress_callback_exceptions=True,
    meta_tags=[{"name": "viewport",
                "content": "width=device-width, initial-scale=1"}],
)
app.title = "CSSE 11+ Analysis Dashboard"

app.layout = html.Div(
    style={"backgroundColor": COLOURS["bg"], "minHeight": "100vh",
           "fontFamily": FONT},
    children=[
        dbc.Navbar(
            dbc.Container([
                html.Span("📊  CSSE 11+ Dashboard",
                          style={"color": COLOURS["text"],
                                 "fontWeight": "700", "fontSize": "18px"}),
                dbc.Nav([
                    dbc.NavLink("Overview",   href="/",       active="exact"),
                    dbc.NavLink("Age Effect", href="/age",    active="exact"),
                    dbc.NavLink("Gender",     href="/gender", active="exact"),
                    dbc.NavLink("Thresholds", href="/thresh", active="exact"),
                    dbc.NavLink("Percentile Lookup", href="/pct", active="exact"),
                ], navbar=True, className="ms-auto"),
            ]),
            color=COLOURS["card"], dark=True, sticky="top",
            style={"borderBottom": "1px solid #2A2D35"},
        ),
        dcc.Location(id="url"),
        dbc.Container(id="page-content", fluid=True,
                      style={"padding": "24px",
                             "backgroundColor": COLOURS["bg"]}),
    ],
)


# ============================================================================
# CALLBACKS
# ============================================================================

@app.callback(Output("page-content", "children"),
              Input("url", "pathname"))
def render_page(pathname):
    routes = {
        "/"       : page_overview,
        "/age"    : page_age,
        "/gender" : page_gender,
        "/thresh" : page_thresholds,
        "/pct"    : page_percentile,
    }
    return routes.get(pathname, page_overview)()


@app.callback(Output("gen-violin", "figure"),
              Input("gen-violin-year", "value"))
def update_violin(year):
    sub = df[df["Entry_Year"] == year]
    fig = go.Figure()
    for g in ["Male","Female"]:
        s   = sub[sub["Gender_Label"] == g]
        clr = COLOURS[g]
        for subj in ["English","Maths"]:
            fig.add_trace(go.Violin(
                x=[subj] * len(s), y=s[subj].dropna(),
                name=g,
                side="negative" if g == "Male" else "positive",
                line_color=clr,
                fillcolor="rgba({},{},{},0.25)".format(
                    int(clr[1:3],16), int(clr[3:5],16), int(clr[5:7],16)),
                meanline_visible=True,
                legendgroup=g,
                showlegend=(subj == "English")))
    fig.update_layout(base_layout(
        f"Score Distribution by Gender – {year} Entry",
        ytitle="Raw Score"))
    fig.update_layout(violinmode="overlay")
    return fig


@app.callback(
    [Output("pct-chart", "figure"),
     Output("pct-table", "children")],
    Input("pct-btn", "n_clicks"),
    [State("pct-subject", "value"),
     State("pct-score",   "value")],
    prevent_initial_call=False,
)
def update_percentile(_clicks, subject, score):
    if score is None:
        score = 40
    results = []
    for yr in YEARS:
        scores = df[df["Entry_Year"] == yr][subject].dropna()
        pct    = round(float(np.mean(scores <= score) * 100), 1)
        results.append({"Year": yr, "Subject": subject,
                         "Score": score, "Percentile": pct,
                         "N": len(scores)})
    res = pd.DataFrame(results)

    bar_clrs = ["#4CAF50" if p >= 75 else "#FF9800" if p >= 50 else "#F44336"
                for p in res["Percentile"]]
    fig = go.Figure()
    fig.add_trace(go.Bar(
        x=res["Year"], y=res["Percentile"],
        marker_color=bar_clrs,
        text=[f"{p}th" for p in res["Percentile"]],
        textposition="outside",
        hovertemplate="Year: %{x}<br>Percentile: %{y:.1f}th<extra></extra>"))
    fig.add_hline(y=50, line_dash="dash",
                  line_color=COLOURS["muted"],
                  annotation_text="50th pct",
                  annotation_position="top right",
                  annotation_font_color=COLOURS["muted"])
    layout = base_layout(
        f"Percentile rank of score {score} in {subject} by Year",
        xtitle="Entry Year", ytitle="Percentile Rank")
    layout["yaxis"]["range"] = [0, 108]
    fig.update_layout(layout)

    table = dash_table.DataTable(
        data=res.to_dict("records"),
        columns=[{"name": c, "id": c}
                 for c in ["Year","Subject","Score","Percentile","N"]],
        style_header={"backgroundColor": COLOURS["bg"],
                      "color": COLOURS["accent"],
                      "fontWeight": "600", "border": "none"},
        style_cell={"backgroundColor": COLOURS["card"],
                    "color": COLOURS["text"],
                    "border": "1px solid #2A2D35",
                    "textAlign": "center", "padding": "8px"},
        style_data_conditional=[
            {"if": {"filter_query": "{Percentile} >= 75"},
             "color": "#4CAF50", "fontWeight": "600"},
            {"if": {"filter_query": "{Percentile} < 50"},
             "color": "#F44336"},
        ],
    )
    return fig, table


# ============================================================================
# ENTRY POINT
# ============================================================================

if __name__ == "__main__":
    print("=" * 55)
    print("  CSSE 11+ Dashboard – starting ...")
    print("  Open your browser at  http://127.0.0.1:8050")
    print("=" * 55)
    app.run(debug=False, port=8050)

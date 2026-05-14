\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{lab-sop-v05}[2026 Simplified SOP class]

% Base class
\LoadClass[11pt]{article}

% ---------- Page layout ----------
\RequirePackage[left=1in,right=1in,top=1in,bottom=1in]{geometry}

% ---------- Packages ----------
\RequirePackage{enumitem}
\RequirePackage{amssymb}
\RequirePackage{gensymb}
\RequirePackage{titlesec}
\RequirePackage{xcolor}
\RequirePackage{hyperref}
\RequirePackage{fancyhdr}
\RequirePackage{lastpage}
\RequirePackage{multirow}
\RequirePackage{graphicx}

% ---------- Hyperref ----------
\hypersetup{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=blue,
  citecolor=blue
}

% ---------- Section formatting ----------
\setcounter{secnumdepth}{1}

\titleformat{\section}
  {\normalfont\bfseries}
  {\makebox[2em][l]{\thesection.}}
  {0pt}{}

\titleformat{\subsection}
  {\normalfont\bfseries}
  {\makebox[2em][l]{\thesubsection.}}
  {0pt}{}

% ---------- Numbered paragraphs ----------
\newcounter{paranum}[section]
\renewcommand{\theparanum}{\thesection.\arabic{paranum}}

\newcommand{\NP}{%
  \par\vspace{8pt}\noindent%
  \refstepcounter{paranum}%
  \textbf{\theparanum}\space
}

% ---------- Bullets ----------
\def\labelitemi{\textcolor{gray}{\raisebox{.45ex}{\rule{.6ex}{.6ex}}}}

% ---------- SOP metadata ----------
\newcommand{\approved}[1]{\def\theapproved{#1}}
\newcommand{\SOPno}[1]{\def\theSOPno{#1}}
\newcommand{\ReviseDate}[1]{\def\theReviseDate{#1}}

% ---------- Title ----------
\RequirePackage{titling}

\let\oldtitle\title
\renewcommand{\title}[1]{\oldtitle{#1}\def\mythetitle{#1}}

\renewcommand{\maketitle}{%
\begin{center}
\begin{tabular}{|c|p{2.7in}|p{1.1in}|p{1.1in}|} \hline
\multirow{3}{*}{\includegraphics[width=2cm]{pomona.jpg}} &
\centering Environmental Analysis Teaching and Research Laboratory &
Date: \thedate &
SOP No. \theSOPno \\ \cline{2-4}
&
\centering Standard Operating Procedure &
\multicolumn{2}{l|}{Title: \mythetitle} \\ \cline{2-4}
&
Approved By: \theapproved &
\multicolumn{2}{l|}{Revised: \theReviseDate} \\ \hline
\end{tabular}
\end{center}
\vspace{1em}
}

% ---------- Headers / Footers ----------
\fancypagestyle{plain}{
  \fancyhf{}
  \fancyfoot[L]{Author: \theauthor}
  \fancyfoot[R]{Page \thepage\ of \pageref*{LastPage}}
}

\pagestyle{fancy}
\fancyhf{}
\fancyhead[R]{SOP \theSOPno\ (\theReviseDate)}
\fancyfoot[L]{Author: \theauthor}
\fancyfoot[R]{Page \thepage\ of \pageref*{LastPage}}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

\endinput
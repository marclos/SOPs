\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{lab-sop}[2026 ISO-style Lab SOP]

\LoadClass[11pt]{article}

% ---------- Page layout ----------
\RequirePackage{geometry}
\geometry{margin=1in}


\AtBeginDocument{%
  \setcounter{secnumdepth}{0}%
}

% ---------- Fonts & spacing ----------
\RequirePackage{setspace}
\onehalfspacing

% ---------- Internal metadata storage ----------
\makeatletter
\newcommand{\SOP@Title}{}
\newcommand{\SOP@Code}{}
\newcommand{\SOP@Version}{}
\newcommand{\SOP@Date}{}
\newcommand{\SOP@Author}{}
\newcommand{\SOP@ApprovedBy}{}

% ---------- Public setter commands ----------
\newcommand{\SOPTitle}[1]{\gdef\SOP@Title{#1}}
\newcommand{\SOPCode}[1]{\gdef\SOP@Code{#1}}
\newcommand{\SOPVersion}[1]{\gdef\SOP@Version{#1}}
\newcommand{\SOPDate}[1]{\gdef\SOP@Date{#1}}
\newcommand{\SOPAuthor}[1]{\gdef\SOP@Author{#1}}
\newcommand{\SOPApprovedBy}[1]{\gdef\SOP@ApprovedBy{#1}}
\makeatother

% ---------- Headers/footers ----------
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\small \SOP@Code}
\fancyhead[R]{\small Version \SOP@Version}
\fancyfoot[C]{\thepage}


% ---------- Section formatting ----------
\RequirePackage{titlesec}
\titleformat{\section}{\large\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}{\normalsize\bfseries}{\thesubsection}{1em}{}

% ---------- Title block ----------
\makeatletter
\renewcommand{\maketitle}{
  \begin{center}
    {\LARGE\bfseries \SOP@Title\par}
    \vspace{1em}
    \begin{tabular}{ll}
      SOP Code:&\SOP@Code \\
      Version:&\SOP@Version \\
      Effective Date:&\SOP@Date \\
      Author:&\SOP@Author \\
      Approved By:&\SOP@ApprovedBy \\
    \end{tabular}
  \end{center}
  \vspace{2em}
}
\makeatother

% ---------- Revision history ----------
\RequirePackage{longtable}
\newcommand{\RevisionTable}{
  \section*{Revision History}
  \begin{longtable}{p{2cm} p{2.5cm} p{3cm} p{5cm}}
    \textbf{Version} & \textbf{Date} & \textbf{Author} & \textbf{Description} \\
    \hline
  \end{longtable}
}

% ---------- Signature table ----------
\newcommand{\SignatureTable}{
  \section*{Approval Signatures}
  \begin{tabular}{p{5cm} p{4cm} p{4cm}}
    \textbf{Name} & \textbf{Signature} & \textbf{Date} \\
    \\[1.5em]
  \end{tabular}
}

% ---------- Safety box ----------
\RequirePackage{tcolorbox}
\newtcolorbox{safetybox}{
  colback=yellow!10,
  colframe=black,
  title=Safety Notice
}
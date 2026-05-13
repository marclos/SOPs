\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{lab-sop}[2026 ISO-style Lab SOP]

\LoadClass[11pt]{article}

% --- Page layout ---
  \RequirePackage{geometry}
\geometry{margin=1in}

% --- Fonts & spacing ---
  \RequirePackage{setspace}
\onehalfspacing

% --- Headers/footers ---
  \RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[L]{\small \SOPCode}
\fancyhead[R]{\small Version \SOPVersion}
\fancyfoot[C]{\thepage}

% --- Section formatting ---
  \RequirePackage{titlesec}
\titleformat{\section}{\large\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}{\normalsize\bfseries}{\thesubsection}{1em}{}

% --- Meta data fields ---
  \newcommand{\SOPTitle}{}
\newcommand{\SOPCode}{}
\newcommand{\SOPVersion}{}
\newcommand{\SOPDate}{}
\newcommand{\SOPAuthor}{}
\newcommand{\SOPApprovedBy}{}

\newcommand{\SOPsetup}[6]{
  \renewcommand{\SOPTitle}{#1}
    \renewcommand{\SOPCode}{#2}
      \renewcommand{\SOPVersion}{#3}
        \renewcommand{\SOPDate}{#4}
          \renewcommand{\SOPAuthor}{#5}
            \renewcommand{\SOPApprovedBy}{#6}
            }
            
            % --- Title block ---
              \renewcommand{\maketitle}{
                \begin{center}
                {\LARGE\bfseries \SOPTitle \par}
                \vspace{1em}
                \begin{tabular}{ll}
                SOP Code: & \SOPCode \\
                Version: & \SOPVersion \\
                Effective Date: & \SOPDate \\
                Author: & \SOPAuthor \\
                Approved By: & \SOPApprovedBy \\
                \end{tabular}
                \end{center}
                \vspace{2em}
              }
            
            % --- Revision history ---
              \RequirePackage{longtable}
            
            \newcommand{\RevisionTable}{
              \section*{Revision History}
              \begin{longtable}{p{2cm} p{2.5cm} p{3cm} p{5cm}}
              \textbf{Version} & \textbf{Date} & \textbf{Author} & \textbf{Description} \\
              \hline
              \end{longtable}
            }
            
            % --- Signature table ---
              \newcommand{\SignatureTable}{
                \section*{Approval Signatures}
                \begin{tabular}{p{5cm} p{4cm} p{4cm}}
                \textbf{Name} & \textbf{Signature} & \textbf{Date} \\
                \\[1.5em]
                \end{tabular}
              }
            
            % --- Safety box ---
              \RequirePackage{tcolorbox}
            \newtcolorbox{safetybox}{
              colback=yellow!10,
              colframe=black,
              title=Safety Notice
            }
% =========================================================
  % SOP5.cls
% Standard Operating Procedure Document Class
% Modernized and Portable Version
% =========================================================
  
  \NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SOP5}[2026/05/10 SOP document class]

% ---------------------------------------------------------
  % Base Class
% ---------------------------------------------------------
  
  \DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax
\LoadClass[12pt]{article}

% ---------------------------------------------------------
  % Required Packages
% ---------------------------------------------------------
  
  \RequirePackage[left=1in,right=1in,top=1in,bottom=1in]{geometry}

\RequirePackage{graphicx}
\RequirePackage{xcolor}
\RequirePackage{hyperref}
\RequirePackage{titlesec}
\RequirePackage{enumitem}
\RequirePackage{fancyhdr}
\RequirePackage{lastpage}
\RequirePackage{titling}
\RequirePackage{multirow}
\RequirePackage{tabularx}
\RequirePackage{textcomp}
\RequirePackage{xspace}
\RequirePackage{gensymb}
\RequirePackage{amssymb}

% ---------------------------------------------------------
  % Graphics Search Paths
% ---------------------------------------------------------
  
  \graphicspath{
    {./}
    {../}
    {../../}
    {assets/}
    {../assets/}
    {../../assets/}
  }

% ---------------------------------------------------------
  % Hyperref Setup
% ---------------------------------------------------------
  
  \hypersetup{
    colorlinks=true,
    citecolor=blue,
    filecolor=blue,
    linkcolor=blue,
    urlcolor=blue
  }

% ---------------------------------------------------------
  % General Formatting
% ---------------------------------------------------------
  
  \setlength{\parindent}{1.5em}
\setlength{\parskip}{0.5em}

\let\@afterindentfalse\@afterindenttrue
\@afterindenttrue

% ---------------------------------------------------------
  % Compact List Environments
% ---------------------------------------------------------
  
  \newenvironment{itemize*}
{
  \begin{itemize}[itemsep=2pt,parsep=0pt,topsep=2pt]
}
{
  \end{itemize}
}

\newenvironment{enumerate*}
{
  \begin{enumerate}[itemsep=2pt,parsep=0pt,topsep=2pt]
}
{
  \end{enumerate}
}

\newenvironment{description*}
{
  \begin{description}[itemsep=2pt,parsep=0pt,topsep=2pt]
}
{
  \end{description}
}

% ---------------------------------------------------------
  % Bullet Styling
% ---------------------------------------------------------
  
  \renewcommand{\labelitemi}{
    \textcolor{gray}{
      \raisebox{0.3ex}{\rule{0.6ex}{0.6ex}}
    }
  }

% ---------------------------------------------------------
  % Section Formatting
% ---------------------------------------------------------
  
  \setcounter{secnumdepth}{2}
\setcounter{tocdepth}{2}

\titleformat{\section}
{\normalfont\large\bfseries}
{\thesection.\quad}
{0pt}
{}

\titleformat{\subsection}
{\normalfont\normalsize\bfseries}
{\thesubsection.\quad}
{0pt}
{}

\titleformat{\subsubsection}
{\normalfont\small\bfseries}
{\thesubsubsection.\quad}
{0pt}
{}

\titlespacing{\section}{0pt}{2ex}{1ex}
\titlespacing{\subsection}{0pt}{1.5ex}{0.5ex}
\titlespacing{\subsubsection}{1em}{1ex}{0.25ex}

% ---------------------------------------------------------
  % SOP Numbered Paragraphs
% ---------------------------------------------------------
  
  \newcounter{paranum}[section]

\renewcommand{\theparanum}{
  \thesection.\arabic{paranum}
}

\newcommand{\NP}{
  \par\medskip
  \refstepcounter{paranum}
  \noindent
  \hangindent=1.5cm
  \hangafter=1
  \textbf{\theparanum}\quad
}

% ---------------------------------------------------------
  % Utility Commands
% ---------------------------------------------------------
  
  \newcommand{\latex}{\LaTeX\xspace}
\newcommand{\tex}{\TeX\xspace}

\newcommand{\tilda}{
  \raisebox{0.5ex}{\texttildelow}
}

% ---------------------------------------------------------
  % Metadata Defaults
% ---------------------------------------------------------
  
  \def\theapproved{}
\def\theSOPno{}
\def\theReviseDate{}
\def\theDepartment{Biogeochemistry Laboratory}
\def\theSOPType{Standard Operating Procedure}
\def\theLogo{pomona.jpg}

% ---------------------------------------------------------
  % Metadata Setters
% ---------------------------------------------------------
  
  \newcommand{\approved}[1]{\gdef\theapproved{#1}}
    \newcommand{\SOPno}[1]{\gdef\theSOPno{#1}}
      \newcommand{\ReviseDate}[1]{\gdef\theReviseDate{#1}}
        \newcommand{\Department}[1]{\gdef\theDepartment{#1}}
          \newcommand{\SOPType}[1]{\gdef\theSOPType{#1}}
            \newcommand{\SOPLogo}[1]{\gdef\theLogo{#1}}
              
              % ---------------------------------------------------------
                % Improved Title Handling
              % ---------------------------------------------------------
                
                \let\oldtitle\title
              
              \renewcommand{\title}[1]{
                \oldtitle{#1}
                  \gdef\mythetitle{#1}
                  }
                  
                  % ---------------------------------------------------------
                    % SOP Title Block
                  % ---------------------------------------------------------
                    
                    \renewcommand{\maketitle}{
                      
                      \thispagestyle{plain}
                      
                      \vspace*{-1em}
                      
                      \noindent
                      \begin{tabularx}{\textwidth}{|c|X|l|l|}
                      \hline
                      
                      \multirow{3}{*}{
                        \includegraphics[width=2cm]{\theLogo}
                      }
                      &
                        \centering \theDepartment
                      &
                        Date: \thedate
                      &
                        SOP No. \theSOPno
                      \\ \cline{2-4}
                      
                      &
                        \centering \theSOPType
                      &
                        \multicolumn{2}{X|}{
                          Title: \mythetitle
                        }
                      \\ \cline{2-4}
                      
                      &
                        Approved By: \theapproved
                      &
                        \multicolumn{2}{l|}{
                          Revised: \theReviseDate
                        }
                      \\ \hline
                      
                      \end{tabularx}
                      
                      \vspace{2em}
                    }
                  
                  % ---------------------------------------------------------
                    % Headers and Footers
                  % ---------------------------------------------------------
                    
                    \fancypagestyle{plain}{
                      \fancyhf{}
                      
                      \fancyfoot[L]{
                        \framebox{
                          Author: \theauthor
                        }
                      }
                      
                      \fancyfoot[R]{
                        \framebox{
                          Page: \thepage\ of \pageref*{LastPage}
                        }
                      }
                      
                      \renewcommand{\headrulewidth}{0pt}
                      \renewcommand{\footrulewidth}{0pt}
                    }
                  
                  \pagestyle{fancy}
                  
                  \fancyhf{}
                  
                  \fancyhead[R]{
                    \begin{tabular}{|c|}
                    \hline
                    SOP: \theSOPno \\
                    Revised: \theReviseDate \\
                    \hline
                    \end{tabular}
                  }
                  
                  \fancyfoot[L]{
                    \framebox{
                      Author: \theauthor
                    }
                  }
                  
                  \fancyfoot[R]{
                    \framebox{
                      Page: \thepage\ of \pageref*{LastPage}
                    }
                  }
                  
                  \renewcommand{\headrulewidth}{0pt}
                  \renewcommand{\footrulewidth}{0pt}
                  
                  % ---------------------------------------------------------
                    % End of Class
                  % ---------------------------------------------------------
                    
                    \endinput
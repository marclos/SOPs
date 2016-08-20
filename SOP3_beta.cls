% SOP Template Document Class
% 3 Beta 
%   - removed numbers for subsections
% Next work: retain numbered subsection and include them in the \NP
% Spacing in sections with two decimals is kerning funny

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{../SOP3_beta}[2016/08/08 v3.2 Modified article class for standard operating procedures]
% http://stackoverflow.com/questions/581916/how-do-you-extend-article-document-class-in-latex

% Passes and class options to the underlying article class
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}

% Some valuable packages
\RequirePackage{enumitem} % Customization of itemized, enumerated, etc.
\RequirePackage{hyperref} % For hyperlinked cross-references
\RequirePackage{hanging}
\RequirePackage{gensymb} % For specific symbols, such as the degree symbol

%\newenvironment{hangparas}[2]{\setlength{\parindent}{\z@}
%\everypar={\hangpara{#1}{#2}}}{\par}

%\usepackage{indentfirst} or these lines:
\let\@afterindentfalse\@afterindenttrue
\@afterindenttrue

% Redefine the page margins
\RequirePackage[left=1in,right=1in,top=1in,bottom=1in]{geometry}

% Modify enumerate/itemize
\newenvironment{itemize*}%
  {\begin{itemize}%
    \setlength{\itemsep}{0pt}%
    \setlength{\parskip}{0pt}}%
  {\end{itemize}}
	
\newenvironment{enumerate*}%
  {\begin{enumerate}%
    \setlength{\itemsep}{0pt}%
    \setlength{\parskip}{0pt}}%
  {\end{enumerate}}
  
% Modify bullet format
\RequirePackage{amssymb}
%\def\labelitemi{\scriptsize$\blacksquare$}
\RequirePackage{xcolor}

\def\labelitemi{\textcolor{gray}{\raisebox{.45ex}{\rule{.6ex}{.6ex}}}}

% Modifications to the section titles
\RequirePackage{titlesec}
%\renewcommand{\thesection}{\Roman{section}}

\titleformat{\section}{\normalfont\bfseries}{\makebox[2em][l]{\thesection{}.}}{0pt}{}

% Subsection Option
\titleformat{\subsection}{\normalfont\bfseries}{}{0pt}{}
\titleformat{\subsection}{\normalfont\bfseries}{\makebox[2em][l]{\thesubsection{}.}}{0pt}{}

% Create Numbered Paragraphs for Each Section
%\setlength{\parindent}{4em}

\newcounter{paranum}[section] 
\renewcommand{\theparanum}{\thesection.\arabic{paranum}} 
\setcounter{secnumdepth}{1} % Number only Sections


% still attempting the hanging indent
\newcommand{\NP}{\par\vspace{10pt}\indent\hangindent=1.4cm\refstepcounter{paranum}\textbf{\theparanum}\ }
%\setlength\parindent{1cm}

% Modification of title block
\RequirePackage{titling}
\RequirePackage{multirow}
\newcommand{\approved}[1]{\newcommand{\theapproved}{#1}}
\newcommand{\SOPno}[1]{\newcommand{\theSOPno}{#1}}
\newcommand{\ReviseDate}[1]{\newcommand{\theReviseDate}{#1}}
% Ref: http://tex.stackexchange.com/questions/3988/titlesec-versus-titling-mangling-thetitle
\let\oldtitle\title
\renewcommand{\title}[1]{\oldtitle{#1}\newcommand{\mythetitle}{#1}}
\renewcommand{\maketitle}{%
\begin{tabular}{|c|p{2in}|l|l|} \hline
\multirow{3}{*}{\includegraphics[width=2cm]{../pomona.jpg}} & \multicolumn{1}{p{2.5in}|}{\centering Environmental Analysis Teaching and Research Laboratory} & Date: \thedate & Number: \theSOPno \\ \cline{2-4}
& \multicolumn{1}{p{2.5in}|}{\centering Standard Operating Procedure } & \multicolumn{2}{p{2.5in}|}{Title: \mythetitle} \\ \cline{2-4}
& Approved By: \theapproved & \multicolumn{2}{l|}{Revision Date: \theReviseDate} \\ \hline
\end{tabular}
}

% For "Page N of M"
\RequirePackage{lastpage}
% For easier construction of page headers/footers
\RequirePackage{fancyhdr}
\fancypagestyle{plain}{ % for first page
\fancyhf{}
\fancyfoot[L]{\framebox{Author: \theauthor}\\ File: \jobname{}.tex} 
\fancyfoot[R]{\framebox{Page: \thepage{} of \pageref*{LastPage}}}
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}
}

\pagestyle{fancy} % for other pages
\fancyhf{}
\fancyhead[R]{%
\begin{tabular}{|c|} \hline %
SOP: \theSOPno ~ (Revised: \theReviseDate) \\ \hline
\end{tabular}%
}
\fancyfoot[L]{\framebox{Author: \theauthor}}
\fancyfoot[R]{\framebox{Page: \thepage{} of \pageref*{LastPage}}} % \pageref* if we use hyperref, \pageref otherwise
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{0pt}

% Ensure first page is correct style
\thispagestyle{plain}
% That's all, folks!
\endinput
% SOP Template Document Class
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SOP2}[2016/07/31 v0.3 Modified article class for standard operating procedures]
% http://stackoverflow.com/questions/581916/how-do-you-extend-article-document-class-in-latex

% Passes and class options to the underlying article class
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}

% Some valuable packages
\RequirePackage{hyperref}

% Redefine the page margins
\RequirePackage[left=1in,right=1in,top=1in,bottom=1in]{geometry}


% Modifications to the section titles
\RequirePackage{titlesec}
%\renewcommand{\thesection}{\Roman{section}}

\titleformat{\section}{\normalfont\bfseries}{\makebox[2em][l]{\thesection{}.}}{0pt}{}

% Subsection Option
\titleformat{\subsection}{\normalfont\bfseries}{}{0pt}{}
\titleformat{\subsection}{\normalfont\bfseries}{\makebox[2em][l]{\thesubsection{}.}}{0pt}{}

% Create Numbered Paragraphs for Each Section
\newcounter{paranum}[section] 
\renewcommand{\theparanum}{\thesection.\arabic{paranum}} 
\newcommand{\NP}{\vspace{10pt}\noindent\refstepcounter{paranum}\textbf{\theparanum}\ }

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

% For easier customization of itemized, enumerated, and other lists
\RequirePackage{enumitem}
% For hyperlinked cross-references
\RequirePackage{hyperref}
% Ensure first page is correct style
\thispagestyle{plain}
% That's all, folks!
\endinput